{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Margin
  (Margin(..),
   addMarginsToMaybeFile,
   addToMaybeFile,
   formatMarginContents,
   label,
   onAllMargins,
   parseMarginFile,
   parseMaybeFile,
  )
where

import Control.Monad (when, void, join)
import Data.Aeson(eitherDecode, FromJSON, ToJSON, eitherDecode, encode )
import Data.ByteString.Lazy( readFile, writeFile )
import Data.Either( rights, lefts, isRight)
import Data.Functor( (<$>) )
import Data.Maybe( fromMaybe )
import Data.Semigroup
import Data.Time.Clock (UTCTime)
import Data.Time( getCurrentTime )
import GHC.Generics
import System.Directory( doesFileExist )
import Data.Csv ((.:), (.=))

import qualified Data.Csv as Csv
import qualified Data.Vector as Vector
import qualified Data.Time.ISO8601 as ISO8601
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as ByteString

defaultFilePath = "data.margin"

data Margin = Margin {
  value :: Float,
  description :: String,
  time :: UTCTime
  } deriving (Show, Generic)

instance FromJSON Margin
instance ToJSON Margin

instance Csv.FromField UTCTime where
  parseField packed =
    let t = Char8.unpack packed
    in case ISO8601.parseISO8601 t of
      Just time -> pure time
      Nothing -> fail (t <> " is not an ISO8601 time")

instance Csv.ToField UTCTime where
  toField = Char8.pack . ISO8601.formatISO8601Millis

instance Csv.FromNamedRecord Margin where
  parseNamedRecord r = Margin
    <$> r .: "value"
    <*> r .: "description"
    <*> r .: "time"

instance Csv.ToNamedRecord Margin where
  toNamedRecord m = Csv.namedRecord [
    "value" .= value m,
    "description" .= description m,
    "time" .= time m]

instance Csv.DefaultOrdered Margin where
  headerOrder _ = Csv.header ["time", "value", "description"]

parseMarginFile :: FilePath -> IO (Either String [Margin])
parseMarginFile path = do
  putStrLn ("reading file " <> path)
  contents <- Data.ByteString.Lazy.readFile path
  case Csv.decodeByName contents of
    Right (_, margins) -> pure $ Right $ Vector.toList margins
    Left err -> do
      putStrLn $ err <> ", trying old JSON format"
      let eitherDecoded = eitherDecode contents
      when (isRight eitherDecoded) (putStrLn "this JSON margin format is deprecated")
      pure eitherDecoded

handledPure
  :: ([Margin] -> a) -> FilePath -> Either String [Margin] -> IO (Maybe a)
handledPure f p = either h (pure . Just . f)
  where h e = putStrLn (p `label` e) >> pure Nothing

handledParse :: ([Margin] -> a) -> FilePath -> IO (Maybe a)
handledParse f = (>>=) <$> parseMarginFile <*> handledPure f

formatMarginContents :: [Margin] -> ByteString.ByteString
formatMarginContents = Csv.encodeDefaultOrderedByName

formatMarginFile :: [Margin] -> FilePath -> IO ()
formatMarginFile margins path =
  Data.ByteString.Lazy.writeFile path $ formatMarginContents margins

getAllMargins :: [String] -> IO [Margin]
getAllMargins paths = do
  eitherDecoded <- mapM parseMarginFile paths
  mapM_ print (lefts eitherDecoded)
  pure ((concat . rights) eitherDecoded)

-- helper for command line scripts. Gets a list of paths and a
-- function to execute on them if they are all correctly parsed as
-- margins
onAllMargins :: ([Margin] -> a) -> [FilePath] -> IO [a]
onAllMargins g = mapMaybe (handledParse g) . defaultMany defaultFilePath

catMaybes
  :: (Applicative t, Monoid (t a), Foldable t)
  => t (Maybe a) -> t a
catMaybes = foldr ((<>) . maybe mempty pure) mempty

mapMaybe
  :: (Traversable t, Applicative t, Applicative f, Monoid (t b), Monoid (t a))
  => (a -> f (Maybe b)) -> t a -> f (t b)
mapMaybe g = fmap catMaybes . traverse g

defaultMany :: (Foldable f, Applicative f) => a -> f a -> f a
defaultMany d m = if null m then pure d else m

label :: Show s => String -> s -> String
label a b = a <> ": " <> show b

marginNow :: Float -> String -> IO Margin
marginNow value description = Margin value description <$> getCurrentTime

applyToMargin
  :: FilePath
  -> (Maybe [Margin] -> [Margin])
  -> IO (Either String [Margin])
applyToMargin path f = do
  exists <- doesFileExist path
  if exists
    then
    do
      eitherParsed <- parseMarginFile path
      case eitherParsed of
        Left error -> pure (Left error)
        Right decoded -> do
          formatMarginFile processed path
          pure (Right processed)
            where processed = f (Just decoded)
    else
    do
      formatMarginFile processed path
      pure (Right processed)
        where processed = f Nothing

addMargins :: [Margin] -> Maybe [Margin] -> [Margin]
addMargins newMargins maybeMargins = fromMaybe [] maybeMargins <> newMargins

addToMaybeFile :: Maybe FilePath -> (Float, String) -> IO ()
addToMaybeFile mPath (value, description) = do
  newMargin <- marginNow value description
  addMarginsToMaybeFile [newMargin] mPath

addMarginsToMaybeFile :: [Margin] -> Maybe FilePath -> IO ()
addMarginsToMaybeFile margins maybeFilePath = void $
  applyToMargin (fromMaybe defaultFilePath maybeFilePath) (addMargins margins)

parseMaybeFile = parseMarginFile . fromMaybe defaultFilePath
