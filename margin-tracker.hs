{-# LANGUAGE RecordWildCards #-}

import Control.Exception (try)
import Control.Monad (when)
import Data.Bool (bool)
import Data.Either (fromRight)
import Data.Fixed (Pico)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Margin
import Options.Applicative
import System.IO.Error ()

import qualified Seline

-- activities start

data Access = Shorter | Chronological deriving Eq

data Activities = Activities {
  strings :: [String] ,
  access :: Access }

makeActivities
  :: Access
  -> [String]
  -- ^ Defaults
  -> [String]
  -- ^ Additional
  -> Activities
makeActivities a d t = Activities (t <> d) a

activityList :: Activities -> [String]
activityList a =
  let f = case access a of
        Shorter -> id
        Chronological -> reverse
  in f . strings $ a

-- activities end

data State = State {
  _stateActivities :: Activities,
  _stateContext :: [String]
  }

data Message = Added String
             | Elapsed Float
             | Started String
             | CancelTracking

instance Show Message where
  show (Added description) = "added " <> description <> " to margin file"
  show (Elapsed h)
    | h >= 1    = show h <> " hours"
    | otherwise = show m <> " minutes"
    where m = h * 60
  show (Started activity) = "⏳ started logging, " <> activity
  show CancelTracking = "tracking discarded"

loop :: Monad f => (s -> f (s, Maybe a)) -> s -> f [a]
loop f s1 = do
  a <- f s1
  case a of
    (_, Nothing) -> pure []
    (s2, Just a) -> do
      l <- loop f s2
      pure (a:l)

prompt, track :: State -> IO (State, Maybe Margin)

prompt state@(State activities context) = do
  let o = Just $ Seline.Options (access activities == Shorter)
  s <- Seline.seline o (activityList activities) context
  case s of
    Just selection -> track (State activities selection)
    Nothing -> pure (state, Nothing)

track state@(State _ context) = do
  eitherInterval <- track
  either onTrackingError onInterval eitherInterval
  where description = unwords (reverse context)
        track :: IO (Either IOError (UTCTime, UTCTime))
        track = do
          print (Started description)
          t1 <- getCurrentTime
          eitherEnter <- try getLine
          t2 <- getCurrentTime
          pure $ (t1, t2) <$ eitherEnter
        onInterval :: (UTCTime, UTCTime) -> IO (State, Maybe Margin)
        onInterval (start, stop) =
          let
            fixToFloat :: Pico -> Float
            fixToFloat = fromRational . toRational
            seconds = fixToFloat $ nominalDiffTimeToSeconds $
                      stop `diffUTCTime` start
            hours = seconds / 3600
          in do
            print (Elapsed hours)
            pure (state, Just $ Margin hours description start)
        onTrackingError :: IOError -> IO (State, Maybe Margin)
        onTrackingError _ = do
          print CancelTracking
          prompt state

defActivities :: [FilePath] -> [FilePath]
defActivities [] = ["margin-tracker-activities"]
defActivities o  = o

data Options = Options {
  activityFiles :: [String],
  marginFile :: Maybe String,
  marginDescriptionLimit :: Maybe Int,
  verbose :: Bool,
  shortFirst :: Bool
  }

options :: Parser Options
options = Options
          <$> many (strOption (long "activities"))
          <*> optional (argument str (metavar "FILE.margin"))
          <*> optional (option auto (long "margin-description-limit"))
          <*> switch (long "verbose")
          <*> switch (long "access-short-first")

main :: IO ()
main = do
  Options{..} <- execParser (info options fullDesc)
  eitherEitherMargin <- try $ parseMaybeFile marginFile
  eitherMargin <- either explainParsing pure eitherEitherMargin
  let parsed = parseMargins . fromRight [] $ eitherMargin
      taken = take (fromMaybe 2000 marginDescriptionLimit) parsed
      access = bool Chronological Shorter shortFirst
  when verbose (showMargins parsed taken)
  let
    paths = defActivities activityFiles
    readDefaults = mapM readFile paths
    tryIO :: IO a -> IO (Either IOError a)
    tryIO = try
    emptyDefaults = fmap (fromRight []) . tryIO
  defaultActivities <- emptyDefaults readDefaults
  let
    activities = makeActivities access (foldMap lines defaultActivities) taken
  m <- loop prompt (State activities [])
  addMarginsToMaybeFile m marginFile
  print . Elapsed . sum . fmap Margin.value $ m

  where
    explainParsing err = do
      print (err :: IOError)
      putStrLn "no activities will be taken by the file"
      pure $ Right []
    fromEnd f = reverse . f . reverse
    recentLast = fromEnd nub
    parseMargins margin = recentLast ((words . description) =<< margin)
    showMargins parsed taken = do
      putStrLn $ "parsed: " <> show parsed
      putStrLn $ "taken:  " <> show taken
