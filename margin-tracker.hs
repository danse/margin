{-# LANGUAGE RecordWildCards #-}

import Control.Exception (try)
import Control.Monad (when)
import Data.Bool (bool)
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

data StepState = Logging | Prompt

data State = State {
  _stateActivities :: Activities,
  _stateContext :: [String],
  stateStep :: StepState
  }

data Message = Added String
             | Elapsed Float
             | Started String
             | CancelTracking
             | QuitMessage

instance Show Message where
  show (Added description) = "added " <> description <> " to margin file"
  show (Elapsed h)
    | h >= 1    = show h <> " hours"
    | otherwise = show m <> " minutes"
    where m = h * 60
  show (Started activity) = "⏳ started logging, " <> activity
  show CancelTracking = "tracking discarded"
  show QuitMessage = "quitting context"

step :: State -> IO [Margin]

step (State activities context Prompt) = do
  let o = Just $ Seline.Options (access activities == Shorter)
  s <- Seline.seline o (activityList activities) context
  case s of
    Just selection -> step (State activities selection Logging)
    Nothing -> pure []

step state@(State _ context Logging) = do
  eitherInterval <- track
  either onTrackingError onInterval eitherInterval
  where nextStep = step state { stateStep = Prompt }
        description = unwords (reverse context)
        track :: IO (Either IOError (UTCTime, UTCTime))
        track = do
          print (Started description)
          t1 <- getCurrentTime
          eitherEnter <- try getLine
          t2 <- getCurrentTime
          pure $ (t1, t2) <$ eitherEnter
        onInterval :: (UTCTime, UTCTime) -> IO [Margin]
        onInterval (start, stop) =
          let
            fixToFloat = fromRational . toRational
            seconds = fixToFloat $ nominalDiffTimeToSeconds $
                      stop `diffUTCTime` start
            hours = seconds / 3600
          in do
            print (Elapsed hours)
            nextData <- nextStep
            pure $ Margin hours description start : nextData
        onTrackingError :: IOError -> IO [Margin]
        onTrackingError _ = do
          print CancelTracking
          nextStep

defActivities :: [FilePath] -> [FilePath]
defActivities [] = ["margin-tracker-activities"]
defActivities o  = o

data Options = Options {
  activityFiles :: [String],
  marginFile :: Maybe String,
  marginDescriptionLimit :: Maybe Int,
  verbose :: Bool,
  short :: Bool
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
  let parsed = parseMargins $ either (const []) id eitherMargin
      taken = take (fromMaybe 2000 marginDescriptionLimit) parsed
      access = bool Chronological Shorter short
  when verbose (showMargins parsed taken)
  let
    paths = defActivities activityFiles
    readDefaults = mapM readFile paths
    tryIO :: IO a -> IO (Either IOError a)
    tryIO = try
    emptyDefaults = fmap (either (const []) id) . tryIO
  defaultActivities <- emptyDefaults readDefaults
  let
    activities = makeActivities access (foldMap lines defaultActivities) taken
  m <- step (State activities [] Prompt)
  addMarginsToMaybeFile m marginFile
  print . Elapsed . sum . fmap Margin.value $ m

  where
    explainParsing error = do
      print (error :: IOError)
      putStrLn "no activities will be taken by the file"
      pure $ Right []
    fromEnd f = reverse . f . reverse
    recentLast = fromEnd nub
    parseMargins margin = recentLast ((words . description) =<< margin)
    showMargins parsed taken = do
      putStrLn $ "parsed: " <> show parsed
      putStrLn $ "taken:  " <> show taken
