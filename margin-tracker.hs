{-# LANGUAGE RecordWildCards #-}

import Control.Exception (try, Exception)
import Control.Monad (join, sequence, when)
import Data.Bool (bool)
import Data.List (partition, intercalate, sortOn, nub, (\\))
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Set (
  Set (..),
  fromList,
  singleton,
  toList,
  difference,
  union)
import Data.Time.Clock
import Margin
import Options.Applicative
import Safe (atMay)
import System.Environment (getArgs)
import System.IO.Error ()
import System.Posix.Time
import System.Posix.Types
import Text.Read (readMaybe)

-- activities start

newtype Activities = Activities { strings :: [String] }
data Access = Shorter | Chronological deriving Eq

makeActivities :: Access -> [String] -> [String] -> Activities
makeActivities _ defaults = Activities . (<> defaults)

activityList :: Access -> Activities -> [String]
activityList Shorter = sortOn length . strings
activityList Chronological = reverse . strings

enumerate :: Access -> Activities -> [(Int, String)]
enumerate Shorter =
  zip [0..] . activityList Shorter
enumerate Chronological =
  reverse . zip [0..] . activityList Chronological

append :: [String] -> Activities -> Activities
append new activities = Activities $ strings activities <> new

consume :: [String] -> Activities -> Activities
consume consumed activities = Activities $ strings activities \\ consumed

-- activities end

data StepState = Logging | Prompt

data State = State {
  stateActivities :: Activities,
  stateContext :: [String],
  stateStep :: StepState
  }

data Message = Enumerate Access State
             | Added String
             | Elapsed Float
             | Started String
             | CancelTracking
             | QuitMessage

instance Show Message where
  show (Enumerate access state) =
    let enumerated :: (Int, String) -> String
        enumerated (i, s) = s <> " · " <> show i
        activities = intercalate ",  " $ fmap enumerated $ enumerate access
                   $ stateActivities state
        context = intercalate " > " (reverse $ stateContext state)
    in activities <> "\n" <> context
  show (Added description) = "added " <> description <> " to margin file"
  show (Elapsed h)
    | h >= 1    = show h <> " hours"
    | otherwise = show m <> " minutes"
    where m = h * 60
  show (Started activity) = "⏳ started logging " <> activity
  show CancelTracking = "tracking discarded"
  show QuitMessage = "quitting context"

data Command = Quit | Deepen Access String | Continue deriving Eq

interpretCommand :: Access -> Either IOError String -> Command
interpretCommand _ (Left _) = Quit
interpretCommand _ (Right "") = Continue
interpretCommand access (Right s) = Deepen access s

-- @updateState@ is used only on @Prompt@ states
updateState :: Command -> State -> State
updateState Quit state@(State activities context Prompt) =
  state {
    stateContext = tail context,
    stateActivities = append (words (head context)) activities
    }
updateState Continue state = state { stateStep = Logging }
updateState (Deepen access userLine) state@(State activities context Prompt) =
  let
    selected :: Maybe String
    selected = readMaybe userLine >>= atMay (activityList access activities)
    selection = fromMaybe userLine selected
  in state {
    stateContext = selection:context,
    stateActivities = consume (words selection) activities
    }

step :: Access -> State -> IO [Margin]

step access state@(State _ context Prompt) = do
  print (Enumerate access state)
  userLine <- try getLine
  let command = interpretCommand access userLine
  if command == Quit && null context
    then return []
    else step access (updateState command state)

step access state@(State _ context Logging) = do
  eitherInterval <- track
  either onTrackingError onInterval eitherInterval
  where nextStep = step access state { stateStep = Prompt }
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
  m <- step access (State activities [] Prompt)
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
