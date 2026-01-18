{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center
import qualified Brick.Types as T
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Data.List.Split (chunksOf)
import qualified Data.IntMap as I
import           Data.IntMap ((!))
import qualified Data.Text as T
import           Data.Foldable (fold, for_)
import           Data.List (sortOn)
import           Data.Text (Text, pack, unpack)
import           Data.Time.Calendar (Day, fromGregorian, addDays, isLeapYear, dayOfWeek, DayOfWeek(..))
import qualified Graphics.Vty as V
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events (Button(..))
import           System.Environment (getArgs)
import           System.Random (randomRIO)

main :: IO AppState
main = do
  args <- getArgs
  case args of
    [yearStr] -> do
      let year = read yearStr :: Integer
      runApp year
    _ -> error "Usage: git-history-painter <year>"

runApp :: Integer -> IO AppState
runApp year = do
  let daysInYear = if isLeapYear year then 366 else 365
      firstDay = fromGregorian year 1 1
      startOffset = dayOfWeekToGitHubRow (dayOfWeek firstDay)
  defaultMain (app year startOffset daysInYear) $
    AS { days = I.fromList $ map (, L0) [1..daysInYear]
       , drawWorkLevel = L1
       }

-- Convert DayOfWeek to GitHub heatmap row (0=Sunday, 6=Saturday)
-- Note: time library uses ISO 8601: Monday=1, ..., Saturday=6, Sunday=7
dayOfWeekToGitHubRow :: DayOfWeek -> Int
dayOfWeekToGitHubRow dow = fromEnum dow `mod` 7

data AppState =
  AS { days:: I.IntMap WorkLevel
     , drawWorkLevel:: WorkLevel
     }

app :: Integer -> Int -> Int -> App AppState () [T.Text]
app year startOffset daysInYear = App
  { appDraw = drawUI startOffset daysInYear
  , appStartEvent = enableMouseSupport
  , appHandleEvent = handleEvent year
  , appAttrMap = const $ attrMap (bg black) attributes
  , appChooseCursor = neverShowCursor
  }

enableMouseSupport :: EventM n s ()
enableMouseSupport = do
  vty <- Brick.getVtyHandle
  let output = V.outputIface vty
  when (V.supportsMode output V.Mouse) $
      liftIO $ V.setMode output V.Mouse True

l0Attr :: AttrName
l0Attr = attrName "l0Attr"

l1Attr :: AttrName
l1Attr = attrName "l1Attr"

l2Attr :: AttrName
l2Attr = attrName "l2Attr"

l3Attr :: AttrName
l3Attr = attrName "l3Attr"

l4Attr :: AttrName
l4Attr = attrName "l4Attr"


attributes :: [(AttrName, Attr)]
attributes = [ (l0Attr, bg $ rgbColor (235 :: Int) 237 240)
             , (l1Attr, bg $ rgbColor (155 :: Int) 233 168)
             , (l2Attr, bg $ rgbColor (64  :: Int) 196 99)
             , (l3Attr, bg $ rgbColor (48  :: Int) 161 78)
             , (l4Attr, bg $ rgbColor (33  :: Int) 110  57)
             ]

data WorkLevel = L0 | L1 | L2 | L3 | L4

data Cell = Empty | DayCell Int

drawUI :: Int -> Int -> AppState -> [Widget [T.Text]]
drawUI startOffset daysInYear AS{days, drawWorkLevel} =
  fold [ [ vBox [ clickable [T.pack "WriteScript"] $ border (str "Write Script")
                , clickable [T.pack "DrawWorkLevel"] . toBox $ workLevelToName drawWorkLevel
                ]
         ]
       , [center . hBox $ fmap vBox $ chunksOf 7 $ mkCell <$> cells]
       ]
  where
    -- Build cells: padding before Jan 1, then actual days, then padding to complete last column
    totalBeforePadding = startOffset + daysInYear
    endPadding = (7 - (totalBeforePadding `mod` 7)) `mod` 7
    cells = [Empty | _ <- [1..startOffset]]
         ++ [DayCell d | d <- [1..daysInYear]]
         ++ [Empty | _ <- [1..endPadding]]
    mkCell Empty = emptyBox
    mkCell (DayCell d) = clickable (toName d) . toBox . workLevelToName $ days ! d
    emptyBox = withBorderStyle unicode $ vLimit 2 $ hLimit 3 $ fill ' '
    toBox wln = withBorderStyle unicode $ vLimit 2 $ hLimit 3 $ withAttr wln (fill ' ')
    toName ix = ["Day", T.pack $ show ix]

handleEvent :: Integer -> BrickEvent [Text] e -> EventM n AppState ()
handleEvent _ (T.VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent _ (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent year (MouseDown name BLeft _ _) = case name of
    ["WriteScript"] -> do
      ds <- gets days
      let dsSorted = sortOn fst $ I.toList ds
      liftIO $ do
        for_ dsSorted $ \(d, wl) -> do
          for_ [1.. workLevelToCommitCount wl] $ \i -> do
            com <- generateCommitCommand year d
            writeCommandToFile $ fold ["echo -n '", pack $ show (d,i), "' > ", touchFile, " && git add ", touchFile, " && ", com]
    ["DrawWorkLevel"] -> do
      modify (\s@AS { drawWorkLevel = dwl } -> s { drawWorkLevel = cycleWorkLevel dwl })
    ["Day", dayText] -> do
      let day = read $ T.unpack dayText
      AS ds dwl <- get
      let ds' = I.adjust (const dwl) day ds
      put $ AS ds' dwl
    attr -> error $ "unknown widget: " <> unpack (fold attr)

handleEvent _ (MouseDown name BRight _ _) = case name of
    ["Day", dayText] -> do
        -- Handle the click for the day
      let day = read $ T.unpack dayText
      AS ds dwl <- get
      let ds' = I.adjust cycleWorkLevel day ds
      put $ AS ds' dwl
    attr -> error $ "unknown widget: " <> unpack (fold attr)
handleEvent _ _ = pure ()

workLevelToCommitCount :: WorkLevel -> Integer
workLevelToCommitCount = \case
 L0 -> 0
 L1 -> 1
 L2 -> 2
 L3 -> 4
 L4 -> 8

cycleWorkLevel :: WorkLevel -> WorkLevel
cycleWorkLevel = \case
 L0 -> L1
 L1 -> L2
 L2 -> L3
 L3 -> L4
 L4 -> L0

workLevelToName :: WorkLevel -> AttrName
workLevelToName = \case
 L0 -> l0Attr
 L1 -> l1Attr
 L2 -> l2Attr
 L3 -> l3Attr
 L4 -> l4Attr

-- Randomly pick an element from a list
randomPick :: [a] -> IO a
randomPick xs = do
    index <- randomRIO (0, length xs - 1)
    return $ xs !! index

generateCommitMessage :: IO Text
generateCommitMessage = do
    verb <- randomPick verbs
    noun <- randomPick nonsenseNouns
    return $ fold [verb, " the ", noun]

touchFile :: Text
touchFile = "cromulate.txt"

scriptFile :: FilePath
scriptFile = "commits.sh"

writeCommandToFile :: Text -> IO ()
writeCommandToFile = appendFile scriptFile . unpack

generateCommitCommand :: Integer -> Int -> IO Text
generateCommitCommand year day = do
  msg <- generateCommitMessage
  let startDate :: Day
      startDate = fromGregorian year 1 1
      -- Calculate the actual date
      targetDate :: Day
      targetDate = addDays (fromIntegral day - 1) startDate
      -- Assuming noon for the commit time
      formattedDate = T.pack (show targetDate) <> "T12:00:00"
  pure $ fold ["GIT_AUTHOR_DATE='"
              , formattedDate
              , "' GIT_COMMITTER_DATE='"
              , formattedDate
              , "' git commit -m '"
              , msg
              , "'\n"
              ]
verbs :: [Text]
verbs = legitVerbs <> nonsenseVerbs

legitVerbs :: [Text]
legitVerbs =
  [ "remove"
  , "fix"
  , "update version of"
  , "update dependencies of"
  , "optimize"
  , "improve performance of"
  , "refactor"
  , "document"
  , "add tests for"
  , "enhance error handling of"
  ]

nonsenseVerbs :: [Text]
nonsenseVerbs =
  [ "thromble"
  , "mimbrenute"
  , "confortimate"
  , "zorf"
  , "jorstle"
  , "flibulate"
  , "quort"
  , "snorfitate"
  , "plumble"
  , "trillicate"
  , "vorfenize"
  , "drindle"
  , "zimberfy"
  , "frumblest"
  , "jibberate"
  , "clorficate"
  , "blibberfy"
  , "tranzip"
  , "smorfle"
  , "thribulate"
  , "glintify"
  , "storbulate"
  , "fribnicate"
  , "slurfitate"
  , "quiblify"
  , "trontificate"
  , "drouble"
  , "snarflicate"
  , "crimbulate"
  , "blortify"
  , "sporfle"
  , "drumbulate"
  , "zwibble"
  , "clurficate"
  , "trobbulate"
  , "flornify"
  , "scrunticate"
  , "brindlefy"
  , "chomble"
  , "twiblify"
  ]

nonsenseNouns :: [Text]
nonsenseNouns =
  [ "flibbertron"
  , "squorfen"
  , "brindlebox"
  , "trontifier"
  , "zorfleplume"
  , "jorstletank"
  , "clurficube"
  , "snorflestone"
  , "plumblecrate"
  , "thribulorb"
  , "drindlescope"
  , "vorfenwick"
  , "blibberstow"
  , "glintifridge"
  , "twiblisphere"
  , "crimbulator"
  , "droublenest"
  , "morflegrid"
  , "slurfitank"
  , "sporflewave"
  , "quiblisphere"
  , "storbulette"
  , "fribnibox"
  , "troblinator"
  , "zwibblesphere"
  , "clintifrack"
  , "smorfling"
  , "blortifan"
  , "chomblewick"
  , "drumblestow"
  , "flornibridge"
  , "scruntipod"
  , "blimblerack"
  , "tranziploom"
  , "spiblicrate"
  , "thwibbleton"
  , "flurbicube"
  , "stronfiblate"
  , "glorfipane"
  , "triblifix"
  ]
