module Main where

import HlAdif
import HlOptions
import Options.Applicative
import System.Environment
import Data.List
import Data.Time.Clock
import Data.Time.Calendar
import Text.Printf
import GHC.Int
import Data.Text hiding (filter, isPrefixOf, map, drop)

data Options = Options
  { tags :: [Tag]
  }

getEnvTags :: IO [Tag]
getEnvTags = do
    env <- filter (isPrefixOf "HL_T_" . fst) <$> getEnvironment
    return $ map (\(k,v)->toTag (drop 5 k, Just v)) env

getCalculatedTags :: IO [Tag]
getCalculatedTags = do
    c <- getCurrentTime
    let
        (year,month,day) = toGregorian $ utctDay c
        udt              = truncate (utctDayTime c) :: Int64
        (hour,hm)        = udt `divMod` 3600
        minute           = hm `div` 60
        datestr          = printf "%04d%02d%02d" year month day
        timestr          = printf "%02d%02d" hour minute
    return [QSO_DATE (pack datestr), TIME_ON (pack timestr)]

getOptionsParserInfo :: IO (ParserInfo Options)
getOptionsParserInfo = do
    calcTags <- getCalculatedTags
    envTags  <- getEnvTags

    let defaultTags = mergeTags calcTags envTags

    return $ info (helper <*> (
        Options
            <$> adifRecordArguments defaultTags
      )) (
        fullDesc
            <> progDesc "Create an ADIF record"
      )

doRecord :: Options -> IO ()
doRecord opt = do
    let fs = tags opt
    putStr $ unpack $ writeRecord $ record fs

main :: IO ()
main = do
    x <- getOptionsParserInfo
    execParser x >>= doRecord
