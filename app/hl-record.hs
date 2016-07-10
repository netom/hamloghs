{-# LANGUAGE OverloadedStrings #-}
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
    return $ map (\(k,v)->toTag (toUpper $ pack $ drop 5 k, (Nothing, Just $ pack v))) env

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
    return [toTag ("QSO_DATE", (Nothing, Just $ pack datestr)), toTag ("TIME_ON", (Nothing, Just $ pack timestr))]

getOptionsParserInfo :: IO (ParserInfo Options)
getOptionsParserInfo = do

    return $ info (helper <*> (
        Options
            <$> adifRecordArguments
      )) (
        fullDesc
            <> progDesc "Create an ADIF record"
      )

doRecord :: Options -> IO ()
doRecord opt = do
    calcTags <- getCalculatedTags
    envTags  <- getEnvTags

    let fs = mergeTags [tags opt, envTags, calcTags]

    putStr $ unpack $ writeRecord $ toRecord fs

main :: IO ()
main = do
    x <- getOptionsParserInfo
    execParser x >>= doRecord
