{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe
import Data.Text.IO hiding (putStrLn)
import HlAdif
import HlOptions
import Options.Applicative
import Prelude hiding (readFile, putStr)
import System.Environment
import System.IO hiding (readFile, putStr)
import Data.Text

data Options = Options
  { basedir :: String
  , outputFormat :: OutputFormat
  }

getOptionsParserInfo :: IO (ParserInfo Options)
getOptionsParserInfo = do
    homeOption <- getHomeOption
    return $ info (helper <*> (
        Options
            <$> homeOption
            <*> outputFormatOption
      )) (
        fullDesc
            <> progDesc "Export the ADIF database to a single file"
      )

doExport :: Options -> IO ()
doExport opt = do
    parseResult <- adifLogParser <$> readFile (basedir opt ++ "/data/hl.adi")
    case parseResult of
        Left errorMsg -> putStrLn errorMsg
        Right log -> do
            case outputFormat opt of
                LIST -> putStrLn $ show log
                ADIF -> do
                    putStr $ writeLog log

main :: IO ()
main = getOptionsParserInfo >>= execParser >>= doExport
