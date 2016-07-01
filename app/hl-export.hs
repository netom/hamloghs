module Main where

import Data.Maybe
import Data.Text.IO hiding (putStrLn)
import HlAdif
import HlOptions
import Options.Applicative
import Prelude hiding (readFile, putStr)
import System.Environment
import System.IO hiding (readFile, putStr)

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
    db <- readFile (basedir opt ++ "/data/hl.adi")
    case outputFormat opt of
        LIST -> do
            let parseResult = adifLogParser db
            case parseResult of
                Left errorMsg -> putStrLn errorMsg
                Right log     -> putStrLn $ show log
        ADIF -> putStr db

main :: IO ()
main = do
    x <- getOptionsParserInfo
    execParser x >>= doExport
