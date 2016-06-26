module Main where

import Data.Maybe
import Data.Text.IO hiding (putStrLn)
import HlAdif
import HlOptions
import Options.Applicative
import Prelude hiding (readFile)
import System.Environment
import System.IO hiding (readFile)

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
doExport dbo = do
    parseResult <- adifLogParser <$> readFile (basedir dbo ++ "/data/hl.adi")
    case parseResult of
        Left errorMsg -> putStrLn errorMsg
        Right log     -> putStrLn $ show log

main :: IO ()
main = do
    x <- getOptionsParserInfo
    execParser x >>= doExport
