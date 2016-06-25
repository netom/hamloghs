module Main where

import Data.Maybe
import Data.Text.IO hiding (putStrLn)
import HlAdif
import HlOptions
import Options.Applicative
import Prelude hiding (readFile)
import System.Environment
import System.IO hiding (readFile)

data DatabaseOptions = DatabaseOptions
  { basedir :: String
  }

getDatabaseOptionsParserInfo :: IO (ParserInfo DatabaseOptions)
getDatabaseOptionsParserInfo = do
    homeOption <- getHomeOption

    return $ info (helper <*> (
        DatabaseOptions
            <$> homeOption
      )) (
        fullDesc
            <> progDesc "Export the ADIF database to a single file"
      )

doExport :: DatabaseOptions -> IO ()
doExport dbo = do
    putStrLn "Exporting ADIF..."
    parsedLog <- testParser <$> readFile "/home/netom/.local/share/WSJT-X/wsjtx_log.adi"
    putStrLn $ show parsedLog
    putStrLn "Export complete."

main :: IO ()
main = do
    x <- getDatabaseOptionsParserInfo
    execParser x >>= doExport
