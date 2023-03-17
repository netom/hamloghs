{-# LANGUAGE OverloadedStrings #-}
module Main where

import HlAdif
import HlOptions
import Options.Applicative
import Prelude hiding (readFile, putStr)
import qualified Data.ByteString.Char8 as B

data Options = Options
  { basedir :: String
  }

getOptionsParserInfo :: IO (ParserInfo Options)
getOptionsParserInfo = do
    homeOption <- getHomeOption
    return $ info (helper <*> (
        Options
            <$> homeOption
      )) (
        fullDesc
            <> progDesc "Export the ADIF database to a single file"
      )

doExport :: Options -> IO ()
doExport opt = do
    parseResult <- adifLogParser <$> B.readFile (basedir opt ++ "/data/hl.adi")
    case parseResult of
        Left errorMsg -> putStrLn errorMsg
        Right l -> B.putStr $ writeLog l

main :: IO ()
main = getOptionsParserInfo >>= execParser >>= doExport
