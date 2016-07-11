module Main where

import HlAdif
import HlLog
import HlOptions
import Options.Applicative
import qualified Data.ByteString.Char8 as B

data Options = Options
  { files :: [String]
  }

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (helper <*> (
        Options
            <$> inputFileArguments
      )) (
        fullDesc
            <> progDesc "Merge multiple ADIF files into a single one"
      )

parseErrorHandler :: Either String Log -> IO Log
parseErrorHandler (Left errorMessage) = fail errorMessage
parseErrorHandler (Right log) = return log

doMerge :: Options -> IO ()
doMerge opt = do
    contentList <- mapM B.readFile $ files opt
    (writeLog <$> mergeLogs <$> mapM (parseErrorHandler . adifLogParser) contentList) >>= B.putStr

main :: IO ()
main = execParser optionsParserInfo >>= doMerge
