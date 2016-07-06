module Main where

import HlAdif
import HlOptions
import Options.Applicative
import qualified Data.Text.IO as TIO

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
    contentList <- mapM TIO.readFile $ files opt
    (mergeLogs <$> mapM (parseErrorHandler . adifLogParser) contentList) >>= (putStr . show)
    (writeLog <$> mergeLogs <$> mapM (parseErrorHandler . adifLogParser) contentList) >>= TIO.putStr

main :: IO ()
main = execParser optionsParserInfo >>= doMerge
