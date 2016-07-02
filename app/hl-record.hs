module Main where

import HlAdif
import HlOptions
import Options.Applicative
import System.Environment
import Data.List

data Options = Options
  { fields :: [Tag]
  }

getDefaultTags :: IO [Tag]
getDefaultTags = do
    env <- filter (isPrefixOf "HL_T_" . fst) <$> getEnvironment
    print env
    return []
    

getOptionsParserInfo :: IO (ParserInfo Options)
getOptionsParserInfo = do
    defaultTags <- getDefaultTags
    return $ info (helper <*> (
        Options
            <$> adifRecordArguments defaultTags
      )) (
        fullDesc
            <> progDesc "Create an ADIF record"
      )

doRecord :: Options -> IO ()
doRecord opt = do
    print $ fields opt

main :: IO ()
main = do
    x <- getOptionsParserInfo
    execParser x >>= doRecord
