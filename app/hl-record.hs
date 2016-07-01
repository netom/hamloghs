module Main where

import HlOptions
import Options.Applicative

data Options = Options
  { fields :: [String]
  }

getOptionsParserInfo :: IO (ParserInfo Options)
getOptionsParserInfo = do
    return $ info (helper <*> (
        Options
            <$> adifOptions
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
