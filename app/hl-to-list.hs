{-# LANGUAGE OverloadedStrings #-}
module Main where

import HlAdif
import HlOptions
import Options.Applicative
import Prelude hiding (readFile, putStr)
import System.IO hiding (readFile, putStr)
import qualified Data.ByteString.Char8 as B

data Options = Options
  { getInputHandle :: IO Handle
  }

getOptionsParserInfo :: IO (ParserInfo Options)
getOptionsParserInfo = do
    return $ info (helper <*> (
        Options
            <$> inputHandleArgument
      )) (
        fullDesc
            <> progDesc "Present an ADIF file as a list"
      )

main :: IO ()
main = getOptionsParserInfo >>= execParser >>= \opt -> do
    h <- getInputHandle opt
    parseResult <- adifLogParser <$> B.hGetContents h

    case parseResult of
        Left errorMsg -> putStrLn errorMsg
        Right l -> putStrLn $ show l
