{-# LANGUAGE OverloadedStrings #-}
module Main where

import HlAdif
import HlLog
import HlOptions
import Options.Applicative
import System.IO hiding (readFile, putStr)
import Data.Char
import Data.Csv
import Data.Vector as V
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (fromStrict)

data AppOptions = AppOptions
  { getInputHandle :: IO Handle
  , separator :: Char
  }

getOptionsParserInfo :: IO (ParserInfo AppOptions)
getOptionsParserInfo = do
    return $ info (helper <*> (
        AppOptions
            <$> inputHandleArgument
            <*> separatorOption
      )) (
        fullDesc
            <> progDesc "Convert a CSV file to ADIF"
      )

main :: IO ()
main = getOptionsParserInfo >>= execParser >>= \opt -> do
    h <- getInputHandle opt
    contents <- B.hGetContents h

    let decopts = DecodeOptions $ fromIntegral $ ord $ separator opt

    let result = decodeWith decopts NoHeader (fromStrict contents) :: Either String (Vector (Vector B.ByteString))

    case result of
        Left errMsg   -> putStrLn errMsg
        Right csvdata -> do
            let headr = csvdata V.! 0
            forM_ (V.tail csvdata) $ \rec -> do
                B.putStr $ writeRecord $ HlLog.toRecord $ vectorsToTags headr rec
    
