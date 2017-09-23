{-# LANGUAGE OverloadedStrings #-}
module Main where

import HlAdif
import HlLog
import HlOptions
import Options.Applicative
import System.IO hiding (readFile, putStr)
import Data.Char
import Data.Csv
import Data.List
import Data.Vector as V
import Text.Printf
import GHC.Int
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (fromStrict)
import Data.Semigroup ((<>))

data Options = Options
  { getInputHandle :: IO Handle
  , separator :: Char
  }

getOptionsParserInfo :: IO (ParserInfo Options)
getOptionsParserInfo = do
    return $ info (helper <*> (
        Options
            <$> inputHandleArgument
            <*> separatorOption
      )) (
        fullDesc
            <> progDesc "Convert a CSV file to ADIF"
      )

doMain :: Options -> IO ()
doMain opt = do
    B.putStr "Hello\n"

main :: IO ()
main = getOptionsParserInfo >>= execParser >>= \opt -> do
    h <- getInputHandle opt
    contents <- B.hGetContents h

    let decopts = DecodeOptions $ fromIntegral $ ord $ separator opt

    let result = decodeWith decopts NoHeader (fromStrict contents) :: Either String (Vector (Vector B.ByteString))

    case result of
        Left errMsg   -> putStrLn errMsg
        Right csvdata -> do
            let header = csvdata V.! 0
            forM_ (V.tail csvdata) $ \rec -> do
                B.putStr $ writeRecord $ HlLog.toRecord $ vectorsToTags header rec
    
