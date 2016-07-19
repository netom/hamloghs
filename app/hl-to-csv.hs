{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe
import HlLog
import HlAdif
import HlOptions
import Options.Applicative
import Prelude hiding (readFile, putStr)
import System.Environment
import System.IO hiding (readFile, putStr)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.List as L
import qualified Data.ByteString.Search as S
import Data.ByteString.Lazy (fromStrict, toStrict)

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
            <> progDesc "Present an ADIF file as CSV"
      )

csvLine :: [ByteString] -> ByteString
csvLine bss = B.intercalate "," (map quoteCsv bss)

quoteCsv :: ByteString -> ByteString
quoteCsv s = quoteOrEmpty <> quoted <> quoteOrEmpty
    where
        quotesNeeded = any (not . B.null . snd) $ map (flip B.breakSubstring s) [",", "\n", "\r"]
        quoteOrEmpty = if quotesNeeded then "\"" else ""
        quoted = if quotesNeeded then toStrict $ S.replace "\"" ("\"\"" :: ByteString) s else s

main :: IO ()
main = getOptionsParserInfo >>= execParser >>= \opt -> do
    h <- getInputHandle opt
    parseResult <- adifLogParser <$> B.hGetContents h

    case parseResult of
        Left errorMsg -> putStrLn errorMsg
        Right log -> mapM_ B.putStrLn $ map csvLine $ qsoTable log
