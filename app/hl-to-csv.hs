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
            <> progDesc "Present an ADIF file as a list"
      )

-- Return field names from an association list in their natural order.
alFields :: (Ord a, Eq a) => [(a, b)] -> [a]
alFields = L.nub . map fst

-- Return values from an association list.
-- The values will be ordered by the appeareance of their
-- field names and according to the field name list given
-- as the first argument.
-- The returned list will be as long as the field name list.
-- The values are wrapped in Maybe. A Nothing value at a position
-- means that no value was found in the association list at the
-- given position.
-- Values with field names not in the field list are ignored.
alValues :: Eq a => [a] -> [(a, b)] -> [Maybe b]
alValues fn al = map (flip lookup al) fn

csvValues :: [Maybe ByteString] -> [ByteString]
csvValues = map (fromMaybe "")

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
        Right log -> do
            -- TODO: would it be better to just filter out tags without any data?
            let alList = map (map (\t->(tagName t, fromMaybe "" $ tagData t)) . fromRecord) $ logRecords log
            let fieldNames = alFields $ concat alList
            B.putStrLn $ csvLine fieldNames
            B.putStr $ B.unlines $ map (csvLine . csvValues . alValues fieldNames) alList
