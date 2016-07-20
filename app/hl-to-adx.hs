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
            <> progDesc "Present the contents of an ADIF file as an ADX XML document."
      )

xmlRow :: Record -> ByteString
xmlRow r = "   <RECORD>\n" <> B.concat (map xmlCell $ fromRecord r) <> "   </RECORD>\n"

xmlCell :: Tag -> ByteString
xmlCell t = case tagData t of
    Nothing -> "    <" <> tagName t <> " />"
    Just d  -> "    <" <> tagName t <> ">" <> escaped d <> "</" <> tagName t <> ">\n"
    where
        escaped d = foldl escape d [("&", "&amp;"), ("\"", "&quot;"), ("'", "&apos;"), ("<", "&lt;"), (">", "&gt;")]
        escape :: ByteString -> (ByteString, ByteString) -> ByteString
        escape s (toS, toR) = toStrict $ S.replace toS toR s

main :: IO ()
main = getOptionsParserInfo >>= execParser >>= \opt -> do
    h <- getInputHandle opt
    parseResult <- adifLogParser <$> B.hGetContents h

    case parseResult of
        Left errorMsg -> putStrLn errorMsg
        Right log -> do
            B.putStr $
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" <>
                "<ADX>\n" <>
                " <HEADER></HEADER>\n" <>
                "  <RECORDS>\n"
            mapM_ B.putStr $ map xmlRow $ logRecords log
            B.putStr $
                " </RECORDS>\n" <>
                "</ADX>\n"
