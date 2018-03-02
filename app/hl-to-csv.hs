{-# LANGUAGE OverloadedStrings #-}
module Main where

import HlLog
import HlAdif
import HlOptions
import Options.Applicative
import Prelude hiding (readFile, putStr)
import System.IO hiding (readFile, putStr)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Search as S
import Data.ByteString.Lazy (toStrict)
import Data.Semigroup ((<>))

data Options = Options
  { getInputHandle :: IO Handle
  , separatorChar  :: Char
  , quoteChar      :: Char
  , escapeChar     :: Char
  }

getOptionsParserInfo :: IO (ParserInfo Options)
getOptionsParserInfo = do
    return $ info (helper <*> (
        Options
            <$> inputHandleArgument
            <*> separatorOption
            <*> quoteOption
            <*> escapeOption
      )) (
        fullDesc
            <> progDesc "Present an ADIF file as CSV, or similar character-separated, quoted-escaped format"
      )

csvLine :: Char -> Char -> Char -> [ByteString] -> ByteString
csvLine sep qt esc bss = B.intercalate (B.singleton sep) (map (quoteCsv sep qt esc) bss)

quoteCsv :: Char -> Char -> Char -> ByteString -> ByteString
quoteCsv sep qt esc s = quoteOrEmpty <> quoted <> quoteOrEmpty
    where
        quotesNeeded = any (not . B.null . snd) $ map (flip B.breakSubstring s) [B.singleton sep, "\n", "\r"]
        quoteOrEmpty = if quotesNeeded then B.singleton qt else ""
        quoted = 
            if quotesNeeded then
                toStrict $ S.replace (B.singleton esc) (B.pack [esc, esc]) $
                if qt /= esc then
                    toStrict $ S.replace (B.singleton qt) (B.pack [esc, qt]) s
                else s
            else s

main :: IO ()
main = getOptionsParserInfo >>= execParser >>= \opt -> do
    h <- getInputHandle opt
    parseResult <- adifLogParser <$> B.hGetContents h

    case parseResult of
        Left errorMsg -> putStrLn errorMsg
        Right l -> mapM_ B.putStrLn $ map (csvLine (separatorChar opt) (quoteChar opt) (escapeChar opt)) $ qsoTable l
