{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe
import HlAdif
import HlOptions
import Options.Applicative
import Prelude hiding (readFile, putStr)
import System.Environment
import System.IO hiding (readFile, putStr)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8

data Options = Options
  { getInputHandle :: IO Handle
  }

data AllTxtLine
    = ATLTransmitting
        { atlMhz    :: Double
        , atlMode   :: ByteString
        , atlCall   :: ByteString
        , atlMyCall :: ByteString
        , atlMyGrid :: ByteString
        }
    | ATLRcvCQ
    | ATLRcvXchg
        { rcvTime  :: ByteString
        , rcvDb    :: Double
        , rcvTOffs :: Double
        , rcvFOffs :: Int
        , rcvMode  :: ByteString
        , rcvCall1 :: ByteString
        , rcvCall2 :: ByteString
        , rcvXchg  :: ByteString
        }
    | ATLRcvOther
        { rcvTime  :: ByteString
        , rcvDb    :: Double
        , rcvTOffs :: Double
        , rcvFOffs :: Int
        , rcvMode  :: ByteString
        , rcvTxt   :: ByteString
        }
    | ATLJunk ByteString

--allTxtLine :: Parser AllTxtLine
--allTxtLine = do
--    char '<'
--    return ATLRcvCQ


getOptionsParserInfo :: IO (ParserInfo Options)
getOptionsParserInfo = do
    return $ info (helper <*> (
        Options
            <$> inputHandleArgument
      )) (
        fullDesc
            <> progDesc "Read ALL.TXT files created by the WSJT-X software, and extract JT9 and JT65 contact information as ADIF"
      )

main :: IO ()
main = getOptionsParserInfo >>= execParser >>= \opt -> do
    fail "NOT IMPLEMENTED"
    h <- getInputHandle opt
    parseResult <- adifLogParser <$> B.hGetContents h

    case parseResult of
        Left errorMsg -> putStrLn errorMsg
        Right log -> putStrLn $ show log
