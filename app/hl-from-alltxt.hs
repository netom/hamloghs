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
import Data.Attoparsec.ByteString.Char8 as A
import Data.Time.Calendar -- Day constructor
import Data.Time.Format -- ParseTime class
import System.CurrentLocale -- currentLocale
import Data.Time.Clock

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
    | ATLTFM
        { tfmTime :: UTCTime
        , tfmMHz  :: Double
        , tfmMode :: ByteString
        }
    | ATLJunk ByteString

allTxtLine :: A.Parser AllTxtLine
allTxtLine = do
    char '<'
    return ATLRcvCQ

-- Time, Mode, Frequency marker
-- "2016-máj.-20 14:50  14.076 MHz  JT9"
tfm :: A.Parser AllTxtLine
tfm = do
    dstr <- A.takeWhile (/=' ') -- Date
    A.takeWhile (==' ')
    tstr <- A.takeWhile (/=' ') -- Time
    -- parseTimeM False ...
    A.takeWhile (==' ')
    --number ...
    A.takeWhile (==' ')
    A.string "Mhz"
    A.takeWhile (==' ')
    --mode ...
    return $ ATLTFM (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)) 0 "JT65"

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
