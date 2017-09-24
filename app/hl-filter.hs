{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.ByteString.Char8 (ByteString)
import HlAdif
import HlLog
import HlOptions
import Options.Applicative
import qualified Data.ByteString.Char8 as B
import Data.Semigroup ((<>))
import System.IO
import Text.Regex.TDFA.ByteString

data Options = Options
  { flexps :: [FlExp]
  , getInputHandle :: IO Handle
  }

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (helper <*> (
        Options
            <$> filterOptions
            <*> inputHandleArgument
      )) (
        fullDesc
            <> progDesc "Filter ADIF records according to filter expressions"
      )

parseErrorHandler :: Either String Log -> IO Log
parseErrorHandler (Left errorMessage) = fail errorMessage
parseErrorHandler (Right log) = return log

tagMatches :: FlExp -> Tag -> Bool
tagMatches (FlExp ftname flop ftvalue) (CTag (tname, Just (tvalue, _))) =
    ftname == tname &&
    case flop of
        FlGt  -> tvalue >  ftvalue
        FlGte -> tvalue >= ftvalue
        FlLt  -> tvalue <  ftvalue
        FlLte -> tvalue <= ftvalue
        FlEq  -> tvalue == ftvalue
        FlNeq -> tvalue /= ftvalue
        FlReg -> tvalue == ftvalue -- TODO: match regular expression

recordMatches :: FlExp -> Record -> Bool
recordMatches exp (CRecord ts) = any (tagMatches exp) ts

filterLog :: [FlExp] -> Log -> Log
filterLog flexps Log{..} = Log logHeaderTxt logHeaderTags $ filter (\rec -> any (flip recordMatches rec) flexps) logRecords

doFilterLog :: Options -> IO ()
doFilterLog opt = do
    parseResult <- adifLogParser <$> (getInputHandle opt >>= B.hGetContents)

    case parseResult of
        Left errorMsg -> putStrLn errorMsg
        Right log -> B.putStr $ writeLog $ filterLog (flexps opt) log

    return ()

main :: IO ()
main = execParser optionsParserInfo >>= doFilterLog
