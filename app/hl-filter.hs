{-# LANGUAGE RecordWildCards #-}
module Main where

--import Data.ByteString.Char8 (ByteString)
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

tagMatches :: FlExp -> Tag -> Bool
tagMatches expr (CTag (tname, mbtval)) =
    case expr of
        FlEx ftname -> ftname == tname
        otherexpr ->
            case mbtval of
                Nothing -> False
                Just (tvalue, _) ->
                    case otherexpr of
                        FlGte ftname fvalue -> ftname == tname && tvalue >= fvalue
                        FlGt  ftname fvalue -> ftname == tname && tvalue >  fvalue
                        FlLte ftname fvalue -> ftname == tname && tvalue <= fvalue
                        FlLt  ftname fvalue -> ftname == tname && tvalue <  fvalue
                        FlReg ftname fregex -> ftname == tname &&
                            case execute fregex tvalue of
                                Right (Just _) -> True
                                _              -> False
                        FlEq  ftname fvalue -> ftname == tname && tvalue == fvalue
                        FlNeq ftname fvalue -> ftname == tname && tvalue /= fvalue
                        _ -> False

recordMatches :: FlExp -> Record -> Bool
recordMatches expr (CRecord ts) = any (tagMatches expr) ts

filterLog :: [FlExp] -> Log -> Log
filterLog flexps Log{..} = Log logHeaderTxt logHeaderTags $ filter (\rec -> any (flip recordMatches rec) flexps) logRecords

doFilterLog :: Options -> IO ()
doFilterLog opt = do
    parseResult <- adifLogParser <$> (getInputHandle opt >>= B.hGetContents)

    case parseResult of
        Left errorMsg -> putStrLn errorMsg
        Right l -> B.putStr $ writeLog $ filterLog (flexps opt) l

    return ()

main :: IO ()
main = execParser optionsParserInfo >>= doFilterLog
