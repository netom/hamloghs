{-# LANGUAGE OverloadedStrings #-}

module HlAdif
    ( adifLogParser
    , writeTag
    , writeRecord
    , writeLog
    ) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Char
import qualified Data.List as L
import Data.Maybe
import Data.Monoid
import Data.String
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.List.Split as S
import HlLog
import Prelude hiding (take, takeWhile)

hamlogHsHeaderTxt = "Created by hl - HamlogHS: the Ham Radio Logger written in Haskell\n"

-- Constraints:
--
-- [X] Tag names are UPPER CASE
-- [ ] Callsigns are UPPER CASE
-- [ ] Maidenhead locators are UPPER CASE
-- [X] TIME_ON and TIME_OFF always use second precision
parseTag :: Parser Tag
parseTag = do
    char '<'
    tName <- B.map toUpper <$> takeWhile (\x -> x /= ':' && x /= '>')
    n1 <- peekChar

    (tDataType, tData) <- case n1 of
        Nothing  -> fail "Unexpected end of tag"
        Just ':' -> do
            take 1 -- Drop :
            length <- decimal
            n2 <- peekChar
            tDataType <- case n2 of
                Nothing  -> fail "Uexpected end of tag"
                Just ':' -> do
                    take 1 -- Drop :
                    Just <$> takeWhile (/='>')
                Just '>' -> do
                    return Nothing
                Just c   -> fail $ "Unexpected character: " ++ [c]
            take 1 -- Drop >
            tData <- Just <$> take length
            return (tDataType, tData)
        Just '>' -> do
            take 1 -- Drop >
            return (Nothing, Nothing)

    takeWhile (/='<') -- Drop extra characters after useful data

    -- Use 6 character long time representation
    let tData' = if tName == "TIME_ON" then (\x-> x <> B.replicate (6 - B.length x) '0') <$> tData else tData

    return $ toTag (tName, (tDataType, tData'))

parseLog :: Parser Log
parseLog = do
    hTxt <- takeWhile $ (/=) '<'
    (headerTags, bodyTags) <- break isEOH <$> many parseTag
    return $ case (headerTags, bodyTags) of
        (hts, [])  -> toLog hTxt []  (records hts)          -- No EOH tag, no header
        (hts, bts) -> toLog hTxt hts (records $ drop 1 bts) -- Use tags after the EOH tag

adifLogParser :: ByteString -> Either String Log
adifLogParser = parseOnly parseLog

writeTag :: Tag -> ByteString
writeTag t = B.concat
    [ "<"
    , tagName t
    , fromMaybe "" $ (":"<>) <$> B.pack <$> show <$> tagDataLength t
    , fromMaybe "" $ (":"<>) <$> tagDataType t
    , ">"
    , fromMaybe "" (tagData t)
    ]

writeRecord :: Record -> ByteString
writeRecord r = B.intercalate "\n" (map writeTag $ fromRecord r) <> "\n<EOR>\n"

writeLog :: Log -> ByteString
writeLog log =
    B.concat [ logHeaderTxt log
             , B.intercalate "\n" $ map writeTag $ logHeaderTags log
             , "<EOH>\n"
             , B.concat $ map writeRecord $ logRecords log
             ]
