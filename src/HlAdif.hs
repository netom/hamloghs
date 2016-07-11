{-# LANGUAGE OverloadedStrings,FlexibleInstances #-}

module HlAdif
    ( adifLogParser
    , mergeTags
    , toRecord
    , records
    , writeTag
    , writeRecord
    , writeLog
    , toTag
    , tagName
    , tagData
    , tagDataLength
    , tagDataType
    , mergeLogs
    , Tag (..)
    , Record (..)
    , Log
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
import GHC.Exts (groupWith, sortWith)
import Prelude hiding (take, takeWhile)

-- Optparse-applicative for argument / option parsing
-- 
-- File formats:
--
-- * ADIF
--     * 1, 2
--     * ADI
--     * ADX
--
-- * Cabrillo
-- * EDI
--
-- * CSV (SCSV, TSV, ...)
--     * Separator, quote, header
--
-- * Protobuf
-- * BSON
-- * Persistent
--     * Mysql, Postgresql, SQLite, ...
--     * Redis
--     * MongoDB
--
-- * Excel XML:
-- <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
-- <data-set xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
--     <record>
--        <LastName>Smith</LastName>
--        <Sales>16753</Sales>
--        <Country>UK</Country>
--        <Quarter>Qtr 3</Quarter>
--     </record>
--     <record>
--        <LastName>Johnson</LastName>
--        <Sales>14808</Sales>
--        <Country>USA</Country>
--        <Quarter>Qtr 4</Quarter>
--     </record>
-- </data-set>
--
-- * OpenOffice / LibreOffice XML
--     * Needs a template odf archive

-- A Tag is the ADIF representation of piece of data or metadata,
-- e.g. a field of a record (Other), or the end of header / end
-- of record marks.
--
-- <TAGNAME:4>data some extra data including the space after "data"
--
-- (  Tag name,  (  Data type,  Actual data  )  )
newtype Tag = CTag { fromTag :: (ByteString, (Maybe ByteString, Maybe ByteString)) }

instance Show Tag where
    show = B.unpack . tShowTag

toTag :: (ByteString, (Maybe ByteString, Maybe ByteString)) -> Tag
toTag = CTag

tagName :: Tag -> ByteString
tagName = fst . fromTag

tagData :: Tag -> Maybe ByteString
tagData = snd . snd . fromTag

tagDataLength :: Tag -> Maybe Int
tagDataLength t = B.length <$> tagData t

tagDataType :: Tag -> Maybe ByteString
tagDataType = fst . snd . fromTag

tShowTag :: Tag -> ByteString
tShowTag t = case tagData t of
    Just d  -> B.concat [tagName t, "=", d]
    Nothing -> tagName t

isTagName :: ByteString -> Tag -> Bool
isTagName tx tg = tx == tagName tg

isEOR = isTagName "EOR"
isEOH = isTagName "EOH"

-- A record is just a list of tags
-- Considering a tag is a tuple, this makes Record an association list
newtype Record = CRecord { fromRecord :: [Tag] }

instance Show Record where
    show = B.unpack . tShowRecord

toRecord :: [Tag] -> Record
toRecord = CRecord

tShowRecord :: Record -> ByteString
tShowRecord r = B.intercalate " "
    [ fromMaybe "------" $ field "QSO_DATE" r
    , fromMaybe "----"   $ field "TIME_ON"  r
    , fromMaybe "------" $ field "CALL"     r
    ]

field :: ByteString -> Record -> Maybe ByteString
field fn r = case lookup fn $ map fromTag $ fromRecord r of
    Just (_, Just td) -> Just td
    Just (_, Nothing) -> Nothing
    Nothing           -> Nothing

-- TODO: rename to qsoKey
qsoId :: Record -> (Maybe ByteString, Maybe ByteString, Maybe ByteString)
qsoId r = (field "QSO_DATE" r, field "TIME_ON" r, field "CALL" r)

-- A log is made out of an optional header string and data specifiers in
-- the header, and a list of records.
data Log = Log { logHeaderTxt :: ByteString
               , logHeaderTags :: [Tag]
               , logRecords :: [Record]
               }

instance Show Log where
    show = B.unpack . tShowLog

tShowLog :: Log -> ByteString
tShowLog (Log htxt htags recs) =  B.intercalate "\n" $ map tShowRecord recs

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

-- Break up a list of tags parsed from the body of an ADIF file to
-- a neat list of records. Empty records are dropped.
records :: [Tag] -> [Record]
records ts = map toRecord $ filter (not . null) $ S.splitWhen isEOR ts

parseLog :: Parser Log
parseLog = do
    hTxt <- takeWhile $ (/=) '<'
    (headerTags, bodyTags) <- break isEOH <$> many parseTag
    return $ case (headerTags, bodyTags) of
        (hts, [])  -> Log hTxt []  (records hts)          -- No EOH tag, no header
        (hts, bts) -> Log hTxt hts (records $ drop 1 bts) -- Use tags after the EOH tag

adifLogParser :: ByteString -> Either String Log
adifLogParser = parseOnly parseLog

-- Warning: uses incomplete function "head". Probably OK, but still.
mergeTags :: [[Tag]] -> [Tag]
mergeTags = map head . groupWith tagName . sortWith tagName . L.concat

mergeRecords :: [[Record]] -> [Record]
mergeRecords = map (toRecord . mergeTags . map fromRecord) . groupWith qsoId . sortWith qsoId . L.concat

mergeLogs :: [Log] -> Log
mergeLogs ls = Log "" (mergeTags $ map logHeaderTags ls) (mergeRecords $ map logRecords ls)

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
writeLog (Log htxt htags brecs) =
    B.concat [ htxt
             , B.intercalate "\n" $ map writeTag htags
             , "<EOH>\n"
             , B.concat $ map writeRecord brecs
             ]
