{-# LANGUAGE OverloadedStrings,FlexibleInstances #-}

module HlAdif
    ( adifLogParser
    , mergeTags
    , record
    , records
    , writeTag
    , writeRecord
    , writeLog
    , toTag
    , fromTag
    , tagName
    , tagData
    , mergeLogs
    , Tag (..)
    , Record (..)
    ) where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.List as L
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text hiding (take, takeWhile, break, tail, map, filter, foldr, head)
import qualified Data.Text as T
import Prelude hiding (take, takeWhile)
import qualified Data.List.Split as S

-- Optparse-applicative for argument / option parsing
-- 
-- File formats:
--
-- * ADIF
--     * 1, 2
--     * ADI
--     * ADX
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
data Tag = QSO_DATE Text
         | TIME_ON Text
         | CALL Text
         | Other Text (Maybe Text)
         | EOH
         | EOR

-- Two tags are equal if their names are equal.
-- Tags with the same name are rarely in the same
-- list (traversable), and if they are, their data
-- rarely matters. For exact comparison, see sameTags
instance Eq Tag where
    (==) t1 t2 = compare t1 t2 == EQ

-- Ordering of tags done only by their name for the same
-- reason as equality is defined as such.
instance Ord Tag where
    compare t1 t2 = compare (tagName t1) (tagName t2)

instance Show Tag where
    show = unpack . showTag

toTag :: (Text, Maybe Text) -> Tag
toTag (n, d) = case toUpper n of
    "CALL"     -> CALL $ fromMaybe "" d
    "QSO_DATE" -> QSO_DATE $ fromMaybe "" d
    "TIME_ON"  -> TIME_ON $ fromMaybe "" d
    "EOH"      -> EOH
    "EOR"      -> EOR
    otherwise  -> Other n d

fromTag :: Tag -> (Text, Maybe Text)
fromTag EOH = ("EOH", Nothing)
fromTag EOR = ("EOR", Nothing)
fromTag (CALL     x) = ("CALL",     Just x)
fromTag (QSO_DATE x) = ("QSO_DATE", Just x)
fromTag (TIME_ON  x) = ("TIME_ON",  Just x)
fromTag (Other n d) = (n, d)

tagName :: Tag -> Text
tagName = fst . fromTag

tagData :: Tag -> Maybe Text
tagData = snd . fromTag

showTag :: Tag -> Text
showTag t = case tagData of
    Just d  -> T.concat [tagName, "=", d]
    Nothing -> tagName
    where
        (tagName, tagData) = fromTag t

-- A record is just a list of tags
data Record = Record { recQsoDate   :: Maybe Tag
                     , recTimeOn    :: Maybe Tag
                     , recCall      :: Maybe Tag
                     , recOtherTags :: [Tag]
                     }

instance Eq Record where
    (==) r1 r2 = compare r1 r2 == EQ

instance Ord Record where
    compare (Record a1 b1 c1 _) (Record a2 b2 c2 _) = compare (a1, b2, c1) (a2, b2, c2)

instance Show Record where
    show = unpack . showRecord

recTags :: Record -> [Tag]
recTags r = catMaybes [recQsoDate r, recTimeOn r, recCall r] ++ recOtherTags r

showRecord :: Record -> Text
showRecord (Record call date timeOn tags) = T.concat [
    (fromMaybe "------" $ date >>= tagData), " ",
    (fromMaybe "----" $ timeOn >>= tagData), " ",
    (fromMaybe "------" $ call >>= tagData)]

emptyRec :: Record
emptyRec = Record Nothing Nothing Nothing []

-- A log is made out of an optional header string and data specifiers in
-- the header, and a list of records.
data Log = Log { logHeaderTxt :: Text
               , logHeaderTags :: [Tag]
               , logRecords :: [Record]
               }

showLog :: Log -> Text
showLog (Log htxt tags recs) =  T.concat [htxt, "\n", intercalate "\n" (map showTag tags), intercalate "\n" (map showRecord recs)]

instance Show Log where
    show = unpack . showLog

parseTag :: Parser (Text, Maybe Text)
parseTag = do
    char '<'
    tname <- takeWhile (\x -> x /= ':' && x /= '>')
    n1 <- peekChar
    tdata <- case n1 of
        Nothing  -> fail "Unexpected end of tag"
        Just ':' -> do
            take 1
            length <- decimal
            n2 <- peekChar
            case n2 of
                Nothing  -> fail "Uexpected end of tag"
                Just ':' -> takeWhile (/='>') -- Drop data type informatin
                Just '>' -> return ""
                Just c   -> fail $ "Unexpected character: " ++ [c]
            take 1
            Just <$> take length
        Just '>' -> do
            take 1
            return Nothing
    takeWhile (/='<')
    return (tname, tdata)

-- Put a tag into a record.
-- If a tag is a call, qso_date or time_on, update the appropriate fields
-- If it's something else, put the tag into the record's tag list
updateRecord :: Tag -> Record -> Record
updateRecord t@(CALL _)     (Record _    qsoDate timeOn ts) = Record (Just t) qsoDate  timeOn   ts
updateRecord t@(QSO_DATE _) (Record call _       timeOn ts) = Record call     (Just t) timeOn   ts
updateRecord t@(TIME_ON _)  (Record call qsoDate _      ts) = Record call     qsoDate  (Just t) ts
updateRecord t (Record call qsoDate timeOn ts)              = Record call qsoDate timeOn (t : ts)

record :: [Tag] -> Record
record = foldr updateRecord emptyRec . L.sort

-- Break up a list of tags parsed from the body of an ADIF file to
-- a neat list of records. Empty records are dropped.
records :: [Tag] -> [Record]
records ts = map record $ filter (/=[]) $ S.splitOn [EOR] ts

parseLog :: Parser Log
parseLog = do
    headerTxt <- takeWhile $ (/=) '<'
    tuples <- many parseTag
    let
        tags = map toTag tuples
        (headerTags, bodyTags) = break ((==) EOH) tags
        bodyRecords = records $ filter (/= EOH) bodyTags

    return $ Log headerTxt headerTags bodyRecords

adifLogParser :: Text -> Either String Log
adifLogParser = parseOnly parseLog

-- Warning: uses incomplete function "head".
mergeTags :: [[Tag]] -> [Tag]
mergeTags = map head . L.group . L.sort . L.concat

mergeRecords :: [[Record]] -> [Record]
mergeRecords = map (record . mergeTags . map recTags) . L.group . L.sort . L.concat

-- TODO: currently there is no logic to handle header tags.
mergeLogs :: [Log] -> Log
mergeLogs = Log "" [] . mergeRecords . map logRecords

writeTag :: Tag -> Text
writeTag t = case fromTag t of
    (tn, Nothing) -> T.concat ["<", toUpper tn, ">"]
    (tn, Just td) -> T.concat ["<", toUpper tn, ":", (pack $ show $ Data.Text.length td), ">", td]

writeRecord :: Record -> Text
writeRecord (Record call qsoDate timeOn tags) = T.concat $ map mbTag2Str (qsoDate : timeOn : call : map Just tags) ++ ["<EOR>\n"]
    where
        mbTag2Str :: Maybe Tag -> Text
        mbTag2Str Nothing  = ""
        mbTag2Str (Just t) = T.concat ["  ", writeTag t, "\n"]

writeLog :: Log -> Text
writeLog (Log htxt htags recs) =
    T.concat [ htxt
             , intercalate "\n" $ map writeTag htags
             , "<EOH>\n"
             , Data.Text.concat $ map writeRecord $ L.sort recs
             ]
