{-# LANGUAGE OverloadedStrings,FlexibleInstances #-}

module HlAdif
    ( adifLogParser
    , mergeTags
    , Tag (..)
    , Record (..)
    , ToTag (..)
    , FromTag (..)
    ) where

import Data.Attoparsec.Text
import Data.List (intercalate)
import Data.Maybe
import Data.Text hiding (take, takeWhile, break, tail, intercalate, map, filter)
import Prelude hiding (take, takeWhile)
import Control.Applicative

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
data Tag = CALL Text
         | QSO_DATE Text
         | TIME_ON Text
         | Other { tagName   :: Text
                 , tagData   :: Maybe Text
                 }
         | EOH
         | EOR
         deriving (Eq, Ord, Show)

class ToTag a where
    toTag :: a -> Tag

class FromTag a where
    fromTag :: Tag -> a

instance ToTag (Text, Maybe Text) where
    toTag (n, d) = case toUpper n of
        "CALL"     -> CALL $ fromMaybe "" d
        "QSO_DATE" -> QSO_DATE $ fromMaybe "" d
        "TIME_ON"  -> TIME_ON $ fromMaybe "" d
        "EOH"      -> EOH
        "EOR"      -> EOR
        otherwise  -> Other n d

instance ToTag (String, Maybe String) where
    toTag (n, v) = toTag (pack n, pack <$> v)

instance FromTag (Text, Maybe Text) where
    fromTag EOH = ("EOH", Nothing)
    fromTag EOR = ("EOR", Nothing)
    fromTag (CALL     x) = ("CALL",     Just x)
    fromTag (QSO_DATE x) = ("QSO_DATE", Just x)
    fromTag (TIME_ON  x) = ("TIME_ON",  Just x)
    fromTag (Other n d) = (n, d)

instance FromTag (String, Maybe String) where
    fromTag tag = (unpack (fst tup), unpack <$> (snd tup))
        where
            tup = fromTag tag

maybeShowTagData :: Tag -> Maybe String
maybeShowTagData = snd . (fromTag :: Tag -> (String, Maybe String))

-- A record is just a list of tags
data Record = Record { recCall    :: Maybe Tag
                     , recQsoDate :: Maybe Tag
                     , recTimeOn  :: Maybe Tag
                     , recTags    :: [Tag]
                     }

instance Show Record where
    show (Record call date timeOn tags) =
        (fromMaybe "------" $ date >>= maybeShowTagData) ++ " " ++
        (fromMaybe "----" $ timeOn >>= maybeShowTagData) ++ " " ++
        (fromMaybe "------" $ call >>= maybeShowTagData)

-- A log is made out of an optional header string and data specifiers in
-- the header, and a list of records.
data Log = Log { logHeaderTxt :: Text
               , logHeaderTags :: [Tag]
               , logRecords :: [Record]
               }

instance Show Log where
    show (Log htxt tags recs) = unpack htxt ++ "\n" ++ intercalate "\n" (Prelude.map show tags) ++ intercalate "\n" (Prelude.map show recs)

parseTag :: Parser (Text, Maybe Text)
parseTag = do
    char '<'
    tagName <- takeWhile (\x -> x /= ':' && x /= '>')
    n1 <- peekChar
    tagData <- case n1 of
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
    return (tagName, tagData)

-- Put a tag into a record.
-- If a tag is a call, qso_date or time_on, update the appropriate fields
-- If it's something else, put the tag into the record's tag list
updateRecord :: Tag -> Record -> Record
updateRecord t@(CALL _)     (Record _    qsoDate timeOn ts) = Record (Just t) qsoDate  timeOn   ts
updateRecord t@(QSO_DATE _) (Record call _       timeOn ts) = Record call     (Just t) timeOn   ts
updateRecord t@(TIME_ON _)  (Record call qsoDate _      ts) = Record call     qsoDate  (Just t) ts
updateRecord t (Record call qsoDate timeOn ts)              = Record call qsoDate timeOn (t : ts)

-- Break up a list of tags parsed from the body of an ADIF file to
-- a neat list of records.
-- Use the above updateRecord function to make sure call, qso_date and time_on
-- get into their proper places in each record
records :: [Tag] -> [Record]
records ts = Prelude.foldr f [] ts
    where
        f :: Tag -> [Record] -> [Record]
        f EOR rs = Record Nothing Nothing Nothing [] : rs
        f t []   = [updateRecord t $ Record Nothing Nothing Nothing []]
        f t (r : rs) = updateRecord t r : rs

parseLogTuples :: Parser [(Text, Maybe Text)]
parseLogTuples = do
    headerTxt <- takeWhile $ (/=) '<'
    many parseTag

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

mergeTags :: [Tag] -> [Tag] -> [Tag]
--mergeTags tags1 tags2 = filter (\x -> any (\y -> (fromTag y :: Maybe Text) == (fromTag x :: Maybe Text)) tags2) tags1 ++ tags2
mergeTags tags1 tags2 = tags1 ++ tags2
