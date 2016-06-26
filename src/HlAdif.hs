{-# LANGUAGE OverloadedStrings #-}

module HlAdif
    ( testParser
    ) where

import Data.Attoparsec.Text
import Data.List (foldl')
import Data.Text hiding (take, takeWhile, break, tail, foldl')
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
-- e.g. a field of a record (DataSpecifier), or the end of header / end
-- of record marks.
--
-- <TAGNAME:4>data some extra data including the space after "data"
--
data Tag = CALL Text
         | QSO_DATE Text
         | TIME_ON Text
         | DataSpecifier { dsName   :: Text
                         , dsType   :: Maybe Text
                         , dsData   :: Text
                         }
         | EOH
         | EOR
         deriving (Eq, Ord, Show)

-- A record is just a list of data specifiers
data Record = Record { recCall    :: Maybe Tag
                     , recQsoDate :: Maybe Tag
                     , recTimeOn  :: Maybe Tag
                     , recTags    :: [Tag]
                     } deriving (Show)

-- A log is made out of an optional header string and data specifiers in
-- the header, and a list of records.
data Log = Log { logHeaderTxt :: Text
               , logHeaderTags :: [Tag]
               , logRecords :: [Record]
               }
               deriving (Show)

-- Parse a simple tag like EOH or EOR, the ones without any data (and length,
-- and type).
expectTag :: Text -> Parser ()
expectTag tagName = do
    char '<'
    asciiCI tagName
    char '>'
    takeWhile $ (/=) '<'
    return ()

-- Parse the data type of a tag.
parseTagType :: Parser (Maybe Text)
parseTagType = do
    next <- peekChar
    case next of
        Nothing   -> fail "Unexpected end of data-specifier"
        Just ':'  -> fmap Just $ takeWhile $ (/=) '>'
        Just '>'  -> return Nothing
        Just c -> fail "Unexpected character" -- TODO: emit c in the error message

-- Parse a data specifier having a dedicated constructor, therefore known
-- data structure. Only the tag data is returned
expectDataSpecifier :: Text -> Parser Text
expectDataSpecifier tagName = do
    char '<'
    asciiCI tagName
    char ':'
    length <- decimal
    parseTagType
    char '>'
    tagData <- take length
    takeWhile $ (/=) '<'
    return tagData

-- Parse an unknown data specifier, the ones that carry opaque data.
parseDataSpecifier :: Parser Tag
parseDataSpecifier = do
    char '<'
    tagName <- takeWhile $ (/=) ':'
    char ':'
    length <- decimal
    mbType <- parseTagType
    char '>'
    tagData <- take length
    takeWhile $ (/=) '<'
    return $ DataSpecifier tagName mbType tagData

parseTag :: Parser Tag
parseTag = do
        (expectTag "EOH" >> return EOH)
    <|> (expectTag "EOR" >> return EOR)
    <|> (expectDataSpecifier "CALL"     >>= \tagData -> return $ CALL     tagData)
    <|> (expectDataSpecifier "QSO_DATE" >>= \tagData -> return $ QSO_DATE tagData)
    <|> (expectDataSpecifier "TIME_ON"  >>= \tagData -> return $ TIME_ON  tagData)
    <|> parseDataSpecifier

-- Put a tag into a record.
-- If a tag is a call, qso_date or time_on, update the appropriate fields
-- If it's something else, put the tag into the record's tag list
updateRecord :: Tag -> Record -> Record
updateRecord t@(CALL _)     (Record _    qsoDate timeOn ts) = Record (Just t) qsoDate  timeOn   ts
updateRecord t@(QSO_DATE _) (Record call _       timeOn ts) = Record call     (Just t) timeOn   ts
updateRecord t@(TIME_ON _)  (Record call qsoDate _      ts) = Record call     qsoDate  (Just t) ts
updateRecord t (Record call qsoDate timeOn ts)              = Record call qsoDate timeOn (t : ts)

records :: [Tag] -> [Record]
--records ts = foldl' (\ acc x -> f x acc) [] ts
records ts = Prelude.foldr f [] ts
    where
        f :: Tag -> [Record] -> [Record]
        f EOR rs = Record Nothing Nothing Nothing [] : rs
        f t []   = [updateRecord t $ Record Nothing Nothing Nothing []]
        f t (r : rs) = updateRecord t r : rs

parseLog :: Parser Log
parseLog = do
    headerTxt <- takeWhile $ (/=) '<'
    dataSpecifiers <- many parseTag
    let
        (headerTags, bodyTags) = break ((==) EOH) dataSpecifiers
        bodyRecords = records $ tail bodyTags

    return $ Log headerTxt headerTags bodyRecords

testParser :: Text -> Either String Log
testParser = parseOnly parseLog
