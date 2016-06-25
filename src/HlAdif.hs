{-# LANGUAGE OverloadedStrings #-}

module HlAdif
    ( testParser
    ) where

import Data.Attoparsec.Text
import Data.Text hiding (take, takeWhile, break, tail)
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

type FieldName = Text
type FieldLength = Int

-- A Tag is the ADIF representation of piece of data or metadata,
-- e.g. a field of a record (DataSpecifier), or the end of header / end
-- of record marks.
--
-- <TAGNAME:4>data some extra data including the space after "data"
--
data Tag = DataSpecifier { dsName   :: FieldName
                         , dsLength :: FieldLength
                         , dsType   :: Maybe Text
                         , dsData   :: Text
                         , dsExtra  :: Text
                         }
         | EOH
         | EOR
         deriving (Eq, Show)

-- A record is just a list of data specifiers
data Record = Record [Tag] deriving (Show)

-- A log is made out of an optional header string and data specifiers in
-- the header, and a list of records.
data Log = Log { logHeaderTxt :: Text
               , logHeaderRecord :: Record
               , logRecords :: [Record]
               }
               deriving (Show)

parseTag :: Parser Tag
parseTag = do
        (asciiCI "<EOH>" >> (takeWhile $ (/=) '<') >> return EOH)
    <|> (asciiCI "<EOR>" >> (takeWhile $ (/=) '<') >> return EOR)
    <|> do
        char '<'
        name <- takeWhile $ (/=) ':'
        char ':'
        length <- decimal

        next <- peekChar

        mbType <- case next of
            Nothing   -> fail "Unexpected end of data-specifier"
            Just ':'  -> fmap Just $ takeWhile $ (/=) '>'
            Just '>'  -> return Nothing
            Just c -> fail "Unexpected character" -- TODO: emit c in the error message

        char '>'
        dsdata <- take length
        extra <- takeWhile $ (/=) '<'

        return $ DataSpecifier name length mbType dsdata extra

records :: [Tag] -> [Record]
records ts = Prelude.foldr f [] ts
    where
        f :: Tag -> [Record] -> [Record]
        f EOR rs             = Record [] : rs
        f t []               = [Record [t]]
        f t (Record ts : rs) = Record (t : ts) : rs

parseLog :: Parser Log
parseLog = do
    headerTxt <- takeWhile $ (/=) '<'
    dataSpecifiers <- many parseTag
    let
        (headerTags, bodyTags) = break ((==) EOH) dataSpecifiers
        bodyRecords = records $ tail bodyTags

    return $ Log headerTxt (Record headerTags) bodyRecords

testParser :: Text -> Either String Log
testParser = parseOnly parseLog
