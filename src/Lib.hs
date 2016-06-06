{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Data.Attoparsec.Text
import Data.Text hiding (take, takeWhile)
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

data Tag = DataSpecifier { dsName   :: FieldName
                         , dsLength :: FieldLength
                         , dsType   :: Maybe Text
                         , dsData   :: Text
                         , dsExtra  :: Text
                         }
         | EOH
         | EOR
         deriving (Show)

data Log = Log { logHeaderTxt :: Text
               , logTags      :: [Tag]
               }
               deriving (Show)

parseTag :: Parser Tag
parseTag = do
        (asciiCI "<EOH>" >> return EOH)
    <|> (asciiCI "<EOR>" >> return EOR)
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

-- parseHeader :: Parser (Maybe Header)
-- parseHeader = do
--     headertxt <- takeWhile  $ (/=) '<'
--     if headertxt == "" then return Nothing else do
--         dss <- many parseDataSpecifier
--         asciiCI "<EOH>"
--         return $ Just $ Header headertxt dss
-- 
-- parseRecord :: Parser Record
-- parseRecord = do
--     dss <- many parseDataSpecifier
--     asciiCI "<EOR>"
--     return dss
-- 
-- parseBody :: Parser Body
-- parseBody = do
--     skipSpace
--     many parseDataSpecifier
-- 

parseLog :: Parser Log
parseLog = do
    headerTxt <- takeWhile  $ (/=) '<'
    tags <- many parseTag
    return $ Log headerTxt tags

someFunc :: IO ()
someFunc = do
    putStrLn "someFunc"
    print $ parseOnly parseLog "HamLogHS v0.0.1\n<APP_HAMLOGHS_TEST:3>121<EOH><call:6>HA5FTL\nthis is some extra data"
    --print $ parseOnly (many parseDataSpecifier) "<call:6>HA5FTL\nthis is some extra data"
