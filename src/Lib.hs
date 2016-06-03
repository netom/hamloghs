module Lib
    ( someFunc
    ) where

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

someFunc :: IO ()
someFunc = putStrLn "someFunc"
