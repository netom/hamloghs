{-# LANGUAGE OverloadedStrings #-}

module HlLog
    ( Tag
    , Record
    , Log
    , toTag
    , fromTag
    , toRecord
    , toLog
    , fromRecord
    , records
    , logHeaderTxt
    , logHeaderTags
    , logRecords
    , mergeLogs
    , mergeTags
    , tagName
    , tagData
    , tagDataType
    , tagDataLength
    , showTag
    , showRecord
    , showLog
    , qsoKey
    , isTagName
    , isEOH
    , isEOR
    ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.List.Split as S
import GHC.Exts (groupWith, sortWith)
import qualified Data.List as L
import Data.Maybe

-- A Tag is the ADIF representation of piece of data or metadata,
-- e.g. a field of a record (Other), or the end of header / end
-- of record marks.
--
-- <TAGNAME:4>data some extra data including the space after "data"
--
-- (  Tag name,  (  Data type,  Actual data  )  )
newtype Tag = CTag { fromTag :: (ByteString, (Maybe ByteString, Maybe ByteString)) }

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

isTagName :: ByteString -> Tag -> Bool
isTagName tx tg = tx == tagName tg

isEOR = isTagName "EOR"
isEOH = isTagName "EOH"

-- A record is just a list of tags
-- Considering a tag is a tuple, this makes Record an association list
newtype Record = CRecord { fromRecord :: [Tag] }

instance Show Record where
    show = B.unpack . showRecord

toRecord :: [Tag] -> Record
toRecord = CRecord

qsoKey :: Record -> (Maybe ByteString, Maybe ByteString, Maybe ByteString)
qsoKey r = (field "QSO_DATE" r, field "TIME_ON" r, field "CALL" r)

field :: ByteString -> Record -> Maybe ByteString
field fn r = case lookup fn $ map fromTag $ fromRecord r of
    Just (_, Just td) -> Just td
    Just (_, Nothing) -> Nothing
    Nothing           -> Nothing

-- Break up a list of tags parsed from the body of an ADIF file to
-- a neat list of records. Empty records are dropped.
records :: [Tag] -> [Record]
records ts = map toRecord $ filter (not . null) $ S.splitWhen isEOR ts

mergeTags :: [[Tag]] -> [Tag]
mergeTags = map head . groupWith tagName . sortWith tagName . L.concat

mergeRecords :: [[Record]] -> [Record]
mergeRecords = map (toRecord . mergeTags . map fromRecord) . groupWith qsoKey . sortWith qsoKey . L.concat

mergeLogs :: [Log] -> Log
mergeLogs ls = Log "" (mergeTags $ map logHeaderTags ls) (mergeRecords $ map logRecords ls)

-- A log is made out of an optional header string and data specifiers in
-- the header, and a list of records.
data Log = Log { logHeaderTxt :: ByteString
               , logHeaderTags :: [Tag]
               , logRecords :: [Record]
               }

instance Show Log where
    show = B.unpack . showLog

toLog :: ByteString -> [Tag] -> [Record] -> Log
toLog = Log

showRecord :: Record -> ByteString
showRecord r = B.intercalate " "
    [ fromMaybe "------" $ field "QSO_DATE" r
    , fromMaybe "----"   $ field "TIME_ON"  r
    , fromMaybe "------" $ field "CALL"     r
    ]

showLog :: Log -> ByteString
showLog (Log htxt htags recs) =  B.intercalate "\n" $ map showRecord recs

showTag :: Tag -> ByteString
showTag t = case tagData t of
    Just d  -> B.concat [tagName t, "=", d]
    Nothing -> tagName t
