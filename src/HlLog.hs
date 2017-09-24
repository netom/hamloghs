{-# LANGUAGE OverloadedStrings #-}

module HlLog
    ( TagName
    , Tag (..)
    , Record (..)
    , Log (..)
    , toTag
    , toRecord
    , toLog
    , records
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
    , qsoTable
    , vectorsToTags
    ) where

import Data.ByteString.Char8 (ByteString)
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.List.Split as S
import GHC.Exts (groupWith, sortWith)
import qualified Data.List as L
import Data.Maybe
import qualified Data.Char as CH
import Data.Monoid
import qualified Data.Vector as V

type TagName = ByteString

-- A Tag is the ADIF representation of piece of data or metadata,
-- e.g. a field of a record (Other), or the end of header / end
-- of record marks.
--
-- <TAGNAME:4>data some extra data including the space after "data"
--
-- (  Tag name,  Maybe ( Actual data, Maybe data type )  )
newtype Tag = CTag { fromTag :: (TagName, Maybe (ByteString, Maybe ByteString)) }

tMap :: (ByteString -> ByteString) -> Tag -> Tag
tMap _ t@(CTag (_, Nothing)) = t
tMap f   (CTag (n, Just(d, mbT))) = CTag (n, Just (f d, mbT))

-- Take care of enforcing certain constraints and hide internals
-- toTag name data dataType
toTag :: ByteString -> Maybe ByteString -> Maybe ByteString -> Tag
toTag n Nothing _         = toTag' (n, Nothing)
toTag n (Just d) Nothing  = toTag' (n, Just (d, Nothing))
toTag n (Just d) (Just t) = toTag' (n, Just (d, Just t))

-- TODO: this operation can fail. Use Monad or MonadFail
toTag' :: (ByteString, Maybe (ByteString, Maybe ByteString)) -> Tag
toTag' (n, mbP) -- The Name and a Maybe Pair

    -- Always use 6 digit, second precision timestamps
    | ucn `elem` ["TIME_ON", "TIME_OFF"] = (\x-> x <> B.replicate (6 - B.length x) '0') `tMap` ucnTag

    -- Use upper case in data with these tags
    | ucn `elem` ["CALL", "GRIDSQUARE", "MY_GRIDSQUARE"] = toUpper `tMap` ucnTag

    -- Otherwise just use the uppercased-but-untreated tag
    | otherwise = ucnTag
    where
        ucnTag = CTag (toUpper n, mbP) -- The name is upper case, but data & data type are untreated
        ucn    = tagName ucnTag        -- The Upper Case Name

tagName :: Tag -> ByteString
tagName = fst . fromTag

tagData :: Tag -> Maybe ByteString
tagData =  (<$>) fst . snd . fromTag

tagDataLength :: Tag -> Maybe Int
tagDataLength = (<$>) (fromIntegral . B.length) . tagData

tagDataType :: Tag -> Maybe ByteString
tagDataType = join . (<$>) snd . snd . fromTag

isTagName :: ByteString -> Tag -> Bool
isTagName = flip $ (==) . tagName

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
field fn r = case join $ lookup fn $ map fromTag $ fromRecord r of
    Just (d, _) -> Just d
    Nothing     -> Nothing

-- Break up a list of tags parsed from the body of an ADIF file to
-- a neat list of records. Empty records are dropped.
records :: [Tag] -> [Record]
records ts = map toRecord $ filter (not . null) $ S.splitWhen isEOR ts

mergeTags :: [[Tag]] -> [Tag]
mergeTags = map head . groupWith tagName . sortWith tagName . L.concat

mergeRecords :: [[Record]] -> [Record]
mergeRecords = map (toRecord . mergeTags . map fromRecord) . groupWith qsoKey . sortWith qsoKey . L.concat

mergeLogs :: [Log] -> Log
mergeLogs ls = Log "" [] (mergeRecords $ map logRecords ls)

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
    , fromMaybe "---"    $ field "MODE"     r
    , fromMaybe "---" (field "BAND" r) <> "/" <> fromMaybe "---" (field "FREQ" r)
    , fromMaybe "---" (field "RST_RCVD" r) <> "/" <> fromMaybe "---" (field "RST_SENT" r)
    ]

showLog :: Log -> ByteString
showLog (Log htxt htags recs) =  B.intercalate "\n" $ map (\(n, r) -> B.pack (show n) <> " " <> r) $ zip [1..] $ map showRecord recs

showTag :: Tag -> ByteString
showTag t = case tagData t of
    Just d  -> B.concat [tagName t, "=", d]
    Nothing -> tagName t

-- TODO: would it be better to just filter out tags without any data?
qsoFieldNames :: Log -> [ByteString]
qsoFieldNames = map head . L.group . L.sort . concat . map (map (fst . fromTag) . fromRecord) . logRecords

-- Return values from an association list.
-- The values will be ordered by the appeareance of their
-- field names and according to the field name list given
-- as the first argument.
-- The returned list will be as long as the field name list.
-- The values are wrapped in Maybe. A Nothing value at a position
-- means that no value was found in the association list at the
-- given position.
-- Values with field names not in the field list are ignored.
alValues :: Eq a => [a] -> [(a, b)] -> [Maybe b]
alValues fn al = map (flip lookup al) fn

qsoTableRow :: [ByteString] -> Record -> [ByteString]
qsoTableRow fns = map (fromMaybe "" . (fst<$>) . join) . alValues fns . map fromTag . fromRecord

qsoTable :: Log -> [[ByteString]]
qsoTable log = fieldNames : map (qsoTableRow fieldNames) (logRecords log)
    where
        fieldNames = qsoFieldNames log

toUpper :: ByteString -> ByteString
toUpper = B.map CH.toUpper

-- Use a name and a value vector to build
vectorsToTags :: V.Vector ByteString -> V.Vector ByteString -> [Tag]
vectorsToTags ns vs = V.toList $ V.map (\(n, v) -> toTag n (Just v) Nothing) $ V.zip ns vs
