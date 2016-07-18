{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe
import HlLog
import HlAdif
import HlOptions
import Options.Applicative
import Prelude hiding (readFile, putStr)
import System.Environment
import System.IO hiding (readFile, putStr)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.List as L
import qualified Data.ByteString.Search as S
import Data.ByteString.Lazy (fromStrict, toStrict)

data Options = Options
  { getInputHandle :: IO Handle
  }

getOptionsParserInfo :: IO (ParserInfo Options)
getOptionsParserInfo = do
    return $ info (helper <*> (
        Options
            <$> inputHandleArgument
      )) (
        fullDesc
            <> progDesc "Present the contents of an ADIF file as a Microsoft Office 2003 XML spreadsheet"
      )

-- Return field names from an association list in their natural order.
alFields :: (Ord a, Eq a) => [(a, b)] -> [a]
alFields = L.nub . map fst

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

csvValues :: [Maybe ByteString] -> [ByteString]
csvValues = map (fromMaybe "")

xmlRow :: [ByteString] -> ByteString
xmlRow bss = "      <Row>\n" <> B.concat (map xmlCell bss) <> "      </Row>\n"

xmlCell :: ByteString -> ByteString
xmlCell s = "        <Cell><Data ss:Type=\"String\">" <> escaped <> "</Data></Cell>\n"
    where
        escaped = foldl escape s [("&", "&amp;"), ("\"", "&quot;"), ("'", "&apos;"), ("<", "&lt;"), (">", "&gt;")]
        escape :: ByteString -> (ByteString, ByteString) -> ByteString
        escape s (toS, toR) = toStrict $ S.replace toS toR s

main :: IO ()
main = getOptionsParserInfo >>= execParser >>= \opt -> do
    h <- getInputHandle opt
    parseResult <- adifLogParser <$> B.hGetContents h

    case parseResult of
        Left errorMsg -> putStrLn errorMsg
        Right log -> do
            -- TODO: would it be better to just filter out tags without any data?
            let alList = map (map (\t->(tagName t, fromMaybe "" $ tagData t)) . fromRecord) $ logRecords log
            let fieldNames = alFields $ concat alList
            B.putStr $
                "<?xml version=\"1.0\"?>\n" <>
                "<?mso-application progid=\"Excel.Sheet\"?>\n" <>
                "<Workbook\n" <>
                "   xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\"\n" <>
                "   xmlns:o=\"urn:schemas-microsoft-com:office:office\"\n" <>
                "   xmlns:x=\"urn:schemas-microsoft-com:office:excel\"\n" <>
                "   xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\"\n" <>
                "   xmlns:html=\"http://www.w3.org/TR/REC-html40\">\n" <>
                "  <DocumentProperties xmlns=\"urn:schemas-microsoft-com:office:office\">\n" <>
                "    <Author>HamLogHS</Author>\n" <>
                "    <LastAuthor>Self</LastAuthor>\n" <>
                "    <Created>2012-03-15T23:04:04Z</Created>\n" <> -- TODO: we could use the real date
                "    <Company>HamLogHS</Company>\n" <> -- TODO: is this necessary?
                "    <Version>1.0</Version>\n" <> -- TODO: and this?
                "  </DocumentProperties>\n" <>
                "  <ExcelWorkbook xmlns=\"urn:schemas-microsoft-com:office:excel\">\n" <> -- TODO: and this?
                "    <WindowHeight>6795</WindowHeight>\n" <>
                "    <WindowWidth>8460</WindowWidth>\n" <>
                "    <WindowTopX>120</WindowTopX>\n" <>
                "    <WindowTopY>15</WindowTopY>\n" <>
                "    <ProtectStructure>False</ProtectStructure>\n" <>
                "    <ProtectWindows>False</ProtectWindows>\n" <>
                "  </ExcelWorkbook>\n" <>
                "  <Styles>\n" <>
                "    <Style ss:ID=\"Default\" ss:Name=\"Normal\">\n" <>
                "      <Alignment ss:Vertical=\"Bottom\" />\n" <>
                "      <Borders />\n" <>
                "      <Font />\n" <>
                "      <Interior />\n" <>
                "      <NumberFormat />\n" <>
                "      <Protection />\n" <>
                "    </Style>\n" <>
                "    <Style ss:ID=\"s21\">\n" <>
                "      <Font x:Family=\"Swiss\" ss:Bold=\"1\" />\n" <>
                "    </Style>\n" <>
                "  </Styles>\n" <>
                "  <Worksheet ss:Name=\"QSOs\">\n" <>
                "    <Table ss:ExpandedColumnCount=\"2\" ss:ExpandedRowCount=\"5\"\n" <>
                "      x:FullColumns=\"1\" x:FullRows=\"1\">\n"
            B.putStr $ xmlRow fieldNames
            mapM_ (B.putStr . xmlRow . csvValues . alValues fieldNames) alList
            B.putStr $
                "    </Table>\n" <>
                "    <WorksheetOptions xmlns=\"urn:schemas-microsoft-com:office:excel\">\n" <>
                "      <Print>\n" <>
                "        <ValidPrinterInfo />\n" <>
                "        <HorizontalResolution>600</HorizontalResolution>\n" <>
                "        <VerticalResolution>600</VerticalResolution>\n" <>
                "      </Print>\n" <>
                "      <Selected />\n" <>
                "      <Panes>\n" <>
                "        <Pane>\n" <>
                "          <Number>3</Number>\n" <>
                "          <ActiveRow>5</ActiveRow>\n" <>
                "          <ActiveCol>1</ActiveCol>\n" <>
                "        </Pane>\n" <>
                "      </Panes>\n" <>
                "      <Table>\n" <>
                "      </Table>\n" <>
                "      <ProtectObjects>False</ProtectObjects>\n" <>
                "      <ProtectScenarios>False</ProtectScenarios>\n" <>
                "    </WorksheetOptions>\n" <>
                "  </Worksheet>\n" <>
                "</Workbook>"

