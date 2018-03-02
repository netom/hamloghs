{-# LANGUAGE OverloadedStrings #-}
module Main where

import HlLog
import HlAdif
import HlOptions
import Options.Applicative
import Prelude hiding (readFile, putStr)
import System.IO hiding (readFile, putStr)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Search as S
import Data.ByteString.Lazy (toStrict)
import Data.Semigroup ((<>))

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
            <> progDesc "Present the contents of an ADIF file as a Microsoft Office 2003 XML spreadsheet."
      )

xmlRow :: [ByteString] -> ByteString
xmlRow bss = "      <Row>\n" <> B.concat (map xmlCell bss) <> "      </Row>\n"

xmlCell :: ByteString -> ByteString
xmlCell s = "        <Cell><Data ss:Type=\"String\">" <> escaped <> "</Data></Cell>\n"
    where
        escaped = foldl escape s [("&", "&amp;"), ("\"", "&quot;"), ("'", "&apos;"), ("<", "&lt;"), (">", "&gt;")]
        escape :: ByteString -> (ByteString, ByteString) -> ByteString
        escape bs (toS, toR) = toStrict $ S.replace toS toR bs

main :: IO ()
main = getOptionsParserInfo >>= execParser >>= \opt -> do
    h <- getInputHandle opt
    parseResult <- adifLogParser <$> B.hGetContents h

    case parseResult of
        Left errorMsg -> putStrLn errorMsg
        Right l -> do
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
            mapM_ B.putStr $ map xmlRow $ qsoTable l
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
