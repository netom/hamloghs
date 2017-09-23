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
            <> progDesc "Present the contents of an ADIF file as a Flat Open Document Spreadsheet."
      )

xmlRow :: [ByteString] -> ByteString
xmlRow bss = "    <table:table-row table:style-name=\"ro1\">\n" <> B.concat (map xmlCell bss) <> "    </table:table-row>\n"

xmlCell :: ByteString -> ByteString
xmlCell s = "     <table:table-cell office:value-type=\"string\" calcext:value-type=\"string\">\n      <text:p>" <> escaped <> "</text:p>\n     </table:table-cell>\n"
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
            B.putStr $
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" <>
                "<office:document xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\" xmlns:style=\"urn:oasis:names:tc:opendocument:xmlns:style:1.0\" xmlns:text=\"urn:oasis:names:tc:opendocument:xmlns:text:1.0\" xmlns:table=\"urn:oasis:names:tc:opendocument:xmlns:table:1.0\" xmlns:draw=\"urn:oasis:names:tc:opendocument:xmlns:drawing:1.0\" xmlns:fo=\"urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:meta=\"urn:oasis:names:tc:opendocument:xmlns:meta:1.0\" xmlns:number=\"urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0\" xmlns:presentation=\"urn:oasis:names:tc:opendocument:xmlns:presentation:1.0\" xmlns:svg=\"urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0\" xmlns:chart=\"urn:oasis:names:tc:opendocument:xmlns:chart:1.0\" xmlns:dr3d=\"urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0\" xmlns:math=\"http://www.w3.org/1998/Math/MathML\" xmlns:form=\"urn:oasis:names:tc:opendocument:xmlns:form:1.0\" xmlns:script=\"urn:oasis:names:tc:opendocument:xmlns:script:1.0\" xmlns:config=\"urn:oasis:names:tc:opendocument:xmlns:config:1.0\" xmlns:ooo=\"http://openoffice.org/2004/office\" xmlns:ooow=\"http://openoffice.org/2004/writer\" xmlns:oooc=\"http://openoffice.org/2004/calc\" xmlns:dom=\"http://www.w3.org/2001/xml-events\" xmlns:xforms=\"http://www.w3.org/2002/xforms\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:rpt=\"http://openoffice.org/2005/report\" xmlns:of=\"urn:oasis:names:tc:opendocument:xmlns:of:1.2\" xmlns:xhtml=\"http://www.w3.org/1999/xhtml\" xmlns:grddl=\"http://www.w3.org/2003/g/data-view#\" xmlns:tableooo=\"http://openoffice.org/2009/table\" xmlns:drawooo=\"http://openoffice.org/2010/draw\" xmlns:calcext=\"urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0\" xmlns:loext=\"urn:org:documentfoundation:names:experimental:office:xmlns:loext:1.0\" xmlns:field=\"urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0\" xmlns:formx=\"urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:form:1.0\" xmlns:css3t=\"http://www.w3.org/TR/css3-text/\" office:version=\"1.2\" office:mimetype=\"application/vnd.oasis.opendocument.spreadsheet\">\n" <>
                " <office:meta>\n" <>
                "  <meta:generator>HamLogHS</meta:generator>\n" <>
                " </office:meta>\n" <>
                " <office:font-face-decls>\n" <>
                "  <style:font-face style:name=\"Liberation Sans\" svg:font-family=\"&apos;Liberation Sans&apos;\" style:font-family-generic=\"swiss\" style:font-pitch=\"variable\"/>\n" <>
                "  <style:font-face style:name=\"DejaVu Sans\" svg:font-family=\"&apos;DejaVu Sans&apos;\" style:font-family-generic=\"system\" style:font-pitch=\"variable\"/>\n" <>
                "  <style:font-face style:name=\"FreeSans\" svg:font-family=\"FreeSans\" style:font-family-generic=\"system\" style:font-pitch=\"variable\"/>\n" <>
                "  <style:font-face style:name=\"SimSun\" svg:font-family=\"SimSun\" style:font-family-generic=\"system\" style:font-pitch=\"variable\"/>\n" <>
                " </office:font-face-decls>\n" <>
                " <office:styles>\n" <>
                "  <style:default-style style:family=\"table-cell\">\n" <>
                "   <style:paragraph-properties style:tab-stop-distance=\"12.5mm\"/>\n" <>
                "   <style:text-properties style:font-name=\"Liberation Sans\" fo:language=\"en\" fo:country=\"US\" style:font-name-asian=\"DejaVu Sans\" style:language-asian=\"zh\" style:country-asian=\"CN\" style:font-name-complex=\"DejaVu Sans\" style:language-complex=\"hi\" style:country-complex=\"IN\"/>\n" <>
                "  </style:default-style>\n" <>
                "  <number:number-style style:name=\"N0\">\n" <>
                "   <number:number number:min-integer-digits=\"1\"/>\n" <>
                "  </number:number-style>\n" <>
                "  <number:currency-style style:name=\"N108P0\" style:volatile=\"true\">\n" <>
                "   <number:number number:decimal-places=\"2\" loext:min-decimal-places=\"2\" number:min-integer-digits=\"1\" number:grouping=\"true\"/>\n" <>
                "   <number:text> </number:text>\n" <>
                "   <number:currency-symbol number:language=\"en\" number:country=\"US\">$</number:currency-symbol>\n" <>
                "  </number:currency-style>\n" <>
                "  <number:currency-style style:name=\"N108\">\n" <>
                "   <style:text-properties fo:color=\"#ff0000\"/>\n" <>
                "   <number:text>-</number:text>\n" <>
                "   <number:number number:decimal-places=\"2\" loext:min-decimal-places=\"2\" number:min-integer-digits=\"1\" number:grouping=\"true\"/>\n" <>
                "   <number:text> </number:text>\n" <>
                "   <number:currency-symbol number:language=\"en\" number:country=\"US\">$</number:currency-symbol>\n" <>
                "   <style:map style:condition=\"value()&gt;=0\" style:apply-style-name=\"N108P0\"/>\n" <>
                "  </number:currency-style>\n" <>
                "  <style:style style:name=\"Default\" style:family=\"table-cell\">\n" <>
                "   <style:text-properties style:font-name-asian=\"SimSun\" style:font-family-asian=\"SimSun\" style:font-family-generic-asian=\"system\" style:font-pitch-asian=\"variable\" style:font-name-complex=\"FreeSans\" style:font-family-complex=\"FreeSans\" style:font-family-generic-complex=\"system\" style:font-pitch-complex=\"variable\"/>\n" <>
                "  </style:style>\n" <>
                "  <style:style style:name=\"Result\" style:family=\"table-cell\" style:parent-style-name=\"Default\">\n" <>
                "   <style:text-properties fo:font-style=\"italic\" style:text-underline-style=\"solid\" style:text-underline-width=\"auto\" style:text-underline-color=\"font-color\" fo:font-weight=\"bold\"/>\n" <>
                "  </style:style>\n" <>
                "  <style:style style:name=\"Result2\" style:family=\"table-cell\" style:parent-style-name=\"Result\" style:data-style-name=\"N108\"/>\n" <>
                "  <style:style style:name=\"Heading\" style:family=\"table-cell\" style:parent-style-name=\"Default\">\n" <>
                "   <style:table-cell-properties style:text-align-source=\"fix\" style:repeat-content=\"false\"/>\n" <>
                "   <style:paragraph-properties fo:text-align=\"center\"/>\n" <>
                "   <style:text-properties fo:font-size=\"16pt\" fo:font-style=\"italic\" fo:font-weight=\"bold\"/>\n" <>
                "  </style:style>\n" <>
                "  <style:style style:name=\"Heading1\" style:family=\"table-cell\" style:parent-style-name=\"Heading\">\n" <>
                "   <style:table-cell-properties style:rotation-angle=\"90\"/>\n" <>
                "  </style:style>\n" <>
                " </office:styles>\n" <>
                " <office:automatic-styles>\n" <>
                "  <style:style style:name=\"co1\" style:family=\"table-column\">\n" <>
                "   <style:table-column-properties fo:break-before=\"auto\" style:column-width=\"22.58mm\"/>\n" <>
                "  </style:style>\n" <>
                "  <style:style style:name=\"ro1\" style:family=\"table-row\">\n" <>
                "   <style:table-row-properties style:row-height=\"4.52mm\" fo:break-before=\"auto\" style:use-optimal-row-height=\"true\"/>\n" <>
                "  </style:style>\n" <>
                "  <style:style style:name=\"ta1\" style:family=\"table\" style:master-page-name=\"Default\">\n" <>
                "   <style:table-properties table:display=\"true\" style:writing-mode=\"lr-tb\"/>\n" <>
                "  </style:style>\n" <>
                "  <style:page-layout style:name=\"pm1\">\n" <>
                "   <style:page-layout-properties style:writing-mode=\"lr-tb\"/>\n" <>
                "   <style:header-style>\n" <>
                "    <style:header-footer-properties fo:min-height=\"7.5mm\" fo:margin-left=\"0mm\" fo:margin-right=\"0mm\" fo:margin-bottom=\"2.5mm\"/>\n" <>
                "   </style:header-style>\n" <>
                "   <style:footer-style>\n" <>
                "    <style:header-footer-properties fo:min-height=\"7.5mm\" fo:margin-left=\"0mm\" fo:margin-right=\"0mm\" fo:margin-top=\"2.5mm\"/>\n" <>
                "   </style:footer-style>\n" <>
                "  </style:page-layout>\n" <>
                "  <style:page-layout style:name=\"pm2\">\n" <>
                "   <style:page-layout-properties style:writing-mode=\"lr-tb\"/>\n" <>
                "   <style:header-style>\n" <>
                "    <style:header-footer-properties fo:min-height=\"7.5mm\" fo:margin-left=\"0mm\" fo:margin-right=\"0mm\" fo:margin-bottom=\"2.5mm\" fo:border=\"2.49pt solid #000000\" fo:padding=\"0.18mm\" fo:background-color=\"#c0c0c0\">\n" <>
                "     <style:background-image/>\n" <>
                "    </style:header-footer-properties>\n" <>
                "   </style:header-style>\n" <>
                "   <style:footer-style>\n" <>
                "    <style:header-footer-properties fo:min-height=\"7.5mm\" fo:margin-left=\"0mm\" fo:margin-right=\"0mm\" fo:margin-top=\"2.5mm\" fo:border=\"2.49pt solid #000000\" fo:padding=\"0.18mm\" fo:background-color=\"#c0c0c0\">\n" <>
                "     <style:background-image/>\n" <>
                "    </style:header-footer-properties>\n" <>
                "   </style:footer-style>\n" <>
                "  </style:page-layout>\n" <>
                " </office:automatic-styles>\n" <>
                " <office:master-styles>\n" <>
                "  <style:master-page style:name=\"Default\" style:page-layout-name=\"pm1\">\n" <>
                "   <style:header>\n" <>
                "    <text:p><text:sheet-name>???</text:sheet-name></text:p>\n" <>
                "   </style:header>\n" <>
                "   <style:header-left style:display=\"false\"/>\n" <>
                "   <style:footer>\n" <>
                "    <text:p>Page <text:page-number>1</text:page-number></text:p>\n" <>
                "   </style:footer>\n" <>
                "   <style:footer-left style:display=\"false\"/>\n" <>
                "  </style:master-page>\n" <>
                "  <style:master-page style:name=\"Report\" style:page-layout-name=\"pm2\">\n" <>
                "   <style:header>\n" <>
                "    <style:region-left>\n" <>
                "     <text:p><text:sheet-name>???</text:sheet-name> (<text:title>???</text:title>)</text:p>\n" <>
                "    </style:region-left>\n" <>
                "    <style:region-right>\n" <>
                "     <text:p><text:date style:data-style-name=\"N2\" text:date-value=\"2016-07-18\">0000.00.00</text:date>, <text:time style:data-style-name=\"N2\" text:time-value=\"16:04:18.212954126\">00:00:00</text:time></text:p>\n" <>
                "    </style:region-right>\n" <>
                "   </style:header>\n" <>
                "   <style:header-left style:display=\"false\"/>\n" <>
                "   <style:footer>\n" <>
                "    <text:p>Page <text:page-number>1</text:page-number> / <text:page-count>99</text:page-count></text:p>\n" <>
                "   </style:footer>\n" <>
                "   <style:footer-left style:display=\"false\"/>\n" <>
                "  </style:master-page>\n" <>
                " </office:master-styles>\n" <>
                " <office:body>\n" <>
                "  <office:spreadsheet>\n" <>
                "   <table:calculation-settings table:automatic-find-labels=\"false\"/>\n" <>
                "   <table:table table:name=\"QSOs\" table:style-name=\"ta1\">\n"
            mapM_ B.putStr $ map xmlRow $ qsoTable log
            B.putStr $
                "   </table:table>\n" <>
                "   <table:named-expressions/>\n" <>
                "  </office:spreadsheet>\n" <>
                " </office:body>\n" <>
                "</office:document>\n"
