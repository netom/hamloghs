module Main where

import HlAdif
import HlOptions
import Options.Applicative
import System.Directory
import System.IO
import System.IO.Temp
import qualified Data.Text.IO as TIO

data Options = Options
  { basedir :: String
  , files :: [String]
  }

getOptionsParserInfo :: IO (ParserInfo Options)
getOptionsParserInfo = do
    homeOption <- getHomeOption
    return $ info (helper <*> (
        Options
            <$> homeOption
            <*> inputFileArguments
      )) (
        fullDesc
            <> progDesc "Import multiple ADIF files into the main database"
      )

parseErrorHandler :: Either String Log -> IO Log
parseErrorHandler (Left errorMessage) = fail errorMessage
parseErrorHandler (Right log) = return log

doImport :: Options -> IO ()
doImport opt = do
    let bd = basedir opt
    let dbFileName = bd ++ "/data/hl.adi"

    handleList   <- mapM (flip openFile ReadMode) $ dbFileName : files opt
    (tmpn, tmph) <- openTempFile bd "tmp.adi"
    contentList  <- mapM TIO.hGetContents handleList

    (writeLog <$> mergeLogs <$> mapM (parseErrorHandler . adifLogParser) contentList) >>= TIO.hPutStr tmph

    hClose tmph
    mapM hClose handleList

    renameFile tmpn $ basedir opt ++ "/data/hl.adi"

main :: IO ()
main = getOptionsParserInfo >>= execParser >>= doImport
