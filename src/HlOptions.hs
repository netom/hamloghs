module HlOptions
    ( getHomeOption
    , outputFormatOption
    , OutputFormat (..)
    , adifRecordArguments
    , inputFileArguments
    , inputHandleArgument
    ) where

import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as BU
import HlLog
import Options.Applicative
import System.Directory
import System.Environment
import System.IO

data OutputFormat = ADIF | LIST deriving (Read)

parseAdifRecord :: Monad m => String -> m Tag
parseAdifRecord s = do
    if '=' `elem` s then do
        let t = break (== '=') s
        return $ toTag (B.pack $ fst t) (Just $ BU.fromString $ drop 1 $ snd t) Nothing
    else return $ toTag (B.pack s) Nothing Nothing

--parseInputFile :: Monad m => 

adifRecordArguments :: Parser [Tag]
adifRecordArguments = some (argument (str >>= parseAdifRecord) (metavar "FIELDS..."))

inputFileArguments :: Parser [String]
inputFileArguments = some (argument str (metavar "FILES..."))

inputHandleArgument :: Parser (IO Handle)
inputHandleArgument = fromMaybe (return stdin) <$> (optional $ flip openFile ReadMode <$> argument str (metavar "FILE..."))

getHomeOption :: IO (Parser String)
getHomeOption = do
    userHome <- getHomeDirectory
    home <- fromMaybe (userHome ++ "/.hl") <$> lookupEnv "HL_HOME"
    return $ strOption
      $ long "home"
     <> value home
     <> metavar "HL_HOME"
     <> help "The root directory of the application data, i.e. where your log is stored."

outputFormatOption = option auto
    $ long "output-format"
   <> short 'o'
   <> value ADIF
   <> metavar "HL_OUTPUT_FORMAT"
   <> help "Output format. One of ADIF, LIST"
