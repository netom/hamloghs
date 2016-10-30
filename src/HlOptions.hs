module HlOptions
    ( getHomeOption
    , OutputFormat (..)
    , adifRecordArguments
    , inputFileArguments
    , inputHandleArgument
    , separatorOption
    , quoteOption
    , escapeOption
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

parseChar :: Monad m => String -> m Char
parseChar (c:[]) = return c
parseChar _    = fail "A single character must be given."

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

separatorOption :: Parser Char
separatorOption = option (str >>= parseChar)
    $ long "separator"
   <> short 's'
   <> value ','
   <> metavar "SEPARATOR"
   <> help "This character separates values in the CSV file. Defaults to a comma."

quoteOption :: Parser Char
quoteOption = option (str >>= parseChar)
    $ long "quote"
   <> short 'q'
   <> value '"'
   <> metavar "QUOTE"
   <> help "This character encloses values if the separator, carriage return, or linefeed appears in them. Default is a double quote."

escapeOption :: Parser Char
escapeOption = option (str >>= parseChar)
    $ long "escape"
   <> short 'e'
   <> value '"'
   <> metavar "ESCAPE"
   <> help "This character appears in a quoted value before any quote characters that appear in it. Quote characters appearing in the value are duplicated. Default is double quote."
