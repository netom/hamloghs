{-# Language OverloadedStrings #-}
module HlOptions
    ( getHomeOption
    , OutputFormat (..)
    , FlExp (..)
    , adifRecordArguments
    , inputFileArguments
    , inputHandleArgument
    , separatorOption
    , quoteOption
    , escapeOption
    , filterOptions
    ) where

import Data.Maybe
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as BU
import HlLog
import Control.Applicative
import Options.Applicative
import System.Directory
import System.Environment
import System.IO
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Text.Regex.Base.RegexLike
import Text.Regex.TDFA.ByteString

data OutputFormat = ADIF | LIST deriving (Read)

data FlExp
    = FlGte TagName ByteString
    | FlGt  TagName ByteString
    | FlLte TagName ByteString
    | FlLt  TagName ByteString
    | FlReg TagName Regex
    | FlEq  TagName ByteString
    | FlNeq TagName ByteString
    | FlEx  TagName

parseAdifRecord :: Monad m => String -> m Tag
parseAdifRecord s = do
    if '=' `elem` s then do
        let t = break (== '=') s
        return $ toTag (B.pack $ fst t) (Just $ BU.fromString $ drop 1 $ snd t) Nothing
    else return $ toTag (B.pack s) Nothing Nothing

parseChar :: (Monad m, MonadFail m) => String -> m Char
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

-- the ap* functions are parsers written with AttoParsec

apParseTagName :: AP.Parser ByteString
apParseTagName = B.pack <$> AP.many1' (AP.letter_ascii <|> AP.digit <|> AP.char '_') <|> fail "A valid tag name must be given"

apParseFlValue :: AP.Parser ByteString
apParseFlValue = AP.takeByteString

runEither :: (Monad m, MonadFail m) => Either String a -> m a
runEither e =
    case e of
    Right x -> return x
    Left s  -> fail s

apParseFlExp :: AP.Parser FlExp
apParseFlExp
      = ( FlGte <$> apParseTagName <*> ( (AP.string ">=") >> apParseFlValue ) )
    <|> ( FlGt  <$> apParseTagName <*> ( (AP.string ">" ) >> apParseFlValue ) )
    <|> ( FlLte <$> apParseTagName <*> ( (AP.string "<=") >> apParseFlValue ) )
    <|> ( FlLt  <$> apParseTagName <*> ( (AP.string "<" ) >> apParseFlValue ) )
    <|> ( FlReg <$> apParseTagName <*> ( (AP.string "=~") >> (apParseFlValue >>= runEither . compile defaultCompOpt defaultExecOpt) ) )
    <|> ( FlEq  <$> apParseTagName <*> ( (AP.string "=" ) >> apParseFlValue ) )
    <|> ( FlNeq <$> apParseTagName <*> ( (AP.string "!=") >> apParseFlValue ) )
    <|> ( AP.string "*" >> FlEx <$> apParseTagName )

parseFlExp :: (Monad m, MonadFail m) => String -> m FlExp
parseFlExp s =
    case AP.parseOnly (apParseFlExp <* AP.endOfInput) (B.pack s) of
    Right flExp -> return flExp
    Left err -> fail err

filterOptions :: Parser [FlExp]
filterOptions = (some . option (str >>= parseFlExp))
    $ long "filter"
   <> short 'f'
   <> metavar "FILTER"
   <> help "A filter expression is a field name and a literal with a binary boolean-valued operator. The supported ones are: <, >, <=, >=, =, ~"
