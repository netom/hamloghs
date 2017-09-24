{-# Language OverloadedStrings #-}
module HlOptions
    ( getHomeOption
    , OutputFormat (..)
    , FlExp (..)
    , FlOp (..)
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
import Data.Semigroup ((<>))
import qualified Data.Attoparsec.ByteString.Char8 as AP

data OutputFormat = ADIF | LIST deriving (Read)

data FlOp
    = FlGte
    | FlGt
    | FlLte
    | FlLt
    | FlReg
    | FlEq
    | FlNeq

data FlExp = FlExp ByteString FlOp ByteString

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

-- the ap* functions are parsers written with AttoParsec

apParseFlExpLeft :: AP.Parser ByteString
apParseFlExpLeft = B.pack <$> AP.many1' (AP.letter_ascii <|> AP.digit <|> AP.char '_') <|> fail "A valid tag name must be given"

apParseFlOp :: AP.Parser FlOp
apParseFlOp
      = ( AP.string ">=" >> return FlGte )
    <|> ( AP.string ">"  >> return FlGt  )
    <|> ( AP.string "<=" >> return FlLte )
    <|> ( AP.string "<"  >> return FlLt  )
    <|> ( AP.string "=~" >> return FlReg )
    <|> ( AP.string "="  >> return FlEq  )
    <|> ( AP.string "!=" >> return FlNeq )
    <|> fail "Missing or unsupported operator"

apParseFlExpRight :: AP.Parser ByteString
apParseFlExpRight = AP.takeByteString

apParseFlExp :: AP.Parser FlExp
apParseFlExp = FlExp <$> apParseFlExpLeft <*> apParseFlOp <*> apParseFlExpRight

parseFlExp :: Monad m => String -> m FlExp
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
