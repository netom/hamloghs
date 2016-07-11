module HlOptions
    ( getHomeOption
    , outputFormatOption
    , OutputFormat (..)
    , adifRecordArguments
    , inputFileArguments
    ) where

import Data.Maybe
import qualified Data.ByteString.Char8 as B
import HlLog
import Options.Applicative
import System.Directory
import System.Environment

data OutputFormat = ADIF | LIST deriving (Read)

parseAdifRecord :: Monad m => String -> m Tag
parseAdifRecord s = do
    if '=' `elem` s then do
        let t = break (== '=') s
        return $ toTag $ (B.pack $ fst t, (Nothing, Just $ B.pack $ drop 1 $ snd t))
    else return $ toTag (B.pack s, (Nothing, Nothing))

adifRecordArguments :: Parser [Tag]
adifRecordArguments = some (argument (str >>= parseAdifRecord) (metavar "FIELDS..."))

inputFileArguments :: Parser [String]
inputFileArguments = some (argument str (metavar "FILES..."))

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
