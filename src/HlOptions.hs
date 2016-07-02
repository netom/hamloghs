module HlOptions
    ( getHomeOption
    , outputFormatOption
    , OutputFormat (..)
    , adifRecordArguments
    ) where

import Data.Maybe
import Data.Text hiding (break, tail, drop)
import HlAdif
import Options.Applicative
import System.Directory
import System.Environment

data OutputFormat = ADIF | LIST deriving (Read)

parseAdifRecord :: Monad m => String -> m Tag
parseAdifRecord s = do
    if '=' `elem` s then do
        let t = break (== '=') s
        return $ toTag $ (fst t, Just $ drop 1 $ snd t)
    else return $ toTag (s, Nothing :: Maybe String)

adifRecordArguments :: [Tag] -> Parser [Tag]
adifRecordArguments defaultTags = do
    mergeTags defaultTags <$> some (argument (str >>= parseAdifRecord) (metavar "FIELDS..."))

getHomeOption :: IO (Parser String)
getHomeOption = do
    userHome <- getHomeDirectory
    home <- fromMaybe (userHome ++ "/.hl") <$> lookupEnv "HL_HOME"
    return $ strOption
      $ long "home"
     <> value home
     <> metavar "HL_HOME"
     <> help "The root directory of the application data, i.e. where your log is stored."

--outputFormatOption :: Parser OutputFormat
outputFormatOption = option auto
    $ long "output-format"
   <> short 'o'
   <> value ADIF
   <> metavar "HL_OUTPUT_FORMAT"
   <> help "Output format. One of ADIF, LIST"
