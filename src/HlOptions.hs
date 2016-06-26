module HlOptions
    ( getHomeOption
    , outputFormatOption
    , OutputFormat (..)
    ) where

import Options.Applicative
import Data.Maybe
import System.Directory
import System.Environment
--import Control.Monad.Trans.Reader (ask)

data OutputFormat = SHOW | ADIF | CSV | TSV deriving (Read)

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
   <> value SHOW
   <> metavar "HL_OUTPUT_FORMAT"
   <> help "Output format. One of SHOW, ADIF, CSV, TSV"
