module HlOptions
    ( getHomeOption
    ) where

import Options.Applicative
import Data.Maybe
import System.Environment

getHomeOption = do
    home <- fromMaybe "~/.hl/data" <$> lookupEnv "HL_HOME"
    return $ strOption
      $ long "home"
     <> value home
     <> metavar "HL_HOME"
     <> help "The root directory of the application, i.e. where you data is kept."
