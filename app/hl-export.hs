module Main where

import Lib
import Options.Applicative

data DatabaseOptions = DatabaseOptions
  { basedir :: String
  }

databaseOptions :: Parser DatabaseOptions
databaseOptions = DatabaseOptions
  <$> strOption
      ( long "basedir"
     <> value "~/.hamloghs/data"
     <> short 'b'
     <> metavar "BASEDIR"
     <> help "The root directod of the " )

greet :: DatabaseOptions -> IO ()
greet dbo = putStrLn $ "Using " ++ basedir dbo

main :: IO ()
main = do
    execParser opts >>= greet
      where
        opts = info (helper <*> databaseOptions)
          ( fullDesc
         <> progDesc "Print a greeting for TARGET"
         <> header "hello - a test for optparse-applicative" )
