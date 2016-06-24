module Main where

import Lib
import Options.Applicative

data DatabaseOptions = DatabaseOptions
  { basedir :: String
  }

databaseOptions :: ParserInfo DatabaseOptions
databaseOptions = info (helper <*> (DatabaseOptions
  <$> strOption -- TODO: options should go into the Lib
      ( long "basedir"
     <> value "~/.hamloghs/data"
     <> short 'b'
     <> metavar "BASEDIR"
     <> help "The root directod of the " )
  ))
  ( fullDesc
         <> progDesc "Print a greeting for TARGET"
         <> header "hello - a test for optparse-applicative" )

greet :: DatabaseOptions -> IO ()
greet dbo = putStrLn $ "Using " ++ basedir dbo

main :: IO ()
main = do
    execParser databaseOptions >>= greet
