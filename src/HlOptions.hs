module HlOptions
    ( getHomeOption
    , outputFormatOption
    , OutputFormat (..)
    , adifOptions
    ) where

import Options.Applicative
import Data.Maybe
import System.Directory
import System.Environment
--import Control.Monad.Trans.Reader (ask)

data OutputFormat = ADIF | LIST deriving (Read)

adifOptionNames = ["address", "address-intl", "age", "a-index", "ant-az", 
    "ant-el", "ant-path", "arrl-sect", "award-submitted", "award-granted", "band", 
    "band-rx", "call", "check", "class", "clublog-qso-upload-date", 
    "clublog-qso-upload-status", "cnty", "comment", "comment-intl", "cont", 
    "contacted-op", "contest-id", "country", "country-intl", "cqz", 
    "credit-submitted", "credit-granted", "distance", "dxcc", "email", "eq-call", 
    "eqsl-qslrdate", "eqsl-qslsdate", "eqsl-qsl-rcvd", "eqsl-qsl-sent", "fists", 
    "fists-cc", "force-init", "freq", "freq-rx", "gridsquare", "guest-op", 
    "hrdlog-qso-upload-date", "hrdlog-qso-upload-status", "iota", "iota-island-id", 
    "ituz", "k-index", "lat", "lon", "lotw-qslrdate", "lotw-qslsdate", 
    "lotw-qsl-rcvd", "lotw-qsl-sent", "max-bursts", "mode", "ms-shower", "my-city", 
    "my-city-intl", "my-cnty", "my-country", "my-country-intl", "my-cq-zone", 
    "my-dxcc", "my-fists", "my-gridsquare", "my-iota", "my-iota-island-id", 
    "my-itu-zone", "my-lat", "my-lon", "my-name", "my-name-intl", "my-postal-code", 
    "my-postal-code-intl", "my-rig", "my-rig-intl", "my-sig", "my-sig-intl", 
    "my-sig-info", "my-sig-info-intl", "my-sota-ref", "my-state", "my-street", 
    "my-street-intl", "my-usaca-counties", "my-vucc-grids", "name", "name-intl", 
    "notes", "notes-intl", "nr-bursts", "nr-pings", "operator", "owner-callsign", 
    "pfx", "precedence", "prop-mode", "public-key", "qrzcom-qso-upload-date", 
    "qrzcom-qso-upload-status", "qslmsg", "qslmsg-intl", "qslrdate", "qslsdate", 
    "qsl-rcvd", "qsl-rcvd-via", "qsl-sent", "qsl-sent-via", "qsl-via", 
    "qso-complete", "qso-date", "qso-date-off", "qso-random", "qth", "qth-intl", 
    "rig", "rig-intl", "rst-rcvd", "rst-sent", "rx-pwr", "sat-mode", "sat-name", 
    "sfi", "sig", "sig-intl", "sig-info", "sig-info-intl", "skcc", "sota-ref", 
    "srx", "srx-string", "state", "station-callsign", "stx", "stx-string", 
    "submode", "swl", "ten-ten", "time-off", "time-on", "tx-pwr", "usaca-counties", 
    "ve-prov", "vucc-grids", "web"]

adifOption :: String -> Parser (Maybe String)
adifOption optName = optional $ strOption $ long optName <> help optName

adifOptions :: Parser [String]
adifOptions = catMaybes <$> traverse adifOption adifOptionNames

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

