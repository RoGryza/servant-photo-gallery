{-|
Server configuration data and related functions.
-}
module PG.Config
  ( Config(..)
  , defaultConfig
  , cfgActualBaseUrl
  , cfgWarpSettings
  , parseConfig
  )
where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Data.Word
import PG.Effects.Logging
import Network.HTTP.Types
import Network.URI
import Network.Wai
import Network.Wai.Handler.Warp
import Text.Toml

-- | Server configuration data. Usually read from "gallery.toml".
data Config = Config
            { cfgSessionDuration :: !NominalDiffTime -- ^ How long before user sesions expire
            , cfgLogLevel :: !LogLevel -- ^ Minimum log level
            , cfgBaseUrl :: !(Maybe URI) -- ^ Base URL for response links
            , cfgHost :: !String -- ^ Host to bind to
            , cfgPort :: !Word16 -- ^ Port to serve on
            , cfgHtpasswd :: !FilePath -- ^ Path to the htpasswd file
            , cfgMediaPath :: !FilePath -- ^ Path to store post media files
            , cfgDatabasePath :: !FilePath -- ^ Path to the post and metadata database
            , cfgTitle :: !Text -- ^ Server title. Can be used by clients to customize the UI
            , cfgDefaultPageSize :: !Word -- ^ How many posts to show if the limit is omitted
            , cfgMaxPageSize :: !Word -- ^ Max posts returned per API call
            } deriving (Show)

instance FromJSON Config where
  parseJSON (Object v) = do
    rawBaseUrl <- v .:? "base_url"
    let baseUrl = case rawBaseUrl of
                     Just u -> Just <$> jsonUri u
                     Nothing -> return Nothing
    Config
                         <$> v .:? "session_duration" .!= cfgSessionDuration defaultConfig
                         <*> v .:? "log_level" .!= cfgLogLevel defaultConfig
                         <*> baseUrl
                         <*> v .:? "host" .!= cfgHost defaultConfig
                         <*> v .:? "port" .!= cfgPort defaultConfig
                         <*> v .:? "htpasswd" .!= cfgHtpasswd defaultConfig
                         <*> v .:? "media_path" .!= cfgMediaPath defaultConfig
                         <*> v .:? "database_path" .!= cfgDatabasePath defaultConfig
                         <*> v .:? "title" .!= cfgTitle defaultConfig
                         <*> v .:? "default_page_size" .!= cfgDefaultPageSize defaultConfig
                         <*> v .:? "max_page_size" .!= cfgMaxPageSize defaultConfig
    where jsonUri :: Value -> Parser URI
          jsonUri (String s) =
            case parseURI (T.unpack s) of
              Just u -> return u
              Nothing -> fail $ "Invalid URI: " <> T.unpack s
          jsonUri u = typeMismatch "String" u
  parseJSON v = typeMismatch "Object" v

-- | Returns the base url for a config or a default based on host and port
cfgActualBaseUrl :: Config -> URI
cfgActualBaseUrl Config { cfgBaseUrl, cfgHost, cfgPort } =
  fromMaybe (fromJust . parseURI $ concat ["http://", cfgHost, ":", show cfgPort]) cfgBaseUrl

-- | Parse a TOML configuration file, using the defaults for omitted values. Returns $Left msg$ on
-- error.
parseConfig :: Text -> Either Text Config
parseConfig s = case parseTomlDoc "" s of
  Right raw -> case fromJSON (toJSON raw) of
    Success c -> Right c
    Error   e -> Left $ T.pack e
  Left e -> Left . T.pack . show $ e

-- | Default configuration values
defaultConfig :: Config
defaultConfig = Config
  { cfgSessionDuration = 30 * 60
  , cfgLogLevel        = LevelDebug
  , cfgBaseUrl         = Nothing
  , cfgHost            = "127.0.0.1"
  , cfgPort            = 8000
  , cfgHtpasswd        = "htpasswd"
  , cfgMediaPath       = "static/media/"
  , cfgDatabasePath    = "gallery.db"
  , cfgTitle           = "Daily Gallery"
  , cfgDefaultPageSize = 10
  , cfgMaxPageSize     = 128
  }

-- | Derive warp settings from server config
cfgWarpSettings :: Config -> Settings
cfgWarpSettings Config { cfgHost, cfgPort } =
  setHost (fromString cfgHost) $ setPort (fromIntegral cfgPort) $ setLogger
    warpLogger
    defaultSettings

warpLogger :: Request -> Status -> Maybe Integer -> IO ()
warpLogger req st maybeSize = do
  now <- liftIO getCurrentTime
  putStrLn
    . unwords
    $ [ show . remoteHost $ req
      , "[" ++ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" now ++ "]"
      , "\""
      ++ show (requestMethod req)
      ++ "/"
      ++ (T.unpack . T.intercalate "/" $ pathInfo req)
      ++ "\""
      , show . statusCode $ st
      , maybe "-" show maybeSize
      ]
