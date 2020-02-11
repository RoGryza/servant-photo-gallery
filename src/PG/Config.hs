{-# LANGUAGE TemplateHaskell #-}
{-|
Server configuration data and related functions.
-}
module PG.Config
  ( Config(..)
  , cfgAuth
  , cfgEnv
  , defaultConfig
  , cfgActualBaseUrl
  , cfgWarpSettings
  , pgInfo
  )
where

import Configuration.Utils
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson.TH
import Control.Lens
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import Data.Time.Format
import Data.Word
import Network.HTTP.Types
import Network.URI
import Network.Wai
import Network.Wai.Handler.Warp
import PG.Auth
import PG.Env
import PG.Effects.Auth
import PG.Types
import PG.Orphans ()
import PG.Util
import Servant.Auth.Server
import System.IO

cfgAuth :: Config -> IO AuthConfig
cfgAuth Config { cfgSessionDuration } = do
  key <- generateKey
  return $ AuthConfig defaultCookieSettings (defaultJWTSettings key) cfgSessionDuration

cfgEnv :: Config -> IO Env
cfgEnv cfg@Config { cfgLogLevel, cfgDatabasePath, cfgMediaPath, cfgHtpasswd, cfgTitle, cfgDefaultPageSize, cfgMaxPageSize }
  = do
    rawHtpasswd <- liftIO $ withFile cfgHtpasswd ReadMode T.hGetContents
    return $ Env
      { envHtpasswd     = parseHtpasswd rawHtpasswd
      , envRootPath     = cfgMediaPath
      , envBaseURL      = cfgActualBaseUrl cfg
      , envDatabasePath = cfgDatabasePath
      , envLogFilter    = \_ l -> l >= cfgLogLevel
      , envAppInfo      = AppInfo cfgTitle
      , envPagingCfg    = PagingConfig
        { pagingCfgDefaultSize = cfgDefaultPageSize
        , pagingCfgMaxSize     = cfgMaxPageSize
        }
      }

-- | Returns the base url for a config or a default based on host and port
cfgActualBaseUrl :: Config -> URI
cfgActualBaseUrl Config { cfgBaseUrl, cfgHost, cfgPort } =
  fromMaybe (fromJust . parseURI $ concat ["http://", cfgHost, ":", show cfgPort]) cfgBaseUrl

-- | Server configuration data.
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

makeLensesWith abbreviatedFields ''Config

$(deriveToJSON (jsonOpts "" "cfg") ''Config)

-- | configuration-tools program info
pgInfo :: ProgramInfo Config
pgInfo = programInfo "Photo Gallery" configParser defaultConfig

instance FromJSON (Config -> Config) where
  parseJSON = withObject "Config" $ \o ->
    id
      <$< sessionDuration
      ..: "session_duration"
      %   o
      <*< logLevel
      ..: "log_level"
      %   o
      <*< baseUrl
      ..: "base_url"
      %   o
      <*< host
      ..: "host"
      %   o
      <*< port
      ..: "port"
      %   o
      <*< htpasswd
      ..: "htpasswd"
      %   o
      <*< mediaPath
      ..: "media_path"
      %   o
      <*< databasePath
      ..: "database_path"
      %   o
      <*< title
      ..: "title"
      %   o
      <*< defaultPageSize
      ..: "default_page_size"
      %   o
      <*< maxPageSize
      ..: "max_page_size"
      %   o

configParser :: MParser Config
configParser =
  id
    <$< sessionDuration
    .:: option (fromInteger <$> auto)
    %   long "session-duration"
    <>  help "How long before user sessions expire, in seconds"
    <*< logLevel
    .:: option (maybeReader (parseLogLevel . T.pack))
    %   long "log-level"
    <>  help "Minimum log level"
    <*< baseUrl
    .:: option (Just <$> maybeReader parseURI)
    %   long "base-url"
    <>  help "Base URL for media links"
    <*< host
    .:: strOption
    %   long "host"
    <>  help "Host to bind to"
    <*< port
    .:: option auto
    %   long "port"
    <>  help "Port to listen on"
    <*< htpasswd
    .:: strOption
    %   long "htpasswd"
    <>  help "Path to htpasswd file"
    <*< mediaPath
    .:: strOption
    %   long "media-path"
    <>  help "Path where to store uploaded media"
    <*< databasePath
    .:: strOption
    %   long "database-path"
    <>  help "Path to post database"
    <*< title
    .:: strOption
    %   long "title"
    <>  help "Application title, used by frontends"
    <*< defaultPageSize
    .:: option auto
    %   long "default-page-size"
    <>  help "Default size for post pages"
    <*< maxPageSize
    .:: option auto
    %   long "max-page-size"
    <>  help "Max posts returned per page"

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
