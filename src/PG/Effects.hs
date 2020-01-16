{-|
Defines the full effect list and interpretation order.
-}
module PG.Effects
  ( Config(..)
  , App
  , AppT
  , runAppT
  , defaultConfig
  , cfgWarpSettings
  )
where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class
import Data.Function
import qualified Data.Text.IO as T
import Database.SQLite.Simple hiding (Error)
import PG.Effects.Auth
import PG.Config
import PG.Effects.FileStore
import PG.Effects.Logging
import PG.Effects.PostDatabase
import Polysemy
import Polysemy.Error
import Polysemy.IO
import Polysemy.Reader
import Servant
import System.IO

-- | Full effect list used by the server
type AppT m = Sem '[Auth, FileStore, PostDatabase, Reader Config, Error ServerError, Log, Embed IO, Embed m]
-- | AppT specialized for use with Servant
type App = AppT Handler

-- | Interprets the server in a given monad
runAppT :: (MonadIO m, MonadError ServerError m) => Config -> AppT m a -> m a
runAppT cfg@Config { cfgLogLevel, cfgDatabasePath, cfgMediaPath, cfgHtpasswd } m = do
  rawHtpasswd <- liftIO $ withFile cfgHtpasswd ReadMode T.hGetContents
  let
    users   = parseHtpasswd rawHtpasswd
    baseUrl = cfgActualBaseUrl cfg
  res <-
    m
    & runAuthHtpasswd users
    & runFileStoreFS cfgMediaPath (baseUrl { uriPath = "/static/media" })
    & runPostDatabaseSqlite (open cfgDatabasePath)
    & runReader cfg
    & runError
    & runLogIO cfgLogLevel
    & embedToMonadIO
    & runM
  case res of
    Left  e -> throwError e
    Right a -> return a
