{-|
Defines the monad stack used by the server.
-}
module PG.Env
  ( Env(..), PagingConfig(..)
  , App
  , AppT
  , runAppT
  )
where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Time.Clock
import Database.SQLite.Simple hiding (Error)
import PG.Effects.Auth
import PG.Effects.Clock
import PG.Effects.FileStore
import PG.Effects.PostDatabase
import PG.Types
import Servant

data Env = Env
         { envHtpasswd :: !Htpasswd
         , envRootPath :: !FilePath
         , envBaseURL :: !URI
         , envDatabasePath :: !String
         , envLogFilter :: !(LogSource -> LogLevel -> Bool)
         , envAppInfo :: !AppInfo
         , envPagingCfg :: !PagingConfig
         }

data PagingConfig = PagingConfig
                    { pagingCfgDefaultSize :: !Word
                    , pagingCfgMaxSize :: !Word
                    }

instance HasUsers Env where
  type PasswordHash Env = ByteString
  fetchUser = fetchUser . envHtpasswd
  validatePassword = validatePassword . envHtpasswd

instance HasFileStore Env where
  getRootPath = envRootPath
  getBaseURL = envBaseURL

instance HasConnection Env where
  acquire = open . envDatabasePath

instance HasClock Env where
  clockCurrentTime = const getCurrentTime

-- | Monad stack used by the server
type AppT m = ReaderT Env (LoggingT m)
-- | AppT specialized for use with Servant
type App = AppT Handler

-- | Runs the server monad stack
runAppT :: MonadIO m => Env -> AppT m a -> m a
runAppT env@Env{envLogFilter} m = runStdoutLoggingT $ filterLogger envLogFilter $ flip runReaderT env $ m
