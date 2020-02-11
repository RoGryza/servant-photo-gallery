{-|
All orphan instances.
-}
{-# OPTIONS_GHC -Wno-orphans #-}
module PG.Orphans
  ()
where

import Control.Monad.Logger (LogLevel(..))
import Data.Aeson.Types
import qualified Data.Text as T
import PG.Util

instance ToJSON LogLevel where
  toJSON = String . \case
    LevelDebug   -> "debug"
    LevelInfo    -> "info"
    LevelWarn    -> "warn"
    LevelError   -> "error"
    LevelOther _ -> error "Unsupported 'LevelOther'"

instance FromJSON LogLevel where
  parseJSON = withText "LogLevel"
    $ \s -> maybe (fail $ "Invalid log level " <> T.unpack s) return $ parseLogLevel s
