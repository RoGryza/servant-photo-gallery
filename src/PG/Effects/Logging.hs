{-|
Effects and handlers for logging.
-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module PG.Effects.Logging
  ( LogLevel(..)
  , Log(..)
  , logMsg
  , logDebug
  , logInfo
  , logWarn
  , logError
  , runLogIO
  )
where

import Control.Monad
import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.Text.IO as T
import PG.Util
import Polysemy

-- |
-- = Types

data LogLevel = LevelDebug
              | LevelInfo
              | LevelWarn
              | LevelError
              deriving (Eq, Ord, Read, Show)

$(deriveJSON (jsonOpts "Level" "") ''LogLevel)

-- |
-- = Effects

-- | Effect for logging
data Log m a where
  LogMsg :: LogLevel -> Text -> Log m ()

makeSem_ ''Log

-- | Log a message under a given log level
logMsg :: Member Log r => LogLevel -> Text -> Sem r ()

logDebug :: Member Log r => Text -> Sem r ()
logDebug = logMsg LevelDebug

logInfo :: Member Log r => Text -> Sem r ()
logInfo = logMsg LevelInfo

logWarn :: Member Log r => Text -> Sem r ()
logWarn = logMsg LevelWarn

logError :: Member Log r => Text -> Sem r ()
logError = logMsg LevelError

-- |
-- = Interpreters

-- | Print log messages to stdout, omitting the ones under the given log level.
runLogIO :: Member (Embed IO) r => LogLevel -> Sem (Log ': r) a -> Sem r a
runLogIO minLevel = interpret $ \case
  LogMsg l msg -> when (l >= minLevel) $ embed $ T.putStrLn msg
