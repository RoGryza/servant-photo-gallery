module PG.Util
  ( jsonOpts
  , snakeCase
  , maybeStripPrefix
  , labelModifier
  , parseLogLevel
  )
where

import Control.Monad.Logger (LogLevel(..))
import Data.Aeson.TH
import Data.Char
import Data.List
import Data.Text (Text)
import Data.Maybe

-- | Aeson TH options which strips a prefix from tags and a prefix from fields
jsonOpts
  :: String -- ^ Tag prefix
  -> String -- ^ Field prefix
  -> Options
jsonOpts tagPrefix fieldPrefix = defaultOptions
  { constructorTagModifier = labelModifier tagPrefix
  , fieldLabelModifier     = labelModifier fieldPrefix
  }

labelModifier :: String -> String -> String
labelModifier p = snakeCase . maybeStripPrefix p

maybeStripPrefix :: String -> String -> String
maybeStripPrefix p s = fromMaybe s $ stripPrefix p s

snakeCase :: String -> String
snakeCase = mconcat . intersperse "_" . splitLowerWords
 where
  splitLowerWords (x : xs) =
    let (word, rest) = span isLower xs in (toLower x : word) : splitLowerWords rest
  splitLowerWords [] = []

parseLogLevel :: Text -> Maybe LogLevel
parseLogLevel "debug" = Just LevelDebug
parseLogLevel "info"  = Just LevelInfo
parseLogLevel "warn"  = Just LevelWarn
parseLogLevel "error" = Just LevelError
parseLogLevel _       = Nothing
