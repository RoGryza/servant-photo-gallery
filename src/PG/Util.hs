module PG.Util
  ( jsonOpts
  )
where

import Data.Aeson.TH
import Data.Char
import Data.List
import Data.Maybe

-- | Aeson TH options which strips a prefix from tags and a prefix from fields
jsonOpts
  :: String -- ^ Tag prefix
  -> String -- ^ Field prefix
  -> Options
jsonOpts tagPrefix fieldPrefix = defaultOptions
  { constructorTagModifier = toSnakeCase . stripOrIgnore tagPrefix
  , fieldLabelModifier     = toSnakeCase . stripOrIgnore fieldPrefix
  }
 where
  stripOrIgnore p s = fromMaybe s $ stripPrefix p s
  toSnakeCase = mconcat . intersperse "_" . splitLowerWords
  splitLowerWords (x : xs) =
    let (word, rest) = span isLower xs in (toLower x : word) : splitLowerWords rest
  splitLowerWords [] = []
