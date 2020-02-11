{-# LANGUAGE TemplateHaskell #-}
module Orphans
  ( runTests
  )
where

import Control.Monad
import Language.Haskell.TH
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Control.Monad.Logger (LogLevel(..))
import Data.Aeson
import PG.Orphans ()

genLevel :: MonadGen m => m LogLevel
genLevel = Gen.element [LevelDebug, LevelInfo, LevelWarn, LevelError]

prop_tripping_log_level_json :: Property
prop_tripping_log_level_json = property $ do
  lvl <- forAll genLevel
  tripping lvl toJSON fromJSON

tests :: Group
tests = $(unType <$> discover) -- unType instead of $$ so that hlint doesn't complain

runTests :: IO ()
runTests = void $ checkParallel tests
