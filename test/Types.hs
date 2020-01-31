{-# LANGUAGE TemplateHaskell #-}
module Types
  ( runTests
  ) where

import Control.Monad
import Servant.Auth.Server
import Data.Text (Text)
import Language.Haskell.TH
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Corpus as Corpus
import PG.Types

genUsername :: MonadGen m => m Text
genUsername = Gen.element Corpus.simpsons

prop_tripping_user_jwt :: Property
prop_tripping_user_jwt = property $ do
  user <- forAll $ User <$> genUsername <*> Gen.bool
  tripping user encodeJWT decodeJWT

prop_tripping_admin_jwt :: Property
prop_tripping_admin_jwt = property $ do
  user <- forAll $ Admin <$> genUsername
  tripping user encodeJWT decodeJWT

tests :: Group
tests = $(unType <$> discover) -- unType instead of $$ so that hlint doesn't complain

runTests :: IO ()
runTests = void $ checkParallel tests
