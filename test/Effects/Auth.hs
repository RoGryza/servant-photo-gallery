{-# LANGUAGE TemplateHaskell #-}

module Effects.Auth
  ( runTests
  )
where

import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.Set as Set
import Data.Text (Text)
import Hedgehog
import qualified Hedgehog.Corpus as Corpus
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import PG.Types
import PG.Effects.Auth

genUsername :: MonadGen m => m Text
genUsername = Gen.element Corpus.simpsons

genPassword :: MonadGen m => m ByteString
genPassword = Gen.element Corpus.glass

prop_htpasswd_roundtrip :: Property
prop_htpasswd_roundtrip = withTests 20 . property $ do
  names <- forAll $ Gen.set (Range.linear 0 3) genUsername
  passwords <- forAll . sequence $ replicate (Set.size names) genPassword
  let namesAndPasswords = zip (Set.toList names) passwords
  serialized <- writeHtpasswd (pure "0000000000000000") namesAndPasswords
  mapM_ (validatePassword' $ parseHtpasswd serialized) namesAndPasswords
  where
    validatePassword' htpasswd (name, p) = do
      (User {userName}, hash) <- case fetchUser htpasswd name of
        Just x -> return x 
        Nothing -> failure
      name === userName
      assert $ validatePassword htpasswd p hash

tests :: Group
tests = $$(discover)

runTests :: IO ()
runTests = void $ checkSequential tests
