{-# LANGUAGE TemplateHaskell #-}

module Effects.Auth
  ( tests
  )
where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import PG.Types
import PG.Effects.Auth

prop_htpasswd_roundtrip :: Property
prop_htpasswd_roundtrip = property $ do
  namesAndPasswords <- forAll $ Gen.list (Range.linear 0 3) genPair
  serialized <- writeHtpasswd (pure "0000000000000000") namesAndPasswords
  mapM_ (validatePassword' $ parseHtpasswd serialized) namesAndPasswords
  where
    genPair = (,) <$> Gen.element ["a", "username", "foobar"] <*> Gen.element ["b", "hunter123", "qux"]
    validatePassword' htpasswd (name, p) = do
      (User {userName}, hash) <- case fetchUser htpasswd name of
        Just x -> return x 
        Nothing -> failure
      name === userName
      assert $ validatePassword htpasswd p hash

tests :: Group
tests = $$(discover)
