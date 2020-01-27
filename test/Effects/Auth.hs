{-# LANGUAGE TemplateHaskell #-}

module Effects.Auth
  ( tests
  ) where

import Hedgehog
-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range
-- import PG.Types
-- import PG.Effects.Auth

-- prop_htpasswd_roundtrip :: Property
-- prop_htpasswd_roundtrip = property $ do
--   namesAndPasswords <- forAll $ Gen.list (Range.linear 0 3) genPair
--   serialized <- writeHtpasswd (pure "0000000000000000") namesAndPasswords
--   mapM_ validatePassword $ zip namesAndPasswords (parseHtpasswd serialized)
--   where
--     genPair = (,) <$> Gen.element ["a", "username", "foobar"] <*> Gen.element ["b", "hunter123", "qux"]
--     validatePassword ((name, p), (User {userName}, h)) = do
--       name === userName
--       assert $ checkHtpasswd p h

tests :: Group
tests = $$(discover)
