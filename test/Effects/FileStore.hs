{-# LANGUAGE TemplateHaskell #-}

module Effects.FileStore
  ( tests
  ) where

import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Lazy (HashMap)
import Data.Maybe
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.URI
import Polysemy
import Polysemy.State
import PG.Types
import PG.Effects.FileStore

newtype HState (v :: * -> *) = HState (HashMap FilePath ByteString)
  deriving (Eq, Show)

initialState :: HState v
initialState = HState mempty

data CStoreFile (v :: * -> *) = CStoreFile ByteString FilePath
  deriving (Eq, Show)

instance HTraversable CStoreFile where
  htraverse _ (CStoreFile bs p) = CStoreFile <$> pure bs <*> pure p

cStoreFile :: (MonadGen n) => Command n (Sem '[FileStore]) HState
cStoreFile = let
  gen _ = Just $ CStoreFile <$> Gen.element ["", "foobar"] <*> Gen.element ["abc", "def"]
  execute (CStoreFile bs p) = storeFile bs p
  in
    Command gen execute
    [ Update $ \(HState s) (CStoreFile bs p) _ ->
        HState . fst . run . runState s $ runFileStoreState (fromJust $ parseURI "") (storeFile bs p)
    ]

prop_fs_state_equivalent :: Property
prop_fs_state_equivalent = property $ do
  actions <- forAll $
    Gen.sequential
    (Range.linear 1 100)
    initialState
    [cStoreFile]

  executeSequential initialState actions

tests :: Group
tests = $$(discover)
