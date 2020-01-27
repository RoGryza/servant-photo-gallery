{-# LANGUAGE TemplateHaskell #-}

module Effects.FileStore
  ( tests
  )
where

import Control.Monad.Morph
import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe
import Hedgehog
import qualified Hedgehog.Corpus as Corpus
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.URI
import PG.Effects.FileStore
import System.Directory

data TestStore = TestStore
               { tsRootPath :: !FilePath
               , tsBaseURL :: !URI
               }

instance HasFileStore TestStore where
  getRootPath = tsRootPath
  getBaseURL = tsBaseURL

type TestMonadStore = PropertyT (ReaderT TestStore IO)

testStore :: TestStore
testStore = TestStore ".test/filestore" $ fromJust (parseURI "http://localhost")

hoistTest :: TestMonadStore a -> PropertyT IO a
hoistTest = hoist $ flip runReaderT testStore

resetTest :: IO ()
resetTest = let p = getRootPath testStore in removePathForcibly p >> createDirectoryIfMissing True p 

genName :: MonadGen m => m FilePath
genName = Gen.element Corpus.simpsons

genContents :: MonadGen m => m ByteString
genContents = Gen.element Corpus.glass

newtype State (v :: * -> *) = State { files :: HashMap FilePath ByteString }
  deriving (Eq, Show)

initialState :: State v
initialState = State mempty

data FileExists (v :: * -> *) = FileExists FilePath
  deriving (Eq, Show)

instance HTraversable FileExists where
  htraverse _ (FileExists p) = FileExists <$> pure p

cFileExists :: MonadGen n => Command n TestMonadStore State
cFileExists = let
  gen _ = Just $ FileExists <$> genName
  execute (FileExists p) = fileExists p
  in
    Command gen execute
    [ Ensure $ \_ (State s) (FileExists p) o -> do
        o === HM.member p s
    ]

data FetchFile (v :: * -> *) = FetchFile FilePath
  deriving (Eq, Show)

instance HTraversable FetchFile where
  htraverse _ (FetchFile p) = FetchFile <$> pure p

cFetchFile :: MonadGen n => Command n TestMonadStore State
cFetchFile = let
  gen (State s) = case HM.keys s of
                    [] -> Nothing
                    xs -> Just  $ FetchFile <$> Gen.element xs
  execute (FetchFile p) = fetchFile p
  in
    Command gen execute
    [ Ensure $ \_ (State s) (FetchFile p) o -> do
        Just o === HM.lookup p s
    ]

data StoreFile (v :: * -> *) = StoreFile ByteString FilePath
  deriving (Eq, Show)

instance HTraversable StoreFile where
  htraverse _ (StoreFile bs p) = StoreFile <$> pure bs <*> pure p

cStoreFile :: MonadGen n => Command n TestMonadStore State
cStoreFile = let
  gen _ = Just $ StoreFile <$> genContents <*> genName
  execute (StoreFile bs p) = storeFile bs p
  in
    Command gen execute
    [ Update $ \(State s) (StoreFile bs p) _ ->
        State $ HM.insert p bs s
    ]

prop_fs_state_equivalent :: Property
prop_fs_state_equivalent = property . hoistTest $ do
  actions <- forAll $
    Gen.sequential
    (Range.linear 1 100)
    initialState
    [cFileExists, cStoreFile, cFetchFile]

  evalIO resetTest
  executeSequential initialState actions

tests :: Group
tests = $$(discover)
