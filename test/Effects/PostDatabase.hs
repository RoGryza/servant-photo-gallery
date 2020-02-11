module Effects.PostDatabase
  ( runTests
  )
where

import Control.Monad.Morph
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.Ord
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.SQLite.Simple hiding (execute)
import Data.Time.Calendar
import Data.Time.Clock
import Hedgehog
import qualified Hedgehog.Corpus as Corpus
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import PG.Effects.PostDatabase
import PG.Types
import System.IO

type TestPostDatabase = PropertyT (ReaderT Connection IO)

data TestPost v = TestPost
                { tpID :: !(Var PostID v)
                , tpCreatedAt :: !UTCTime
                , tpMedia :: !(Maybe (MediaF FilePath))
                }
                deriving (Eq, Show)

eqTestPost :: PGPostF FilePath -> TestPost Concrete -> Test ()
eqTestPost p p' = do
  pgPostId p === concrete (tpID p')
  pgPostCreatedAt p === tpCreatedAt p'
  pgPostMedia p === maybeToList (tpMedia p')

hoistTest :: Connection -> TestPostDatabase a -> PropertyT IO a
hoistTest conn = hoist $ flip runReaderT conn

resetTest :: Connection -> IO ()
resetTest conn = do
  execute_ conn "DELETE FROM media;"
  execute_ conn "DELETE FROM posts;"

genTimestamp :: MonadGen m => m UTCTime
genTimestamp = do
  -- Limited range so we can have some collisions
  day <-
    fromGregorian
    <$> Gen.integral_ (Range.linear 2000 2002)
    <*> Gen.integral_ (Range.linear 9 12)
    <*> Gen.integral_ (Range.linear 8 11)
  -- Any time of day
  time <- secondsToDiffTime <$> Gen.integral_ (Range.linear 0 $ 60 * 60 * 24)
  return $ UTCTime day time

genMedia :: MonadGen m => m (MediaF FilePath)
genMedia =
  MediaF
    <$> Gen.element Corpus.glass
    <*> Gen.enumBounded
    <*> Gen.element Corpus.simpsons
    <*> Gen.integral_ (Range.linear 0 4096)
    <*> Gen.integral_ (Range.linear 0 4096)

newtype State v = State { files :: [TestPost v] }
  deriving (Eq, Show)

initialState :: State v
initialState = State []

stInsertEmpty :: Eq1 v => Var PostID v -> UTCTime -> State v -> State v
stInsertEmpty i t (State xs) = State $ TestPost i t Nothing : filter diffKey xs
  where diffKey = (/= i) . tpID

stInsertMedia :: Eq1 v => Var PostID v -> MediaF FilePath -> State v -> State v
stInsertMedia i m s@(State xs) = case found of
  [x] -> State $ x { tpMedia = Just m } : rest
  _   -> s
  where (found, rest) = partition ((== i) . tpID) xs

stFetch :: UTCTime -> Word -> State v -> [TestPost v]
stFetch t l (State xs) =
  take (fromIntegral l) . filter ((< t) . tpCreatedAt) . sortOn (Down . tpCreatedAt) $ xs

data FetchPosts (v :: * -> *) = FetchPosts UTCTime Word
  deriving (Eq, Show)

instance HTraversable FetchPosts where
  htraverse _ (FetchPosts t l) = FetchPosts <$> pure t <*> pure l

cFetchPosts :: MonadGen n => Command n TestPostDatabase State
cFetchPosts =
  let
    gen (State s) = Just $ do
      ts <- if null s
        then genTimestamp
        else Gen.frequency [(1, Gen.element $ tpCreatedAt <$> s), (4, genTimestamp)]
      FetchPosts ts <$> Gen.integral_ (Range.exponential 0 512)
    execute (FetchPosts t l) = fetchPosts t l
  in Command
    gen
    execute
    [ Ensure $ \_ s@(State inner) (FetchPosts t l) o -> do
        let
          ltPosts = filter ((< t) . tpCreatedAt) inner
          gtPosts = filter ((> t) . tpCreatedAt) inner
          eqPosts = filter ((== t) . tpCreatedAt) inner
        cover 10 "limit < total" $ fromIntegral l < length inner
        cover 10 "has no posts" $ null inner
        cover 10 "has no posts before" $ null ltPosts
        cover 10 "has no posts after" $ null gtPosts
        cover 10 "has eq posts" $ not $ null eqPosts
        mapM_ (uncurry eqTestPost) (zip o $ stFetch t l s)
    ]

data InsertPost (v :: * -> *) = InsertPost UTCTime
  deriving (Eq, Show)

instance HTraversable InsertPost where
  htraverse _ (InsertPost t) = InsertPost <$> pure t

cInsertPost :: MonadGen n => Command n TestPostDatabase State
cInsertPost =
  let
    gen _ = Just $ InsertPost <$> genTimestamp
    execute (InsertPost t) = insertPost t
  in Command gen execute [Update $ \s (InsertPost t) o -> stInsertEmpty o t s]

data InsertMedia v = InsertMedia (Var PostID v) (MediaF FilePath)
  deriving (Eq, Show)

instance HTraversable InsertMedia where
  htraverse f (InsertMedia i m) = InsertMedia <$> htraverse f i <*> pure m

cInsertMedia :: MonadGen n => Command n TestPostDatabase State
cInsertMedia =
  let
    gen (State inner) =
      let withoutMedia = filter (isNothing . tpMedia) inner
      in
        if null withoutMedia
          then Nothing
          else Just $ InsertMedia <$> Gen.element (tpID <$> withoutMedia) <*> genMedia
    execute (InsertMedia i m) = insertMedia (concrete i) m
  in Command gen execute [Update $ \s (InsertMedia i m) _ -> stInsertMedia i m s]

prop_db_state_equivalent :: Connection -> Property
prop_db_state_equivalent conn = withTests 100 . property . hoistTest conn $ do
  actions <- forAll
    $ Gen.sequential (Range.linear 1 100) initialState [cFetchPosts, cInsertPost, cInsertMedia]

  evalIO $ resetTest conn
  executeSequential initialState actions

runTests :: IO ()
runTests = do
  migrations <-
    filter ("\n" /=)
    .   fmap T.unpack
    .   T.splitOn ";"
    <$> withFile "migrations/01_initial.sql" ReadMode T.hGetContents
  withConnection ":memory:" $ \conn -> do
    mapM_ (execute_ conn . fromString) migrations
    void $ checkSequential $ Group
      "Effects.PostDatabase"
      [("prop_db_state_equivalent", prop_db_state_equivalent conn)]
