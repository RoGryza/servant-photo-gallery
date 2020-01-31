{-# LANGUAGE TupleSections #-}
module Auth
  ( runTests
  ) where

import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Corpus as Corpus
import qualified Hedgehog.Range as Range
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Servant.Auth.Client
import Servant.Auth.Server
import PG.Auth
import PG.Effects.Auth
import PG.Effects.Clock
import PG.Types
import Web.FormUrlEncoded

validUsers :: [User]
validUsers = [User "user" False, User "admin" True]

invalidUsers :: [User]
invalidUsers = [User "baduser1" False, User "baduser2" True]

userPassword :: User -> Text
userPassword = T.reverse . userName -- Just some arbitrary password

genUsername :: MonadGen m => m Text
genUsername = Gen.element Corpus.simpsons

genPassword :: MonadGen m => m Text
genPassword = Gen.text (Range.linear 0 512) Gen.unicode

invalidToken :: (MonadIO m, MonadTest m) => User -> m Token
invalidToken = tokenFromSettings (BS.replicate 256 1)

tokenFromSettings :: (MonadIO m, MonadTest m) => ByteString -> User -> m Token
tokenFromSettings key u = do
  raw <- liftIO $ makeJWT u (defaultJWTSettings $ fromSecret key) Nothing
  Token . LBS.toStrict <$> evalEither raw

type TestApi = AuthApi PublicApi AdminApi
type PublicApi = "public" :> Raw
type AdminApi = "admin" :> Raw

data Env = Env

instance HasClock Env where
  -- TODO test expiry
  clockCurrentTime _ = getCurrentTime

instance HasUsers Env where
  type PasswordHash Env = Text
  fetchUser _ name = (,T.reverse name) <$> find ((== name) . userName) validUsers
  validatePassword _ = (==) . decodeUtf8

type TestHandler = ReaderT Env Handler

runTestHandler :: Env -> TestHandler a -> Handler a
runTestHandler = flip runReaderT

testServer :: AuthConfig -> ServerT TestApi TestHandler
testServer = authServer (Proxy :: Proxy PublicApi) (Proxy :: Proxy AdminApi) (Tagged $ dummyApp "public") (Tagged $ dummyApp "admin")

dummyApp :: LBS.ByteString -> Application
dummyApp name _ respond = respond $ responseLBS status200 [] name

authCfg :: AuthConfig
authCfg = 
  let key = fromSecret $ BS.replicate 256 0
  in  AuthConfig defaultCookieSettings (defaultJWTSettings key) (60 * 30)

testApp :: Env -> Application
testApp env = hoistAuthServer authCfg (Proxy :: Proxy TestApi) (runTestHandler env) testServer

postToken :: TokenRequest -> ClientM TokenResponse
getPublic :: Token -> Method -> ClientM (ResponseF LBS.ByteString)
getAdmin :: Token -> Method -> ClientM (ResponseF LBS.ByteString)
postToken :<|> getPublic :<|> getAdmin = client (Proxy :: Proxy TestApi)

prop_tripping_token_request_form :: Property
prop_tripping_token_request_form = property $ do
  req <- forAll $ TokenRequest <$> genUsername <*> genPassword
  tripping req toForm fromForm

prop_tripping_token_response_json :: Property
prop_tripping_token_response_json = property $ do
  resp <- forAll $ TokenResponse <$> (LBS.fromStrict . encodeUtf8 <$> genPassword)
  tripping resp toJSON fromJSON

prop_unauth_returns_401 :: ClientEnv -> Property
prop_unauth_returns_401 env = property $ do
  (users, getPassword) <- forAllWith (show . fst) $ Gen.element
    [ (validUsers, userName)
    , (invalidUsers, userPassword)
    ]
  user <- forAll $ Gen.element users
  tok <- invalidToken user
  let req = TokenRequest (userName user) (getPassword user)
  assertError env 401 $ postToken req
  assertError env 401 $ getPublic tok "GET"
  assertError env 401 $ getAdmin tok "GET"

prop_auth_allows_access :: ClientEnv -> Property
prop_auth_allows_access env = property $ do
  user <- forAll $ Gen.element validUsers
  tok <- authUser user
  void . evalClientM env $ getPublic tok "GET"
  if userIsAdmin user
    then void . evalClientM env $ getAdmin tok "GET"
    else assertError env 401 $ getAdmin tok "GET"
  where
    authUser u@User {userName} = do
      let req = TokenRequest userName (userPassword u)
      TokenResponse t <- evalClientM env $ postToken req
      return . Token $ LBS.toStrict t

assertError :: Show a => ClientEnv -> Int -> ClientM a -> PropertyT IO ()
assertError env status a = do
    res <- evalIO $ runClientM a env
    annotateShow res
    case res of
      Right _ -> failure
      Left (FailureResponse _ r) -> statusCode (responseStatusCode r) === status
      Left _ -> failure

evalClientM :: ClientEnv -> ClientM a -> PropertyT IO a
evalClientM env a = evalEither =<< evalIO (runClientM a env)

tests :: ClientEnv -> Group
tests env = Group "Auth" [ ("prop_tripping_token_request_form", prop_tripping_token_request_form)
                         , ("prop_tripping_token_response_json", prop_tripping_token_response_json)
                         , ("prop_unauth_returns_401", prop_unauth_returns_401 env)
                         , ("prop_auth_allows_access", prop_auth_allows_access env)
                         ]

runTests :: IO ()
runTests =
  testWithApplication (return $ testApp Env) $ \port -> do
    let url = BaseUrl Http "localhost" port ""
    manager <- liftIO $ newManager defaultManagerSettings
    void $ checkParallel $ tests $ mkClientEnv manager url
