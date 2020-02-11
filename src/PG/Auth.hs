{-# LANGUAGE RankNTypes #-}
{-|
API-agnostic authentication server.
-}
module PG.Auth
  ( AuthApi
  , AuthConfig(..)
  , TokenRequest(..)
  , TokenResponse(..)
  , hoistAuthServer
  , authServer
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Clock
import GHC.Exts (fromList)
import PG.Effects.Auth
import PG.Effects.Clock
import PG.Types
import Servant
import Servant.Auth.Server
import Web.FormUrlEncoded
import qualified Data.ByteString.Lazy as BL

-- |
-- = API

type AuthHandler env m
  = (MonadReader env m, HasClock env, MonadAuth m, MonadError ServerError m, MonadIO m)

-- brittany-disable-next-binding

-- | Generic authentication API with a sub-api for authenticated users and another for admin users
-- only.
type AuthApi a b = "token" :> ReqBody '[FormUrlEncoded] TokenRequest :> Post '[JSON] TokenResponse
               :<|> Auth '[JWT] User :> a
               :<|> Auth '[JWT] Admin :> b

data TokenRequest = TokenRequest { tokenRequestUsername :: !Text
                                 , tokenRequestPassword :: !Text
                                 }
  deriving (Eq, Show)

instance ToForm TokenRequest where
  toForm r = fromList
    [ ("username", toQueryParam $ tokenRequestUsername r)
    , ("password", toQueryParam $ tokenRequestPassword r)
    ]

instance FromForm TokenRequest where
  fromForm f = TokenRequest <$> parseUnique "username" f <*> parseUnique "password" f

data TokenResponse = TokenResponse { tokenResponseAccessToken :: !ByteString
                                   }
  deriving (Eq, Show)

instance ToJSON TokenResponse where
  toJSON TokenResponse { tokenResponseAccessToken } =
    object ["access_token" .= (String . serializeJwt $ tokenResponseAccessToken)]
  toEncoding TokenResponse { tokenResponseAccessToken } =
    pairs ("access_token" .= (String . serializeJwt $ tokenResponseAccessToken))

instance FromJSON TokenResponse where
  parseJSON = withObject "TokenRespons"
    $ \v -> TokenResponse <$> (BL.fromStrict . encodeUtf8 <$> v .: "access_token")

serializeJwt :: ByteString -> Text
serializeJwt = decodeUtf8 . BL.toStrict

-- | Configuration for @authServer@
data AuthConfig = AuthConfig
                  { authCfgCookie :: !CookieSettings
                  , authCfgJWT :: !JWTSettings
                  , authCfgSessionDuration :: !NominalDiffTime
                  }

hoistAuthServer
  :: (HasServer a '[CookieSettings, JWTSettings])
  => AuthConfig
  -> Proxy a
  -> (forall x . m x -> Handler x)
  -> (AuthConfig -> ServerT a m)
  -> Application
hoistAuthServer cfg@AuthConfig { authCfgCookie, authCfgJWT } api nt server =
  let
    ctxProxy = Proxy :: Proxy '[CookieSettings, JWTSettings]
    ctx      = authCfgCookie :. authCfgJWT :. EmptyContext
  in serveWithContext api ctx $ hoistServerWithContext api ctxProxy nt (server cfg)

-- | Server implementation for @AuthApi@
authServer
  :: (AuthHandler env m, ThrowAll (ServerT a m), ThrowAll (ServerT b m))
  => Proxy a
  -> Proxy b
  -> ServerT a m
  -> ServerT b m
  -> AuthConfig
  -> ServerT (AuthApi a b) m
authServer _ _ a b cfg = postTokenHandler cfg :<|> requireAuth a :<|> requireAuth b

requireAuth :: ThrowAll b => b -> AuthResult a -> b
requireAuth endpoint (Authenticated _) = endpoint
requireAuth _        _                 = throwAll err401

postTokenHandler :: AuthHandler env m => AuthConfig -> TokenRequest -> m TokenResponse
postTokenHandler AuthConfig { authCfgJWT, authCfgSessionDuration } (TokenRequest username password)
  = do
    maybeUser <- checkPassword username (encodeUtf8 password)
    user      <- case maybeUser of
      Just u  -> return u
      Nothing -> throwError err401
    expire <- addUTCTime authCfgSessionDuration <$> utcCurrentTime
    etoken <- liftIO $ makeJWT user authCfgJWT $ Just expire
    case etoken of
      Left  _ -> throwError err500
      Right v -> return $ TokenResponse v
