{-|
API-agnostic authentication server.
-}
{-# LANGUAGE TemplateHaskell #-}
module PG.Auth
  ( AuthApi
  , TokenRequest(..)
  , TokenResponse(..)
  , authServer
  )
where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Clock
import PG.Effects
import PG.Effects.Auth
import PG.Orphans ()
import PG.Types
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Reader
import Servant
import Servant.Auth.Server hiding (Auth)
import qualified Servant.Auth.Server
import Web.FormUrlEncoded
import qualified Data.ByteString.Lazy as BL

-- |
-- = API

-- | Generic authentication API with a sub-api for authenticated users and another for admin users
-- only.
type AuthApi a b = "token" :> ReqBody '[FormUrlEncoded] TokenRequest :> Post '[JSON] TokenResponse
               :<|> Servant.Auth.Server.Auth '[JWT] User :> a
               :<|> Servant.Auth.Server.Auth '[JWT] Admin :> b

data TokenRequest = TokenRequest { tokenRequestUsername :: !Text
                                 , tokenRequestPassword :: !Text
                                 }

instance FromForm TokenRequest where
  fromForm f = TokenRequest <$> parseUnique "username" f <*> parseUnique "password" f

data TokenResponse = TokenResponse { tokenResponseAccessToken :: !ByteString
                                   }

instance ToJSON TokenResponse where
  toJSON TokenResponse {tokenResponseAccessToken} = object
    [ "access_token" .= (String . serializeJwt $ tokenResponseAccessToken) ]
  toEncoding TokenResponse {tokenResponseAccessToken} = pairs
    ( "access_token" .= (String . serializeJwt $ tokenResponseAccessToken) )

serializeJwt :: ByteString -> Text
serializeJwt = decodeUtf8 . BL.toStrict

authServer :: ( Members '[Input UTCTime, Auth, Reader Config, Error ServerError, Embed IO] r
              , ThrowAll (ServerT a (Sem r))
              , ThrowAll (ServerT b (Sem r))
              -- TODO clean up these constraints
              )
           => JWTSettings -> Proxy a -> Proxy b
           -> ServerT a (Sem r) -> ServerT b (Sem r) -> ServerT (AuthApi a b) (Sem r)
authServer jwtCfg _ _ a b =
  postTokenHandler jwtCfg
  :<|> requireAuth a
  :<|> requireAuth b

requireAuth :: ThrowAll b => b -> AuthResult a -> b
requireAuth endpoint (Authenticated _) = endpoint
requireAuth _          _                 = throwAll err401

postTokenHandler :: Members '[Input UTCTime, Auth, Reader Config, Error ServerError, Embed IO] r
                 => JWTSettings -> TokenRequest -> Sem r TokenResponse
postTokenHandler jwtCfg (TokenRequest username password) = do
  maybeUser <- checkPassword username (encodeUtf8 password)
  user      <- case maybeUser of
    Just u  -> return u
    Nothing -> throw err401
  sessionDuration <- asks cfgSessionDuration
  expire          <- addUTCTime sessionDuration <$> input
  etoken          <- liftIO $ makeJWT user jwtCfg $ Just expire
  case etoken of
    Left  _ -> throw err500
    Right v -> return $ TokenResponse v
