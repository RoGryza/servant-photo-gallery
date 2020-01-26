{-|
REST API for the gallery server.
-}
{-# LANGUAGE TemplateHaskell #-}
module PG.Api
  ( PGApi
  , TokenRequest(..)
  , TokenResponse(..)
  , UploadRequest(..)
  , UploadResponse(..)
  , PostRequest(..)
  , PostResponse(..)
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Clock
import PG.Types
import PG.Util
import Network.HTTP.Media ((//))
import Servant
import Servant.Auth.Server
import Servant.Multipart
import Web.FormUrlEncoded
import qualified Data.ByteString.Lazy as BL

-- |
-- = API

-- | JPEG image content type
data JPEG

instance Accept JPEG where
  contentType _ = "image" // "jpeg"

instance MimeRender JPEG ByteString where
  mimeRender _ = id

-- | Gallery server API
type PGApi = "token" :> ReqBody '[FormUrlEncoded] TokenRequest :> Post '[JSON] TokenResponse
           :<|> Auth '[JWT] User :> UserApi
           :<|> Auth '[JWT] Admin :> AdminApi

type UserApi = "info" :> Get '[JSON] AppInfo
             :<|> "posts" :> QueryParam "upto" UTCTime :> QueryParam "limit" Word :> Get '[JSON] [PGPost]
             :<|> "static" :> "media" :> CaptureAll "path" FilePath :> Get '[JPEG] ByteString

type AdminApi = "upload" :> MultipartForm Mem UploadRequest :> Post '[JSON] UploadResponse
                :<|> "posts" :> ReqBody '[JSON] PostRequest :> PostCreated '[JSON] PostResponse

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

data UploadRequest = UploadRequest { uploadRequestData :: !ByteString
                                   }

instance FromMultipart Mem UploadRequest where
  fromMultipart MultipartData { files = [ FileData { fdFileCType, fdPayload } ] }
    | fdFileCType == "image/jpeg" = Just $ UploadRequest fdPayload
  fromMultipart _ = Nothing

data UploadResponse = UploadResponse { uploadResponsePath :: !FilePath
                                     }

data PostRequest = PostRequest { postRequestPath :: !FilePath
                               , postRequestCaption :: !Text
                               , postRequestCreatedAt :: !(Maybe UTCTime)
                               }

data PostResponse = PostResponse { postResponsePostId :: !PostID
                                 , postResponseCreatedAt :: !UTCTime
                                 }

$(deriveJSON (jsonOpts "" "tokenRequest") ''TokenRequest)
$(deriveJSON (jsonOpts "" "uploadResponse") ''UploadResponse)
$(deriveJSON (jsonOpts "" "postRequest") ''PostRequest)
$(deriveJSON (jsonOpts "" "postResponse") ''PostResponse)
