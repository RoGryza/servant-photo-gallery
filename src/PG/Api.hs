{-|
REST API for the gallery server.
-}
{-# LANGUAGE TemplateHaskell #-}
module PG.Api
  ( PGApi
  , UserApi
  , AdminApi
  , UploadRequest(..)
  , UploadResponse(..)
  , PostRequest(..)
  , PostResponse(..)
  )
where

import Data.Aeson.TH
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Time.Clock
import PG.Types
import PG.Util
import Network.HTTP.Media ((//))
import Servant
import Servant.Multipart
import PG.Auth

-- |
-- = API

-- | JPEG image content type
data JPEG

instance Accept JPEG where
  contentType _ = "image" // "jpeg"

instance MimeRender JPEG ByteString where
  mimeRender _ = id

-- | Gallery server API
type PGApi = AuthApi UserApi AdminApi
           :<|> "static" :> "media" :> CaptureAll "path" FilePath :> Get '[JPEG] ByteString

-- brittany-disable-next-binding
type UserApi = "info" :> Get '[JSON] AppInfo
             :<|> "posts" :> QueryParam "upto" UTCTime :> QueryParam "limit" Word :> Get '[JSON] [PGPost]

-- brittany-disable-next-binding
type AdminApi = "upload" :> MultipartForm Mem UploadRequest :> Post '[JSON] UploadResponse
                :<|> "posts" :> ReqBody '[JSON] PostRequest :> PostCreated '[JSON] PostResponse

data UploadRequest = UploadRequest { uploadRequestData :: !ByteString
                                   }

instance FromMultipart Mem UploadRequest where
  fromMultipart MultipartData { files = [FileData { fdFileCType, fdPayload }] }
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

$(deriveJSON (jsonOpts "" "uploadResponse") ''UploadResponse)
$(deriveJSON (jsonOpts "" "postRequest") ''PostRequest)
$(deriveJSON (jsonOpts "" "postResponse") ''PostResponse)
