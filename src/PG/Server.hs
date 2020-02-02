{-|
Server implementation for PG.API
-}
module PG.Server
  ( pgApp
  , pgApiServer
  )
where

import Codec.Picture.Jpg
import Codec.Picture.Types
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Crypto.Hash
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List
import Data.Maybe
import Data.Time.Clock
import qualified Data.Text as T
import Servant
import PG.Api
import PG.Auth
import PG.Env
import PG.Effects.Clock
import PG.Effects.FileStore
import PG.Effects.PostDatabase
import PG.Types

pgApp :: AuthConfig -> Env -> Application
pgApp authCfg env =
  let api = Proxy :: Proxy PGApi in hoistAuthServer authCfg api (runAppT env) pgApiServer

pgApiServer :: AuthConfig -> ServerT PGApi App
pgApiServer =
  let
    userApi  = Proxy :: Proxy UserApi
    adminApi = Proxy :: Proxy AdminApi
  in \cfg ->
    authServer
        userApi
        adminApi
        (getAppInfoHandler :<|> getPostsHandler)
        (postUpload :<|> postPost)
        cfg
      :<|> getMediaFile

getAppInfoHandler :: App AppInfo
getAppInfoHandler = asks envAppInfo

getPostsHandler :: Maybe UTCTime -> Maybe Word -> App [PGPost]
getPostsHandler maybeUpto maybeLimit = do
  upto <- maybe utcCurrentTime return maybeUpto
  PagingConfig { pagingCfgDefaultSize, pagingCfgMaxSize } <- asks envPagingCfg
  let limit = min pagingCfgMaxSize $ fromMaybe pagingCfgDefaultSize maybeLimit
  posts <- fetchPosts upto limit
  mapM (mapM fileURL) posts

getMediaFile :: [FilePath] -> App ByteString
getMediaFile xs = do
  let fileName = mconcat $ intersperse "/" xs
  exists <- fileExists fileName
  if exists then fetchFile fileName else throwError err404

postUpload :: UploadRequest -> App UploadResponse
postUpload (UploadRequest payload) = do
  let
    fileHash = hash (LBS.toStrict payload) :: Digest SHA3_512
    filePath = show fileHash <> ".jpeg"
  exists <- fileExists filePath
  unless exists $ storeFile payload filePath
  return $ UploadResponse filePath

postPost :: PostRequest -> App PostResponse
postPost PostRequest { postRequestPath, postRequestCaption, postRequestCreatedAt } = do
  exists <- fileExists postRequestPath
  unless exists $ throwError err422
  imgBytes        <- fetchFile postRequestPath
  (width, height) <- case decodeJpeg . BS.concat . LBS.toChunks $ imgBytes of
    Right img -> return (dynamicMap imageWidth img, dynamicMap imageHeight img)
    Left  e   -> do
      logErrorN ("Failed to decode " <> T.pack postRequestPath <> ": " <> T.pack e)
      throwError err500
  createdAt <- maybe utcCurrentTime return postRequestCreatedAt
  postId    <- insertPost createdAt
  let
    media = MediaF
      { mediaSrc     = postRequestPath
      , mediaCaption = postRequestCaption
      , mediaType    = MediaTypeImage
      , mediaWidth   = fromIntegral width
      , mediaHeight  = fromIntegral height
      }
  insertMedia postId media
  return $ PostResponse postId createdAt
