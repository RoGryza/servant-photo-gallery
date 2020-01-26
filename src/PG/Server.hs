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
import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Maybe
import Data.Time.Clock
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import Servant
import Servant.Auth.Server hiding (Auth)
import PG.Api
import PG.Auth
import PG.Effects
import PG.Orphans ()
import PG.Effects.FileStore
import PG.Effects.Logging
import PG.Types
import PG.Effects.PostDatabase
import Polysemy
import Polysemy.Error
import Polysemy.Reader

pgApp :: CookieSettings -> JWTSettings -> Config -> Application
pgApp cookieCfg jwtCfg cfg =
  let
    api      = Proxy :: Proxy PGApi
    ctxProxy = Proxy :: Proxy '[CookieSettings, JWTSettings]
    ctx      = cookieCfg :. jwtCfg :. EmptyContext
  in serveWithContext api ctx
    $ hoistServerWithContext api ctxProxy (runAppT cfg) (pgApiServer jwtCfg)

pgApiServer :: JWTSettings -> ServerT PGApi App
pgApiServer jwtCfg = let
    userApi = Proxy :: Proxy UserApi
    adminApi = Proxy :: Proxy AdminApi
  in
    authServer jwtCfg userApi adminApi
    (getAppInfoHandler :<|> getPostsHandler :<|> getMediaFile) (postUpload :<|> postPost)

getAppInfoHandler :: App AppInfo
getAppInfoHandler = do
  title <- asks cfgTitle
  return $ AppInfo title

getPostsHandler :: Maybe UTCTime -> Maybe Word -> App [PGPost]
getPostsHandler maybeUpto maybeLimit = do
  upto <- case maybeUpto of
    Nothing -> liftIO getCurrentTime
    Just d  -> return d
  Config { cfgDefaultPageSize, cfgMaxPageSize } <- ask
  let limit = min cfgMaxPageSize $ fromMaybe cfgDefaultPageSize maybeLimit
  posts <- fetchPosts upto limit
  mapM (mapM fileURL) posts

getMediaFile :: [FilePath] -> App ByteString
getMediaFile xs = do
  let fileName = mconcat $ intersperse "/" xs
  exists <- fileExists fileName
  if exists then fetchFile fileName else throw err404

postUpload :: UploadRequest -> App UploadResponse
postUpload (UploadRequest payload) = do
  reqId <- embed UUID4.nextRandom
  let filePath = UUID.toString reqId <> ".jpeg"
  storeFile payload filePath
  return $ UploadResponse filePath

postPost :: PostRequest -> App PostResponse
postPost PostRequest { postRequestPath, postRequestCaption, postRequestCreatedAt }
  = do
    exists <- fileExists postRequestPath
    unless exists $ throw err422
    imgBytes        <- fetchFile postRequestPath
    (width, height) <- case decodeJpeg . BS.concat . BL.toChunks $ imgBytes of
      Right img -> return (dynamicMap imageWidth img, dynamicMap imageHeight img)
      Left  e   -> do
        logError ("Failed to decode " <> T.pack postRequestPath <> ": " <> T.pack e)
        throw err500
    createdAt <- maybe (embed getCurrentTime) return postRequestCreatedAt
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
