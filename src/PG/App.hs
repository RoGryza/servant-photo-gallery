{-|
Main entrypoint for the server application.
-}
module PG.App
  ( pgMain
  )
where

import Data.Text (Text)
import qualified Data.Text.IO as T
import PG.Server
import PG.Config
import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp
import System.Directory
import System.Environment
import System.IO

-- | Main entrypoint for the application. Handles configuration and runs the server
pgMain :: IO ()
pgMain = do
  args    <- getArgs
  cfgPath <- case args of
    [path] -> do
      exists <- doesFileExist path
      if exists then return path else error $ "File " <> path <> " not found"
    [] -> return "gallery.toml"
    _  -> error "Usage: gallery [config_file]"
  cfgRes <- readConfig cfgPath
  let
    cfg = case cfgRes of
      Right c -> c
      Left  e -> error $ show e
  authCfg <- cfgAuth cfg
  env     <- cfgEnv cfg
  putStrLn $ "Starting gallery at " ++ show (cfgActualBaseUrl cfg)
  putStrLn $ "Showing media from " ++ cfgMediaPath cfg
  let app = pgApp authCfg env
  runSettings (cfgWarpSettings cfg) $ corsWithContentType app

readConfig :: FilePath -> IO (Either Text Config)
readConfig p = do
  cfgExists <- doesFileExist p
  if cfgExists
    then parseConfig <$> withFile p ReadMode T.hGetContents
    else return . Right $ defaultConfig

corsWithContentType :: Middleware
corsWithContentType = cors (const $ Just policy)
 where
  policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Authorization", "Content-Type"] }
