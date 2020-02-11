{-|
Main entrypoint for the server application.
-}
module PG.App
  ( pgMain
  )
where

import Configuration.Utils
import PG.Server
import PG.Config
import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp

-- | Main entrypoint for the application. Handles configuration and runs the server
pgMain :: IO ()
pgMain = runWithConfiguration pgInfo $ \cfg -> do
  authCfg <- cfgAuth cfg
  env     <- cfgEnv cfg
  putStrLn $ "Starting gallery at " ++ show (cfgActualBaseUrl cfg)
  putStrLn $ "Showing media from " ++ cfgMediaPath cfg
  let app = pgApp authCfg env
  runSettings (cfgWarpSettings cfg) $ corsWithContentType app

corsWithContentType :: Middleware
corsWithContentType = cors (const $ Just policy)
 where
  policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Authorization", "Content-Type"] }
