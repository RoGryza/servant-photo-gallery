{-|
Swagger API definition and UI.
-}
module PG.Swagger
  ( pgSwaggerSpec
  )
where

import Data.Proxy
import Servant.Auth.Swagger ()
import Servant.Swagger
import Data.Swagger (Swagger)
import PG.Api

-- | Swagger spec for PGApi
pgSwaggerSpec :: Swagger
pgSwaggerSpec = toSwagger (Proxy :: Proxy PGApi)
