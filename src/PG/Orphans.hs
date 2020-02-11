{-|
All orphan instances.
-}
{-# OPTIONS_GHC -Wno-orphans #-}
module PG.Orphans
  ()
where

import Control.Lens
import Control.Monad.Logger (LogLevel(..))
import Data.Aeson.Types
import Data.Proxy
import Data.Swagger
import qualified Data.Text as T
import PG.Util
import Servant
import Servant.Multipart
import Servant.Swagger

instance ToJSON LogLevel where
  toJSON = String . \case
    LevelDebug   -> "debug"
    LevelInfo    -> "info"
    LevelWarn    -> "warn"
    LevelError   -> "error"
    LevelOther _ -> error "Unsupported 'LevelOther'"

instance FromJSON LogLevel where
  parseJSON = withText "LogLevel"
    $ \s -> maybe (fail $ "Invalid log level " <> T.unpack s) return $ parseLogLevel s

instance (FromMultipart tag a, HasSwagger api) => HasSwagger (MultipartForm tag a :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api) & addParam param
   where
    param =
      mempty
        &  name
        .~ "file"
        &  required
        ?~ True
        &  description
        ?~ "File to upload"
        &  schema
        .~ ParamOther
             (mempty & in_ .~ ParamFormData & paramSchema .~ (mempty & type_ .~ SwaggerFile))
