{-|
DSL for file storage.
-}
module PG.Effects.FileStore
  ( HasFileStore(..)
  , MonadFileStore(..)
  )
where

import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Network.URI
import System.Directory
import System.FilePath.Posix

class HasFileStore a where
  getRootPath :: a -> FilePath
  getBaseURL :: a -> URI

-- | Typeclass for persistent file storage
class Monad m => MonadFileStore m where
  -- | Returns the URL for a file. Doesn't check that the file is actually stored.
  fileURL :: FilePath -> m URI
  -- | Checks whether a file exists in the storage.
  fileExists :: FilePath -> m Bool
  -- | Get the contents from a file.
  fetchFile :: FilePath -> m ByteString
  -- | Store a file in a path.
  storeFile :: ByteString -> FilePath -> m ()

instance (HasFileStore env, MonadIO m) => MonadFileStore (ReaderT env m) where
  fileURL name = do
    baseURL <- asks getBaseURL
    return $ baseURL { uriPath = uriPath baseURL <> "/" <> name }
  fileExists name = do
    root <- asks getRootPath
    liftIO $ doesFileExist $ root </> name
  fetchFile name = do
    root <- asks getRootPath
    liftIO $ BL.readFile $ root </> name
  storeFile bs name = do
    root <- asks getRootPath
    liftIO $ BL.writeFile (root </> name) bs
