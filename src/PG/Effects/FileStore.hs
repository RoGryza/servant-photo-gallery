{-|
Effects and handlers for file storage.
-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module PG.Effects.FileStore
  ( FileStore(..)
  , fileURL
  , fetchFile
  , fileExists
  , storeFile
  , runFileStoreFS
  )
where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Network.URI
import Polysemy
import System.Directory
import System.FilePath.Posix

-- |
-- = Effects

-- | Effect for persistent file storage
data FileStore m a where
  FileURL :: FilePath -> FileStore m URI
  FileExists :: FilePath -> FileStore m Bool
  FetchFile :: FilePath -> FileStore m ByteString
  StoreFile :: ByteString -> FilePath -> FileStore m ()

makeSem_ ''FileStore

-- | Returns the URL for a file. Doesn't check that the file is actually stored.
fileURL :: Member FileStore r => FilePath -> Sem r URI

-- | Checks whether a file exists in the storage.
fileExists :: Member FileStore r => FilePath -> Sem r Bool

-- | Get the contents from a file.
fetchFile :: Member FileStore r => FilePath -> Sem r ByteString

-- | Store a file in a path.
storeFile :: Member FileStore r => ByteString -> FilePath -> Sem r ()

-- |
-- = Interpreters

-- | Interpreter storing files on the local filesystem.
runFileStoreFS
  :: Member (Embed IO) r
  => FilePath -- ^ Path to store files
  -> URI -- ^ Base URL prepended to pathes for fileURL
  -> Sem (FileStore ': r) a
  -> Sem r a
runFileStoreFS root baseURL = interpret $ \case
  FileURL    name   -> embed $ return $ baseURL { uriPath = uriPath baseURL <> name }
  FileExists name   -> embed $ doesFileExist $ root </> name
  FetchFile  name   -> embed $ BL.readFile $ root </> name
  StoreFile bs name -> embed $ BL.writeFile (root </> name) bs
