{-|
Effects and handlers for the media metadata and posts database.
-}
{-# LANGUAGE TupleSections #-}
module PG.Effects.PostDatabase
  ( HasConnection(..), MonadPostDatabase(..)
  )
where

import Control.Monad
import Control.Monad.Reader
import Database.SQLite.Simple hiding (withTransaction)
import Database.SQLite.Simple.Internal
import Data.Function
import Data.Functor
import Data.List
import Data.Text (Text)
import Data.Time.Clock
import PG.Types

class HasConnection a where
  acquire :: a -> IO Connection

-- | Typeclass for media metadata and posts database
class Monad m => MonadPostDatabase m where
  -- | Return the first posts created up to a timestamp, in descending time order.
  fetchPosts
    :: UTCTime -- ^ Timestamp limit
    -> Word -- ^ Max amount of posts to fetch
    -> m [PGPostF FilePath]
  -- | Inserts a given post into the database, returning its ID.
  insertPost
    :: UTCTime -- ^ created at
    -> m PostID

  -- | Inserts media metadata for a post.
  insertMedia
    :: PostID -- ^ Post containing the media
    -> MediaF FilePath -- ^ Media metadata
    -> m ()

instance (HasConnection env, MonadIO m) => MonadPostDatabase (ReaderT env m) where
  fetchPosts upto limit = do
    conn       <- asks acquire >>= liftIO
    postParams <- liftIO $ queryWith
      rowParser
      conn
      "SELECT p.post_id, p.post_created_at, m.file_name, m.caption, m.width, m.height \
      \ FROM posts AS p \
      \ LEFT JOIN media AS m ON p.post_id = m.post_id \
      \ WHERE p.post_created_at < ? \
      \ ORDER BY p.post_created_at DESC, m.media_index ASC \
      \ LIMIT ?"
      (upto, limit)
    posts <- forM postParams $ \(i, d, m) -> do
      let media = m <&> \(f, c, w, h) -> mkMedia c w h f
      return (i, d, media)
    return $ mkPost <$> groupBy ((==) `on` fst3) posts
    where
      rowParser :: RowParser (PostID, UTCTime, [(FilePath, Text, Word, Word)])
      rowParser = do
        postId      <- PostID <$> field
        createdAt   <- field
        fileName    <- field
        mediaParams <- case fileName of
          Just f -> do
            mediaParams <- (f, , , ) <$> field <*> field <*> field
            return [mediaParams]
          Nothing -> return []
        return (postId, createdAt, mediaParams)

      mkPost xs@((postId, createdAt, _) : _) = PGPostF
        { pgPostId        = postId
        , pgPostCreatedAt = createdAt
        , pgPostMedia     = mconcat . fmap third $ xs
        }
      mkPost _ = error "Invalid query result"

      mkMedia c w h u = MediaF
        { mediaCaption = c
        , mediaType    = MediaTypeImage
        , mediaSrc     = u
        , mediaWidth   = w
        , mediaHeight  = h
        }
      fst3 (x, _, _) = x
      third (_, _, x) = x

  insertPost createdAt = do
    conn       <- asks acquire >>= liftIO
    liftIO $ execute conn "INSERT INTO posts(post_created_at) VALUES (?)" (Only createdAt)
    liftIO $ PostID . fromIntegral <$> lastInsertRowId conn

  insertMedia (PostID postId) MediaF { mediaSrc, mediaCaption, mediaWidth, mediaHeight } = do
    conn       <- asks acquire >>= liftIO
    liftIO $ execute
      conn
      "INSERT INTO media(post_id, media_index, file_name, caption, width, height) \
      \ VALUES(?, 0, ?, ?, ?, ?)"
      (postId, mediaSrc, mediaCaption, mediaWidth, mediaHeight)
