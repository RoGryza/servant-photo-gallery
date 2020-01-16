{-|
Effects and handlers for the media metadata and posts database.
-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module PG.Effects.PostDatabase
  ( PostDatabase(..)
  , fetchPosts
  , insertPost
  , insertMedia
  , runPostDatabaseSqlite
  )
where

import Control.Monad
import Database.SQLite.Simple hiding (withTransaction)
import Database.SQLite.Simple.Internal
import Data.Function
import Data.Functor
import Data.List
import Data.Text (Text)
import Data.Time.Clock
import PG.Types
import Polysemy

-- |
-- = Effects

-- | Effect for the media metadata and posts database
data PostDatabase m a where
  FetchPosts :: UTCTime -> Word -> PostDatabase m [PGPostF FilePath]
  InsertPost :: UTCTime -> PostDatabase m PostID
  InsertMedia :: PostID -> MediaF FilePath -> PostDatabase m ()

makeSem_ ''PostDatabase

-- | Return the first posts created up to a timestamp, in descending time order.
fetchPosts
  :: Member PostDatabase r
  => UTCTime -- ^ Timestamp limit
  -> Word -- ^ Max amount of posts to fetch
  -> Sem r [PGPostF FilePath]

-- | Inserts a given post into the database, returning its ID.
insertPost
  :: Member PostDatabase r
  => UTCTime -- ^ created at
  -> Sem r PostID

-- | Inserts media metadata for a post.
insertMedia
  :: Member PostDatabase r
  => PostID -- ^ Post containing the media
  -> MediaF FilePath -- ^ Media metadata
  -> Sem r ()

-- |
-- = Interpreters

-- | Interprets the post database as an SQLite database. Database schema should match the latest
-- migration under `migrations`
runPostDatabaseSqlite
  :: Member (Embed IO) r
  => IO Connection -- ^ Action to acquire a db connection
  -> Sem (PostDatabase ': r) a
  -> Sem r a
runPostDatabaseSqlite acquire = interpret $ \case
  FetchPosts upto limit -> do
    conn       <- embed acquire
    postParams <- embed $ queryWith
      rowParser
      conn
      "SELECT p.post_id, p.post_created_at, m.file_name, m.caption, m.width, m.height \
      \ FROM posts AS p \
      \ LEFT JOIN media AS m ON p.post_id = m.post_id \
      \ WHERE p.post_created_at <= ? \
      \ ORDER BY p.post_created_at DESC, m.media_index ASC \
      \ LIMIT ?"
      (upto, limit)
    posts <- forM postParams $ \(i, d, m) -> do
      let media = m <&> \(f, c, w, h) -> mkMedia c w h f
      return (i, d, media)
    return $ mkPost <$> groupBy ((==) `on` fst3) posts
  InsertPost createdAt -> do
    conn <- embed acquire
    embed $ execute conn "INSERT INTO posts(post_created_at) VALUES (?)" (Only createdAt)
    embed $ PostID . fromIntegral <$> lastInsertRowId conn
  InsertMedia (PostID postId) MediaF { mediaSrc, mediaCaption, mediaWidth, mediaHeight } -> do
    conn <- embed acquire
    embed $ execute
      conn
      "INSERT INTO media(post_id, media_index, file_name, caption, width, height) \
      \ VALUES(?, 0, ?, ?, ?, ?)"
      (postId, mediaSrc, mediaCaption, mediaWidth, mediaHeight)
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
