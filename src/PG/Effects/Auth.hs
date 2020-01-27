{-|
DSL for authentication.
-}
module PG.Effects.Auth
  ( HasUsers(..), MonadAuth(..)
  , Htpasswd
  , parseHtpasswd, writeHtpasswd
  )
where

import Control.Monad.Reader
import Crypto.KDF.BCrypt (bcrypt)
import qualified Crypto.KDF.BCrypt as BCrypt
import Data.ByteString (ByteString)
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import PG.Types

-- | Typeclass for an user database
class HasUsers a where
  type PasswordHash a

  -- | Find the user instance and password hash for an username.
  fetchUser :: a -> Text -> Maybe (User, PasswordHash a)
  -- | Check that a password matches a given hash.
  validatePassword :: a -> ByteString -> PasswordHash a -> Bool

-- | Typeclass for validating user authentication
class Monad m => MonadAuth m where
  -- | Returns @Nothing@ if the user doesn't exist or the password doesn't match and @Just user@
  checkPassword :: Text -> ByteString -> m (Maybe User)

instance (HasUsers env, MonadIO m) => MonadAuth (ReaderT env m) where
  checkPassword username password = do
    maybeUser <- asks fetchUser <*> pure username
    case maybeUser of
      Just (u, hash) -> do
        valid <- asks validatePassword <*> pure password <*> pure hash
        if valid
          then return $ Just u
          else return Nothing
      Nothing -> return Nothing

-- | Parsed htpasswd file
newtype Htpasswd = Htpasswd [(User, ByteString)]

instance HasUsers Htpasswd where
  type PasswordHash Htpasswd = ByteString

  fetchUser (Htpasswd users) name = find ((== name) . userName . fst) users
  validatePassword _ = BCrypt.validatePassword

-- | Parses the contents of an htpasswd file. Only accepts bcrypt hashed passwords.
--
-- Doesn't do any validation.
parseHtpasswd :: Text -> Htpasswd
parseHtpasswd = Htpasswd . fmap parseLine . zip (True : repeat False) . Text.lines
 where
  parseLine (isAdmin, ln) =
    let
      (u, p) = Text.break (== ':') ln
      user   = User {userName = u, userIsAdmin = isAdmin}
    in (user, encodeUtf8 $ Text.drop 1 p)

-- | Given a salt generator and a list of @(userName, password)@ pairs, return the equivalent htpasswd
-- contents.
writeHtpasswd :: Monad m
              => m ByteString -- ^ Action to generate salt
              -> [(Text, ByteString)]
              -> m Text
writeHtpasswd mkSalt =
  fmap Text.unlines . mapM writeUser
  where
    writeUser (name , p) = do
      salt <- mkSalt
      return $ name <> ":" <> decodeUtf8 (bcrypt 10 salt p)
