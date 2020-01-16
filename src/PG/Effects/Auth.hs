{-|
Effects and handlers for authentication.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module PG.Effects.Auth
  ( Auth(..)
  , checkPassword
  , runAuthMem
  , runAuthHtpasswd
  , parseHtpasswd
  , checkHtpasswd
  )
where

import Polysemy
import Crypto.KDF.BCrypt
import Data.ByteString (ByteString)
import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import PG.Types

-- |
-- = Effects

-- | Effect for validating user authentication
data Auth m a where
  CheckPassword :: Text -> ByteString -> Auth m (Maybe User)

makeSem_ ''Auth

-- | Returns @Nothing@ if the user doesn't exist or the password doesn't match and @Just user@
-- otherwise.
checkPassword
  :: Member Auth r
  => Text       -- ^ Username
  -> ByteString -- ^ Password
  -> Sem r (Maybe User)

-- |
-- = Interpreters

-- | Pure auth interpreter taking a function mapping usernames to (user, passwordHash) pairs and a
-- function for checking passwords
runAuthMem
  :: (Text -> Maybe (User, hash)) -> (ByteString -> hash -> Bool) -> Sem (Auth ': r) a -> Sem r a
runAuthMem fetchUser validate = interpret $ \case
  CheckPassword username password -> case fetchUser username of
    Just (u, hash) | validate password hash -> return . Just $ u
    _ -> return Nothing

-- | Runs an auth effect by checking if users are present in the given (user, bcrypt password hash)
-- list.
--
-- parseHtpasswd can be used to get a user list from an htpasswd file.
runAuthHtpasswd :: [(User, ByteString)] -> Sem (Auth ': r) a -> Sem r a
runAuthHtpasswd users = runAuthMem findUser checkHtpasswd
  where findUser name = find ((== name) . userName . fst) users

-- |
-- = Other

-- | Parses the contents of an htpasswd file into a list of @(user, passwordHash)@ pairs. Only
-- accepts -- bcrypt hashed passwords.
--
-- Doesn't do any validation.
parseHtpasswd :: Text -> [(User, ByteString)]
parseHtpasswd = fmap parseLine . zip (True : repeat False) . Text.lines
 where
  parseLine (isAdmin, ln) =
    let
      (u, p) = Text.break (== ':') ln
      user   = User {userName = u, userIsAdmin = isAdmin}
    in (user, encodeUtf8 $ Text.drop 1 p)

-- | Check a password against its bcrypt hash.
--
-- Returns @True@ if the hash is valid and matches the password.
checkHtpasswd
  :: ByteString -- ^ Password
  -> ByteString -- ^ Hash
  -> Bool
checkHtpasswd = validatePassword
