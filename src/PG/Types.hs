{-|
Main types for the application
-}
{-# LANGUAGE TemplateHaskell #-}
module PG.Types
  ( PostID(..)
  , AppInfo(..)
  , PGPostF(..)
  , PGPost
  , MediaF(..)
  , Media
  , MediaType(..)
  , User(..)
  , Admin(..)
  )
where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import Data.Time.Clock
import Crypto.JWT
import Network.URI
import Servant.Auth.Server
import PG.Util

-- | Unique ID for a post.
newtype PostID = PostID Int
               deriving (Ord, Show, Read, Eq)
               deriving newtype (ToJSON, FromJSON)

-- | General application instance-specific information.
data AppInfo = AppInfo
             { appInfoTitle :: !Text
             } deriving (Show)

-- | Post. Polymorhic on the media src type.
data PGPostF u = PGPostF
               { pgPostId :: !PostID
               , pgPostCreatedAt :: !UTCTime
               , pgPostMedia :: ![MediaF u]
               } deriving (Eq, Show, Functor, Foldable, Traversable)

type PGPost = PGPostF URI

-- | Media metadata. Polymorphic on the src type.
data MediaF u = MediaF
             { mediaCaption :: !Text
             , mediaType :: !MediaType
             , mediaSrc :: !u
             , mediaWidth :: !Word
             , mediaHeight :: !Word
             } deriving (Eq, Show, Functor, Foldable, Traversable)

type Media = MediaF URI

data MediaType = MediaTypeImage
  deriving (Eq, Show, Enum, Bounded)

-- | Data for an user session.
data User = User { userName :: !Text
                 , userIsAdmin :: !Bool
                 } deriving (Eq, Show)

-- | Admin-only user session.
data Admin = Admin { adminName :: !Text
                   } deriving (Eq, Show)

$(deriveJSON (jsonOpts "PG" "pgPost") ''PGPostF)
$(deriveJSON (jsonOpts "" "media") ''MediaF)
$(deriveJSON (jsonOpts "MediaType" ""){tagSingleConstructors = True} ''MediaType)
$(deriveJSON (jsonOpts "App" "appInfo") ''AppInfo)

instance ToJWT User where
  encodeJWT User { userName, userIsAdmin } =
    (claimSub ?~ review string userName) $ addClaim "admin" (Bool userIsAdmin) emptyClaimsSet

instance FromJWT User where
  decodeJWT claims = case claims ^. claimSub >>= preview string of
    Just sub -> case M.lookup "admin" (claims ^. unregisteredClaims) of
      Just (Bool b) -> Right $ User { userName = sub, userIsAdmin = b }
      _             -> Left "Invalid or missing admin"
    _ -> Left "Invalid or missing sub"

instance ToJWT Admin where
  encodeJWT Admin { adminName } = encodeJWT User { userName = adminName, userIsAdmin = True }

instance FromJWT Admin where
  decodeJWT claims = do
    user <- decodeJWT claims
    if userIsAdmin user then Right Admin { adminName = userName user } else Left "User is not admin"
