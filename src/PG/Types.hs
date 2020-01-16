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
               deriving (Show, Read, Eq)
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
               } deriving (Show, Functor, Foldable, Traversable)

type PGPost = PGPostF URI

-- | Media metadata. Polymorphic on the src type.
data MediaF u = MediaF
             { mediaCaption :: !Text
             , mediaType :: !MediaType
             , mediaSrc :: !u
             , mediaWidth :: !Word
             , mediaHeight :: !Word
             } deriving (Show, Functor, Foldable, Traversable)

type Media = MediaF URI

data MediaType = MediaTypeImage
  deriving (Show)

-- | Data for an user session.
data User = User { userName :: !Text
                 , userIsAdmin :: !Bool
                 } deriving (Show)

$(deriveJSON (jsonOpts "PG" "pgPost") ''PGPostF)
$(deriveJSON (jsonOpts "" "media") ''MediaF)
$(deriveJSON (jsonOpts "MediaType" ""){tagSingleConstructors = True} ''MediaType)
$(deriveJSON (jsonOpts "App" "appInfo") ''AppInfo)

instance ToJWT User where
  encodeJWT User { userName, userIsAdmin } =
    (claimSub ?~ review string userName) $
    addClaim "admin" (Bool userIsAdmin)
    emptyClaimsSet

instance FromJWT User where
  decodeJWT claims = case claims ^. claimSub >>= preview string of
    Just sub -> case M.lookup "admin" (claims ^. unregisteredClaims) of
      Just (Bool b) -> Right $ User { userName = sub
                                    , userIsAdmin = b
                                    }
      _ -> Left "Invalid or missing admin"
    _ -> Left "Invalid or missing sub"
