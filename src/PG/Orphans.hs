{-|
All orphan instances.
-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module PG.Orphans
  ()
where

import Polysemy
import Polysemy.Error
import Servant
import Servant.Auth.Server

-- | Needed to use throwAll with the $Sem (Error ServerError : r)$ monad
instance Member (Error ServerError) r  => ThrowAll (Sem r a) where
  throwAll = throw
