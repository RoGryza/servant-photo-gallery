-- | DSL for getting the current time
module PG.Effects.Clock
  ( HasClock(..)
  , utcCurrentTime
  )
where

import Control.Monad.Reader
import Data.Time.Clock

class HasClock a where
  clockCurrentTime :: a -> IO UTCTime

utcCurrentTime :: (MonadReader env m, HasClock env, MonadIO m) => m UTCTime
utcCurrentTime = asks clockCurrentTime >>= liftIO
