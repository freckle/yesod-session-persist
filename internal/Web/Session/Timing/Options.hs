module Web.Session.Timing.Options
  ( TimingOptions (..)
  , defaultTimingOptions
  ) where

import Web.Session.Prelude

import Web.Session.Timing.Timeout

-- | Time duration settings
--
-- See 'defaultTimingOptions'.
data TimingOptions a = TimingOptions
  { timeout :: Timeout a
  -- ^ How long sessions are allowed to live
  , resolution :: Maybe a
  -- ^ If @'Just' resolution@, this setting provides an optimization that can prevent
  --   excessive database writes. If the only thing that needs to be updated is the
  --   session's last access time, the write will be skipped if the previously recorded
  --   access time is within @resolution@ long ago.
  }
  deriving stock (Eq, Show)

-- | Default timing options
--
--   - timeout = 'defaultTimeout'
--   - resolution = 10 minutes
defaultTimingOptions :: TimingOptions NominalDiffTime
defaultTimingOptions =
  TimingOptions
    { timeout = defaultTimeout
    , resolution = Just $ minutes 10
    }
 where
  minutes = (*) 60
