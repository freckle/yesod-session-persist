module Yesod.Session.Persist.Timing.Math
  ( nextExpires
  , isExpired
  ) where

import Yesod.Session.Persist.Prelude

import Yesod.Session.Persist.Timing.Time
import Yesod.Session.Persist.Timing.Timeout

-- | Calculate the next point in time where the given session
--   will expire assuming that it sees no activity until then
--
-- Returns 'Nothing' iff the settings do not specify any timeout limits.
nextExpires
  :: Timeout NominalDiffTime
  -> Time UTCTime
  -- ^ A session's timing information
  -> Maybe UTCTime
nextExpires timeout time =
  (fmap minimum . nonEmpty . catMaybes)
    [ flip addUTCTime time.accessed <$> timeout.idle
    , flip addUTCTime time.created <$> timeout.absolute
    ]

-- | Check if a session has expired
isExpired
  :: Timeout NominalDiffTime
  -- ^ Settings
  -> UTCTime
  -- ^ Now
  -> Time UTCTime
  -- ^ A session's timing information
  -> Bool
isExpired timeout now time =
  maybe False (<= now) $ nextExpires timeout time
