module Yesod.Session.Memcache.Expiration
  ( MemcacheExpiration (..)
  , getCacheExpiration
  , noExpiration
  , minDiffTime
  ) where

import Internal.Prelude

import Database.Memcache.Types qualified as Memcache
import Session.Timing.Timeout (Timeout (..))
import Time (NominalDiffTime, UTCTime, utcTimeToPOSIXSeconds)

newtype Exceptions = InvalidExpiration NominalDiffTime
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data MemcacheExpiration
  = -- | Do not set expiration times; memache will only evict when the cache is full
    NoMemcacheExpiration
  | -- | Sessions will be stored in memcache with the same expiration time that we
    -- send to the HTTP client, the lesser of the idle and absolute timeouts.
    UseMemcacheExpiration

-- | Get expiration time for Memcache.
--
-- This function will throw if an expiration time is out of bounds, but since
-- this function is called at the time of app initialization, it is acceptable to
-- throw.
getCacheExpiration
  :: (MonadIO m, MonadThrow m)
  => MemcacheExpiration
  -> m UTCTime
  -> Timeout NominalDiffTime
  -> m Memcache.Expiration
getCacheExpiration NoMemcacheExpiration _clock _timeout = pure noExpiration
getCacheExpiration UseMemcacheExpiration clock timeout =
  case getMinimumTimeout timeout of
    Nothing -> pure noExpiration
    Just duration -> do
      now <- clock

      let timeStamp = utcTimeToPOSIXSeconds now + duration

      unless (expirationWithinBounds timeStamp)
        $ throwWithCallStack
        $ InvalidExpiration timeStamp

      pure $ floor timeStamp

-- | Do not expire the session via Memcache's expiration mechanism.
--
--  Memcache will evict the session when the cache is full.
noExpiration :: Memcache.Expiration
noExpiration = 0

-- | Get minimum of idle and absolute timeouts.
--
-- If both are 'Nothing', return 'Nothing'.
getMinimumTimeout :: Timeout NominalDiffTime -> Maybe NominalDiffTime
getMinimumTimeout (Timeout idle absolute) = fmap getMin $ idleMin <> absoluteMin
 where
  idleMin = Min <$> idle
  absoluteMin = Min <$> absolute

-- | Determine if we have a valid 'NominalDiffTime' time.
--
-- Memcache interprets an expiration value differently depending on the value:
--
--  * If the value is less than 'minDiffTime', it is a duration
--
--  * If the value is greater than or equal to 'minDiffTime', it is a timestamp
--
--  We bar values lower than the 'minDiffTime' in order to avoid this dual
--  interpretation.
--
--  The 'maxDiffTime' check is there to ensure we do not overflow 'Memcache.Expiration'.
expirationWithinBounds :: NominalDiffTime -> Bool
expirationWithinBounds expiration = expiration >= fromInteger minDiffTime && expiration <= maxDiffTime
 where
  maxDiffTime = fromInteger $ toInteger $ maxBound @Memcache.Expiration

-- | Minimum timestamp that will be interpreted as a timestamp by Memcache.
--
-- See: https://github.com/dterei/memcache-hs/blob/83957ee9c4983f87447b0e7476a9a9155474dc80/Database/Memcache/Client.hs#L49-L59
minDiffTime :: Integer
minDiffTime = 2592000 + 1
