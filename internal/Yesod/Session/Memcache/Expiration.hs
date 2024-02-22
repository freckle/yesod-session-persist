module Yesod.Session.Memcache.Expiration
  ( MemcacheExpiration (..)
  , noExpiration
  , fromUTC
  , maxTimestamp
  , minTimestamp
  ) where

import Internal.Prelude

import Data.Fixed (Pico)
import Database.Memcache.Types qualified as Memcache
import Time (UTCTime, nominalDiffTimeToSeconds, utcTimeToPOSIXSeconds)

newtype Exceptions = InvalidExpiration Pico
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data MemcacheExpiration
  = -- | Do not set expiration times; memache will only evict when the cache is full
    NoMemcacheExpiration
  | -- | Sessions will be stored in memcache with the same expiration time that we
    -- send to the HTTP client, the lesser of the idle and absolute timeouts.
    UseMemcacheExpiration

-- | Do not expire the session via Memcache's expiration mechanism.
--
--  Memcache will evict the session when the cache is full.
noExpiration :: Memcache.Expiration
noExpiration = 0

-- | Convert 'UTCTime' to 'Word32', with possibility of failure.
--
-- This function guards against UTCTime values that, converted to a timestamp,
-- would be too big or too small.
--
-- See 'maxTimestamp' and 'minTimestamp' for definitions of too 'big / small'.
fromUTC :: MonadThrow m => UTCTime -> m Word32
fromUTC utcTime = do
  when (tooLarge || tooSmall) $ throwWithCallStack $ InvalidExpiration seconds
  pure $ ceiling seconds
 where
  seconds = nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds utcTime
  tooLarge = seconds > maxTimestamp
  tooSmall = seconds < minTimestamp

-- | Minimum value that will be interpreted as a timestamp by Memcache
--
-- Values lower than this are considered to be "number of seconds" in the future
-- to expire a key / value pair. This is /not/ the interpretation we want.
--
-- See: https://github.com/dterei/memcache-hs/blob/83957ee9c4983f87447b0e7476a9a9155474dc80/Database/Memcache/Client.hs#L49-L59
--
-- This value is ~1960.
minTimestamp :: Num a => a
minTimestamp = 2592000 + 1 -- Values lower than this

-- | Check to make sure we don't overflow.
--
-- 4_294_967_295 is ~2096
maxTimestamp :: Num a => a
maxTimestamp = fromIntegral $ maxBound @Word32
