module Session.Timing.Timeout
  ( Timeout (..)
  , defaultTimeout
  ) where

import Internal.Prelude

import Time

-- | How long sessions are allowed to live
--
-- See 'defaultTimeout'.
data Timeout a = Timeout
  { idle :: Maybe a
  -- ^ The amount of time a session will remain active in case there
  --   is no activity in the session
  --
  -- This is used both on the client side (by setting the cookie expires fields)
  -- and on the server.
  --
  -- Setting to 'Nothing' removes the idle timeout.
  , absolute :: Maybe a
  -- ^ The maximum amount of time a session can be active
  --
  -- This is used both on the client side (by setting the cookie expires fields)
  -- and on the server side.
  --
  -- Setting to 'Nothing' removes the absolute timeout.
  }
  deriving stock (Eq, Show, Generic, Functor)

instance Arbitrary a => Arbitrary (Timeout a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- | Default timeouts
--
--   - idle = 8 hours
--   - absolute = 30 days
defaultTimeout :: Timeout NominalDiffTime
defaultTimeout =
  Timeout
    { idle = Just $ hours 8
    , absolute = Just $ days 30
    }
