module Web.Session.Timing.Time
  ( Time (..)
  ) where

import Web.Session.Prelude

-- | Creation and access times, used to determine session expiration
data Time a = Time
  { created :: a
  -- ^ When the session was created
  --
  -- This is used to apply the absolute timeout.
  , accessed :: a
  -- ^ When the session was last accessed
  --
  -- This is used to apply the idle timeout.
  }
  deriving stock (Eq, Show)
