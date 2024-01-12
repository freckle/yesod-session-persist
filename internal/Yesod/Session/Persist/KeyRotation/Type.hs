module Yesod.Session.Persist.KeyRotation.Type
  ( KeyRotation (..)
  ) where

import Yesod.Session.Persist.Prelude

-- | /Key rotation/ means we delete the session on the server
--   and copy the stored data into a new session with a different key.
data KeyRotation
  = -- | Generate a new session key and invalidate the old one
    RotateSessionKey
  deriving stock (Eq, Ord, Show, Read, Bounded, Enum)