module Web.Session.Invalidation.Type
  ( SessionInvalidation (..)
  ) where

import Web.Session.Prelude

-- | To "invalidate" a session means to delete the session on the server
--   and copy the stored data into a new session with a different key.
data SessionInvalidation
  = -- | Invalidate the current session key
    InvalidateCurrentSession
  deriving stock (Eq, Ord, Show, Read, Bounded, Enum)
