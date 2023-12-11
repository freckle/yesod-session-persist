module Web.Session.Freeze.Type
  ( SessionFreeze (..)
  ) where

import Web.Session.Prelude

data SessionFreeze
  = FreezeSessionForCurrentRequest
  deriving stock (Eq, Ord, Show, Read, Bounded, Enum)
