module Yesod.Session.Persist.Freeze.Type
  ( SessionFreeze (..)
  ) where

import Yesod.Session.Persist.Prelude

data SessionFreeze
  = FreezeSessionForCurrentRequest
  deriving stock (Eq, Ord, Show, Read, Bounded, Enum)
