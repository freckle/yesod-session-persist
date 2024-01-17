module Session.Freeze
  ( SessionFreeze (..)
  ) where

import Internal.Prelude

data SessionFreeze
  = FreezeSessionForCurrentRequest
  deriving stock (Eq, Ord, Show, Read, Bounded, Enum)
