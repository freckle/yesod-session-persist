module Yesod.Session.Persist.Comparison
  ( Comparison (..)
  , differsOn
  ) where

import Yesod.Session.Persist.Prelude

data Comparison a = Comparison
  { old :: a
  , new :: a
  }

differsOn :: Eq b => (a -> b) -> Comparison a -> Bool
differsOn f Comparison {old, new} = f old /= f new
