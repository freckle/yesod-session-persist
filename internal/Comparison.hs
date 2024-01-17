module Comparison
  ( Comparison (..)
  , differsOn
  ) where

import Internal.Prelude

data Comparison a = Comparison
  { old :: a
  , new :: a
  }

differsOn :: Eq b => (a -> b) -> Comparison a -> Bool
differsOn f Comparison {old, new} = f old /= f new
