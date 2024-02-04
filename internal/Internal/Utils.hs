module Internal.Utils
  ( fstOf3
  , (<$$>)
  )
where

import Data.Functor (Functor (fmap), (<$>))

fstOf3 :: (a, b, c) -> a
fstOf3 (a, _, _) = a

infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
h <$$> m = fmap h <$> m
