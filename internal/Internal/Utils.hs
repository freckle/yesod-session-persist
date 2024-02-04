module Internal.Utils
  ( fstOf3
  )
where

fstOf3 :: (a, b, c) -> a
fstOf3 (a, _, _) = a
