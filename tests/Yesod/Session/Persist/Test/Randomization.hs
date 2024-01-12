module Yesod.Session.Persist.Test.Randomization
  ( newRandomization
  ) where

import Yesod.Session.Persist.Prelude

import System.Random qualified as Random

newRandomization :: Int -> STM (Randomization STM)
newRandomization seed =
  deterministicallyRandomSTM
    $ let go g =
            DeterministicRandomization $ \n ->
              let (bs, g') = Random.genByteString (fromIntegral n) g
              in  (bs, go g')
      in  go $ Random.mkStdGen seed
