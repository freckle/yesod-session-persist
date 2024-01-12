module Yesod.Session.Persist.Test.Randomization
  ( newRandomization
  ) where

import Yesod.Session.Persist.Prelude

import Control.Concurrent.STM.TVar (newTVar, readTVar, writeTVar)
import Control.Monad.STM (STM)
import System.Random qualified as Random

newRandomization :: Int -> STM (Randomization STM)
newRandomization seed =
  deterministicallyRandomSTM
    $ let go g =
            DeterministicRandomization $ \n ->
              let (bs, g') = Random.genByteString (fromIntegral n) g
              in  (bs, go g')
      in  go $ Random.mkStdGen seed

deterministicallyRandomSTM
  :: DeterministicRandomization -> STM (Randomization STM)
deterministicallyRandomSTM =
  newTVar >=> pure . \ref ->
    Randomization $ \n -> do
      DeterministicRandomization gen <- readTVar ref
      let (bs, gen') = gen n
      writeTVar ref $! gen'
      pure bs
