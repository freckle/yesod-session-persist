module Web.Session.Prelude.Randomization
  ( Randomization (..)
  , deterministicallyRandomIO
  , deterministicallyRandomSTM
  , DeterministicRandomization (..)
  , hoistRandomization
  ) where

import Web.Session.Prelude.Reexports

-- | General means of obtaining randomness
newtype Randomization m = Randomization
  { getRandomBytes :: Natural -> m ByteString
  -- ^ Given a requested number of bytes, this action
  --   should produce a 'ByteString' of that length.
  }

hoistRandomization
  :: (forall a. m a -> m' a) -> Randomization m -> Randomization m'
hoistRandomization f (Randomization g) = Randomization (f . g)

-- | Convert from a deterministic generator to an effectful one
deterministicallyRandomIO :: DeterministicRandomization -> IO (Randomization IO)
deterministicallyRandomIO =
  newIORef >=> pure . \ref ->
    Randomization $ \n ->
      atomicModifyIORef' ref $ \(DeterministicRandomization gen) ->
        swap $ gen n

-- | Like 'deterministicallyRandomIO' but in 'STM' rather than 'IO'
deterministicallyRandomSTM
  :: DeterministicRandomization -> STM (Randomization STM)
deterministicallyRandomSTM =
  newTVar >=> pure . \ref ->
    Randomization $ \n -> do
      DeterministicRandomization gen <- readTVar ref
      let (bs, gen') = gen n
      writeTVar ref $! gen'
      pure bs

-- | A deterministic random generator
newtype DeterministicRandomization = DeterministicRandomization
  { nextRandomBytes :: Natural -> (ByteString, DeterministicRandomization)
  -- ^ Given a requested number of bytes, this function should give a
  --   'ByteString' of that length and a new deterministic generator.
  }
