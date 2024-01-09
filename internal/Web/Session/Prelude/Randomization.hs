module Web.Session.Prelude.Randomization
  ( Randomization (..)
  , deterministicallyRandomIO
  , deterministicallyRandomSTM
  , DeterministicRandomization (..)
  , hoistRandomization
  , defaultRandomization
  ) where

import Web.Session.Prelude.Reexports

import Crypto.Random (ChaChaDRG, DRG (randomBytesGenerate), drgNew)

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
deterministicallyRandomIO
  :: DeterministicRandomization -> IO (Randomization IO)
deterministicallyRandomIO =
  liftIO . newIORef >=> pure . \ref ->
    Randomization $ \n ->
      liftIO $ atomicModifyIORef' ref $ \(DeterministicRandomization gen) ->
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

defaultRandomization :: IO (Randomization IO)
defaultRandomization =
  deterministicallyRandomIO . makeDeterministicRandomization =<< liftIO drgNew
 where
  makeDeterministicRandomization :: ChaChaDRG -> DeterministicRandomization
  makeDeterministicRandomization drg =
    DeterministicRandomization $ \n ->
      let (bs, drg') = randomBytesGenerate (fromIntegral n) drg
      in  (bs, makeDeterministicRandomization drg')
