module Yesod.Session.Persist.Prelude.Randomization
  ( Randomization (..)
  , deterministicallyRandom
  , DeterministicRandomization (..)
  , hoistRandomization
  , defaultRandomization
  ) where

import Yesod.Session.Persist.Prelude.Reexports

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
deterministicallyRandom
  :: DeterministicRandomization -> IO (Randomization IO)
deterministicallyRandom =
  liftIO . newIORef >=> pure . \ref ->
    Randomization $ \n ->
      liftIO $ atomicModifyIORef' ref $ \(DeterministicRandomization gen) ->
        swap $ gen n

-- | A deterministic random generator
newtype DeterministicRandomization = DeterministicRandomization
  { nextRandomBytes :: Natural -> (ByteString, DeterministicRandomization)
  -- ^ Given a requested number of bytes, this function should give a
  --   'ByteString' of that length and a new deterministic generator.
  }

defaultRandomization :: IO (Randomization IO)
defaultRandomization =
  deterministicallyRandom . makeDeterministicRandomization =<< liftIO drgNew
 where
  makeDeterministicRandomization :: ChaChaDRG -> DeterministicRandomization
  makeDeterministicRandomization drg =
    DeterministicRandomization $ \n ->
      let (bs, drg') = randomBytesGenerate (fromIntegral n) drg
      in  (bs, makeDeterministicRandomization drg')
