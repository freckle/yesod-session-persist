module Web.Session.Options
  ( Options (..)
  , defaultOptions
  , hoistOptions
  , SessionEmbeddings (..)
  , HasSessionEmbeddings (..)
  ) where

import Web.Session.Prelude

import Web.Session.Freeze.Type
import Web.Session.KeyRotation.Type
import Web.Session.MapEmbedding
import Web.Session.Timing.Options
import Web.Session.TransportSecurity

import Data.Time qualified as Time

-- | Settings that have defaults
--
-- See 'defaultOptions'.
data Options tx m = Options
  { cookieName :: Text
  -- ^ The name of cookie where the session key will be saved
  , timing :: TimingOptions NominalDiffTime
  -- ^ Various time duration settings
  , transportSecurity :: TransportSecurity
  , embedding :: SessionEmbeddings
  -- ^ Whether cookies require HTTPS
  , clock :: m UTCTime
  -- ^ How to determine the current time;
  --   you can change this to a fake for testing
  , randomization :: m (Randomization tx)
  -- ^ Generator of random byte strings, used to contrive session keys
  }

data SessionEmbeddings = SessionEmbeddings
  { keyRotation :: SessionMapEmbedding KeyRotation
  -- ^ How to represent a key rotation instruction in the session data;
  --   see 'Web.Session.assignSessionKeyRotation'
  , freeze :: SessionMapEmbedding SessionFreeze
  -- ^ How to represent a freeze instruction in the session data;
  --   see 'Web.Session.assignSessionFreeze'
  }

class HasSessionEmbeddings a where
  getSessionEmbeddings :: a -> SessionEmbeddings

instance HasSessionEmbeddings SessionEmbeddings where
  getSessionEmbeddings = id

-- | Default options
--
--   - cookieName = @"session-key"@
--   - keyRotationEmbedding = @'showReadKeyEmbedding' "session-key-rotation"@
--   - freezeEmbedding = @'showReadKeyEmbedding' "session-freeze"@
--   - timing = 'defaultTimingOptions'
--   - transportSecurity = 'AllowPlaintextTranport' (change this in production)
--   - clock = 'Time.getCurrentTime'
defaultOptions :: Options IO IO
defaultOptions =
  Options
    { cookieName = "session-key"
    , timing = defaultTimingOptions
    , transportSecurity = AllowPlaintextTranport
    , clock = Time.getCurrentTime
    , randomization = defaultRandomization
    , embedding =
        SessionEmbeddings
          { keyRotation = showReadKeyEmbedding "session-key-rotation"
          , freeze = showReadKeyEmbedding "session-freeze"
          }
    }

hoistOptions
  :: Functor m2
  => (forall a. tx1 a -> tx2 a)
  -> (forall a. m1 a -> m2 a)
  -> Options tx1 m1
  -> Options tx2 m2
hoistOptions f g Options {..} =
  Options
    { clock = g clock
    , randomization = hoistRandomization f <$> g randomization
    , ..
    }
