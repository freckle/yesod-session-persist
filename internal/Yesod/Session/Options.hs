module Yesod.Session.Options
  ( Options (..)
  , defaultOptions
  , hoistOptions
  ) where

import Internal.Prelude

import Comparison
import Data.Time qualified as Time
import Randomization
import Session.KeyRotation
import Session.Timing.Options
import Session.TransportSecurity
import Time (NominalDiffTime, UTCTime)
import Yesod.Core (SessionMap)
import Yesod.Session.Embedding.Map
import Yesod.Session.Embedding.Options

-- | Settings that have defaults
--
-- See 'defaultOptions'.
data Options tx m = Options
  { cookieName :: Text
  -- ^ The name of cookie where the session key will be saved
  , timing :: TimingOptions NominalDiffTime
  -- ^ Various time duration settings
  , transportSecurity :: TransportSecurity
  -- ^ Whether cookies require HTTPS
  , embedding :: SessionEmbeddings
  -- ^ How special session management indicators get smuggled through a 'SessionMap'
  , clock :: m UTCTime
  -- ^ How to determine the current time;
  --   you can change this to a fake for testing
  , randomization :: m (Randomization tx)
  -- ^ Generator of random byte strings, used to contrive session keys
  , keyRotationTrigger :: Comparison SessionMap -> Maybe KeyRotation
  -- ^ At the end of request handling, compare old session data to new
  --   session data to determine whether a key rotation should be performed
  }

-- | Default options
--
--   - cookieName = @"session-key"@
--   - timing = 'defaultTimingOptions'
--   - transportSecurity = 'AllowPlaintextTranport' (change this in production)
--   - embedding.keyRotation = @'showReadKeyEmbedding' "session-key-rotation"@
--   - embedding.freeze = @'showReadKeyEmbedding' "session-freeze"@
--   - clock = 'Time.getCurrentTime'
--   - randomization = 'defaultRandomization'
--   - keyRotationTrigger = 'const' 'Nothing'
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
    , keyRotationTrigger = const Nothing
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
