module Web.Session.Options
  ( Options (..)
  , defaultOptions
  , hoistOptions
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
data Options m = Options
  { cookieName :: Text
  -- ^ The name of cookie where the session key will be saved
  , keyRotationEmbedding :: SessionMapEmbedding KeyRotation
  -- ^ How to represent a key rotation instruction in the session data;
  --   see 'Web.Session.assignSessionKeyRotation'
  , freezeEmbedding :: SessionMapEmbedding SessionFreeze
  -- ^ How to represent a freeze instruction in the session data;
  --   see 'Web.Session.assignSessionFreeze'
  , timing :: TimingOptions NominalDiffTime
  -- ^ Various time duration settings
  , transportSecurity :: TransportSecurity
  -- ^ Whether cookies require HTTPS
  , clock :: m UTCTime
  -- ^ How to determine the current time;
  --   you can change this to a fake for testing
  }

-- | Default options
--
--   - cookieName = @"session-key"@
--   - keyRotationEmbedding = @'showReadKeyEmbedding' "session-key-rotation"@
--   - freezeEmbedding = @'showReadKeyEmbedding' "session-freeze"@
--   - timing = 'defaultTimingOptions'
--   - transportSecurity = 'AllowPlaintextTranport' (change this in production)
--   - clock = 'Time.getCurrentTime'
defaultOptions :: Options IO
defaultOptions =
  Options
    { cookieName = "session-key"
    , keyRotationEmbedding = showReadKeyEmbedding "session-key-rotation"
    , freezeEmbedding = showReadKeyEmbedding "session-freeze"
    , timing = defaultTimingOptions
    , transportSecurity = AllowPlaintextTranport
    , clock = Time.getCurrentTime
    }

hoistOptions :: (forall a. m a -> n a) -> Options m -> Options n
hoistOptions f Options {..} =
  Options
    { clock = f clock
    , ..
    }
