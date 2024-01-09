module Web.Session.SessionManager
  ( SessionManager (..)
  , sessionKeyAppearsReasonable
  , checkedSessionKeyFromCookieValue
  , newSessionKey
  ) where

import Web.Session.Prelude

import Web.Session.Options
import Web.Session.SessionKey
import Web.Session.Storage.Operation

-- | Server-wide state for the session mechanism
data SessionManager m = forall tx.
  Monad tx =>
  SessionManager
  { keyManager :: SessionKeyManager tx
  -- ^ A random session key generator
  , storage :: forall a. StorageOperation a -> tx a
  -- ^ The storage backend
  , options :: Options tx m
  , runTransaction :: forall a. tx a -> m a
  }

sessionKeyAppearsReasonable :: SessionManager m -> SessionKey -> Bool
sessionKeyAppearsReasonable SessionManager {keyManager = SessionKeyManager {check}} = check

checkedSessionKeyFromCookieValue
  :: SessionManager m -> ByteString -> Maybe SessionKey
checkedSessionKeyFromCookieValue x =
  sessionKeyFromCookieValue
    >=> (\v -> guard (sessionKeyAppearsReasonable x v) $> v)

newSessionKey :: SessionManager m -> m SessionKey
newSessionKey SessionManager {keyManager, runTransaction} =
  runTransaction keyManager.new
