module Yesod.Session.Memcache.Storage
  ( memcacheStorage
  , SessionPersistence (..)
  ) where

import Internal.Prelude

import Control.Monad.Reader (MonadReader (ask))
import Database.Memcache.Client qualified as Memcache
import Database.Memcache.Types qualified as Memcache
import Session.Key
import Yesod.Session.SessionType
import Yesod.Session.Storage.Exceptions
import Yesod.Session.Storage.Operation

-- | Mapping between 'Session' and Memcache representation.
data SessionPersistence = SessionPersistence
  { databaseKey :: SessionKey -> Memcache.Key
  , toDatabase :: Session -> Memcache.Value
  , fromDatabase :: Memcache.Value -> Session
  , client :: Memcache.Client
  }

memcacheStorage
  :: forall m result
   . (MonadThrow m, MonadReader Memcache.Client m, MonadIO m)
  => SessionPersistence
  -> StorageOperation result
  -> m result
memcacheStorage sp@SessionPersistence {} = \case
  GetSession sessionKey -> do
    client <- ask
    result <- liftIO $ Memcache.get client (sp.databaseKey sessionKey)
    pure $ result <&> \(value, _flags, _version) -> sp.fromDatabase value
  DeleteSession sessionKey -> do
    client <- ask
    void $ liftIO $ Memcache.delete client (sp.databaseKey sessionKey) bypassCAS
  InsertSession session -> do
    let
      key = sp.databaseKey session.key
      value = sp.toDatabase session

    client <- ask
    mVersion <- liftIO $ Memcache.add client key value defaultFlags cacheForever
    throwOnNothing SessionAlreadyExists mVersion
  ReplaceSession session -> do
    let key = sp.databaseKey session.key
    client <- ask
    mVersion <- liftIO $ Memcache.replace client key (sp.toDatabase session) defaultFlags cacheForever bypassCAS
    throwOnNothing SessionDoesNotExist mVersion
  where throwOnNothing exception maybeValue = maybe (throwWithCallStack exception) (const $ pure ()) maybeValue

-- | Do not expire the session via Memcache expiration.
--
-- Not all storage backends support expiration of keys. Furthermore, we want to
-- rely on /timing/ fields in the 'Options' data type to determine when a session expires.
--
-- N.B. No garbage collection of expired sessions is performed by this library.
cacheForever :: Memcache.Expiration
cacheForever = 0

defaultFlags :: Memcache.Flags
defaultFlags = 0

-- | Do not do any CAS checking.
--
-- Logically, a 'Version' (a.k.a CAS) value is optional. However, this optionality is represented
-- by a 'Version' of /0/. This is documented in the Memcache docs for the /set/, /add/,
-- and /replace/ commands:
--
-- https://github.com/memcached/memcached/wiki/BinaryProtocolRevamped#set-add-replace
--
-- But it applies at the level of the binary protocol itself. The /0/ 'Version'
-- sentinel value means "do not do any CAS checking".
bypassCAS :: Memcache.Version
bypassCAS = 0
