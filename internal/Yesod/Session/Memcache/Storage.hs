module Yesod.Session.Memcache.Storage
  ( memcacheStorage
  , SessionPersistence (..)
  ) where

import Internal.Prelude

import Control.Monad.Reader (MonadReader, asks)
import Database.Memcache.Client qualified as Memcache
import Database.Memcache.Types qualified as Memcache
import Session.Key
import Yesod.Session.Memcache.Class (HasMemcacheClient (..))
import Yesod.Session.SessionType
import Yesod.Session.Storage.Exceptions
import Yesod.Session.Storage.Operation

-- | Mapping between 'Session' and Memcache representation.
data SessionPersistence = SessionPersistence
  { databaseKey :: SessionKey -> Memcache.Key
  , toDatabase :: Session -> Memcache.Value
  , fromDatabase :: Memcache.Value -> Session
  , runDB :: forall a b m. ReaderT b IO a -> m a
  }

memcacheStorage
  :: forall env m result
   . (MonadReader env m, HasMemcacheClient env, MonadIO m)
  => SessionPersistence
  -> StorageOperation result
  -> m result
memcacheStorage sp@SessionPersistence {} = \case
  GetSession sessionKey ->
    let get = \client -> Memcache.get client (sp.databaseKey sessionKey)
    in  getClient
          >>= liftIO
          . get
          >>= pure
          . fmap (sp.fromDatabase . fstOf3)
  DeleteSession sessionKey ->
    getClient
      >>= \client ->
        liftIO
          $ Memcache.delete client (sp.databaseKey sessionKey) bypassCAS
          >>= \success -> when (not success) $ throwWithCallStack $ FailedToDeleteSession sessionKey
  InsertSession session ->
    let
      key = sp.databaseKey session.key
      value = sp.toDatabase session
      sessionAlreadyExistsError = throwWithCallStack $ SessionAlreadyExistsSimple session
    in
      getClient
        >>= \client ->
          liftIO
            $ Memcache.add client key value defaultFlags defaultExpiration
            >>= \case
              Nothing -> sessionAlreadyExistsError
              Just _ -> pure ()
  ReplaceSession session ->
    let
      key = sp.databaseKey session.key
      sessionDoesNotExistError = throwWithCallStack $ SessionDoesNotExist session
    in
      getClient
        >>= \client ->
          liftIO
            $ Memcache.replace
              client
              key
              (sp.toDatabase session)
              defaultFlags
              defaultExpiration
              bypassCAS
            >>= \case
              Nothing -> sessionDoesNotExistError
              Just _ -> pure ()
 where
  getClient = (asks @env @m) getMemcacheClient

defaultExpiration :: Memcache.Expiration
defaultExpiration = 0

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
