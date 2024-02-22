module Yesod.Session.Memcache.Storage
  ( memcacheStorage
  , SessionPersistence (..)
  , getMemcacheExpiration
  ) where

import Internal.Prelude

import Database.Memcache.Client qualified as Memcache
import Database.Memcache.Types qualified as Memcache
import Session.Key
import Session.Timing.Math (nextExpires)
import Time (NominalDiffTime)
import Session.Timing.Options (TimingOptions (timeout))
import Session.Timing.Time (Time (..))
import Time (UTCTime)
import Session.Timing.Timeout (Timeout(..))
import Yesod.Core (SessionMap)
import Yesod.Session.Memcache.Expiration
  ( MemcacheExpiration (UseMemcacheExpiration, NoMemcacheExpiration)
  , fromUTC
  , noExpiration
  )
import Yesod.Session.Options (Options (timing))
import Yesod.Session.SessionType
import Yesod.Session.Storage.Exceptions
import Yesod.Session.Storage.Operation

-- | Mapping between 'Session' and Memcache representation.
data SessionPersistence = SessionPersistence
  { databaseKey :: SessionKey -> Memcache.Key
  , toDatabase :: (SessionMap, Time UTCTime) -> Memcache.Value
  , fromDatabase
      :: Memcache.Value
      -> Either SomeException (SessionMap, Time UTCTime)
  , client :: Memcache.Client
  , expiration :: MemcacheExpiration
  }

memcacheStorage
  :: forall m result
   . (MonadThrow m, MonadIO m)
  => SessionPersistence
  -> Options IO IO
  -> StorageOperation result
  -> m result
memcacheStorage sp opt = \case
  GetSession sessionKey -> do
    mValue <-
      liftIO $ fmap fstOf3 <$> Memcache.get sp.client (sp.databaseKey sessionKey)

    case mValue of
      Nothing -> pure Nothing
      Just value -> do
        (map, time) <- either throwM pure $ sp.fromDatabase value
        pure $ Just Session {key = sessionKey, map, time}
  DeleteSession sessionKey -> do
    void $ liftIO $ Memcache.delete sp.client (sp.databaseKey sessionKey) bypassCAS
  InsertSession session -> do
    let
      key = sp.databaseKey session.key
      value = sp.toDatabase (session.map, session.time)

    expiration <- getMemcacheExpiration sp.expiration opt.timing.timeout session.time

    mVersion <- liftIO $ Memcache.add sp.client key value defaultFlags expiration
    throwOnNothing SessionAlreadyExists mVersion
  ReplaceSession session -> do
    let key = sp.databaseKey session.key

    expiration <- getMemcacheExpiration sp.expiration opt.timing.timeout session.time

    mVersion <-
      liftIO
        $ Memcache.replace
          sp.client
          key
          (sp.toDatabase (session.map, session.time))
          defaultFlags
          expiration
          bypassCAS
    throwOnNothing SessionDoesNotExist mVersion
 where
  throwOnNothing exception maybeValue = maybe (throwWithCallStack exception) (const $ pure ()) maybeValue
  fstOf3 (a, _, _) = a


-- | Determine what 'Memcache.Expiration' value to use.
getMemcacheExpiration :: MonadThrow m => MemcacheExpiration -> Timeout NominalDiffTime -> Time UTCTime -> m Memcache.Expiration
getMemcacheExpiration UseMemcacheExpiration timeout time = maybe (pure noExpiration) fromUTC $ nextExpires timeout time
getMemcacheExpiration NoMemcacheExpiration _timeout _time = pure noExpiration


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
