module Yesod.Session.Persist.Storage
  ( persistentStorage
  , SessionPersistence (..)

    -- * Persistent reëxports
  , PersistEntity
  , PersistEntityBackend
  , SafeToInsert
  , ConnectionPool
  ) where

import Internal.Prelude

import Database.Persist (Key, PersistRecordBackend)
import Database.Persist qualified as Persist
import Database.Persist.Class
  ( PersistEntity
  , PersistEntityBackend
  , SafeToInsert
  )
import Database.Persist.Sql (ConnectionPool)
import Session.Key
import Yesod.Session.SessionType
import Yesod.Session.Storage.Exceptions
import Yesod.Session.Storage.Operation

-- | Mapping between 'Yesod.Session.Persist.Session' and
--   a Persistent entity of your choice
data SessionPersistence backend record m = ( PersistRecordBackend record backend
                                           , Persist.PersistStoreWrite backend
                                           , SafeToInsert record
                                           ) =>
  SessionPersistence
  { databaseKey :: SessionKey -> Key record
  , toDatabase :: Session -> record
  , fromDatabase :: record -> Session
  , runDB :: forall a. ReaderT backend IO a -> m a
  }

persistentStorage
  :: forall record backend result m
   . (PersistRecordBackend record backend, Persist.PersistStoreWrite backend)
  => SessionPersistence backend record m
  -> StorageOperation result
  -> ReaderT backend IO result
persistentStorage sp@SessionPersistence {} = \case
  GetSession sessionKey ->
    fmap sp.fromDatabase <$> Persist.get (sp.databaseKey sessionKey)
  DeleteSession sessionKey ->
    Persist.delete $ sp.databaseKey sessionKey
  InsertSession session ->
    persistentStorage sp (GetSession session.key) >>= \case
      Nothing -> void $ Persist.insert $ sp.toDatabase session
      Just _ -> throwWithCallStack SessionAlreadyExists
  ReplaceSession session ->
    let key = sp.databaseKey session.key
    in  Persist.get key >>= \case
          Nothing -> throwWithCallStack SessionDoesNotExist
          Just _old -> void $ Persist.replace key $ sp.toDatabase session
