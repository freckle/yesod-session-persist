module Yesod.Session.Persist.Storage.Persistent
  ( persistentStorage
  , SessionPersistence (..)

    -- * Persistent reëxports
  , PersistEntity
  , PersistEntityBackend
  , SafeToInsert
  , ConnectionPool
  ) where

import Yesod.Session.Persist.Prelude

import Yesod.Session.Persist.Session
import Yesod.Session.Persist.SessionKey
import Yesod.Session.Persist.Storage.Exceptions
import Yesod.Session.Persist.Storage.Operation

import Database.Persist (Key, PersistRecordBackend)
import Database.Persist.Class
  ( PersistEntity
  , PersistEntityBackend
  , SafeToInsert
  )
import Database.Persist.Sql (ConnectionPool)

import Database.Persist qualified as Persist

-- | Mapping between 'Session' and a Persistent entity of your choice
data SessionPersistence backend record m = ( PersistRecordBackend record backend
                                           , Persist.PersistStoreWrite backend
                                           , SafeToInsert record
                                           ) =>
  SessionPersistence
  { databaseKey :: SessionKey -> Key record
  , toDatabase :: Session -> record
  , fromDatabase :: record -> Session
  , runTransaction :: forall a. ReaderT backend IO a -> m a
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
      Just old -> throwWithCallStack $ SessionAlreadyExists old session
  ReplaceSession session ->
    let key = sp.databaseKey session.key
    in  Persist.get key >>= \case
          Nothing -> throwWithCallStack $ SessionDoesNotExist session
          Just _old -> void $ Persist.replace key $ sp.toDatabase session
