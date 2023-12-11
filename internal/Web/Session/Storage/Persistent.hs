module Web.Session.Storage.Persistent
  ( persistentStorage
  , SessionPersistence (..)

    -- * Persistent reëxports
  , PersistEntity
  , PersistEntityBackend
  , SqlBackend
  , SafeToInsert
  , ConnectionPool
  ) where

import Web.Session.Prelude

import Web.Session.Session
import Web.Session.SessionKey
import Web.Session.Storage.Exceptions
import Web.Session.Storage.Operation

import Database.Persist (Key)
import Database.Persist.Class
  ( PersistEntity
  , PersistEntityBackend
  , SafeToInsert
  )
import Database.Persist.Sql (ConnectionPool, SqlBackend, SqlPersistT)

import Database.Persist qualified as Persist

-- | Mapping between 'Session' and a Persistent entity of your choice
data SessionPersistence a = (PersistEntity a, PersistEntityBackend a ~ SqlBackend, SafeToInsert a) =>
  SessionPersistence
  { databaseKey :: SessionKey -> Key a
  , toDatabase :: Session -> a
  , fromDatabase :: a -> Session
  }

persistentStorage
  :: SessionPersistence a -> StorageOperation result -> SqlPersistT IO result
persistentStorage sp@SessionPersistence {} = f
 where
  f :: forall result. StorageOperation result -> SqlPersistT IO result
  f = \case
    GetSession sessionKey -> fmap sp.fromDatabase <$> Persist.get (sp.databaseKey sessionKey)
    DeleteSession sessionKey -> Persist.delete $ sp.databaseKey sessionKey
    InsertSession session ->
      f (GetSession session.key) >>= \case
        Nothing -> void $ Persist.insert $ sp.toDatabase session
        Just old -> throwWithCallStack $ SessionAlreadyExists old session
    ReplaceSession session ->
      let key = sp.databaseKey session.key
      in  Persist.get key >>= \case
            Nothing -> throwWithCallStack $ SessionDoesNotExist session
            Just _old -> void $ Persist.replace key $ sp.toDatabase session
