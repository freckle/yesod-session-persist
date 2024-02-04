module Yesod.Session.Storage.Exceptions
  ( StorageException (..)
  ) where

import Internal.Prelude

import Yesod.Session.SessionType (Session (..))
import Session.Key (SessionKey)

-- | Common exceptions that may be thrown by any storage.
data StorageException
  = -- | Thrown when attempting to insert a new session and
    --   another session with the same key already exists
    SessionAlreadyExists
      { existingSession :: Session
      , newSession :: Session
      }
  | -- | Thrown when attempting to insert a new session
    --   and another session with the same key already exists.
    --
    --   Use this when a storage backend has a primitive that fails if a key
    --   already exists, and does not return the existing object.
    SessionAlreadyExistsSimple
      { newSession :: Session
      }
  | -- | Thrown when attempting to replace an existing session
    --   but no session with the same key exists
    SessionDoesNotExist
      {newSession :: Session}
  | FailedToDeleteSession
      {existingSessionKey:: SessionKey}
  deriving stock (Eq, Show)
  deriving anyclass (Exception)
