module Yesod.Session.Persist.Storage.Exceptions
  ( StorageException (..)
  ) where

import Yesod.Session.Persist.Prelude

import Yesod.Session.Persist.Session (Session (..))

-- | Common exceptions that may be thrown by any storage.
data StorageException
  = -- | Thrown when attempting to insert a new session and
    --   another session with the same key already exists
    SessionAlreadyExists
      { existingSession :: Session
      , newSession :: Session
      }
  | -- | Thrown when attempting to replace an existing session
    --   but no session with the same key exists
    SessionDoesNotExist
      {newSession :: Session}
  deriving stock (Eq, Show)
  deriving anyclass (Exception)
