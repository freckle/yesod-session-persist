module Yesod.Session.Storage.Exceptions
  ( StorageException (..)
  ) where

import Internal.Prelude

-- | Common exceptions that may be thrown by any storage.
data StorageException
  = -- | Thrown when attempting to insert a new session and
    --   another session with the same key already exists
    SessionAlreadyExists
  | -- | Thrown when attempting to replace an existing session
    --   but no session with the same key exists
    SessionDoesNotExist
  deriving stock (Eq, Show)
  deriving anyclass (Exception)
