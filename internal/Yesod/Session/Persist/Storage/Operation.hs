module Yesod.Session.Persist.Storage.Operation
  ( StorageOperation (..)
  , StorageOperation' (..)
  ) where

import Yesod.Session.Persist.Prelude

import Yesod.Session.Persist.Session
import Yesod.Session.Persist.SessionKey

data StorageOperation'
  = forall result. StorageOperation' (StorageOperation result)

deriving stock instance Show StorageOperation'

{- FOURMOLU_DISABLE -}

instance Eq StorageOperation' where
  (==) = \case
    StorageOperation' a@GetSession{}     -> \case StorageOperation' b@GetSession{}     -> a == b; _ -> False
    StorageOperation' a@DeleteSession{}  -> \case StorageOperation' b@DeleteSession{}  -> a == b; _ -> False
    StorageOperation' a@InsertSession{}  -> \case StorageOperation' b@InsertSession{}  -> a == b; _ -> False
    StorageOperation' a@ReplaceSession{} -> \case StorageOperation' b@ReplaceSession{} -> a == b; _ -> False

{- FOURMOLU_ENABLE -}

data StorageOperation result
  = -- | Get the session for the given session key
    --
    --   Returns 'Nothing' if the session is not found.
    result ~ Maybe Session => GetSession SessionKey
  | -- | Delete the session with given session key
    --
    -- Does not do anything if the session is not found.
    result ~ () => DeleteSession SessionKey
  | -- | Insert a new session
    --
    -- Throws 'SessionAlreadyExists' if there already exists a session with the same key.
    -- We only call this method after generating a fresh session key.
    result ~ () => InsertSession Session
  | -- | Replace the contents of a session
    --
    -- Throws 'SessionDoesNotExist' if there is no session with the given session key.
    -- We only call this method when updating a session that is known to exist.
    result ~ () => ReplaceSession Session

deriving stock instance Eq (StorageOperation result)
deriving stock instance Show (StorageOperation result)
