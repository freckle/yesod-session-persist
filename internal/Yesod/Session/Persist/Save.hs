module Yesod.Session.Persist.Save
  ( saveSession
  , Save (..)
  ) where

import Internal.Prelude

import Comparison
import Control.Monad.State qualified as State
import Session.Freeze
import Session.KeyRotation
import Yesod.Core (SessionMap)
import Yesod.Session.Embedding.Core
import Yesod.Session.Manager
import Yesod.Session.Options
import Yesod.Session.Persist.Load
import Yesod.Session.SessionType
import Yesod.Session.Storage.Operation
import Yesod.Session.Storage.Save qualified as Storage

data Save a
  = -- | Nothing was done because a session freeze was requested
    Frozen
  | -- | There were no changes worth saving.
    NoChange
  | -- | A session was saved (either a new or existing session key).
    Saved a
  | -- | A session was deleted, and no new session was inserted.
    Deleted
  deriving stock (Eq, Ord, Show)

-- | Save the session on the storage backend
--
-- A 'SessionLoad' given by 'loadSession' is expected besides
-- the new contents of the session.
--
-- Returns 'Nothing' if the session was empty and didn't need to be saved.
-- Note that this does /not/ necessarily means that nothing was done.
-- If you ask for a session key to be rotated and clear every other sesssion
-- variable, then 'saveSession' will delete the older session but will
-- avoid creating a new, empty one.
saveSession
  :: Monad tx
  => SessionManager tx m
  -> Load Session
  -> SessionMap
  -> m (Save Session)
saveSession SessionManager {options, storage, keyManager, runTransaction} load outputData =
  runTransaction
    $ case freeze of
      Just FreezeSessionForCurrentRequest -> pure Frozen
      Nothing ->
        case (load.got, rotation) of
          (Just s, Just RotateSessionKey) -> do
            storage $ DeleteSession s.key
            maybe Deleted Saved <$> save Nothing
          _ -> maybe NoChange Saved <$> save load.got
 where
  ((requestedRotation, freeze), newInfo) =
    flip State.runState outputData
      $ (,)
      <$> extractIgnoringError options.embedding.keyRotation
      <*> extractIgnoringError options.embedding.freeze

  autoRotation =
    options.keyRotationTrigger
      Comparison {old = loadedData load, new = newInfo}

  rotation = requestedRotation <|> autoRotation

  save oldSessionMaybe =
    Storage.save options storage keyManager load.time newInfo oldSessionMaybe
