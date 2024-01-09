module Web.Session.Save
  ( saveSession
  , Save (..)
  ) where

import Web.Session.Prelude

import Web.Session.Freeze.Type
import Web.Session.KeyRotation.Type
import Web.Session.Load
import Web.Session.Options
import Web.Session.Session
import Web.Session.SessionKey
import Web.Session.SessionManager
import Web.Session.Storage.Operation
import Web.Session.Timing.Options
import Web.Session.Timing.Time

import Control.Monad.State qualified as State
import Data.Map.Strict qualified as Map

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
  :: SessionManager m -> Load Session -> SessionMap -> m (Save Session)
saveSession SessionManager {options, storage, keyManager, runTransaction} load outputData =
  let ((rotation, freeze), newInfo) =
        flip State.runState outputData
          $ (,)
          <$> extractIgnoringError options.keyRotationEmbedding
          <*> extractIgnoringError options.freezeEmbedding
  in  runTransaction
        $ case freeze of
          Just FreezeSessionForCurrentRequest -> pure Frozen
          Nothing ->
            let save oldSessionMaybe =
                  saveSessionOnDb
                    options
                    storage
                    keyManager
                    load.time
                    newInfo
                    oldSessionMaybe
            in  case load.got of
                  Nothing -> save Nothing
                  Just s -> case rotation of
                    Just RotateSessionKey -> do
                      storage $ DeleteSession s.key
                      saveResult <- save Nothing
                      pure $ case saveResult of
                        NoChange -> Deleted
                        x -> x
                    Nothing -> save (Just s)

-- | Save a session to the database
--
-- If an old session is supplied, it is replaced, otherwise a new session is generated.
-- If the session is empty, it is not saved and 'Nothing' is returned.
-- If the timeout resolution optimization is applied (cf. 'setTimeoutResolution'),
-- the old session is returned and no update is made.
saveSessionOnDb
  :: Monad tx
  => Options m
  -> (forall a. StorageOperation a -> tx a)
  -> SessionKeyManager tx
  -> UTCTime
  -- ^ The current time
  -> SessionMap
  -- ^ The new session data to be saved
  -> Maybe Session
  -- ^ What's in the database
  -> tx (Save Session)
saveSessionOnDb options storage sessionKeyManager now newInfo oldSessionMaybe =
  asumM
    [ runMaybeT $ do
        guardMaybeT $ isNothing oldSessionMaybe
        guardMaybeT $ Map.null newInfo
        pure NoChange
    , runMaybeT $ do
        -- If the data is the same and the old access time is within
        -- the timeout resolution, just return the old session without
        -- doing anything else.
        res <- assertJust options.timing.resolution
        old <- assertJust oldSessionMaybe
        guardMaybeT $ old.map == newInfo
        guardMaybeT $ diffUTCTime now old.time.accessed < res
        pure NoChange
    , runMaybeT $ do
        oldSession <- assertJust oldSessionMaybe
        let newSession =
              Session
                { key = oldSession.key
                , map = newInfo
                , time = Time {created = oldSession.time.created, accessed = now}
                }
        lift $ storage $ ReplaceSession newSession
        pure $ Saved newSession
    ]
    `orElseM` do
      sessionKey <- sessionKeyManager.new
      let newSession =
            Session
              { key = sessionKey
              , map = newInfo
              , time = Time {created = now, accessed = now}
              }
      storage $ InsertSession newSession
      pure $ Saved newSession

orElseM :: Monad m => m (Maybe a) -> m a -> m a
a `orElseM` b = a >>= maybe b pure

asumM :: Monad m => [m (Maybe a)] -> m (Maybe a)
asumM = \case [] -> pure Nothing; x : xs -> x >>= maybe (asumM xs) (pure . Just)

guardMaybeT :: Monad m => Bool -> MaybeT m ()
guardMaybeT = \case True -> pure (); False -> MaybeT (pure Nothing)

assertJust :: Monad m => Maybe a -> MaybeT m a
assertJust = MaybeT . pure
