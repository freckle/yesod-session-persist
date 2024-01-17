module Yesod.Session.Storage.Save
  ( save
  ) where

import Internal.Prelude

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Map.Strict qualified as Map
import Session.Timing.Options
import Session.Timing.Time
import Time
import Yesod.Core (SessionMap)
import Yesod.Session.Key
import Yesod.Session.Options
import Yesod.Session.SessionType
import Yesod.Session.Storage.Operation

-- | Save a session to the database
--
-- Return value of 'Nothing' indicates that no changes were made.
-- 'Just' is returned when a session was saved, either by an insert
-- or a replace operation.
save
  :: Monad tx
  => Options tx m
  -> (forall a. StorageOperation a -> tx a)
  -> SessionKeyManager tx
  -> UTCTime
  -- ^ The current time
  -> SessionMap
  -- ^ The new session data to be saved
  -> Maybe Session
  -- ^ What's in the database
  -> tx (Maybe Session)
save options storage sessionKeyManager now newInfo oldSessionMaybe =
  asumM
    [ runMaybeT $ do
        guardMaybeT $ isNothing oldSessionMaybe
        guardMaybeT $ Map.null newInfo
        pure Nothing
    , runMaybeT $ do
        -- If the data is the same and the old access time is within
        -- the timeout resolution, just return the old session without
        -- doing anything else.
        res <- assertJust options.timing.resolution
        old <- assertJust oldSessionMaybe
        guardMaybeT $ old.map == newInfo
        guardMaybeT $ diffUTCTime now old.time.accessed < res
        pure Nothing
    , runMaybeT $ do
        oldSession <- assertJust oldSessionMaybe
        let newSession =
              Session
                { key = oldSession.key
                , map = newInfo
                , time = Time {created = oldSession.time.created, accessed = now}
                }
        lift $ storage $ ReplaceSession newSession
        pure $ Just newSession
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
      pure $ Just newSession

orElseM :: Monad m => m (Maybe a) -> m a -> m a
a `orElseM` b = a >>= maybe b pure

asumM :: Monad m => [m (Maybe a)] -> m (Maybe a)
asumM = \case [] -> pure Nothing; x : xs -> x >>= maybe (asumM xs) (pure . Just)

guardMaybeT :: Monad m => Bool -> MaybeT m ()
guardMaybeT = \case True -> pure (); False -> MaybeT (pure Nothing)

assertJust :: Monad m => Maybe a -> MaybeT m a
assertJust = MaybeT . pure
