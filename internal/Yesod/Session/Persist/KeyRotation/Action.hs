module Yesod.Session.Persist.KeyRotation.Action
  ( rotateSessionKey
  , assignSessionKeyRotation
  ) where

import Yesod.Session.Persist.Prelude

import Yesod.Session.Persist.KeyRotation.Type

import Yesod.Session.Persist.Options

import Yesod.Core (MonadHandler (liftHandler), getYesod)

-- | Indicate whether the current session key should be rotated
--
-- The key rotation does not occur immediately;
-- this action only places a value into the session map.
--
-- Later calls to 'assignSessionKeyRotation' on the same handler will
-- override earlier calls.
--
-- At the end of the request handler, if the value is 'Just',
-- the session key will be rotated.
--
-- The session variable set by this function is then discarded
-- and is not persisted across requests.
assignSessionKeyRotation
  :: (MonadHandler m, HasSessionEmbeddings (HandlerSite m))
  => Maybe KeyRotation
  -- ^ 'Just' to rotate, or 'Nothing' to cancel any previous
  --   request for rotation and restore the default behavior
  -> m ()
assignSessionKeyRotation kr = do
  embedding <- getSessionEmbeddings <$> getYesod
  liftHandler $ embed embedding.keyRotation kr

rotateSessionKey
  :: (MonadHandler m, HasSessionEmbeddings (HandlerSite m)) => m ()
rotateSessionKey = assignSessionKeyRotation (Just RotateSessionKey)
