module Web.Session.KeyRotation.Action
  ( assignSessionKeyRotation
  ) where

import Web.Session.Prelude

import Web.Session.KeyRotation.Type

import Web.Session.Options

import Yesod.Core (MonadHandler (liftHandler))

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
  :: MonadHandler m
  => Options tx m
  -> Maybe KeyRotation
  -- ^ 'Just' to rotate, or 'Nothing' to cancel any previous
  --   request for rotation and restore the default behavior
  -> m ()
assignSessionKeyRotation options =
  liftHandler . embed options.keyRotationEmbedding
