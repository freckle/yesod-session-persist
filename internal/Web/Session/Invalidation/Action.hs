module Web.Session.Invalidation.Action
  ( assignSessionInvalidation
  ) where

import Web.Session.Prelude

import Web.Session.Invalidation.Type

import Web.Session.Options

import Yesod.Core (MonadHandler (liftHandler))

-- | Indicate whether the current session should be invalidated
--
-- The invalidation does not occur immediately;
-- this action only places a value into the session map.
--
-- Later calls to 'assignSessionInvalidation' on the same handler will
-- override earlier calls.
--
-- At the end of the request handler, if the value is 'Just',
-- the session will be invalidated.
--
-- The session variable set by this function is then discarded
-- and is not persisted across requests.
assignSessionInvalidation
  :: MonadHandler m
  => Options m
  -> Maybe SessionInvalidation
  -- ^ 'Just' to invalidate the session, or 'Nothing' to cancel any previous
  --   request for session invalidation and restore the default behavior
  -> m ()
assignSessionInvalidation options =
  liftHandler . embed options.invalidationEmbedding
