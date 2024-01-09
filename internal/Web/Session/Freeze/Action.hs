module Web.Session.Freeze.Action
  ( assignSessionFreeze
  ) where

import Web.Session.Prelude

import Web.Session.Freeze.Type

import Web.Session.Options

import Yesod.Core (MonadHandler (liftHandler))

-- | Indicate whether the session should be frozen for the handling
--   of the current request
--
-- At the end of the request handler, if the value is 'Just', no
-- database actions will be performed and no cookies will be set.
assignSessionFreeze
  :: MonadHandler m
  => Options tx m
  -> Maybe SessionFreeze
  -- ^ 'Just' to freeze the session, or 'Nothing' to cancel any previous
  --   request for session freezing and restore the default behavior
  -> m ()
assignSessionFreeze options =
  liftHandler . embed options.freezeEmbedding
