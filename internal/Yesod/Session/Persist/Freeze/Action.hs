module Yesod.Session.Persist.Freeze.Action
  ( disableSessionManagement
  , assignSessionFreeze
  ) where

import Yesod.Session.Persist.Prelude

import Yesod.Session.Persist.Freeze.Type

import Yesod.Session.Persist.Options

import Yesod.Core (MonadHandler (liftHandler), getYesod)

-- | Indicate whether the session should be frozen for the handling
--   of the current request
--
-- At the end of the request handler, if the value is 'Just', no
-- database actions will be performed and no cookies will be set.
assignSessionFreeze
  :: (MonadHandler m, HasSessionEmbeddings (HandlerSite m))
  => Maybe SessionFreeze
  -- ^ 'Just' to freeze the session, or 'Nothing' to cancel any previous
  --   request for session freezing and restore the default behavior
  -> m ()
assignSessionFreeze f = do
  embedding <- getSessionEmbeddings <$> getYesod
  liftHandler $ embed embedding.freeze f

disableSessionManagement
  :: (MonadHandler m, HasSessionEmbeddings (HandlerSite m)) => m ()
disableSessionManagement = assignSessionFreeze (Just FreezeSessionForCurrentRequest)
