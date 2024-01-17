module Yesod.Session.Freeze
  ( disableSessionManagement
  , assignSessionFreeze
  ) where

import Internal.Prelude

import Embedding
import Session.Freeze
import Yesod.Core (HandlerSite, MonadHandler (liftHandler), getYesod)
import Yesod.Session.Options

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
