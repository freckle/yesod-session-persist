module Yesod.Session.Memcache.Yesod
  ( makeSessionBackend
  , SessionConfiguration (..)
  ) where

import Internal.Prelude

import Control.Monad.Reader (runReaderT)
import Database.Memcache.Client qualified as Memcache
import Yesod.Core.Types (SessionBackend (..))
import Yesod.Session.Memcache.Storage
import Yesod.Session.Options
import Yesod.Session.Storage.Yesod
  ( SessionConfiguration' (..)
  , makeSessionBackend'
  )

data SessionConfiguration env = SessionConfiguration
  { persistence :: SessionPersistence
  -- ^ Mapping between 'Yesod.Session.Persist.Session' and Memcache
  --   representation
  , options :: Options (ReaderT env IO) IO
  -- ^ Various options that have defaults; see 'defaultOptions'
  }

-- | Use this to implement 'Yesod.Core.makeSessionBackend'.
makeSessionBackend
  :: SessionConfiguration Memcache.Client
  -> IO SessionBackend
makeSessionBackend configuration =
  let SessionConfiguration {persistence, options} = configuration
  in  case persistence of
        SessionPersistence {client} ->
          makeSessionBackend'
            SessionConfiguration'
              { storage = memcacheStorage persistence options
              , options = options
              , runDB = flip runReaderT client
              }
