module Yesod.Session.Persist.Yesod
  ( -- * Concretely
    makeSessionBackend
  , SessionConfiguration (..)
  ) where

import Internal.Prelude

import Yesod.Core.Types (SessionBackend (..))
import Yesod.Session.Options
import Yesod.Session.Persist.Storage
import Yesod.Session.Storage.Yesod (SessionConfiguration'(..), makeSessionBackend')

data SessionConfiguration persistentBackend persistentRecord = SessionConfiguration
  { persistence :: SessionPersistence persistentBackend persistentRecord IO
  -- ^ Mapping between 'Yesod.Session.Persist.Session' and your Persistent entity
  , options :: Options (ReaderT persistentBackend IO) IO
  -- ^ Various options that have defaults; see 'defaultOptions'
  }

-- | Use this to implement 'Yesod.Core.makeSessionBackend'.
--
-- The @session@ type parameter represents the Persistent entity
-- you're using to store sessions
-- (see the 'SessionPersistence' field of the configuration).
makeSessionBackend
  :: forall persistentBackend persistentRecord
   . SessionConfiguration persistentBackend persistentRecord
  -> IO SessionBackend
makeSessionBackend configuration =
  let SessionConfiguration {persistence, options} = configuration
  in  case persistence of
        SessionPersistence {runDB} ->
          makeSessionBackend'
            SessionConfiguration'
              { storage = persistentStorage persistence
              , options = options
              , runDB
              }
