module Yesod.Session.Persist.Yesod
  ( -- * Concretely
    makeSessionBackend
  , SessionConfiguration (..)

    -- * More general
  , makeSessionBackend'
  , SessionConfiguration' (..)

    -- * Extra general
  , makeSessionBackend''

    -- * Reëxport
  , SessionBackend
  ) where

import Yesod.Session.Persist.Prelude

import Yesod.Core.Types (SessionBackend (..))
import Yesod.Session.Persist.Cookie.Logic
import Yesod.Session.Persist.Cookie.Reading
import Yesod.Session.Persist.Load
import Yesod.Session.Persist.Options
import Yesod.Session.Persist.Save
import Yesod.Session.Persist.SessionKey
import Yesod.Session.Persist.SessionManager
import Yesod.Session.Persist.Storage.Operation
import Yesod.Session.Persist.Storage.Persistent

data SessionConfiguration persistentBackend persistentRecord = SessionConfiguration
  { persistence :: SessionPersistence persistentBackend persistentRecord IO
  -- ^ Mapping between 'Session' and your Persistent entity
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
        SessionPersistence {runTransaction} ->
          makeSessionBackend'
            SessionConfiguration'
              { storage = persistentStorage persistence
              , options = options
              , runTransaction
              }

data SessionConfiguration' session = forall tx.
  Monad tx =>
  SessionConfiguration'
  { storage :: forall a. StorageOperation a -> tx a
  , options :: Options tx IO
  , runTransaction :: forall a. tx a -> IO a
  }

makeSessionBackend' :: SessionConfiguration' session -> IO SessionBackend
makeSessionBackend' SessionConfiguration' {options = options :: Options tx m, ..} = do
  keyManager :: SessionKeyManager tx <-
    makeSessionKeyManager <$> options.randomization
  let sessionManager = SessionManager {keyManager, storage, options, runTransaction}
  pure $ makeSessionBackend'' sessionManager

makeSessionBackend'' :: Monad tx => SessionManager tx IO -> SessionBackend
makeSessionBackend'' sessionManager@SessionManager {options} =
  SessionBackend
    { sbLoadSession = \req -> do
        let
          cookie = findSessionKey (encodeUtf8 options.cookieName) req
          sessionKeyMaybe = cookie >>= checkedSessionKeyFromCookieValue sessionManager

        load <- loadSessionMaybe sessionManager sessionKeyMaybe

        pure
          ( loadedData load
          , \newData -> do
              save <- saveSession sessionManager load newData
              pure $ setCookie options CookieContext {cookie, load = load.got, save}
          )
    }