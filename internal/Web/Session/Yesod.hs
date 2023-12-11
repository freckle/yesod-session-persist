module Web.Session.Yesod
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

import Web.Session.Prelude

import Web.Session.Cookie.Logic
import Web.Session.Cookie.Reading
import Web.Session.Load
import Web.Session.Options
import Web.Session.Save
import Web.Session.SessionKey
import Web.Session.SessionManager
import Web.Session.Storage.Operation
import Web.Session.Storage.Persistent

import Yesod.Core.Types (SessionBackend (..))

import Database.Persist.Sql (runSqlPool)

data SessionConfiguration session = SessionConfiguration
  { persistence :: SessionPersistence session
  -- ^ Mapping between 'Session' and your Persistent entity
  , connectionPool :: ConnectionPool
  -- ^ SQL connection pool
  , randomization :: Randomization IO
  -- ^ Generator of random byte strings, used to contrive session keys
  , options :: Options IO
  -- ^ Various options that have defaults; see 'defaultOptions'
  }

-- | Use this to implement 'Yesod.Core.makeSessionBackend'.
--
-- The @session@ type parameter represents the Persistent entity
-- you're using to store sessions
-- (see the 'SessionPersistence' field of the configuration).
makeSessionBackend :: SessionConfiguration session -> SessionBackend
makeSessionBackend x =
  makeSessionBackend'
    SessionConfiguration'
      { storage = persistentStorage x.persistence
      , randomization = hoistRandomization lift x.randomization
      , options = x.options
      , runTransaction = (`runSqlPool` x.connectionPool)
      }

data SessionConfiguration' session = forall tx.
  Monad tx =>
  SessionConfiguration'
  { storage :: forall a. StorageOperation a -> tx a
  , randomization :: Randomization tx
  , options :: Options IO
  , runTransaction :: forall a. tx a -> IO a
  }

makeSessionBackend' :: SessionConfiguration' session -> SessionBackend
makeSessionBackend' SessionConfiguration' {..} =
  let
    keyManager = makeSessionKeyManager randomization
    sessionManager = SessionManager {keyManager, storage, options, runTransaction}
  in
    makeSessionBackend'' sessionManager

makeSessionBackend'' :: SessionManager IO -> SessionBackend
makeSessionBackend'' sessionManager =
  SessionBackend
    { sbLoadSession = \req -> do
        let
          cookie = findSessionKey (encodeUtf8 sessionManager.options.cookieName) req
          sessionKeyMaybe = cookie >>= checkedSessionKeyFromCookieValue sessionManager

        load <- loadSessionMaybe sessionManager sessionKeyMaybe

        pure
          ( loadedData load
          , \newData -> do
              save <- saveSession sessionManager load newData
              pure
                $ setCookie
                  sessionManager.options
                  CookieContext {cookie, load = load.got, save}
          )
    }
