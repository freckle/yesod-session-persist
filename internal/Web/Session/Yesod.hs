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

import Database.Persist.Sql (SqlPersistT, runSqlPool)

data SessionConfiguration session = SessionConfiguration
  { persistence :: SessionPersistence session
  -- ^ Mapping between 'Session' and your Persistent entity
  , connectionPool :: ConnectionPool
  -- ^ SQL connection pool
  , options :: Options (SqlPersistT IO) IO
  -- ^ Various options that have defaults; see 'defaultOptions'
  }

-- | Use this to implement 'Yesod.Core.makeSessionBackend'.
--
-- The @session@ type parameter represents the Persistent entity
-- you're using to store sessions
-- (see the 'SessionPersistence' field of the configuration).
makeSessionBackend :: SessionConfiguration session -> IO SessionBackend
makeSessionBackend x =
  makeSessionBackend'
    SessionConfiguration'
      { storage = persistentStorage x.persistence
      , options = x.options
      , runTransaction = (`runSqlPool` x.connectionPool)
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
