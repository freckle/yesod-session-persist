module Web.Session.Load
  ( loadSessionMaybe
  , loadSession
  , loadNothing
  , Load (..)
  , didSessionLoad
  , loadedData
  ) where

import Web.Session.Prelude

import Web.Session.Options
import Web.Session.Session
import Web.Session.SessionKey
import Web.Session.SessionManager
import Web.Session.Storage.Operation
import Web.Session.Timing.Math
import Web.Session.Timing.Options

import Data.Map.Strict qualified as Map

data Load a = Load
  { got :: Maybe a
  -- ^ The original session that was loaded from the database, if any
  , time :: UTCTime
  -- ^ The time at which the session was loaded
  }
  deriving stock (Eq, Show)

didSessionLoad :: Load a -> Bool
didSessionLoad = isJust . (.got)

loadedData :: Load Session -> SessionMap
loadedData load =
  maybe Map.empty (.map) load.got

loadSession :: Monad m => SessionManager m -> SessionKey -> m (Load Session)
loadSession SessionManager {options, storage, runTransaction} sessionKey = do
  now <- options.clock
  got <-
    runMaybeT $ do
      session <-
        MaybeT $ runTransaction $ storage $ GetSession sessionKey
      MaybeT $ pure $ guard $ not $ isExpired options.timing.timeout now session.time
      pure session
  pure Load {got, time = now}

loadNothing :: Monad m => SessionManager m -> m (Load a)
loadNothing SessionManager {options} = do
  now <- options.clock
  pure Load {got = Nothing, time = now}

loadSessionMaybe
  :: Monad m => SessionManager m -> Maybe SessionKey -> m (Load Session)
loadSessionMaybe sm = maybe (loadNothing sm) (loadSession sm)
