{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module YesodApp where

import Web.Session.Prelude

import Mockery
import Web.Session
import Web.Session.SessionManager
import Web.Session.Yesod

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Generics (Generic)
import Yesod

newApp :: TimingOptions NominalDiffTime -> IO App
newApp timing = do
  mock <- newMock' timing
  pure App {mock}

newtype App = App
  { mock :: Mock STM IO
  }

-- Derive routes and instances for App.
mkYesod
  "App"
  [parseRoutes|
    / HomeR GET
    /ping PingR GET
    /user UserR GET
    /log-in LogInR POST
    /log-out LogOutR POST
  |]

instance HasSessionEmbeddings App where
  getSessionEmbeddings app =
    let
      App {mock} = app
      Mock {sessionManager} = mock
      SessionManager {options} = sessionManager
      Options {embedding} = options
    in
      embedding

instance Yesod App where
  makeSessionBackend App {mock} = do
    let Mock {sessionManager} = mock
    pure $ Just $ makeSessionBackend'' sessionManager

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getPingR :: Handler Text
getPingR = do
  disableSessionManagement
  pure "pong"

getUserR :: Handler Text
getUserR =
  maybe "-" (T.pack . show) . Map.lookup "user-id" <$> getSession

postLogInR :: Handler ()
postLogInR = do
  form :: LoginForm <- requireInsecureJsonBody
  setSession "user-id" form.uid
  rotateSessionKey

postLogOutR :: Handler ()
postLogOutR = do
  deleteSession "user-id"
  rotateSessionKey

newtype LoginForm = LoginForm
  { uid :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)
