{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Yesod.Session.Persist.YesodApp where

import Test.Prelude

import Control.Monad.STM (STM)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time qualified as Time
import Time
import Yesod
  ( FromJSON
  , Html
  , RenderRoute (renderRoute)
  , Yesod (defaultLayout, makeSessionBackend)
  , deleteSession
  , getSession
  , mkYesod
  , parseRoutes
  , requireInsecureJsonBody
  , setSession
  , whamlet
  )
import Yesod.Core (SessionMap)

newApp :: TimingOptions NominalDiffTime -> IO App
newApp timing = do
  -- We have limited ability to mock time in a Yesod test, because
  -- the cookie manager will expire cookies based on the real time.
  time <- Time.getCurrentTime

  randomSeed <- generate arbitrary
  let mockInit = MockInit {randomSeed, time, timing}
  mock <- newMock (\x -> x {keyRotationTrigger}) mockInit
  pure App {mock}

keyRotationTrigger :: Comparison SessionMap -> Maybe KeyRotation
keyRotationTrigger x = do
  guard $ differsOn (Map.lookup "user-id") x
  Just RotateSessionKey

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
    /rotate RotateR GET
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

postLogOutR :: Handler ()
postLogOutR = deleteSession "user-id"

newtype LoginForm = LoginForm
  { uid :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

getRotateR :: Handler ()
getRotateR = rotateSessionKey
