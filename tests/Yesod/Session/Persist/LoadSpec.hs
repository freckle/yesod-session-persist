module Yesod.Session.Persist.LoadSpec
  ( spec
  ) where

import TestPrelude

import Yesod.Session.Persist.Load
import Yesod.Session.Persist.SessionManager

import Data.Sequence qualified as Seq

spec :: Spec
spec = context "Session loading" $ do
  specify "may load a session" $ hedgehog $ do
    mock@Mock {sessionManager} <- newMock defaultMockOptions
    let genOptions = defaultSessionGenOptions {liveness = Just Live}
    sessionKey <- createArbitrarySession mock genOptions
    load <- loadSession sessionManager sessionKey
    assert $ didSessionLoad load

  context "may load nothing" $ do
    specify "when there is no session key" $ hedgehog $ do
      mock@Mock {sessionManager} <- newMock defaultMockOptions
      load <- loadNothing sessionManager
      assert $ not $ didSessionLoad load
      takeTranscript mock.mockStorage >>= (=== Seq.empty)

    specify "when the key is not in storage" $ hedgehog $ do
      Mock {sessionManager} <- newMock defaultMockOptions
      sessionKey <- newSessionKey sessionManager
      load <- loadSession sessionManager sessionKey
      assert $ not $ didSessionLoad load

    specify "when the session is expired" $ hedgehog $ do
      mock@Mock {sessionManager} <-
        newMock $ defaultMockOptions & requireSomeTimeLimit
      let genOptions = defaultSessionGenOptions {liveness = Just $ Expired Nothing}
      sessionKey <- createArbitrarySession mock genOptions
      load <- loadSession sessionManager sessionKey
      assert $ not $ didSessionLoad load
