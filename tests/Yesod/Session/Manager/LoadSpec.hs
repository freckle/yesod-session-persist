module Yesod.Session.Manager.LoadSpec
  ( spec
  ) where

import Test.Prelude

import Data.Sequence qualified as Seq
import Yesod.Session.Manager
import Yesod.Session.Manager.Load
import Yesod.Session.SessionType

spec :: Spec
spec = context "Session loading" $ do
  specify "may load a session"
    $ forAll (genMockInit id)
    $ \mockInit ->
      forAll (genSessionInit requireLive mockInit) $ \sessionInit ->
        forAll (genVectorOfRange (0, 5) $ genSessionInit id mockInit)
          $ \otherSessionInits ->
            ioProperty $ do
              mock@Mock {sessionManager} <- newMock id mockInit
              traverse_ (createArbitrarySession mock) otherSessionInits
              sessionKey <- createArbitrarySession mock sessionInit
              load <- loadSession sessionManager sessionKey
              pure $ counterexample (show load) $ didSessionLoad load

  context "may load nothing" $ do
    specify "when there is no session key"
      $ forAll (genMockInit id)
      $ \mockInit ->
        forAll (genVectorOfRange (0, 5) $ genSessionInit id mockInit)
          $ \sessionInits -> ioProperty $ do
            mock@Mock {sessionManager} <- newMock id mockInit
            traverse_ (createArbitrarySession mock) sessionInits
            load :: Load Session <- loadNothing sessionManager
            transcript <- takeTranscript mock.mockStorage
            pure
              $ counterexample (show load) (not $ didSessionLoad load)
              .&&. counterexample (show transcript) (transcript == Seq.empty)

    specify "when the key is not in storage"
      $ forAll (genMockInit id)
      $ \mockInit ->
        forAll (genVectorOfRange (0, 5) $ genSessionInit id mockInit)
          $ \sessionInits -> ioProperty $ do
            mock@Mock {sessionManager} <- newMock id mockInit
            traverse_ (createArbitrarySession mock) sessionInits
            sessionKey <- newSessionKey sessionManager
            load <- loadSession sessionManager sessionKey
            pure $ counterexample (show load) (not $ didSessionLoad load)

    specify "when the session is expired"
      $ forAll (genMockInit requireSomeTimeLimit)
      $ \mockInit ->
        forAll (genSessionInit requireExpired mockInit)
          $ \sessionInit -> ioProperty $ do
            mock@Mock {sessionManager} <- newMock id mockInit
            sessionKey <- createArbitrarySession mock sessionInit
            load <- loadSession sessionManager sessionKey
            pure $ counterexample (show load) (not $ didSessionLoad load)
