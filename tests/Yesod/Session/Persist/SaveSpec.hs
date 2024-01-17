module Yesod.Session.Persist.SaveSpec
  ( spec
  ) where

import Test.Prelude

import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Monad.State (execState)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Yesod.Core (SessionMap)
import Yesod.Session.Embedding.Core

spec :: Spec
spec = context "saveSession" $ do
  specify "doesn't unnecessarily create a session"
    $ forAll (genMockInit id)
    $ \mockInit -> ioProperty $ do
      mock@Mock {sessionManager} <- newMock id mockInit
      load <- loadNothing sessionManager
      save <- saveSession sessionManager load $ loadedData load
      transcript <- takeTranscript mock.mockStorage
      pure
        $ counterexample (show save) (save === NoChange)
        .&&. counterexample (show transcript) (transcript == Seq.empty)

  specify "may create a session"
    $ forAll (genMockInit id)
    $ \mockInit -> ioProperty $ do
      mock@Mock {sessionManager} <- newMock id mockInit
      now <- readTVarIO mock.currentTime
      load <- loadNothing sessionManager
      let newData = loadedData load & Map.insert "a" "b"
      session <- assertSaved =<< saveSession sessionManager load newData
      transcript <- takeTranscript mock.mockStorage
      pure
        $ counterexample
          (show session)
          ( (session.map == newData)
              .&&. (session.time.created == now)
              .&&. (session.time.accessed == now)
          )
        .&&. counterexample
          (show transcript)
          (transcript == Seq.fromList [StorageOperation' $ InsertSession session])

  specify "may update a loaded session"
    $ forAll (genMockInit noTimeoutResolution)
    $ \mockInit -> ioProperty $ do
      mock@Mock {sessionManager} <- newMock id mockInit
      time1 <- readTVarIO mock.currentTime
      session1 <- do
        load <- loadNothing sessionManager
        let newData = loadedData load & Map.insert "a" "b"
        assertSaved =<< saveSession sessionManager load newData
      advanceTimeBriefly mock
      time2 <- readTVarIO mock.currentTime
      void $ takeTranscript mock.mockStorage
      load <- loadSession sessionManager session1.key
      let newData = loadedData load & Map.insert "c" "d"
      session2 <- assertSaved =<< saveSession sessionManager load newData
      transcript <- takeTranscript mock.mockStorage
      pure
        $ counterexample
          (show session2)
          ( (session2.map == Map.fromList [("a", "b"), ("c", "d")])
              .&&. (session2.time.created == time1)
              .&&. (session2.time.accessed == time2)
          )
        .&&. counterexample
          (show transcript)
          ( transcript
              == Seq.fromList
                [ StorageOperation' $ GetSession session2.key
                , StorageOperation' $ ReplaceSession session2
                ]
          )

  specify "changes the session key when we rotate"
    $ forAll (genMockInit noTimeoutResolution)
    $ \mockInit -> ioProperty $ do
      mock@Mock {sessionManager} <- newMock id mockInit
      let SessionManager {options} = sessionManager
      session1 <- do
        load <- loadNothing sessionManager
        let newData = loadedData load & Map.insert "a" "b"
        assertSaved =<< saveSession sessionManager load newData
      advanceTimeBriefly mock
      (session2, transcript) <- do
        load <- loadSession sessionManager session1.key
        void $ takeTranscript mock.mockStorage
        let newData =
              loadedData load
                & setSessionKeyRotation options (Just RotateSessionKey)
                & Map.insert "c" "d"
        session <- assertSaved =<< saveSession sessionManager load newData
        transcript <- takeTranscript mock.mockStorage
        pure (session, transcript)
      loadForOldSession <- loadSession sessionManager session1.key
      loadForNewSession <- loadSession sessionManager session2.key
      pure
        $ counterexample (show (session1, session2)) (session1.key /= session2.key)
        .&&. counterexample (show loadForOldSession) (not $ didSessionLoad loadForOldSession)
        .&&. counterexample
          (show loadForNewSession)
          (loadedData loadForNewSession == Map.fromList [("a", "b"), ("c", "d")])
        .&&. counterexample
          (show transcript)
          ( transcript
              == Seq.fromList
                [ StorageOperation' $ DeleteSession session1.key
                , StorageOperation' $ InsertSession session2
                ]
          )

setSessionKeyRotation
  :: Options tx m -> Maybe KeyRotation -> SessionMap -> SessionMap
setSessionKeyRotation options =
  execState . embed options.embedding.keyRotation

assertSaved :: Show a => Save a -> IO a
assertSaved = \case
  Saved x -> pure x
  x -> fail $ "Expected Saved, but got: " <> show x
