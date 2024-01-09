module Web.Session.SaveSpec
  ( spec
  ) where

import TestPrelude

import Web.Session.Load
import Web.Session.Save
import Web.Session.SessionManager

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq

spec :: Spec
spec = context "saveSession" $ do
  specify "doesn't unnecessarily create a session" $ hedgehog $ do
    mock <- newMock defaultMockOptions
    load <- loadNothing mock.sessionManager
    save <- saveSession mock.sessionManager load $ loadedData load
    save === NoChange
    takeTranscript mock.mockStorage >>= (=== Seq.empty)

  specify "may create a session" $ hedgehog $ do
    mock <- newMock defaultMockOptions
    now <- readTVarIO mock.currentTime
    load <- loadNothing mock.sessionManager
    let newData = loadedData load & Map.insert "a" "b"
    save <- saveSession mock.sessionManager load newData
    annotateShow save
    session <- assertSaved save
    session.map === newData
    session.time.created === now
    session.time.accessed === now
    transcript <- takeTranscript mock.mockStorage
    transcript === Seq.fromList [StorageOperation' $ InsertSession session]

  specify "may update a loaded session" $ hedgehog $ do
    mock <- newMock $ defaultMockOptions & noTimeoutResolution
    time1 <- readTVarIO mock.currentTime
    sessionKey <- do
      load <- loadNothing mock.sessionManager
      let newData = loadedData load & Map.insert "a" "b"
      save <- saveSession mock.sessionManager load newData
      session <- assertSaved save
      pure session.key
    pause mock
    time2 <- readTVarIO mock.currentTime
    void $ takeTranscript mock.mockStorage
    load <- loadSession mock.sessionManager sessionKey
    let newData = loadedData load & Map.insert "c" "d"
    save <- saveSession mock.sessionManager load newData
    annotateShow save
    session <- assertSaved save
    session.map === Map.fromList [("a", "b"), ("c", "d")]
    session.time.created === time1
    session.time.accessed === time2
    transcript <- takeTranscript mock.mockStorage
    transcript
      === Seq.fromList
        [ StorageOperation' $ GetSession session.key
        , StorageOperation' $ ReplaceSession session
        ]

  specify "changes the session key when we rotate" $ hedgehog $ do
    mock <- newMock $ defaultMockOptions & noTimeoutResolution
    sessionKey1 <- do
      load <- loadNothing mock.sessionManager
      let newData = loadedData load & Map.insert "a" "b"
      save <- saveSession mock.sessionManager load newData
      session <- assertSaved save
      pure session.key
    pause mock
    sessionKey2 <- do
      load <- loadSession mock.sessionManager sessionKey1
      void $ takeTranscript mock.mockStorage
      let sessionMap2 =
            loadedData load
              & setSessionKeyRotation
                mock.sessionManager.options
                (Just RotateSessionKey)
              & Map.insert "c" "d"
      save <- saveSession mock.sessionManager load sessionMap2
      annotateShow save
      session <- assertSaved save
      transcript <- takeTranscript mock.mockStorage
      transcript
        === Seq.fromList
          [ StorageOperation' $ DeleteSession sessionKey1
          , StorageOperation' $ InsertSession session
          ]
      pure session.key
    sessionKey1 /== sessionKey2
    do
      load <- loadSession mock.sessionManager sessionKey1
      didSessionLoad load === False
    load <- loadSession mock.sessionManager sessionKey2
    loadedData load === Map.fromList [("a", "b"), ("c", "d")]

setSessionKeyRotation
  :: Options m -> Maybe KeyRotation -> SessionMap -> SessionMap
setSessionKeyRotation options =
  execState . embed options.keyRotationEmbedding

assertSaved :: Show a => Save a -> PropertyT IO a
assertSaved = \case
  Saved x -> pure x
  x -> do
    annotateShow x
    failure
