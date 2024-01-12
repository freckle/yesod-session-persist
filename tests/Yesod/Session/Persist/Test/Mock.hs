module Yesod.Session.Persist.Test.Mock
  ( Mock (..)
  , newMock
  , createArbitrarySession
  , advanceTime
  , advanceTimeBriefly
  ) where

import Yesod.Session.Persist.Prelude

import Control.Concurrent.STM.TVar
  ( TVar
  , modifyTVar'
  , newTVarIO
  , readTVarIO
  )
import Control.Monad.STM (STM, atomically)
import Yesod.Session.Persist
import Yesod.Session.Persist.SessionKey
import Yesod.Session.Persist.SessionManager
import Yesod.Session.Persist.Storage.Operation
import Yesod.Session.Persist.Test.Gen.Mock
import Yesod.Session.Persist.Test.Gen.Session
import Yesod.Session.Persist.Test.MockStorage
import Yesod.Session.Persist.Test.Randomization

data Mock tx m = Mock
  { sessionManager :: SessionManager tx m
  , currentTime :: TVar UTCTime
  , mockStorage :: MockStorage m
  }

newMock :: (Options STM IO -> Options STM IO) -> MockInit -> IO (Mock STM IO)
newMock opt MockInit {randomSeed, time, timing} = do
  let randomization = liftIO $ atomically $ newRandomization randomSeed
  currentTime <- newTVarIO time
  let clock = readTVarIO currentTime
  mockStorage@MockStorage {storage} <-
    hoistMockStorage atomically <$> atomically newMockStorage
  let options = opt defaultOptions {timing, clock, randomization}
  keyManager <- makeSessionKeyManager <$> randomization
  let sessionManager =
        SessionManager {keyManager, storage, options, runTransaction = atomically}
  pure Mock {sessionManager, currentTime, mockStorage}

createArbitrarySession :: Mock STM IO -> SessionInit -> IO SessionKey
createArbitrarySession mock sessionInit =
  let
    Mock {mockStorage, sessionManager} = mock
    SessionManager {storage, runTransaction} = sessionManager
  in
    offTheRecordIO mockStorage
      $ do
        key <- newSessionKey sessionManager
        let session =
              Session
                { key
                , map = sessionInit.map
                , time = sessionInit.time
                }
        runTransaction $ storage $ InsertSession session
        pure session.key

advanceTime :: MonadIO m => NominalDiffTime -> Mock STM IO -> m ()
advanceTime amount mock = do
  liftIO $ atomically $ modifyTVar' mock.currentTime $ addUTCTime amount

advanceTimeBriefly :: MonadIO m => Mock STM IO -> m ()
advanceTimeBriefly mock =
  let
    timeout = mock.sessionManager.options.timing.timeout

    change = case timeout.idle <|> timeout.absolute of
      Nothing -> secondsToNominalDiffTime 1
      Just upperBound -> upperBound / 10
  in
    advanceTime change mock
