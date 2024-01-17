module Test.MockStorage
  ( newMockStorage
  , MockStorage (..)
  , hoistMockStorage
  , offTheRecordIO
  , takeTranscript
  ) where

import Internal.Prelude

import Control.Concurrent.STM.TVar
  ( TVar
  , modifyTVar'
  , newTVar
  , readTVar
  , readTVarIO
  , writeTVar
  )
import Control.Monad.STM (STM, atomically)
import Data.Map.Strict qualified as Map
import Data.Sequence ((|>))
import Data.Sequence qualified as Seq
import Yesod.Session.Key
import Yesod.Session.SessionType
import Yesod.Session.Storage.Exceptions
import Yesod.Session.Storage.Operation

data MockStorage m = MockStorage
  { storage :: forall a. StorageOperation a -> STM a
  , runSTM :: forall a. STM a -> m a
  , transcript :: TVar (Seq StorageOperation')
  , getSessionKeys :: STM (Set SessionKey)
  , recordingVar :: TVar Bool
  }

hoistMockStorage :: (forall a. m a -> m' a) -> MockStorage m -> MockStorage m'
hoistMockStorage f MockStorage {..} =
  MockStorage {runSTM = f . runSTM, ..}

-- | Perform some action without modifying the transcript
offTheRecordIO :: MonadIO m => MockStorage m -> m a -> m a
offTheRecordIO mock action = do
  wasRecording <- liftIO $ readTVarIO mock.recordingVar
  liftIO $ atomically $ writeTVar mock.recordingVar False
  x <- action
  liftIO $ atomically $ writeTVar mock.recordingVar wasRecording
  pure x

takeTranscript :: MockStorage m -> m (Seq StorageOperation')
takeTranscript MockStorage {transcript, runSTM} =
  runSTM $ readTVar transcript <* writeTVar transcript Seq.empty

newMockStorage :: HasCallStack => STM (MockStorage STM)
newMockStorage = do
  transcript <- newTVar Seq.empty
  sessionsVar <- newTVar Map.empty
  recordingVar <- newTVar True

  pure
    MockStorage
      { transcript
      , recordingVar
      , getSessionKeys = Map.keysSet <$> readTVar sessionsVar
      , storage = \(op :: StorageOperation result) ->
          do
            readTVar recordingVar
              >>= (`when` modifyTVar' transcript (|> StorageOperation' op))
            handleOp sessionsVar op
      , runSTM = id
      }

handleOp
  :: HasCallStack
  => TVar (Map SessionKey Session)
  -> StorageOperation result
  -> STM result
handleOp ref = \case
  GetSession sessionKey -> readTVar ref <&> Map.lookup sessionKey
  DeleteSession sessionKey -> modifyTVar' ref $ Map.delete sessionKey
  InsertSession newSession -> do
    modifyTVarSTM ref
      $ Map.alterF
        ( maybe
            (pure $ Just newSession)
            ( \existingSession ->
                throwWithCallStack SessionAlreadyExists {existingSession, newSession}
            )
        )
        newSession.key
  ReplaceSession newSession ->
    modifyTVarSTM ref
      $ Map.alterF
        ( maybe
            (throwWithCallStack SessionDoesNotExist {newSession})
            (const $ pure $ Just newSession)
        )
        newSession.key

modifyTVarSTM :: TVar a -> (a -> STM a) -> STM ()
modifyTVarSTM ref f = readTVar ref >>= f >>= (writeTVar ref $!)
