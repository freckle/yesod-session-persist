module Web.Session.Prelude.STM
  ( atomically
  , newTVarIO
  , readTVarIO
  , writeTVarIO
  , modifyTVarSTM
  )
where

import Web.Session.Prelude.Reexports

import Control.Concurrent.STM qualified as STM

atomically :: MonadIO m => STM a -> m a
atomically = liftIO . STM.atomically

newTVarIO :: MonadIO m => a -> m (TVar a)
newTVarIO = liftIO . STM.newTVarIO

readTVarIO :: MonadIO m => TVar a -> m a
readTVarIO = liftIO . STM.readTVarIO

writeTVarIO :: MonadIO m => TVar a -> a -> m ()
writeTVarIO ref x = liftIO $ atomically $ STM.writeTVar ref x

modifyTVarSTM :: TVar a -> (a -> STM a) -> STM ()
modifyTVarSTM ref f = readTVar ref >>= f >>= (writeTVar ref $!)
