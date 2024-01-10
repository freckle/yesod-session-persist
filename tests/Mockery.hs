module Mockery
  ( -- * Mock
    Mock (..)
  , newMock

    -- * Options
  , MockOptions (..)
  , defaultMockOptions
  , requireSomeTimeLimit
  , noTimeoutResolution

    -- * Sessions generation
  , createArbitrarySession
  , SessionGenOptions (..)
  , defaultSessionGenOptions
  , Liveness (..)
  , ExpirationReason (..)

    -- * Controlling time
  , pause

    -- * Transcript
  , takeTranscript

    -- * Randomization
  , newRandomization

    -- * Miscellany
  , repeat_
  ) where

import Yesod.Session.Persist.Prelude

import Yesod.Session.Persist
import Yesod.Session.Persist.SessionKey
import Yesod.Session.Persist.SessionManager
import Yesod.Session.Persist.Storage.Mock
import Yesod.Session.Persist.Storage.Operation

import Test.Hspec.Hedgehog

import Generators qualified as Gen
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Random qualified as Random

data Mock tx m = Mock
  { sessionManager :: SessionManager tx m
  , currentTime :: TVar UTCTime
  , mockStorage :: MockStorage m
  }

newtype MockOptions = MockOptions
  { timeout :: Gen.TimeoutGenOptions
  }

defaultMockOptions :: MockOptions
defaultMockOptions =
  MockOptions
    { timeout = Gen.defaultTimeoutGenOptions
    }

-- | Ensure that the generated settings have at least and idle or an absolute timeout
--
-- Use this for tests that require an expired session, since without timeout
-- settings no session can expire.
requireSomeTimeLimit :: MockOptions -> MockOptions
requireSomeTimeLimit MockOptions {timeout} =
  MockOptions {timeout = timeout {Gen.requireSomeLimit = True}}

-- | Ensure that the generate settings do not have a timeout resolution
--
-- This disables the optimization that prevents a session from being saved to the
-- database when the only change is a small increment in the access time.
-- Use this when the optimization would overcomplicate a test's assertion that it
-- performs an update to an existing session.
noTimeoutResolution :: MockOptions -> MockOptions
noTimeoutResolution MockOptions {timeout} =
  MockOptions {timeout = timeout {Gen.generateResolution = Just False}}

newMock :: MockOptions -> PropertyT IO (Mock STM (PropertyT IO))
newMock mockOptions = do
  seed <- forAll Gen.enumBounded
  let randomization = liftIO $ atomically $ newRandomization seed

  currentTime <- newTVarIO =<< forAll Gen.time
  let clock = readTVarIO currentTime

  mockStorage@MockStorage {storage} <-
    hoistMockStorage atomically <$> atomically newMockStorage

  timing <- forAll $ Gen.timingOptions mockOptions.timeout

  let options = defaultOptions {timing, clock, randomization}

  keyManager <- makeSessionKeyManager <$> randomization

  let sessionManager =
        SessionManager {keyManager, storage, options, runTransaction = atomically}

  let mock = Mock {sessionManager, currentTime, mockStorage}

  -- Let the tests all start out with some sessions in the storage
  repeat_ (Range.linear 0 5)
    $ createArbitrarySession mock defaultSessionGenOptions

  pure mock

newRandomization :: Int -> STM (Randomization STM)
newRandomization seed =
  deterministicallyRandomSTM
    $ let go g =
            DeterministicRandomization $ \n ->
              let (bs, g') = Random.genByteString (fromIntegral n) g
              in  (bs, go g')
      in  go $ Random.mkStdGen seed

-- | Advance time by some brief (possibly zero) amount
--
-- The time elapsed will be shorter than any timeout settings.
pause :: Mock STM (PropertyT IO) -> PropertyT IO ()
pause mock@Mock {sessionManager = SessionManager {options}} = do
  let
    timeout = options.timing.timeout
    upperBound =
      maybe (secondsToNominalDiffTime 10) (/ 2) (timeout.idle <|> timeout.absolute)
  amount <-
    forAll
      $ Gen.frequency
        [ (1,) $ pure 0
        , (10,) $ Gen.realFrac_ $ Range.linearFrac 0 upperBound
        ]
  atomically $ modifyTVar' mock.currentTime $ addUTCTime amount

newtype SessionGenOptions = SessionGenOptions
  { liveness :: Maybe Liveness
  }

defaultSessionGenOptions :: SessionGenOptions
defaultSessionGenOptions = SessionGenOptions {liveness = Nothing}

data Liveness = Live | Expired (Maybe ExpirationReason)

data ExpirationReason = IdleTimeout | AbsoluteTimeout
  deriving stock (Show)

createArbitrarySession
  :: Mock STM (PropertyT IO) -> SessionGenOptions -> PropertyT IO SessionKey
createArbitrarySession mock opt =
  let
    Mock {mockStorage, sessionManager} = mock
    SessionManager {storage, runTransaction, options} = sessionManager
  in
    offTheRecordIO mockStorage
      $ do
        key <- newSessionKey sessionManager
        sessionMap <- forAll Gen.sessionData
        now <- options.clock
        let timeout = options.timing.timeout
        timeGen <- case opt.liveness of
          Nothing -> pure $ whatever now
          Just Live -> pure $ live timeout now
          Just (Expired reasonMaybe) -> do
            reason <- case reasonMaybe of
              Nothing ->
                case (nonEmpty . catMaybes)
                  [ timeout.idle $> IdleTimeout
                  , timeout.absolute $> AbsoluteTimeout
                  ] of
                  Just xs -> forAll $ Gen.element $ toList xs
                  Nothing ->
                    fail
                      "Cannot generate an expired session for a configuration with no timeout limits"
              Just x -> pure x
            case reason of
              IdleTimeout ->
                maybe
                  ( fail
                      "Cannot generate an expired-by-idle-timeout session for a configuration with no idle timeout limit"
                  )
                  pure
                  $ expiredViaIdleTimeout timeout now
              AbsoluteTimeout ->
                maybe
                  ( fail
                      "Cannot generate an expired-by-absolute-timeout session for a configuration with no absolute timeout limit"
                  )
                  pure
                  $ expiredViaAbsoluteTimeout timeout now
        time <- forAll timeGen

        let session = Session {key, map = sessionMap, time}
        runTransaction $ storage $ InsertSession session
        pure session.key

whatever :: UTCTime -> Gen (Time UTCTime)
whatever now = do
  created <- Gen.timeInRange (subtractUTCTime nominalDay now, now)
  accessed <- Gen.timeInRange (created, now)
  pure Time {accessed, created}

-- | Generates times for a session that is still live
live :: Timeout NominalDiffTime -> UTCTime -> Gen (Time UTCTime)
live timeout now = do
  accessed <-
    Gen.timeInRange
      ( subtractUTCTime
          ( case (timeout.idle, timeout.absolute) of
              (Just idleTimeout, _) -> pred idleTimeout
              (_, Just absoluteTimeout) -> pred absoluteTimeout
              _ -> nominalDay
          )
          now
      , now
      )
  created <-
    Gen.timeInRange
      ( case timeout.absolute of
          Just absoluteTimeout -> subtractUTCTime (pred absoluteTimeout) now
          Nothing -> subtractUTCTime nominalDay accessed
      , accessed
      )
  pure Time {accessed, created}

-- | Generates times for a session that is expired due to idle timeout
--   (returns 'Nothing' if there is no idle timeout)
expiredViaIdleTimeout
  :: Timeout NominalDiffTime -> UTCTime -> Maybe (Gen (Time UTCTime))
expiredViaIdleTimeout timeout now =
  timeout.idle <&> \idleTimeout -> do
    accessed <-
      Gen.timeInRange
        ( case timeout.absolute of
            Just absoluteTimeout -> subtractUTCTime absoluteTimeout now
            Nothing -> subtractUTCTime (idleTimeout + nominalDay) now
        , subtractUTCTime idleTimeout now
        )
    created <-
      Gen.timeInRange
        ( case timeout.absolute of
            Just absoluteTimeout -> subtractUTCTime absoluteTimeout now
            Nothing -> subtractUTCTime nominalDay accessed
        , accessed
        )
    pure Time {accessed, created}

-- | Generates times for a session that is expired due to absolute timeout
--   (returns 'Nothing' if there is no absolute timeout)
expiredViaAbsoluteTimeout
  :: Timeout NominalDiffTime -> UTCTime -> Maybe (Gen (Time UTCTime))
expiredViaAbsoluteTimeout timeout now =
  timeout.absolute <&> \absoluteTimeout -> do
    created <-
      let base = subtractUTCTime absoluteTimeout now
       in Gen.timeInRange (subtractUTCTime nominalDay base, base)
    accessed <-
      Gen.timeInRange
        ( case timeout.idle of
            Just idleTimeout -> subtractUTCTime idleTimeout now
            Nothing -> created
        , now
        )
    pure Time {accessed, created}

repeat_ :: Range Int -> PropertyT IO a -> PropertyT IO ()
repeat_ r a = do
  n <- forAll $ Gen.integral r
  replicateM_ n a
