module Yesod.Session.Persist.Test.Gen.Session
  ( SessionInit (..)
  , genSessionInit
  , SessionGenOptions
  , requireLive
  , requireExpired
  ) where

import Yesod.Session.Persist.Prelude

import Data.Map.Strict qualified as Map
import Test.QuickCheck (Gen)
import Test.QuickCheck.Gen qualified as Gen
import Yesod.Session.Persist
import Yesod.Session.Persist.Test.Gen.General
import Yesod.Session.Persist.Test.Gen.Mock
import Prelude (error)

data SessionInit = SessionInit
  { time :: Time UTCTime
  , map :: SessionMap
  }
  deriving stock (Eq, Show)

genSessionInit
  :: (SessionGenOptions -> SessionGenOptions) -> MockInit -> Gen SessionInit
genSessionInit fsgo mockInit = do
  let SessionGenOptions {liveness} = fsgo defaultSessionGenOptions
  map <- genSessionData
  let now = mockInit.time
  let timeout = mockInit.timing.timeout
  time <- case liveness of
    Nothing -> whatever now
    Just Live -> live timeout now
    Just (Expired reasonMaybe) -> do
      reason <- case reasonMaybe of
        Nothing ->
          case (nonEmpty . catMaybes)
            [ timeout.idle $> IdleTimeout
            , timeout.absolute $> AbsoluteTimeout
            ] of
            Just xs -> Gen.elements $ toList xs
            Nothing ->
              error
                "Cannot generate an expired session for a configuration \
                \with no timeout limits"
        Just x -> pure x
      case reason of
        IdleTimeout ->
          fromMaybe
            ( error
                "Cannot generate an expired-by-idle-timeout \
                \session for a configuration with no idle timeout limit"
            )
            $ expiredViaIdleTimeout timeout now
        AbsoluteTimeout ->
          fromMaybe
            ( error
                "Cannot generate an expired-by-absolute-timeout session \
                \for a configuration with no absolute timeout limit"
            )
            $ expiredViaAbsoluteTimeout timeout now

  pure SessionInit {..}

whatever :: UTCTime -> Gen (Time UTCTime)
whatever now = do
  created <- chooseTime (subtractUTCTime nominalDay now, now)
  accessed <- chooseTime (created, now)
  pure Time {accessed, created}

-- | Generates times for a session that is still live
live :: Timeout NominalDiffTime -> UTCTime -> Gen (Time UTCTime)
live timeout now = do
  accessed <-
    chooseTime
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
    chooseTime
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
      chooseTime
        ( case timeout.absolute of
            Just absoluteTimeout -> subtractUTCTime absoluteTimeout now
            Nothing -> subtractUTCTime (idleTimeout + nominalDay) now
        , subtractUTCTime idleTimeout now
        )
    created <-
      chooseTime
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
       in chooseTime (subtractUTCTime nominalDay base, base)
    accessed <-
      chooseTime
        ( case timeout.idle of
            Just idleTimeout -> subtractUTCTime idleTimeout now
            Nothing -> created
        , now
        )
    pure Time {accessed, created}

newtype SessionGenOptions = SessionGenOptions
  { liveness :: Maybe Liveness
  }

defaultSessionGenOptions :: SessionGenOptions
defaultSessionGenOptions = SessionGenOptions {liveness = Nothing}

requireLive :: SessionGenOptions -> SessionGenOptions
requireLive x = x {liveness = Just Live}

requireExpired :: SessionGenOptions -> SessionGenOptions
requireExpired x = x {liveness = Just (Expired Nothing)}

data Liveness = Live | Expired (Maybe ExpirationReason)

data ExpirationReason = IdleTimeout | AbsoluteTimeout
  deriving stock (Show)

genSessionData :: Gen (Map Text ByteString)
genSessionData = fmap Map.fromList $ do
  k <- Gen.choose (0, 5)
  Gen.vectorOf k
    $ (,)
    <$> Gen.elements ["", "a", "bc", "def", "ghij"]
    <*> Gen.elements ["", "a", "\0", "what"]
