module Yesod.Session.Persist.Test.Gen.Mock
  ( MockInit (..)
  , genMockInit
  , TimeoutGenOptions
  , requireSomeTimeLimit
  , noTimeoutResolution
  )
where

import Yesod.Session.Persist.Prelude

import Test.QuickCheck (Arbitrary (arbitrary), Gen)
import Test.QuickCheck.Gen qualified as Gen
import Yesod.Session.Persist
import Yesod.Session.Persist.Test.Gen.General
import Yesod.Session.Persist.Time

data MockInit = MockInit
  { randomSeed :: Int
  , time :: UTCTime
  , timing :: TimingOptions NominalDiffTime
  }
  deriving stock (Eq, Show)

genMockInit :: (TimeoutGenOptions -> TimeoutGenOptions) -> Gen MockInit
genMockInit timeoutOptions = do
  randomSeed <- arbitrary
  time <-
    chooseTime
      (UTCTime (fromOrdinalDate 1950 0) 0, UTCTime (fromOrdinalDate 2050 0) 0)
  timing <- genTimingOptions (timeoutOptions defaultTimeoutGenOptions)
  pure MockInit {..}

data TimeoutGenOptions = TimeoutGenOptions
  { requireSomeLimit :: Bool
  , generateResolution :: Maybe Bool
  }

defaultTimeoutGenOptions :: TimeoutGenOptions
defaultTimeoutGenOptions =
  TimeoutGenOptions
    { requireSomeLimit = False
    , generateResolution = Nothing
    }

-- | Ensure that the generated settings have at least and idle or an absolute timeout
--
-- Use this for tests that require an expired session, since without timeout
-- settings no session can expire.
requireSomeTimeLimit :: TimeoutGenOptions -> TimeoutGenOptions
requireSomeTimeLimit x = x {requireSomeLimit = True}

-- | Ensure that the generate settings do not have a timeout resolution
--
-- This disables the optimization that prevents a session from being saved to the
-- database when the only change is a small increment in the access time.
-- Use this when the optimization would overcomplicate a test's assertion that it
-- performs an update to an existing session.
noTimeoutResolution :: TimeoutGenOptions -> TimeoutGenOptions
noTimeoutResolution x = x {generateResolution = Just False}

genTimingOptions :: TimeoutGenOptions -> Gen (TimingOptions NominalDiffTime)
genTimingOptions x = do
  timeout <- do
    (requireIdle, requireAbsolute) <-
      if x.requireSomeLimit
        then Gen.elements [(False, True), (True, False)]
        else pure (False, False)
    idle <-
      (if requireIdle then fmap Just else genMaybe)
        $ chooseNominalDiffTime (secondsToNominalDiffTime 60, nominalDay)
    absolute <-
      (if requireAbsolute then fmap Just else genMaybe)
        $ maybe id (+) idle -- Absolute timeout should be greater than idle timeout
        <$> chooseNominalDiffTime (secondsToNominalDiffTime 60, nominalDay)
    pure Timeout {idle, absolute}
  let resolutionGenerator = case timeout.idle <|> timeout.absolute of
        Just t ->
          -- Resolution should be a fraction of the smaller of the timeout limits
          chooseNominalDiffTime (t / 10, t / 2)
        Nothing ->
          chooseNominalDiffTime (secondsToNominalDiffTime 2, secondsToNominalDiffTime 60)
  resolution <- case x.generateResolution of
    Nothing -> genMaybe resolutionGenerator
    Just True -> Just <$> resolutionGenerator
    Just False -> pure Nothing
  pure TimingOptions {timeout, resolution}
