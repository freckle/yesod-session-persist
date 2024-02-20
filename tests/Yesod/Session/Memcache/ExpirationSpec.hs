module Yesod.Session.Memcache.ExpirationSpec
  ( spec
  ) where

import Test.Prelude

import Session.Timing.Timeout (Timeout (..))
import Time (POSIXTime, getCurrentTime, posixSecondsToUTCTime)
import Yesod.Session.Memcache.Expiration
  ( MemcacheExpiration (..)
  , getCacheExpiration
  , minDiffTime
  , noExpiration
  )

spec :: Spec
spec =
  describe "getCacheExpiration" $ do
    context "when NoMemcacheExpiration" $ do
      it "returns noExpiration" $ property $ \timeout -> do
        actual <-
          getCacheExpiration NoMemcacheExpiration getCurrentTime
            $ fmap fromInteger timeout
        actual `shouldBe` noExpiration

    context "when UseMemcacheExpiration " $ do
      it "does not allow a 'Timeout' that would be interpreted as a duration" $ forAll tooSmallRange $ \seconds -> do
        let
          clock = pure $ posixSecondsToUTCTime beginningOfTime
          timeout = Timeout {idle = Nothing, absolute = Just (fromInteger seconds)}

        getCacheExpiration UseMemcacheExpiration clock timeout
          `shouldThrow` anyException

      it "does not allow a 'Timeout' that would overflow a 'Word32'" $ forAll tooLargeRange $ \seconds -> do
        let
          clock = pure $ posixSecondsToUTCTime beginningOfTime
          timeout = Timeout {idle = Nothing, absolute = Just (fromInteger seconds)}

        getCacheExpiration UseMemcacheExpiration clock timeout
          `shouldThrow` anyException

      it "allows minDiffTime" $ do
        let
          clock = pure @IO $ posixSecondsToUTCTime beginningOfTime
          minNominalDiffTime = fromInteger minDiffTime
          timeout = Timeout {idle = Nothing, absolute = Just minNominalDiffTime}

        actual <- getCacheExpiration UseMemcacheExpiration clock timeout
        fromInteger (toInteger actual) `shouldBe` minNominalDiffTime

      it "allows a 'Timeout' that is at the maxBoundary of a 'Word32'" $ do
        let
          clock = pure @IO $ posixSecondsToUTCTime beginningOfTime
          maxNominalDiffTime = fromInteger $ toInteger $ maxBound @Word32
          timeout = Timeout {idle = Nothing, absolute = Just maxNominalDiffTime}

        actual <- getCacheExpiration UseMemcacheExpiration clock timeout
        fromInteger (toInteger actual) `shouldBe` maxNominalDiffTime

      it "gets minimum of idle and absolute timeouts" $ forAll correctRange $ \idle -> forAll correctRange $ \absolute -> do
        let
          clock = pure @IO $ posixSecondsToUTCTime beginningOfTime
          timeout =
            Timeout {idle = Just (fromInteger idle), absolute = Just (fromInteger absolute)}

        actual <- getCacheExpiration UseMemcacheExpiration clock timeout
        fromInteger (toInteger actual) `shouldBe` min idle absolute

-- | Pretend it is currently 1970-01-01 00:00 UTC to make math easy.
beginningOfTime :: POSIXTime
beginningOfTime = 0

-- | Always generate "durations"
--
-- See: 'minDiffTime'.
tooSmallRange :: Gen Integer
tooSmallRange = chooseInteger (0, minDiffTime - 1)

correctRange :: Gen Integer
correctRange = chooseInteger (minDiffTime, toInteger maxWord32)

-- | Always generate values that overflow 'Word32'.
tooLargeRange :: Gen Integer
tooLargeRange = do
  randomInt <- getPositive <$> arbitrary
  pure $ randomInt + toInteger maxWord32

maxWord32 :: Word32
maxWord32 = maxBound @Word32
