module Yesod.Session.Memcache.StorageSpec
  ( spec
  ) where

import Test.Prelude

import Time (UTCTime, posixSecondsToUTCTime,)
import Yesod.Session.Memcache.Storage (getMemcacheExpiration)
import Session.Timing.Time (Time(..))
import Session.Timing.Timeout (defaultTimeout, Timeout(Timeout, idle, absolute))
import Yesod.Session.Memcache.Expiration (MemcacheExpiration(NoMemcacheExpiration, UseMemcacheExpiration), maxTimestamp, minTimestamp)
import Yesod.Session.Memcache.Expiration (noExpiration)
--

spec :: Spec
spec =
  describe "getMemcacheExpiration" $ do

    context "NoMemcacheExpiration" $ do
      it "is 'noExpiration'" $ do
        let time = Time unixEpoch unixEpoch
        getMemcacheExpiration NoMemcacheExpiration defaultTimeout time `shouldBe` Just noExpiration

    context "UseMemcacheExpiration" $ do
      let useMemcacheOption = UseMemcacheExpiration

      it "is 'noExpiration' when timeouts are not provided" $ do
        let time = Time unixEpoch unixEpoch
            timeout = Timeout { idle = Nothing, absolute = Nothing }
        getMemcacheExpiration useMemcacheOption timeout time `shouldBe` Just noExpiration

      describe "it does NOT catch failures from 'fromUTC'" $ do
        it "fails when 'fromUTC' fails on 'tooLarge'" $ do
          let time = Time unixEpoch unixEpoch
              tooBig = succ maxTimestamp
              timeout = Timeout { idle = Nothing, absolute = (Just tooBig) }
          getMemcacheExpiration useMemcacheOption timeout time `shouldBe` Nothing

        it "fails when 'fromUTC' fails on 'tooSmall'" $ do
          let time = Time unixEpoch unixEpoch
              tooSmall = pred minTimestamp
              timeout = Timeout { idle = Nothing, absolute = (Just tooSmall) }
          getMemcacheExpiration useMemcacheOption timeout time `shouldBe` Nothing

        it "accepts 'minTimestamp'" $ do
          let time = Time unixEpoch unixEpoch
              timeout = Timeout { idle = Nothing, absolute = (Just minTimestamp) }
          getMemcacheExpiration useMemcacheOption timeout time `shouldBe` Just minTimestamp

        it "accepts 'maxTimestamp'" $ do
          let time = Time unixEpoch unixEpoch
              timeout = Timeout { idle = Nothing, absolute = (Just maxTimestamp ) }
          getMemcacheExpiration useMemcacheOption timeout time `shouldBe` Just maxTimestamp

        it "accepts something between 'minTimestamp' and 'maxTimestamp'" $ do
          let time = Time unixEpoch unixEpoch
              timeout = Timeout { idle = Nothing, absolute = (Just (pred maxTimestamp) ) }
          getMemcacheExpiration useMemcacheOption timeout time `shouldBe` Just (pred maxTimestamp)


-- The Unix Epoc: 1970-01-01 00:00:00 UTC
--
-- Makes time math easier to reason about.
unixEpoch :: UTCTime
unixEpoch = posixSecondsToUTCTime 0
