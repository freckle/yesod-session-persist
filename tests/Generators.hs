module Generators
  ( -- * Times
    time
  , timeInRange

    -- * Timeout options
  , timingOptions
  , TimeoutGenOptions (..)
  , defaultTimeoutGenOptions

    -- * Session data
  , sessionData
  ) where

import Yesod.Session.Persist.Prelude

import Yesod.Session.Persist

import Test.Hspec.Hedgehog

import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

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

timingOptions :: TimeoutGenOptions -> Gen (TimingOptions NominalDiffTime)
timingOptions x = do
  timeout <- do
    (requireIdle, requireAbsolute) <-
      if x.requireSomeLimit
        then Gen.element [(False, True), (True, False)]
        else pure (False, False)
    idle <-
      (if requireIdle then fmap Just else Gen.maybe)
        $ Gen.realFrac_
        $ Range.linearFrac (secondsToNominalDiffTime 60) nominalDay
    absolute <-
      (if requireAbsolute then fmap Just else Gen.maybe)
        $ maybe id (+) idle -- Absolute timeout should be greater than idle timeout
        <$> Gen.realFrac_ (Range.linearFrac (secondsToNominalDiffTime 60) nominalDay)
    pure Timeout {idle, absolute}
  let resolutionGenerator = case timeout.idle <|> timeout.absolute of
        Just t ->
          -- Resolution should be a fraction of the smaller of the timeout limits
          (t *) <$> Gen.realFrac_ (Range.linearFrac (1 / 2) (1 / 10))
        Nothing ->
          Gen.realFrac_
            $ Range.linearFrac
              (secondsToNominalDiffTime 2)
              (secondsToNominalDiffTime 60)
  resolution <- case x.generateResolution of
    Nothing -> Gen.maybe resolutionGenerator
    Just True -> Just <$> resolutionGenerator
    Just False -> pure Nothing
  pure TimingOptions {timeout, resolution}

time :: Gen UTCTime
time = timeInRange (start, end)
 where
  start = UTCTime (fromOrdinalDate 1950 0) 0
  end = UTCTime (fromOrdinalDate 2050 0) 0

-- | Generates a time between inclusive endpoints, shrinking toward both endpoints.
timeInRange :: (UTCTime, UTCTime) -> Gen UTCTime
timeInRange (start, end) =
  Gen.frequency
    [ (1,) $ Gen.element [start, end]
    , (10,) $ do
        d <- Gen.realFrac_ $ Range.linearFrac 0 $ diffUTCTime end start
        Gen.element [addUTCTime d start, addUTCTime (negate d) end]
    ]

sessionData :: Gen (Map Text ByteString)
sessionData =
  Gen.map (Range.linear 0 5)
    $ (,)
    <$> Gen.element ["", "a", "bc", "def", "ghij"]
    <*> Gen.element ["", "a", "\0", "what"]
