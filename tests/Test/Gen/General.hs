module Test.Gen.General
  ( chooseNominalDiffTime
  , chooseTime
  , chooseFixed
  , genMaybe
  , genVectorOfRange
  ) where

import Internal.Prelude

import Test.QuickCheck (Gen, choose)
import Test.QuickCheck.Gen qualified as Gen
import Time

chooseNominalDiffTime
  :: (NominalDiffTime, NominalDiffTime) -> Gen NominalDiffTime
chooseNominalDiffTime =
  fmap secondsToNominalDiffTime
    . chooseFixed
    . both nominalDiffTimeToSeconds

chooseFixed :: (Fixed a, Fixed a) -> Gen (Fixed a)
chooseFixed = fmap MkFixed . Gen.choose . both (\(MkFixed x) -> x)

both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

chooseTime :: (UTCTime, UTCTime) -> Gen UTCTime
chooseTime (a, b) =
  Gen.frequency
    [ (1,) $ Gen.elements [a, b]
    , (10,) $ do
        d <- chooseNominalDiffTime (0, diffUTCTime b a)
        Gen.elements [addUTCTime d a, addUTCTime (negate d) b]
    ]

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = Gen.oneof [pure Nothing, Just <$> g]

genVectorOfRange :: (Int, Int) -> Gen a -> Gen [a]
genVectorOfRange (a, b) g = do
  i <- choose (a, b)
  Gen.vectorOf i g
