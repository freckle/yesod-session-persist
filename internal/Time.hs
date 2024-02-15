module Time
  ( subtractUTCTime
  , minutes
  , hours
  , days
  , years
  , module X
  ) where

import Internal.Prelude

import Data.Time as X
  ( NominalDiffTime
  , UTCTime (..)
  , addUTCTime
  , diffUTCTime
  , getCurrentTime
  , nominalDay
  , nominalDiffTimeToSeconds
  , secondsToDiffTime
  , secondsToNominalDiffTime
  )
import Data.Time.Calendar.OrdinalDate as X (fromOrdinalDate)
import Data.Time.Clock.POSIX as X
  ( POSIXTime
  , posixSecondsToUTCTime
  , utcTimeToPOSIXSeconds
  )
import Data.Time.Clock.System as X (systemEpochDay)

subtractUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
subtractUTCTime d t = addUTCTime (negate d) t

minutes :: NominalDiffTime -> NominalDiffTime
minutes = (* 60)

hours :: NominalDiffTime -> NominalDiffTime
hours = (* 60) . minutes

days :: NominalDiffTime -> NominalDiffTime
days = (* 24) . hours

years :: NominalDiffTime -> NominalDiffTime
years = (* 365.2) . days
