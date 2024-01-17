module Yesod.Session.Persist.Time
  ( subtractUTCTime
  , module X
  ) where

import Yesod.Session.Persist.Prelude

import Data.Time as X
  ( NominalDiffTime
  , UTCTime (..)
  , addUTCTime
  , diffUTCTime
  , nominalDay
  , nominalDiffTimeToSeconds
  , secondsToDiffTime
  , secondsToNominalDiffTime
  )
import Data.Time.Calendar.OrdinalDate as X (fromOrdinalDate)
import Data.Time.Clock.System as X (systemEpochDay)

subtractUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
subtractUTCTime d t = addUTCTime (negate d) t
