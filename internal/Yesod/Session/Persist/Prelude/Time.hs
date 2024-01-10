module Yesod.Session.Persist.Prelude.Time
  ( subtractUTCTime
  ) where

import Yesod.Session.Persist.Prelude.Reexports

subtractUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
subtractUTCTime d t = addUTCTime (negate d) t
