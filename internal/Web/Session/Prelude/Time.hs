module Web.Session.Prelude.Time
  ( subtractUTCTime
  ) where

import Web.Session.Prelude.Reexports

subtractUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
subtractUTCTime d t = addUTCTime (negate d) t
