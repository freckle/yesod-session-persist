module Yesod.Session.Persist.Session
  ( Session (..)
  )
where

import Yesod.Session.Persist.Prelude

import Yesod.Session.Persist.SessionKey
import Yesod.Session.Persist.Timing.Time

-- | What a saved session looks like in the database
data Session = Session
  { key :: SessionKey
  -- ^ Session key (primary key)
  , map :: SessionMap
  -- ^ Arbitrary session data
  , time :: Time UTCTime
  -- ^ Creation and access times, used to determine expiration
  }
  deriving stock (Eq, Show)
