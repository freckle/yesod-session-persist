module Yesod.Session.SessionType
  ( Session (..)
  )
where

import Internal.Prelude

import Time
import Yesod.Core (SessionMap)
import Yesod.Session.Key
import Yesod.Session.Timing.Time

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
