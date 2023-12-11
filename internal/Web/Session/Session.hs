module Web.Session.Session
  ( Session (..)
  )
where

import Web.Session.Prelude

import Web.Session.SessionKey
import Web.Session.Timing.Time

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
