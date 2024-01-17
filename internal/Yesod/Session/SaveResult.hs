module Yesod.Session.SaveResult
  ( SaveResult (..)
  ) where

import Internal.Prelude

data SaveResult a
  = -- | Nothing was done because a session freeze was requested
    Frozen
  | -- | There were no changes worth saving.
    NoChange
  | -- | A session was saved (either a new or existing session key).
    Saved a
  | -- | A session was deleted, and no new session was inserted.
    Deleted
  deriving stock (Eq, Ord, Show)
