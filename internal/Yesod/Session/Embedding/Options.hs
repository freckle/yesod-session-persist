module Yesod.Session.Embedding.Options
  ( SessionEmbeddings (..)
  , HasSessionEmbeddings (..)
  ) where

import Internal.Prelude

import Session.Freeze
import Session.KeyRotation
import Yesod.Session.Embedding.Map

data SessionEmbeddings = SessionEmbeddings
  { keyRotation :: SessionMapEmbedding KeyRotation
  -- ^ How to represent a key rotation instruction in the session data;
  --   see 'Yesod.Session.Persist.assignSessionKeyRotation'
  , freeze :: SessionMapEmbedding SessionFreeze
  -- ^ How to represent a freeze instruction in the session data;
  --   see 'Yesod.Session.Persist.assignSessionFreeze'
  }

class HasSessionEmbeddings a where
  getSessionEmbeddings :: a -> Maybe SessionEmbeddings

instance HasSessionEmbeddings SessionEmbeddings where
  getSessionEmbeddings x = pure x
