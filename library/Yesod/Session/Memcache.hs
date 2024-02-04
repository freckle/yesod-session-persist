module Yesod.Session.Memcache
  ( -- * Setup
    makeSessionBackend
  , SessionConfiguration (..)

    -- * Options
  , Options (..)
  , defaultOptions
  , hoistOptions

    -- * Timing
  , TimingOptions (..)
  , defaultTimingOptions

    -- * Timeout
  , Timeout (..)
  , defaultTimeout

    -- * Transport security
  , TransportSecurity (..)

    -- * Session data model
  , Session (..)
  , SessionKey (..)
  , Time (..)

    -- * Randomization
  , Randomization (..)
  , defaultRandomization
  , deterministicallyRandom
  , DeterministicRandomization (..)

    -- * Storage
  , SessionPersistence (..)
  , StorageException (..)

    -- * Key rotation
  , rotateSessionKey
  , assignSessionKeyRotation
  , KeyRotation (..)

    -- * Freezing
  , disableSessionManagement
  , assignSessionFreeze
  , SessionFreeze (..)

    -- * Session map embedding
  , SessionEmbeddings (..)
  , HasSessionEmbeddings (..)
  , Embedding (..)
  , SessionMapEmbedding
  , MapOperations (..)
  , bsKeyEmbedding
  , dimapEmbedding
  , showReadKeyEmbedding

    -- * Comparison
  , Comparison (..)
  , differsOn
  ) where

import Yesod.Session.Memcache.Storage
import Yesod.Session.Memcache.Yesod
import Yesod.Session.Storage
