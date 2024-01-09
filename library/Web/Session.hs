module Web.Session
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
  , deterministicallyRandomIO
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
  ) where

import Web.Session.Freeze.Action
import Web.Session.Freeze.Type
import Web.Session.KeyRotation.Action
import Web.Session.KeyRotation.Type
import Web.Session.MapEmbedding
import Web.Session.Options
import Web.Session.Prelude
import Web.Session.Session
import Web.Session.SessionKey
import Web.Session.Storage.Exceptions
import Web.Session.Storage.Persistent
import Web.Session.Timing.Options
import Web.Session.Timing.Time
import Web.Session.Timing.Timeout
import Web.Session.TransportSecurity
import Web.Session.Yesod
