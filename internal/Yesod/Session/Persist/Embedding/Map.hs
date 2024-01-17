module Yesod.Session.Persist.Embedding.Map
  ( SessionMapEmbedding
  , MapOperations (..)
  , bsKeyEmbedding
  , showReadKeyEmbedding
  ) where

import Yesod.Session.Persist.Prelude

import Control.Monad.State qualified as State
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Yesod.Core (HandlerFor, deleteSession, lookupSessionBS, setSessionBS)
import Yesod.Session.Persist.Embedding.Core

-- | Specifies how we represent some value within a 'SessionMap'
--
-- We use this to sort of abuse the session; key rotation and freezing are
-- done by embedding special values among the session data. These special
-- values are extracted from the map before persisting to storage and are
-- never actually saved.
type SessionMapEmbedding a = Embedding (MapOperations Text ByteString) () a

-- | A monadic context with operations over some 'Map'-like state
--
-- This allows us to generalize between pure operations over 'Map' and
-- the more limited session manipulation utilities afforded by Yesod.
-- (See the instance list for this class.)
class (Monad m, Ord k) => MapOperations k v m | m -> k v where
  lookup :: k -> m (Maybe v)
  assign :: k -> Maybe v -> m ()

instance MapOperations Text ByteString (HandlerFor site) where
  lookup k = lookupSessionBS k
  assign k v = maybe (deleteSession k) (setSessionBS k) v

instance (Monad m, Ord k) => MapOperations k v (StateT (Map k v) m) where
  lookup k = State.gets $ Map.lookup k
  assign k v = State.modify' $ Map.alter (const v) k

-- | An embedding which stores a value at some particular key in a map-like structure
bsKeyEmbedding :: k -> Embedding (MapOperations k a) e a
bsKeyEmbedding key =
  Embedding
    { embed = assign key
    , extract = fmap Right $ lookup key <* assign key Nothing
    }

-- | Represents a value in a 'SessionMap' by storing the
--   UTF-8 encoding of its 'show' representation at the given key
showReadKeyEmbedding
  :: (Read a, Show a) => k -> Embedding (MapOperations k ByteString) () a
showReadKeyEmbedding k =
  dimapEmbedding
    ( maybe (throwError ()) pure
        . readMaybe
        <=< (bimap (const ()) T.unpack . decodeUtf8')
    )
    (encodeUtf8 . T.pack . show)
    (bsKeyEmbedding k)
