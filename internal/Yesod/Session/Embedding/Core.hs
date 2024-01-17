module Yesod.Session.Embedding.Core
  ( Embedding (..)
  , embed
  , extract
  , extractIgnoringError
  , dimapEmbedding
  ) where

import Internal.Prelude

-- | Targets a value that is optionally present in some stateful monadic context
data Embedding (con :: (Type -> Type) -> Constraint) e a = Embedding
  { embed :: forall m. con m => Maybe a -> m ()
  -- ^ Sets or clears the value
  , extract :: forall m. (Functor m, con m) => m (Either e (Maybe a))
  -- ^ Removes the value if present, returning what was removed
  }

embed :: con m => Embedding con e a -> Maybe a -> m ()
embed Embedding {embed = x} = x

extract :: (Functor m, con m) => Embedding con e a -> m (Either e (Maybe a))
extract Embedding {extract = x} = x

extractIgnoringError :: (Functor m, con m) => Embedding con e a -> m (Maybe a)
extractIgnoringError e = extract e <&> fromRight Nothing

dimapEmbedding
  :: (a -> Either e b)
  -> (b -> a)
  -> Embedding con e a
  -> Embedding con e b
dimapEmbedding g f Embedding {embed = embed', extract = extract'} =
  Embedding
    { embed = embed' . fmap f
    , extract =
        extract' <&> \case
          Left e -> Left e
          Right Nothing -> Right Nothing
          Right (Just x) -> Just <$> g x
    }
