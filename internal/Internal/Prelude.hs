module Internal.Prelude
  ( module X
  )
where

import Control.Applicative as X (Applicative (..), empty, (<|>))
import Control.Category as X ((>>>))
import Control.Exception as X (Exception, SomeException (..))
import Control.Exception.Annotated as X
  ( checkpointCallStack
  , throwWithCallStack
  )
import Control.Monad as X
  ( Monad (..)
  , guard
  , join
  , replicateM
  , replicateM_
  , unless
  , when
  , (<=<)
  , (=<<)
  , (>=>)
  )
import Control.Monad.Catch as X (MonadThrow, throwM)
import Control.Monad.Except as X (MonadError, throwError)
import Control.Monad.Fail as X (fail)
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Control.Monad.Reader as X (ReaderT)
import Control.Monad.Trans as X (MonadTrans (lift))
import Control.Monad.Trans.Identity as X (IdentityT (..))
import Data.Bifunctor as X (bimap)
import Data.Bool as X
import Data.ByteString as X (ByteString)
import Data.Char as X (Char)
import Data.Either as X (Either (..), either, fromRight)
import Data.Eq as X (Eq, (/=), (==))
import Data.Fixed as X (Fixed (MkFixed))
import Data.Foldable as X (all, foldMap, for_, toList, traverse_)
import Data.Function as X (const, flip, id, ($), (&), (.))
import Data.Functor as X (Functor, fmap, void, ($>), (<$>), (<&>))
import Data.Kind as X (Constraint, Type)
import Data.List as X (concat, concatMap, filter)
import Data.List.NonEmpty as X (nonEmpty)
import Data.Map as X (Map)
import Data.Maybe as X
  ( Maybe (..)
  , catMaybes
  , fromMaybe
  , isJust
  , isNothing
  , maybe
  )
import Data.Ord as X (Ord (..))
import Data.Semigroup as X (Min (..), (<>))
import Data.Sequence as X (Seq)
import Data.Set as X (Set)
import Data.String as X (String)
import Data.Text as X (Text)
import Data.Traversable as X (for)
import Data.Tuple as X (fst, snd, swap)
import Data.Type.Equality as X
import Data.Word as X (Word32)
import GHC.Generics as X (Generic)
import GHC.Stack as X (HasCallStack)
import Numeric.Natural as X (Natural)
import System.IO as X (IO)
import Test.QuickCheck as X (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Arbitrary.Generic as X
  ( GenericArbitrary
  , genericArbitrary
  , genericShrink
  )
import Text.Read as X (Read, readMaybe)
import Text.Show as X (Show, show)
import Prelude as X
  ( Bounded (..)
  , Enum
  , Int
  , Integer
  , Integral
  , RealFrac (ceiling, floor)
  , fromInteger
  , fromIntegral
  , minimum
  , negate
  , pred
  , succ
  , toInteger
  , truncate
  , uncurry
  , ($!)
  , (*)
  , (+)
  , (-)
  , (/)
  )

{-# ANN module ("HLint: ignore Avoid restricted alias" :: String) #-}
{-# ANN module ("HLint: ignore Avoid restricted qualification" :: String) #-}
