{-# LANGUAGE TypeApplications #-}

module T21003 where

import Data.IntMap (IntMap, mapKeysMonotonic, Key)
import Data.Coerce (coerce)

import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Foldable (sequence_)
import Data.Monoid (Ap(..))

-- Original example from #21003
newtype MyMap = MyMap (IntMap Bool)
shouldWork :: (Key -> Key) -> MyMap -> MyMap
shouldWork = coerce mapKeysMonotonic

-- Examples included in documentation

-- 'foldMap' defined using 'traverse'
foldMapUsingTraverse :: forall t m a. (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMapUsingTraverse = coerce $ traverse @t @(Const m)

-- 'traverse_' defined using 'foldMap'
traverse_UsingFoldMap :: forall f t a. (Foldable t, Applicative f) => (a -> f ()) -> t a -> f ()
traverse_UsingFoldMap = coerce $ foldMap @t @(Ap f ())

-- 'sequence_', but for two nested 'Foldable' structures
sequenceNested_ :: forall f1 f2. (Foldable f1, Foldable f2) => f1 (f2 (IO ())) -> IO ()
sequenceNested_ = coerce $ sequence_ @(Compose f1 f2)

-- Minimisation of an example from the 'vulkan' library
newtype Size = Size Word
test :: Size -> (Int -> ()) -> ()
test sz f = f (fromIntegral $ coerce sz)
