{-# LANGUAGE CPP         #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe        #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- | Indexed Foldables.
module Data.Foldable.WithIndex (
    -- * Indexed Foldables
    FoldableWithIndex (..),
    -- ** Indexed Foldable Combinators
    iany,
    iall,
    inone, none,
    itraverse_,
    ifor_,
    imapM_,
    iforM_,
    iconcatMap,
    ifind,
    ifoldrM,
    ifoldlM,
    itoList,
) where


import Prelude (Bool, Maybe (..), Monad (..), flip, not, (.), curry)

import Control.Applicative (Applicative (..))
import Control.Monad       (liftM, void)
import Data.Foldable       (Foldable, any)
import Data.Monoid         (All (..), Any (..))

import GhcExts (build)
import WithIndex

-- | Return whether or not any element in a container satisfies a predicate, with access to the index @i@.
--
-- When you don't need access to the index then 'any' is more flexible in what it accepts.
--
-- @
-- 'any' ≡ 'iany' '.' 'const'
-- @
iany :: FoldableWithIndex i f => (i -> a -> Bool) -> f a -> Bool
iany f = getAny #. ifoldMap (\i -> Any #. f i)
{-# INLINE iany #-}

-- | Return whether or not all elements in a container satisfy a predicate, with access to the index @i@.
--
-- When you don't need access to the index then 'all' is more flexible in what it accepts.
--
-- @
-- 'all' ≡ 'iall' '.' 'const'
-- @
iall :: FoldableWithIndex i f => (i -> a -> Bool) -> f a -> Bool
iall f = getAll #. ifoldMap (\i -> All #. f i)
{-# INLINE iall #-}

-- | Return whether or not none of the elements in a container satisfy a predicate, with access to the index @i@.
--
-- When you don't need access to the index then 'none' is more flexible in what it accepts.
--
-- @
-- 'none' ≡ 'inone' '.' 'const'
-- 'inone' f ≡ 'not' '.' 'iany' f
-- @
inone :: FoldableWithIndex i f => (i -> a -> Bool) -> f a -> Bool
inone f = not . iany f
{-# INLINE inone #-}

-- | Determines whether no elements of the structure satisfy the predicate.
--
-- @
-- 'none' f ≡ 'not' '.' 'any' f
-- @
none :: Foldable f => (a -> Bool) -> f a -> Bool
none f = not . any f
{-# INLINE none #-}

-- | Traverse elements with access to the index @i@, discarding the results.
--
-- When you don't need access to the index then 'traverse_' is more flexible in what it accepts.
--
-- @
-- 'traverse_' l = 'itraverse' '.' 'const'
-- @
itraverse_ :: (FoldableWithIndex i t, Applicative f) => (i -> a -> f b) -> t a -> f ()
itraverse_ f = void . getTraversed #. ifoldMap (\i -> Traversed #. f i)
{-# INLINE itraverse_ #-}

-- | Traverse elements with access to the index @i@, discarding the results (with the arguments flipped).
--
-- @
-- 'ifor_' ≡ 'flip' 'itraverse_'
-- @
--
-- When you don't need access to the index then 'for_' is more flexible in what it accepts.
--
-- @
-- 'for_' a ≡ 'ifor_' a '.' 'const'
-- @
ifor_ :: (FoldableWithIndex i t, Applicative f) => t a -> (i -> a -> f b) -> f ()
ifor_ = flip itraverse_
{-# INLINE ifor_ #-}

-- | Run monadic actions for each target of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with access to the index,
-- discarding the results.
--
-- When you don't need access to the index then 'Control.Lens.Fold.mapMOf_' is more flexible in what it accepts.
--
-- @
-- 'mapM_' ≡ 'imapM' '.' 'const'
-- @
imapM_ :: (FoldableWithIndex i t, Monad m) => (i -> a -> m b) -> t a -> m ()
imapM_ f = liftM skip . getSequenced #. ifoldMap (\i -> Sequenced #. f i)
{-# INLINE imapM_ #-}

-- | Run monadic actions for each target of an 'IndexedFold' or 'Control.Lens.IndexedTraversal.IndexedTraversal' with access to the index,
-- discarding the results (with the arguments flipped).
--
-- @
-- 'iforM_' ≡ 'flip' 'imapM_'
-- @
--
-- When you don't need access to the index then 'Control.Lens.Fold.forMOf_' is more flexible in what it accepts.
--
-- @
-- 'Control.Lens.Fold.forMOf_' l a ≡ 'iforMOf' l a '.' 'const'
-- @
iforM_ :: (FoldableWithIndex i t, Monad m) => t a -> (i -> a -> m b) -> m ()
iforM_ = flip imapM_
{-# INLINE iforM_ #-}

-- | Concatenate the results of a function of the elements of an indexed container with access to the index.
--
-- When you don't need access to the index then 'concatMap' is more flexible in what it accepts.
--
-- @
-- 'concatMap' ≡ 'iconcatMap' '.' 'const'
-- 'iconcatMap' ≡ 'ifoldMap'
-- @
iconcatMap :: FoldableWithIndex i f => (i -> a -> [b]) -> f a -> [b]
iconcatMap = ifoldMap
{-# INLINE iconcatMap #-}

-- | Searches a container with a predicate that is also supplied the index, returning the left-most element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- When you don't need access to the index then 'find' is more flexible in what it accepts.
--
-- @
-- 'find' ≡ 'ifind' '.' 'const'
-- @
ifind :: FoldableWithIndex i f => (i -> a -> Bool) -> f a -> Maybe (i, a)
ifind p = ifoldr (\i a y -> if p i a then Just (i, a) else y) Nothing
{-# INLINE ifind #-}

-- | Monadic fold right over the elements of a structure with an index.
--
-- When you don't need access to the index then 'foldrM' is more flexible in what it accepts.
--
-- @
-- 'foldrM' ≡ 'ifoldrM' '.' 'const'
-- @
ifoldrM :: (FoldableWithIndex i f, Monad m) => (i -> a -> b -> m b) -> b -> f a -> m b
ifoldrM f z0 xs = ifoldl f' return xs z0
  where f' i k x z = f i x z >>= k
{-# INLINE ifoldrM #-}

-- | Monadic fold over the elements of a structure with an index, associating to the left.
--
-- When you don't need access to the index then 'foldlM' is more flexible in what it accepts.
--
-- @
-- 'foldlM' ≡ 'ifoldlM' '.' 'const'
-- @
ifoldlM :: (FoldableWithIndex i f, Monad m) => (i -> b -> a -> m b) -> b -> f a -> m b
ifoldlM f z0 xs = ifoldr f' return xs z0
  where f' i x k z = f i z x >>= k
{-# INLINE ifoldlM #-}

-- | Extract the key-value pairs from a structure.
--
-- When you don't need access to the indices in the result, then 'toList' is more flexible in what it accepts.
--
-- @
-- 'toList' ≡ 'Data.List.map' 'snd' '.' 'itoList'
-- @
itoList :: FoldableWithIndex i f => f a -> [(i,a)]
itoList xs = build (\c n -> ifoldr (curry c) n xs)
{-# INLINE itoList #-}
