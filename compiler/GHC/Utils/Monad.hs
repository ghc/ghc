-- | Utilities related to Monad and Applicative classes
--   Mostly for backwards compatibility.

module GHC.Utils.Monad
        ( Applicative(..)
        , (<$>)

        , MonadFix(..)
        , MonadIO(..)

        , zipWith3M, zipWith3M_, zipWith4M, zipWithAndUnzipM
        , mapAndUnzipM, mapAndUnzip3M, mapAndUnzip4M, mapAndUnzip5M
        , mapAccumLM
        , mapSndM
        , concatMapM
        , mapMaybeM
        , fmapMaybeM, fmapEitherM
        , anyM, allM, orM
        , foldlM, foldlM_, foldrM
        , maybeMapM
        , whenM, unlessM
        , filterOutM
        ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import GHC.Prelude

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Foldable (sequenceA_, foldlM, foldrM)
import Data.List (unzip4, unzip5, zipWith4)

-------------------------------------------------------------------------------
-- Common functions
--  These are used throughout the compiler
-------------------------------------------------------------------------------

{-

Note [Inline @zipWithNM@ functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The inline principle for 'zipWith3M', 'zipWith4M' and 'zipWith3M_' is the same
as for 'zipWithM' and 'zipWithM_' in "Control.Monad", see
Note [Fusion for zipN/zipWithN] in GHC/List.hs for more details.

The 'zipWithM'/'zipWithM_' functions are inlined so that the `zipWith` and
`sequenceA` functions with which they are defined have an opportunity to fuse.

Furthermore, 'zipWith3M'/'zipWith4M' and 'zipWith3M_' have been explicitly
rewritten in a non-recursive way similarly to 'zipWithM'/'zipWithM_', and for
more than just uniformity: after [D5241](https://phabricator.haskell.org/D5241)
for issue #14037, all @zipN@/@zipWithN@ functions fuse, meaning
'zipWith3M'/'zipWIth4M' and 'zipWith3M_'@ now behave like 'zipWithM' and
'zipWithM_', respectively, with regards to fusion.

As such, since there are not any differences between 2-ary 'zipWithM'/
'zipWithM_' and their n-ary counterparts below aside from the number of
arguments, the `INLINE` pragma should be replicated in the @zipWithNM@
functions below as well.

-}

zipWith3M :: Monad m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
{-# INLINE zipWith3M #-}
-- Inline so that fusion with 'zipWith3' and 'sequenceA' has a chance to fire.
-- See Note [Inline @zipWithNM@ functions] above.
zipWith3M f xs ys zs = sequenceA (zipWith3 f xs ys zs)

zipWith3M_ :: Monad m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m ()
{-# INLINE zipWith3M_ #-}
-- Inline so that fusion with 'zipWith4' and 'sequenceA' has a chance to fire.
-- See  Note [Inline @zipWithNM@ functions] above.
zipWith3M_ f xs ys zs = sequenceA_ (zipWith3 f xs ys zs)

zipWith4M :: Monad m => (a -> b -> c -> d -> m e)
          -> [a] -> [b] -> [c] -> [d] -> m [e]
{-# INLINE zipWith4M #-}
-- Inline so that fusion with 'zipWith5' and 'sequenceA' has a chance to fire.
-- See  Note [Inline @zipWithNM@ functions] above.
zipWith4M f xs ys ws zs = sequenceA (zipWith4 f xs ys ws zs)

zipWithAndUnzipM :: Monad m
                 => (a -> b -> m (c, d)) -> [a] -> [b] -> m ([c], [d])
{-# INLINABLE zipWithAndUnzipM #-}
-- See Note [flatten_args performance] in GHC.Tc.Solver.Flatten for why this
-- pragma is essential.
zipWithAndUnzipM f (x:xs) (y:ys)
  = do { (c, d) <- f x y
       ; (cs, ds) <- zipWithAndUnzipM f xs ys
       ; return (c:cs, d:ds) }
zipWithAndUnzipM _ _ _ = return ([], [])

{-

Note [Inline @mapAndUnzipNM@ functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The inline principle is the same as 'mapAndUnzipM' in "Control.Monad".
The 'mapAndUnzipM' function is inlined so that the `unzip` and `traverse`
functions with which it is defined have an opportunity to fuse, see
Note [Inline @unzipN@ functions] in Data/OldList.hs for more details.

Furthermore, the @mapAndUnzipNM@ functions have been explicitly rewritten in a
non-recursive way similarly to 'mapAndUnzipM', and for more than just
uniformity: after [D5249](https://phabricator.haskell.org/D5249) for Trac
ticket #14037, all @unzipN@ functions fuse, meaning 'mapAndUnzip3M',
'mapAndUnzip4M' and 'mapAndUnzip5M' now behave like 'mapAndUnzipM' with regards
to fusion.

As such, since there are not any differences between 2-ary 'mapAndUnzipM' and
its n-ary counterparts below aside from the number of arguments, the `INLINE`
pragma should be replicated in the @mapAndUnzipNM@ functions below as well.

-}

-- | mapAndUnzipM for triples
mapAndUnzip3M :: Monad m => (a -> m (b,c,d)) -> [a] -> m ([b],[c],[d])
{-# INLINE mapAndUnzip3M #-}
-- Inline so that fusion with 'unzip3' and 'traverse' has a chance to fire.
-- See Note [Inline @mapAndUnzipNM@ functions] above.
mapAndUnzip3M f xs =  unzip3 <$> traverse f xs

mapAndUnzip4M :: Monad m => (a -> m (b,c,d,e)) -> [a] -> m ([b],[c],[d],[e])
{-# INLINE mapAndUnzip4M #-}
-- Inline so that fusion with 'unzip4' and 'traverse' has a chance to fire.
-- See Note [Inline @mapAndUnzipNM@ functions] above.
mapAndUnzip4M f xs =  unzip4 <$> traverse f xs

mapAndUnzip5M :: Monad m => (a -> m (b,c,d,e,f)) -> [a] -> m ([b],[c],[d],[e],[f])
{-# INLINE mapAndUnzip5M #-}
-- Inline so that fusion with 'unzip5' and 'traverse' has a chance to fire.
-- See Note [Inline @mapAndUnzipNM@ functions] above.
mapAndUnzip5M f xs =  unzip5 <$> traverse f xs

-- TODO: mapAccumLM is used in many places. Surely most of
-- these don't actually want to be lazy. We should add a strict
-- variant and use it where appropriate.

-- | Monadic version of mapAccumL
mapAccumLM :: Monad m
            => (acc -> x -> m (acc, y)) -- ^ combining function
            -> acc                      -- ^ initial state
            -> [x]                      -- ^ inputs
            -> m (acc, [y])             -- ^ final state, outputs
mapAccumLM f s xs =
  go s xs
  where
    go s (x:xs) = do
      (s1, x')  <- f s x
      (s2, xs') <- go s1 xs
      return    (s2, x' : xs')
    go s [] = return (s, [])

-- | Monadic version of mapSnd
mapSndM :: Monad m => (b -> m c) -> [(a,b)] -> m [(a,c)]
mapSndM f xs = go xs
  where
    go []         = return []
    go ((a,b):xs) = do { c <- f b; rs <- go xs; return ((a,c):rs) }

-- | Monadic version of concatMap
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

-- | Applicative version of mapMaybe
mapMaybeM :: Applicative m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = foldr g (pure [])
  where g a = liftA2 (maybe id (:)) (f a)

-- | Monadic version of fmap
fmapMaybeM :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
fmapMaybeM _ Nothing  = return Nothing
fmapMaybeM f (Just x) = f x >>= (return . Just)

-- | Monadic version of fmap
fmapEitherM :: Monad m => (a -> m b) -> (c -> m d) -> Either a c -> m (Either b d)
fmapEitherM fl _ (Left  a) = fl a >>= (return . Left)
fmapEitherM _ fr (Right b) = fr b >>= (return . Right)

-- | Monadic version of 'any', aborts the computation at the first @True@ value
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM f xs = go xs
  where
    go [] = return False
    go (x:xs) = do b <- f x
                   if b then return True
                        else go xs

-- | Monad version of 'all', aborts the computation at the first @False@ value
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f bs = go bs
  where
    go []     = return True
    go (b:bs) = (f b) >>= (\bv -> if bv then go bs else return False)

-- | Monadic version of or
orM :: Monad m => m Bool -> m Bool -> m Bool
orM m1 m2 = m1 >>= \x -> if x then return True else m2

-- | Monadic version of foldl that discards its result
foldlM_ :: (Monad m, Foldable t) => (a -> b -> m a) -> a -> t b -> m ()
foldlM_ = foldM_

-- | Monadic version of fmap specialised for Maybe
maybeMapM :: Monad m => (a -> m b) -> (Maybe a -> m (Maybe b))
maybeMapM _ Nothing  = return Nothing
maybeMapM m (Just x) = liftM Just $ m x

-- | Monadic version of @when@, taking the condition in the monad
whenM :: Monad m => m Bool -> m () -> m ()
whenM mb thing = do { b <- mb
                    ; when b thing }

-- | Monadic version of @unless@, taking the condition in the monad
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condM acc = do { cond <- condM
                       ; unless cond acc }

-- | Like 'filterM', only it reverses the sense of the test.
filterOutM :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterOutM p =
  foldr (\ x -> liftA2 (\ flg -> if flg then id else (x:)) (p x)) (pure [])
