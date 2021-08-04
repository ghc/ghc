{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Control.Monad.Compat (
  module Base
, Monad
#if MIN_VERSION_base(4,9,0)
, MonadFail
#endif
, fail
, MonadPlus(..)
#if !(MIN_VERSION_base(4,8,0))
, foldM
, foldM_
, forM
, forM_
, guard
, mapM
, mapM_
, msum
, sequence
, sequence_
, unless
, when

, (<$!>)
#endif
#if !(MIN_VERSION_base(4,9,0))
, forever
, filterM
, mapAndUnzipM
, zipWithM
, zipWithM_
, replicateM
, replicateM_
#endif
) where

#if MIN_VERSION_base(4,9,0)
import Control.Monad as Base hiding (fail)
import Control.Monad.Fail as Base
#else
import Control.Monad as Base hiding (
    forever
  , filterM
  , mapAndUnzipM
  , zipWithM
  , zipWithM_
  , replicateM
  , replicateM_
# if !(MIN_VERSION_base(4,8,0))
  , foldM
  , foldM_
  , forM
  , forM_
  , guard
  , mapM
  , mapM_
  , msum
  , sequence
  , sequence_
  , unless
  , when
# endif
  )
import Control.Applicative
import Data.Foldable.Compat
import Data.Traversable
import Prelude.Compat
#endif

#if !(MIN_VERSION_base(4,8,0))
-- | Conditional execution of 'Applicative' expressions. For example,
--
-- > when debug (putStrLn "Debugging")
--
-- will output the string @Debugging@ if the Boolean value @debug@
-- is 'True', and otherwise do nothing.
when      :: (Applicative f) => Bool -> f () -> f ()
{-# INLINEABLE when #-}
{-# SPECIALISE when :: Bool -> IO () -> IO () #-}
{-# SPECIALISE when :: Bool -> Maybe () -> Maybe () #-}
when p s  = if p then s else pure ()

-- | @'guard' b@ is @'pure' ()@ if @b@ is 'True',
-- and 'empty' if @b@ is 'False'.
guard           :: (Alternative f) => Bool -> f ()
guard True      =  pure ()
guard False     =  empty

-- | The reverse of 'when'.
unless            :: (Applicative f) => Bool -> f () -> f ()
{-# INLINEABLE unless #-}
{-# SPECIALISE unless :: Bool -> IO () -> IO () #-}
{-# SPECIALISE unless :: Bool -> Maybe () -> Maybe () #-}
unless p s        =  if p then pure () else s

{- | The 'foldM' function is analogous to 'foldl', except that its result is
encapsulated in a monad. Note that 'foldM' works from left-to-right over
the list arguments. This could be an issue where @('>>')@ and the `folded
function' are not commutative.


>       foldM f a1 [x1, x2, ..., xm]

==

>       do
>         a2 <- f a1 x1
>         a3 <- f a2 x2
>         ...
>         f am xm

If right-to-left evaluation is required, the input list should be reversed.

Note: 'foldM' is the same as 'foldlM'
-}

foldM          :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
{-# INLINEABLE foldM #-}
{-# SPECIALISE foldM :: (a -> b -> IO a) -> a -> [b] -> IO a #-}
{-# SPECIALISE foldM :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a #-}
foldM          = foldlM

-- | Like 'foldM', but discards the result.
foldM_         :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()
{-# INLINEABLE foldM_ #-}
{-# SPECIALISE foldM_ :: (a -> b -> IO a) -> a -> [b] -> IO () #-}
{-# SPECIALISE foldM_ :: (a -> b -> Maybe a) -> a -> [b] -> Maybe () #-}
foldM_ f a xs  = foldlM f a xs >> return ()

infixl 4 <$!>

-- | Strict version of 'Data.Functor.<$>'.
--
-- /Since: 4.8.0.0/
(<$!>) :: Monad m => (a -> b) -> m a -> m b
{-# INLINE (<$!>) #-}
f <$!> m = do
  x <- m
  let z = f x
  z `seq` return z
#endif

#if !(MIN_VERSION_base(4,9,0))
-- | @'forever' act@ repeats the action infinitely.
forever     :: (Applicative f) => f a -> f b
{-# INLINE forever #-}
forever a   = let a' = a *> a' in a'
-- Use explicit sharing here, as it is prevents a space leak regardless of
-- optimizations.

-- | This generalizes the list-based 'filter' function.
{-# INLINE filterM #-}
filterM          :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM p        = foldr (\ x -> liftA2 (\ flg -> if flg then (x:) else id) (p x)) (pure [])

-- | The 'mapAndUnzipM' function maps its first argument over a list, returning
-- the result as a pair of lists. This function is mainly used with complicated
-- data structures or a state-transforming monad.
mapAndUnzipM      :: (Applicative m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
{-# INLINE mapAndUnzipM #-}
mapAndUnzipM f xs =  unzip <$> traverse f xs

-- | The 'zipWithM' function generalizes 'zipWith' to arbitrary applicative functors.
zipWithM          :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
{-# INLINE zipWithM #-}
zipWithM f xs ys  =  sequenceA (zipWith f xs ys)

-- | 'zipWithM_' is the extension of 'zipWithM' which ignores the final result.
zipWithM_         :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ f xs ys =  sequenceA_ (zipWith f xs ys)

-- | @'replicateM' n act@ performs the action @n@ times,
-- gathering the results.
replicateM        :: (Applicative m) => Int -> m a -> m [a]
{-# INLINEABLE replicateM #-}
{-# SPECIALISE replicateM :: Int -> IO a -> IO [a] #-}
{-# SPECIALISE replicateM :: Int -> Maybe a -> Maybe [a] #-}
replicateM cnt0 f =
    loop cnt0
  where
    loop cnt
        | cnt <= 0  = pure []
        | otherwise = liftA2 (:) f (loop (cnt - 1))

-- | Like 'replicateM', but discards the result.
replicateM_       :: (Applicative m) => Int -> m a -> m ()
{-# INLINEABLE replicateM_ #-}
{-# SPECIALISE replicateM_ :: Int -> IO a -> IO () #-}
{-# SPECIALISE replicateM_ :: Int -> Maybe a -> Maybe () #-}
replicateM_ cnt0 f =
    loop cnt0
  where
    loop cnt
        | cnt <= 0  = pure ()
        | otherwise = f *> loop (cnt - 1)
#endif
