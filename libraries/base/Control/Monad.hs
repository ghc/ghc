{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The 'Functor', 'Monad' and 'MonadPlus' classes,
-- with some useful operations on monads.

module Control.Monad
    (
    -- * Functor and monad classes

      Functor(fmap)
    , Monad((>>=), (>>), return, fail)
    , MonadPlus(mzero, mplus)
    -- * Functions

    -- ** Naming conventions
    -- $naming

    -- ** Basic @Monad@ functions

    , mapM
    , mapM_
    , forM
    , forM_
    , sequence
    , sequence_
    , (=<<)
    , (>=>)
    , (<=<)
    , forever
    , void

    -- ** Generalisations of list functions

    , join
    , msum
    , mfilter
    , filterM
    , mapAndUnzipM
    , zipWithM
    , zipWithM_
    , foldM
    , foldM_
    , replicateM
    , replicateM_

    -- ** Conditional execution of monadic expressions

    , guard
    , when
    , unless

    -- ** Monadic lifting operators

    , liftM
    , liftM2
    , liftM3
    , liftM4
    , liftM5

    , ap

    -- ** Strict monadic functions

    , (<$!>)
    ) where

import Data.Foldable ( Foldable, sequence_, sequenceA_, msum, mapM_, foldlM, forM_ )
import Data.Functor ( void, (<$>) )
import Data.Traversable ( forM, mapM, traverse, sequence, sequenceA )

import GHC.Base hiding ( mapM, sequence )
import GHC.List ( zipWith, unzip )
import GHC.Num  ( (-) )

-- -----------------------------------------------------------------------------
-- Functions mandated by the Prelude

-- | @'guard' b@ is @'pure' ()@ if @b@ is 'True',
-- and 'empty' if @b@ is 'False'.
guard           :: (Alternative f) => Bool -> f ()
guard True      =  pure ()
guard False     =  empty

-- | This generalizes the list-based 'filter' function.

{-# INLINE filterM #-}
filterM          :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM p        = foldr (\ x -> liftA2 (\ flg -> if flg then (x:) else id) (p x)) (pure [])

infixr 1 <=<, >=>

-- | Left-to-right Kleisli composition of monads.
(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g     = \x -> f x >>= g

-- | Right-to-left Kleisli composition of monads. @('>=>')@, with the arguments flipped.
--
-- Note how this operator resembles function composition @('.')@:
--
-- > (.)   ::            (b ->   c) -> (a ->   b) -> a ->   c
-- > (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(<=<)       :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<)       = flip (>=>)

-- | @'forever' act@ repeats the action infinitely.
forever     :: (Applicative f) => f a -> f b
{-# INLINE forever #-}
forever a   = let a' = a *> a' in a'
-- Use explicit sharing here, as it is prevents a space leak regardless of
-- optimizations.

-- -----------------------------------------------------------------------------
-- Other monad functions

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

{-
Note [Worker/wrapper transform on replicateM/replicateM_
--------------------------------------------------------

The implementations of replicateM and replicateM_ both leverage the
worker/wrapper transform. The simpler implementation of replicateM_, as an
example, would be:

    replicateM_ 0 _ = pure ()
    replicateM_ n f = f *> replicateM_ (n - 1) f

However, the self-recrusive nature of this implementation inhibits inlining,
which means we never get to specialise to the action (`f` in the code above).
By contrast, the implementation below with a local loop makes it possible to
inline the entire definition (as hapens for foldr, for example) thereby
specialising for the particular action.

For further information, see this Trac comment, which includes side-by-side
Core.

https://ghc.haskell.org/trac/ghc/ticket/11795#comment:6

-}

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


-- | The reverse of 'when'.
unless            :: (Applicative f) => Bool -> f () -> f ()
{-# INLINEABLE unless #-}
{-# SPECIALISE unless :: Bool -> IO () -> IO () #-}
{-# SPECIALISE unless :: Bool -> Maybe () -> Maybe () #-}
unless p s        =  if p then pure () else s

infixl 4 <$!>

-- | Strict version of 'Data.Functor.<$>'.
--
-- @since 4.8.0.0
(<$!>) :: Monad m => (a -> b) -> m a -> m b
{-# INLINE (<$!>) #-}
f <$!> m = do
  x <- m
  let z = f x
  z `seq` return z


-- -----------------------------------------------------------------------------
-- Other MonadPlus functions

-- | Direct 'MonadPlus' equivalent of 'filter'
-- @'filter'@ = @(mfilter:: (a -> Bool) -> [a] -> [a]@
-- applicable to any 'MonadPlus', for example
-- @mfilter odd (Just 1) == Just 1@
-- @mfilter odd (Just 2) == Nothing@

mfilter :: (MonadPlus m) => (a -> Bool) -> m a -> m a
{-# INLINEABLE mfilter #-}
mfilter p ma = do
  a <- ma
  if p a then return a else mzero

{- $naming

The functions in this library use the following naming conventions:

* A postfix \'@M@\' always stands for a function in the Kleisli category:
  The monad type constructor @m@ is added to function results
  (modulo currying) and nowhere else.  So, for example,

>  filter  ::              (a ->   Bool) -> [a] ->   [a]
>  filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

* A postfix \'@_@\' changes the result type from @(m a)@ to @(m ())@.
  Thus, for example:

>  sequence  :: Monad m => [m a] -> m [a]
>  sequence_ :: Monad m => [m a] -> m ()

* A prefix \'@m@\' generalizes an existing function to a monadic form.
  Thus, for example:

>  sum  :: Num a       => [a]   -> a
>  msum :: MonadPlus m => [m a] -> m a

-}
