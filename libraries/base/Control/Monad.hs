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

      Functor(..)
    , Monad((>>=), (>>), return)
    , MonadFail(fail)
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

import Control.Monad.Fail ( MonadFail(fail) )
import Data.Foldable ( Foldable, sequence_, sequenceA_, msum, mapM_, foldlM, forM_ )
import Data.Functor ( void, (<$>) )
import Data.Traversable ( forM, mapM, traverse, sequence, sequenceA )

import GHC.Base hiding ( mapM, sequence )
import GHC.List ( zipWith, unzip )
import GHC.Num  ( (-) )

-- $setup
-- >>> import Prelude
-- >>> let safeDiv x y = guard (y /= 0) >> Just (x `div` y :: Int)

-- -----------------------------------------------------------------------------
-- Functions mandated by the Prelude

-- | Conditional failure of 'Alternative' computations. Defined by
--
-- @
-- guard True  = 'pure' ()
-- guard False = 'empty'
-- @
--
-- ==== __Examples__
--
-- Common uses of 'guard' include conditionally signaling an error in
-- an error monad and conditionally rejecting the current choice in an
-- 'Alternative'-based parser.
--
-- As an example of signaling an error in the error monad 'Maybe',
-- consider a safe division function @safeDiv x y@ that returns
-- 'Nothing' when the denominator @y@ is zero and @'Just' (x \`div\`
-- y)@ otherwise. For example:
--
-- >>> safeDiv 4 0
-- Nothing
--
-- >>> safeDiv 4 2
-- Just 2
--
-- A definition of @safeDiv@ using guards, but not 'guard':
--
-- @
-- safeDiv :: Int -> Int -> Maybe Int
-- safeDiv x y | y /= 0    = Just (x \`div\` y)
--             | otherwise = Nothing
-- @
--
-- A definition of @safeDiv@ using 'guard' and 'Monad' @do@-notation:
--
-- @
-- safeDiv :: Int -> Int -> Maybe Int
-- safeDiv x y = do
--   guard (y /= 0)
--   return (x \`div\` y)
-- @
guard           :: (Alternative f) => Bool -> f ()
guard True      =  pure ()
guard False     =  empty

-- | This generalizes the list-based 'Data.List.filter' function.

{-# INLINE filterM #-}
filterM          :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM p        = foldr (\ x -> liftA2 (\ flg -> if flg then (x:) else id) (p x)) (pure [])

infixr 1 <=<, >=>

-- | Left-to-right composition of Kleisli arrows.
--
-- \'@(bs '>=>' cs) a@\' can be understood as the @do@ expression
--
-- @
-- do b <- bs a
--    cs b
-- @
(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g     = \x -> f x >>= g

-- | Right-to-left composition of Kleisli arrows. @('>=>')@, with the arguments
-- flipped.
--
-- Note how this operator resembles function composition @('.')@:
--
-- > (.)   ::            (b ->   c) -> (a ->   b) -> a ->   c
-- > (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(<=<)       :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<)       = flip (>=>)

-- | Repeat an action indefinitely.
--
-- ==== __Examples__
--
-- A common use of 'forever' is to process input from network sockets,
-- 'System.IO.Handle's, and channels
-- (e.g. 'Control.Concurrent.MVar.MVar' and
-- 'Control.Concurrent.Chan.Chan').
--
-- For example, here is how we might implement an [echo
-- server](https://en.wikipedia.org/wiki/Echo_Protocol), using
-- 'forever' both to listen for client connections on a network socket
-- and to echo client input on client connection handles:
--
-- @
-- echoServer :: Socket -> IO ()
-- echoServer socket = 'forever' $ do
--   client <- accept socket
--   'Control.Concurrent.forkFinally' (echo client) (\\_ -> hClose client)
--   where
--     echo :: Handle -> IO ()
--     echo client = 'forever' $
--       hGetLine client >>= hPutStrLn client
-- @
--
-- Note that "forever" isn't necessarily non-terminating.
-- If the action is in a @'MonadPlus'@ and short-circuits after some number of iterations.
-- then @'forever'@ actually returns `mzero`, effectively short-circuiting its caller.
forever     :: (Applicative f) => f a -> f b
{-# INLINE forever #-}
forever a   = let a' = a *> a' in a'
-- Use explicit sharing here, as it prevents a space leak regardless of
-- optimizations.

-- -----------------------------------------------------------------------------
-- Other monad functions

-- | The 'mapAndUnzipM' function maps its first argument over a list, returning
-- the result as a pair of lists. This function is mainly used with complicated
-- data structures or a state monad.
mapAndUnzipM      :: (Applicative m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
{-# INLINE mapAndUnzipM #-}
-- Inline so that fusion with 'unzip' and 'traverse' has a chance to fire.
-- See Note [Inline @unzipN@ functions] in GHC/OldList.hs.
mapAndUnzipM f xs =  unzip <$> traverse f xs

-- | The 'zipWithM' function generalizes 'zipWith' to arbitrary applicative functors.
zipWithM          :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
{-# INLINE zipWithM #-}
-- Inline so that fusion with zipWith and sequenceA have a chance to fire
-- See Note [Fusion for zipN/zipWithN] in List.hs]
zipWithM f xs ys  =  sequenceA (zipWith f xs ys)

-- | 'zipWithM_' is the extension of 'zipWithM' which ignores the final result.
zipWithM_         :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m ()
{-# INLINE zipWithM_ #-}
-- Inline so that fusion with zipWith and sequenceA have a chance to fire
-- See Note [Fusion for zipN/zipWithN] in List.hs.
zipWithM_ f xs ys =  sequenceA_ (zipWith f xs ys)

{- | The 'foldM' function is analogous to 'Data.Foldable.foldl', except that its result is
encapsulated in a monad. Note that 'foldM' works from left-to-right over
the list arguments. This could be an issue where @('>>')@ and the `folded
function' are not commutative.


> foldM f a1 [x1, x2, ..., xm]
>
> ==
>
> do
>   a2 <- f a1 x1
>   a3 <- f a2 x2
>   ...
>   f am xm

If right-to-left evaluation is required, the input list should be reversed.

Note: 'foldM' is the same as 'foldlM'
-}

foldM          :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
{-# INLINABLE foldM #-}
{-# SPECIALISE foldM :: (a -> b -> IO a) -> a -> [b] -> IO a #-}
{-# SPECIALISE foldM :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a #-}
foldM          = foldlM

-- | Like 'foldM', but discards the result.
foldM_         :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()
{-# INLINABLE foldM_ #-}
{-# SPECIALISE foldM_ :: (a -> b -> IO a) -> a -> [b] -> IO () #-}
{-# SPECIALISE foldM_ :: (a -> b -> Maybe a) -> a -> [b] -> Maybe () #-}
foldM_ f a xs  = foldlM f a xs >> return ()

{-
Note [Worker/wrapper transform on replicateM/replicateM_]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The implementations of replicateM and replicateM_ both leverage the
worker/wrapper transform. The simpler implementation of replicateM_, as an
example, would be:

    replicateM_ 0 _ = pure ()
    replicateM_ n f = f *> replicateM_ (n - 1) f

However, the self-recursive nature of this implementation inhibits inlining,
which means we never get to specialise to the action (`f` in the code above).
By contrast, the implementation below with a local loop makes it possible to
inline the entire definition (as happens for foldr, for example) thereby
specialising for the particular action.

For further information, see this issue comment, which includes side-by-side
Core: https://gitlab.haskell.org/ghc/ghc/issues/11795#note_118976
-}

-- | @'replicateM' n act@ performs the action @act@ @n@ times,
-- and then returns the list of results:
--
-- ==== __Examples__
--
-- >>> import Control.Monad.State
-- >>> runState (replicateM 3 $ state $ \s -> (s, s + 1)) 1
-- ([1,2,3],4)
--
replicateM        :: (Applicative m) => Int -> m a -> m [a]
{-# INLINABLE replicateM #-}
{-# SPECIALISE replicateM :: Int -> IO a -> IO [a] #-}
{-# SPECIALISE replicateM :: Int -> Maybe a -> Maybe [a] #-}
replicateM cnt0 f =
    loop cnt0
  where
    loop cnt
        | cnt <= 0  = pure []
        | otherwise = liftA2 (:) f (loop (cnt - 1))

-- | Like 'replicateM', but discards the result.
--
-- ==== __Examples__
--
-- >>> replicateM_ 3 (putStrLn "a")
-- a
-- a
-- a
--
replicateM_       :: (Applicative m) => Int -> m a -> m ()
{-# INLINABLE replicateM_ #-}
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
{-# INLINABLE unless #-}
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

-- | Direct 'MonadPlus' equivalent of 'Data.List.filter'.
--
-- ==== __Examples__
--
-- The 'Data.List.filter' function is just 'mfilter' specialized to
-- the list monad:
--
-- @
-- 'Data.List.filter' = ( 'mfilter' :: (a -> Bool) -> [a] -> [a] )
-- @
--
-- An example using 'mfilter' with the 'Maybe' monad:
--
-- >>> mfilter odd (Just 1)
-- Just 1
-- >>> mfilter odd (Just 2)
-- Nothing
--
mfilter :: (MonadPlus m) => (a -> Bool) -> m a -> m a
{-# INLINABLE mfilter #-}
mfilter p ma = do
  a <- ma
  if p a then return a else mzero

{- $naming

The functions in this library use the following naming conventions:

* A postfix \'@M@\' always stands for a function in the Kleisli category:
  The monad type constructor @m@ is added to function results
  (modulo currying) and nowhere else.  So, for example,

> filter  ::              (a ->   Bool) -> [a] ->   [a]
> filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

* A postfix \'@_@\' changes the result type from @(m a)@ to @(m ())@.
  Thus, for example:

> sequence  :: Monad m => [m a] -> m [a]
> sequence_ :: Monad m => [m a] -> m ()

* A prefix \'@m@\' generalizes an existing function to a monadic form.
  Thus, for example:

> filter  ::                (a -> Bool) -> [a] -> [a]
> mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a

-}
