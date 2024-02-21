{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- This is a non-exposed internal module.
--
-- This code contains utility function and data structures that are used
-- to improve the efficiency of several instances in the Data.* namespace.
-----------------------------------------------------------------------------
module GHC.Internal.Data.Functor.Utils where

import GHC.Internal.Data.Coerce (Coercible, coerce)
import GHC.Internal.Base ( Applicative(..), Functor(..), Maybe(..), Monad (..)
                , Monoid(..), Ord(..), Semigroup(..), ($), liftM, otherwise )
import qualified GHC.Internal.List as List

-- We don't expose Max and Min because, as Edward Kmett pointed out to me,
-- there are two reasonable ways to define them. One way is to use Maybe, as we
-- do here; the other way is to impose a Bounded constraint on the Monoid
-- instance. We may eventually want to add both versions, but we don't want to
-- trample on anyone's toes by imposing Max = MaxMaybe.

newtype Max a = Max {getMax :: Maybe a}
newtype Min a = Min {getMin :: Maybe a}

-- | @since base-4.11.0.0
instance Ord a => Semigroup (Max a) where
    {-# INLINE (<>) #-}
    m <> Max Nothing = m
    Max Nothing <> n = n
    (Max m@(Just x)) <> (Max n@(Just y))
      | x >= y    = Max m
      | otherwise = Max n

-- | @since base-4.8.0.0
instance Ord a => Monoid (Max a) where
    mempty = Max Nothing
    -- By default, we would get a lazy right fold. This forces the use of a strict
    -- left fold instead.
    mconcat = List.foldl' (<>) mempty
    {-# INLINE mconcat #-}

-- | @since base-4.11.0.0
instance Ord a => Semigroup (Min a) where
    {-# INLINE (<>) #-}
    m <> Min Nothing = m
    Min Nothing <> n = n
    (Min m@(Just x)) <> (Min n@(Just y))
      | x <= y    = Min m
      | otherwise = Min n

-- | @since base-4.8.0.0
instance Ord a => Monoid (Min a) where
    mempty = Min Nothing
    -- By default, we would get a lazy right fold. This forces the use of a strict
    -- left fold instead.
    mconcat = List.foldl' (<>) mempty
    {-# INLINE mconcat #-}

-- left-to-right state-transforming monad
newtype StateL s a = StateL { runStateL :: s -> (s, a) }

-- | @since base-4.0
instance Functor (StateL s) where
    fmap f (StateL k) = StateL $ \ s -> let (s', v) = k s in (s', f v)

-- | @since base-4.0
instance Applicative (StateL s) where
    pure x = StateL (\ s -> (s, x))
    StateL kf <*> StateL kv = StateL $ \ s ->
        let (s', f) = kf s
            (s'', v) = kv s'
        in (s'', f v)
    liftA2 f (StateL kx) (StateL ky) = StateL $ \s ->
        let (s', x) = kx s
            (s'', y) = ky s'
        in (s'', f x y)

-- right-to-left state-transforming monad
newtype StateR s a = StateR { runStateR :: s -> (s, a) }

-- | @since base-4.0
instance Functor (StateR s) where
    fmap f (StateR k) = StateR $ \ s -> let (s', v) = k s in (s', f v)

-- | @since base-4.0
instance Applicative (StateR s) where
    pure x = StateR (\ s -> (s, x))
    StateR kf <*> StateR kv = StateR $ \ s ->
        let (s', v) = kv s
            (s'', f) = kf s'
        in (s'', f v)
    liftA2 f (StateR kx) (StateR ky) = StateR $ \ s ->
        let (s', y) = ky s
            (s'', x) = kx s'
        in (s'', f x y)

-- | A state transformer monad parameterized by the state and inner monad.
-- The implementation is copied from the transformers package with the
-- return tuple swapped.
--
-- @since base-4.18.0.0
newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }

-- | @since base-4.18.0.0
instance Monad m => Functor (StateT s m) where
    fmap = liftM
    {-# INLINE fmap #-}

-- | @since base-4.18.0.0
instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (s, a)
    {-# INLINE pure #-}
    StateT mf <*> StateT mx = StateT $ \ s -> do
        (s', f) <- mf s
        (s'', x) <- mx s'
        return (s'', f x)
    {-# INLINE (<*>) #-}
    m *> k = m >>= \_ -> k
    {-# INLINE (*>) #-}

-- | @since base-4.18.0.0
instance (Monad m) => Monad (StateT s m) where
    m >>= k  = StateT $ \ s -> do
        (s', a) <- runStateT m s
        runStateT (k a) s'
    {-# INLINE (>>=) #-}

-- See Note [Function coercion]
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}

{-
Note [Function coercion]
~~~~~~~~~~~~~~~~~~~~~~~

Several functions here use (#.) instead of (.) to avoid potential efficiency
problems relating to #7542. The problem, in a nutshell:

If N is a newtype constructor, then N x will always have the same
representation as x (something similar applies for a newtype deconstructor).
However, if f is a function,

N . f = \x -> N (f x)

This looks almost the same as f, but the eta expansion lifts it--the lhs could
be _|_, but the rhs never is. This can lead to very inefficient code.  Thus we
steal a technique from Shachaf and Edward Kmett and adapt it to the current
(rather clean) setting. Instead of using  N . f,  we use  N #. f, which is
just

coerce f `asTypeOf` (N . f)

That is, we just *pretend* that f has the right type, and thanks to the safety
of coerce, the type checker guarantees that nothing really goes wrong. We still
have to be a bit careful, though: remember that #. completely ignores the
*value* of its left operand.
-}
