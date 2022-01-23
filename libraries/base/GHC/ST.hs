{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples, RankNTypes #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.ST
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'ST' Monad.
--
-----------------------------------------------------------------------------

module GHC.ST (
        ST(..), STret(..), STRep,
        runST,

        -- * Unsafe functions
        liftST, unsafeInterleaveST, unsafeDupableInterleaveST
    ) where

import GHC.Base
import GHC.Show

default ()

-- The 'ST' monad proper.  By default the monad is strict;
-- too many people got bitten by space leaks when it was lazy.

-- | The strict 'ST' monad.
-- The 'ST' monad allows for destructive updates, but is escapable (unlike IO).
-- A computation of type @'ST' s a@ returns a value of type @a@, and
-- execute in "thread" @s@. The @s@ parameter is either
--
-- * an uninstantiated type variable (inside invocations of 'runST'), or
--
-- * 'RealWorld' (inside invocations of 'Control.Monad.ST.stToIO').
--
-- It serves to keep the internal states of different invocations
-- of 'runST' separate from each other and from invocations of
-- 'Control.Monad.ST.stToIO'.
--
-- The '>>=' and '>>' operations are strict in the state (though not in
-- values stored in the state).  For example,
--
-- @'runST' (writeSTRef _|_ v >>= f) = _|_@
newtype ST s a = ST (STRep s a)
type STRep s a = State# s -> (# State# s, a #)

-- | @since 2.01
instance Functor (ST s) where
    fmap f (ST m) = ST $ \ s ->
      case (m s) of { (# new_s, r #) ->
      (# new_s, f r #) }

-- | @since 4.4.0.0
instance Applicative (ST s) where
    {-# INLINE pure #-}
    {-# INLINE (*>)   #-}
    pure x = ST (\ s -> (# s, x #))
    m *> k = m >>= \ _ -> k
    (<*>) = ap
    liftA2 = liftM2

-- | @since 2.01
instance Monad (ST s) where
    {-# INLINE (>>=)  #-}
    (>>) = (*>)
    (ST m) >>= k
      = ST (\ s ->
        case (m s) of { (# new_s, r #) ->
        case (k r) of { ST k2 ->
        (k2 new_s) }})

-- | @since 4.11.0.0
instance Semigroup a => Semigroup (ST s a) where
    (<>) = liftA2 (<>)

-- | @since 4.11.0.0
instance Monoid a => Monoid (ST s a) where
    mempty = pure mempty

data STret s a = STret (State# s) a

-- liftST is useful when we want a lifted result from an ST computation.
liftST :: ST s a -> State# s -> STret s a
liftST (ST m) = \s -> case m s of (# s', r #) -> STret s' r

noDuplicateST :: ST s ()
noDuplicateST = ST $ \s -> (# noDuplicate# s, () #)

-- | 'unsafeInterleaveST' allows an 'ST' computation to be deferred
-- lazily.  When passed a value of type @ST a@, the 'ST' computation will
-- only be performed when the value of the @a@ is demanded.
{-# INLINE unsafeInterleaveST #-}
unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST m = unsafeDupableInterleaveST (noDuplicateST >> m)

-- | 'unsafeDupableInterleaveST' allows an 'ST' computation to be deferred
-- lazily.  When passed a value of type @ST a@, the 'ST' computation will
-- only be performed when the value of the @a@ is demanded.
--
-- The computation may be performed multiple times by different threads,
-- possibly at the same time. To prevent this, use 'unsafeInterleaveST' instead.
--
-- @since 4.11
{-# NOINLINE unsafeDupableInterleaveST #-}
-- See Note [unsafeDupableInterleaveIO should not be inlined]
-- in GHC.IO.Unsafe
unsafeDupableInterleaveST :: ST s a -> ST s a
unsafeDupableInterleaveST (ST m) = ST ( \ s ->
    let
        r = case m s of (# _, res #) -> res
    in
    (# s, r #)
  )

-- | @since 2.01
instance  Show (ST s a)  where
    showsPrec _ _  = showString "<<ST action>>"
    showList       = showList__ (showsPrec 0)

{-# INLINE runST #-}
-- | Return the value computed by a state thread.
-- The @forall@ ensures that the internal state used by the 'ST'
-- computation is inaccessible to the rest of the program.
runST :: (forall s. ST s a) -> a
runST (ST st_rep) = case runRW# st_rep of (# _, a #) -> a
-- See Note [Definition of runRW#] in GHC.Magic
