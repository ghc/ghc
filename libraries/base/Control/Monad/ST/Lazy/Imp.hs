{-# LANGUAGE Unsafe #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash, UnboxedTuples, RankNTypes #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ST.Lazy.Imp
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires universal quantification for runST)
--
-- This module presents an identical interface to "Control.Monad.ST",
-- except that the monad delays evaluation of 'ST' operations until
-- a value depending on them is required.
--
-----------------------------------------------------------------------------

module Control.Monad.ST.Lazy.Imp (
        -- * The 'ST' monad
        ST,
        runST,
        fixST,

        -- * Converting between strict and lazy 'ST'
        strictToLazyST, lazyToStrictST,

        -- * Converting 'ST' To 'IO'
        RealWorld,
        stToIO,

        -- * Unsafe operations
        unsafeInterleaveST,
        unsafeIOToST
    ) where

import Control.Monad.Fix

import qualified Control.Monad.ST as ST
import qualified Control.Monad.ST.Unsafe as ST

import qualified GHC.ST as GHC.ST
import GHC.Base

-- | The lazy @'ST'@ monad.
-- The ST monad allows for destructive updates, but is escapable (unlike @IO@).
-- A computation of type @'ST' s a@ returns a value of type @a@, and
-- executes in "thread" @s@. The @s@ parameter is either
--
-- * an uninstantiated type variable (inside invocations of 'runST'), or
--
-- * 'RealWorld' (inside invocations of 'stToIO').
--
-- It serves to keep the internal states of different invocations of
-- 'runST' separate from each other and from invocations of 'stToIO'.
--
-- The '>>=' and '>>' operations are not strict in the state.  For example,
--
-- @'runST' (writeSTRef _|_ v >>= readSTRef _|_ >> return 2) = 2@
newtype ST s a = ST { unST :: State s -> (a, State s) }

-- A lifted state token. This can be imagined as a moment in the timeline
-- of a lazy state thread. Forcing the token forces all delayed actions in
-- the thread up until that moment to be performed.
data State s = S# (State# s)

{- Note [Lazy ST and multithreading]

We used to imagine that passing a polymorphic state token was all that we
needed to keep state threads separate (see Launchbury and Peyton Jones, 1994:
https://www.microsoft.com/en-us/research/publication/lazy-functional-state-threads/).
But this breaks down in the face of concurrency (see #11760). Whereas a strict
ST computation runs to completion before producing anything, a value produced
by running a lazy ST computation may contain a thunk that, when forced, will
lead to further stateful computations. If such a thunk is entered by more than
one thread, then they may both read from and write to the same references and
arrays, interfering with each other. To work around this, any time we lazily
suspend execution of a lazy ST computation, we bind the result pair to a
NOINLINE binding (ensuring that it is not duplicated) and calculate that
pair using (unsafePerformIO . evaluate), ensuring that only one thread will
enter the thunk. We still use lifted state tokens to actually drive execution,
so in these cases we effectively deal with *two* state tokens: the lifted
one we get from the previous computation, and the unlifted one we pull out of
thin air. -}

{- Note [Lazy ST: not producing lazy pairs]

The fixST and strictToLazyST functions used to construct functions that
produced lazy pairs. Why don't we need that laziness? The ST type is kept
abstract, so no one outside this module can ever get their hands on a (result,
State s) pair. We ourselves never match on such pairs when performing ST
computations unless we also force one of their components. So no one should be
able to detect the change. By refraining from producing such thunks (which
reference delayed ST computations), we avoid having to ask whether we have to
wrap them up with unsafePerformIO. See Note [Lazy ST and multithreading]. -}

-- | This is a terrible hack to prevent a thunk from being entered twice.
-- Simon Peyton Jones would very much like to be rid of it.
noDup :: a -> a
noDup a = runRW# (\s ->
  case noDuplicate# s of
    _ -> a)

-- | @since 2.01
instance Functor (ST s) where
    fmap f m = ST $ \ s ->
      let
        -- See Note [Lazy ST and multithreading]
        {-# NOINLINE res #-}
        res = noDup (unST m s)
        (r,new_s) = res
      in
        (f r,new_s)

    x <$ m = ST $ \ s ->
      let
        {-# NOINLINE s' #-}
        -- See Note [Lazy ST and multithreading]
        s' = noDup (snd (unST m s))
      in (x, s')

-- | @since 2.01
instance Applicative (ST s) where
    pure a = ST $ \ s -> (a,s)

    fm <*> xm = ST $ \ s ->
       let
         {-# NOINLINE res1 #-}
         !res1 = unST fm s
         !(f, s') = res1

         {-# NOINLINE res2 #-}
         -- See Note [Lazy ST and multithreading]
         res2 = noDup (unST xm s')
         (x, s'') = res2
       in (f x, s'')
    -- Why can we use a strict binding for res1? If someone
    -- forces the (f x, s'') pair, then they must need
    -- f or s''. To get s'', they need s'.

    liftA2 f m n = ST $ \ s ->
      let
        {-# NOINLINE res1 #-}
        -- See Note [Lazy ST and multithreading]
        res1 = noDup (unST m s)
        (x, s') = res1

        {-# NOINLINE res2 #-}
        res2 = noDup (unST n s')
        (y, s'') = res2
      in (f x y, s'')
    -- We don't get to be strict in liftA2, but we clear out a
    -- NOINLINE in comparison to the default definition, which may
    -- help the simplifier.

    m *> n = ST $ \s ->
       let
         {-# NOINLINE s' #-}
         -- See Note [Lazy ST and multithreading]
         s' = noDup (snd (unST m s))
       in unST n s'

    m <* n = ST $ \s ->
       let
         {-# NOINLINE res1 #-}
         !res1 = unST m s
         !(mr, s') = res1

         {-# NOINLINE s'' #-}
         -- See Note [Lazy ST and multithreading]
         s'' = noDup (snd (unST n s'))
       in (mr, s'')
    -- Why can we use a strict binding for res1? The same reason as
    -- in <*>. If someone demands the (mr, s'') pair, then they will
    -- force mr or s''. To get s'', they need s'.

-- | @since 2.01
instance Monad (ST s) where
    (>>) = (*>)

    m >>= k = ST $ \ s ->
       let
         -- See Note [Lazy ST and multithreading]
         {-# NOINLINE res #-}
         res = noDup (unST m s)
         (r,new_s) = res
       in
         unST (k r) new_s

-- | @since 4.10
instance MonadFail (ST s) where
    fail s = errorWithoutStackTrace s

-- | Return the value computed by an 'ST' computation.
-- The @forall@ ensures that the internal state used by the 'ST'
-- computation is inaccessible to the rest of the program.
runST :: (forall s. ST s a) -> a
runST (ST st) = runRW# (\s -> case st (S# s) of (r, _) -> r)

-- | Allow the result of an 'ST' computation to be used (lazily)
-- inside the computation.
-- Note that if @f@ is strict, @'fixST' f = _|_@.
fixST :: (a -> ST s a) -> ST s a
fixST m = ST (\ s ->
                let
                   q@(r,_s') = unST (m r) s
                in q)
-- Why don't we need unsafePerformIO in fixST? We create a thunk, q,
-- to perform a lazy state computation, and we pass a reference to that
-- thunk, r, to m. Uh oh? No, I think it should be fine, because that thunk
-- itself is demanded directly in the `let` body. See also
-- Note [Lazy ST: not producing lazy pairs].

-- | @since 2.01
instance MonadFix (ST s) where
        mfix = fixST

-- ---------------------------------------------------------------------------
-- Strict <--> Lazy

{-|
Convert a strict 'ST' computation into a lazy one.  The strict state
thread passed to 'strictToLazyST' is not performed until the result of
the lazy state thread it returns is demanded.
-}
strictToLazyST :: ST.ST s a -> ST s a
strictToLazyST (GHC.ST.ST m) = ST $ \(S# s) ->
  case m s of
    (# s', a #) -> (a, S# s')
-- See Note [Lazy ST: not producing lazy pairs]

{-|
Convert a lazy 'ST' computation into a strict one.
-}
lazyToStrictST :: ST s a -> ST.ST s a
lazyToStrictST (ST m) = GHC.ST.ST $ \s ->
        case (m (S# s)) of (a, S# s') -> (# s', a #)

-- | A monad transformer embedding lazy 'ST' in the 'IO'
-- monad.  The 'RealWorld' parameter indicates that the internal state
-- used by the 'ST' computation is a special one supplied by the 'IO'
-- monad, and thus distinct from those used by invocations of 'runST'.
stToIO :: ST RealWorld a -> IO a
stToIO = ST.stToIO . lazyToStrictST

-- ---------------------------------------------------------------------------
-- Strict <--> Lazy

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST = strictToLazyST . ST.unsafeInterleaveST . lazyToStrictST

unsafeIOToST :: IO a -> ST s a
unsafeIOToST = strictToLazyST . ST.unsafeIOToST

