{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.RWS.Strict
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- A monad transformer that combines 'ReaderT', 'WriterT' and 'StateT'.
-- This version is strict; for a lazy version with the same interface,
-- see "Control.Monad.Trans.RWS.Lazy".
-----------------------------------------------------------------------------

module Control.Monad.Trans.RWS.Strict (
    -- * The RWS monad
    RWS,
    rws,
    runRWS,
    evalRWS,
    execRWS,
    mapRWS,
    withRWS,
    -- * The RWST monad transformer
    RWST(..),
    evalRWST,
    execRWST,
    mapRWST,
    withRWST,
    -- * Reader operations
    reader,
    ask,
    local,
    asks,
    -- * Writer operations
    writer,
    tell,
    listen,
    listens,
    pass,
    censor,
    -- * State operations
    state,
    get,
    put,
    modify,
    gets,
    -- * Lifting other operations
    liftCallCC,
    liftCallCC',
    liftCatch,
  ) where

import Control.Monad.IO.Class
import Control.Monad.Signatures
import Control.Monad.Trans.Class
import Data.Functor.Identity

import Control.Applicative
import Control.Monad
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
import Control.Monad.Fix
import Data.Monoid

-- | A monad containing an environment of type @r@, output of type @w@
-- and an updatable state of type @s@.
type RWS r w s = RWST r w s Identity

-- | Construct an RWS computation from a function.
-- (The inverse of 'runRWS'.)
rws :: (r -> s -> (a, s, w)) -> RWS r w s a
rws f = RWST (\ r s -> Identity (f r s))
{-# INLINE rws #-}

-- | Unwrap an RWS computation as a function.
-- (The inverse of 'rws'.)
runRWS :: RWS r w s a -> r -> s -> (a, s, w)
runRWS m r s = runIdentity (runRWST m r s)
{-# INLINE runRWS #-}

-- | Evaluate a computation with the given initial state and environment,
-- returning the final value and output, discarding the final state.
evalRWS :: RWS r w s a  -- ^RWS computation to execute
        -> r            -- ^initial environment
        -> s            -- ^initial value
        -> (a, w)       -- ^final value and output
evalRWS m r s = let
    (a, _, w) = runRWS m r s
    in (a, w)
{-# INLINE evalRWS #-}

-- | Evaluate a computation with the given initial state and environment,
-- returning the final state and output, discarding the final value.
execRWS :: RWS r w s a  -- ^RWS computation to execute
        -> r            -- ^initial environment
        -> s            -- ^initial value
        -> (s, w)       -- ^final state and output
execRWS m r s = let
    (_, s', w) = runRWS m r s
    in (s', w)
{-# INLINE execRWS #-}

-- | Map the return value, final state and output of a computation using
-- the given function.
--
-- * @'runRWS' ('mapRWS' f m) r s = f ('runRWS' m r s)@
mapRWS :: ((a, s, w) -> (b, s, w')) -> RWS r w s a -> RWS r w' s b
mapRWS f = mapRWST (Identity . f . runIdentity)
{-# INLINE mapRWS #-}

-- | @'withRWS' f m@ executes action @m@ with an initial environment
-- and state modified by applying @f@.
--
-- * @'runRWS' ('withRWS' f m) r s = 'uncurry' ('runRWS' m) (f r s)@
withRWS :: (r' -> s -> (r, s)) -> RWS r w s a -> RWS r' w s a
withRWS = withRWST
{-# INLINE withRWS #-}

-- ---------------------------------------------------------------------------
-- | A monad transformer adding reading an environment of type @r@,
-- collecting an output of type @w@ and updating a state of type @s@
-- to an inner monad @m@.
newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a, s, w) }

-- | Evaluate a computation with the given initial state and environment,
-- returning the final value and output, discarding the final state.
evalRWST :: (Monad m)
         => RWST r w s m a      -- ^computation to execute
         -> r                   -- ^initial environment
         -> s                   -- ^initial value
         -> m (a, w)            -- ^computation yielding final value and output
evalRWST m r s = do
    (a, _, w) <- runRWST m r s
    return (a, w)
{-# INLINE evalRWST #-}

-- | Evaluate a computation with the given initial state and environment,
-- returning the final state and output, discarding the final value.
execRWST :: (Monad m)
         => RWST r w s m a      -- ^computation to execute
         -> r                   -- ^initial environment
         -> s                   -- ^initial value
         -> m (s, w)            -- ^computation yielding final state and output
execRWST m r s = do
    (_, s', w) <- runRWST m r s
    return (s', w)
{-# INLINE execRWST #-}

-- | Map the inner computation using the given function.
--
-- * @'runRWST' ('mapRWST' f m) r s = f ('runRWST' m r s)@
mapRWST :: (m (a, s, w) -> n (b, s, w')) -> RWST r w s m a -> RWST r w' s n b
mapRWST f m = RWST $ \ r s -> f (runRWST m r s)
{-# INLINE mapRWST #-}

-- | @'withRWST' f m@ executes action @m@ with an initial environment
-- and state modified by applying @f@.
--
-- * @'runRWST' ('withRWST' f m) r s = 'uncurry' ('runRWST' m) (f r s)@
withRWST :: (r' -> s -> (r, s)) -> RWST r w s m a -> RWST r' w s m a
withRWST f m = RWST $ \ r s -> uncurry (runRWST m) (f r s)
{-# INLINE withRWST #-}

instance (Functor m) => Functor (RWST r w s m) where
    fmap f m = RWST $ \ r s ->
        fmap (\ (a, s', w) -> (f a, s', w)) $ runRWST m r s
    {-# INLINE fmap #-}

instance (Monoid w, Functor m, Monad m) => Applicative (RWST r w s m) where
    pure a = RWST $ \ _ s -> return (a, s, mempty)
    {-# INLINE pure #-}
    RWST mf <*> RWST mx = RWST $ \ r s -> do
        (f, s', w)  <- mf r s
        (x, s'',w') <- mx r s'
        return (f x, s'', w `mappend` w')
    {-# INLINE (<*>) #-}

instance (Monoid w, Functor m, MonadPlus m) => Alternative (RWST r w s m) where
    empty = RWST $ \ _ _ -> mzero
    {-# INLINE empty #-}
    RWST m <|> RWST n = RWST $ \ r s -> m r s `mplus` n r s
    {-# INLINE (<|>) #-}

instance (Monoid w, Monad m) => Monad (RWST r w s m) where
#if !(MIN_VERSION_base(4,8,0))
    return a = RWST $ \ _ s -> return (a, s, mempty)
    {-# INLINE return #-}
#endif
    m >>= k  = RWST $ \ r s -> do
        (a, s', w)  <- runRWST m r s
        (b, s'',w') <- runRWST (k a) r s'
        return (b, s'', w `mappend` w')
    {-# INLINE (>>=) #-}
    fail msg = RWST $ \ _ _ -> fail msg
    {-# INLINE fail #-}

#if MIN_VERSION_base(4,9,0)
instance (Monoid w, Fail.MonadFail m) => Fail.MonadFail (RWST r w s m) where
    fail msg = RWST $ \ _ _ -> Fail.fail msg
    {-# INLINE fail #-}
#endif

instance (Monoid w, MonadPlus m) => MonadPlus (RWST r w s m) where
    mzero = RWST $ \ _ _ -> mzero
    {-# INLINE mzero #-}
    RWST m `mplus` RWST n = RWST $ \ r s -> m r s `mplus` n r s
    {-# INLINE mplus #-}

instance (Monoid w, MonadFix m) => MonadFix (RWST r w s m) where
    mfix f = RWST $ \ r s -> mfix $ \ ~(a, _, _) -> runRWST (f a) r s
    {-# INLINE mfix #-}

instance (Monoid w) => MonadTrans (RWST r w s) where
    lift m = RWST $ \ _ s -> do
        a <- m
        return (a, s, mempty)
    {-# INLINE lift #-}

instance (Monoid w, MonadIO m) => MonadIO (RWST r w s m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

-- ---------------------------------------------------------------------------
-- Reader operations

-- | Constructor for computations in the reader monad (equivalent to 'asks').
reader :: (Monoid w, Monad m) => (r -> a) -> RWST r w s m a
reader = asks
{-# INLINE reader #-}

-- | Fetch the value of the environment.
ask :: (Monoid w, Monad m) => RWST r w s m r
ask = RWST $ \ r s -> return (r, s, mempty)
{-# INLINE ask #-}

-- | Execute a computation in a modified environment
--
-- * @'runRWST' ('local' f m) r s = 'runRWST' m (f r) s@
local :: (r -> r) -> RWST r w s m a -> RWST r w s m a
local f m = RWST $ \ r s -> runRWST m (f r) s
{-# INLINE local #-}

-- | Retrieve a function of the current environment.
--
-- * @'asks' f = 'liftM' f 'ask'@
asks :: (Monoid w, Monad m) => (r -> a) -> RWST r w s m a
asks f = RWST $ \ r s -> return (f r, s, mempty)
{-# INLINE asks #-}

-- ---------------------------------------------------------------------------
-- Writer operations

-- | Construct a writer computation from a (result, output) pair.
writer :: (Monad m) => (a, w) -> RWST r w s m a
writer (a, w) = RWST $ \ _ s -> return (a, s, w)
{-# INLINE writer #-}

-- | @'tell' w@ is an action that produces the output @w@.
tell :: (Monad m) => w -> RWST r w s m ()
tell w = RWST $ \ _ s -> return ((),s,w)
{-# INLINE tell #-}

-- | @'listen' m@ is an action that executes the action @m@ and adds its
-- output to the value of the computation.
--
-- * @'runRWST' ('listen' m) r s = 'liftM' (\\ (a, w) -> ((a, w), w)) ('runRWST' m r s)@
listen :: (Monad m) => RWST r w s m a -> RWST r w s m (a, w)
listen m = RWST $ \ r s -> do
    (a, s', w) <- runRWST m r s
    return ((a, w), s', w)
{-# INLINE listen #-}

-- | @'listens' f m@ is an action that executes the action @m@ and adds
-- the result of applying @f@ to the output to the value of the computation.
--
-- * @'listens' f m = 'liftM' (id *** f) ('listen' m)@
--
-- * @'runRWST' ('listens' f m) r s = 'liftM' (\\ (a, w) -> ((a, f w), w)) ('runRWST' m r s)@
listens :: (Monad m) => (w -> b) -> RWST r w s m a -> RWST r w s m (a, b)
listens f m = RWST $ \ r s -> do
    (a, s', w) <- runRWST m r s
    return ((a, f w), s', w)
{-# INLINE listens #-}

-- | @'pass' m@ is an action that executes the action @m@, which returns
-- a value and a function, and returns the value, applying the function
-- to the output.
--
-- * @'runRWST' ('pass' m) r s = 'liftM' (\\ ((a, f), w) -> (a, f w)) ('runRWST' m r s)@
pass :: (Monad m) => RWST r w s m (a, w -> w) -> RWST r w s m a
pass m = RWST $ \ r s -> do
    ((a, f), s', w) <- runRWST m r s
    return (a, s', f w)
{-# INLINE pass #-}

-- | @'censor' f m@ is an action that executes the action @m@ and
-- applies the function @f@ to its output, leaving the return value
-- unchanged.
--
-- * @'censor' f m = 'pass' ('liftM' (\\ x -> (x,f)) m)@
--
-- * @'runRWST' ('censor' f m) r s = 'liftM' (\\ (a, w) -> (a, f w)) ('runRWST' m r s)@
censor :: (Monad m) => (w -> w) -> RWST r w s m a -> RWST r w s m a
censor f m = RWST $ \ r s -> do
    (a, s', w) <- runRWST m r s
    return (a, s', f w)
{-# INLINE censor #-}

-- ---------------------------------------------------------------------------
-- State operations

-- | Construct a state monad computation from a state transformer function.
state :: (Monoid w, Monad m) => (s -> (a,s)) -> RWST r w s m a
state f = RWST $ \ _ s -> case f s of (a,s') -> return (a, s', mempty)
{-# INLINE state #-}

-- | Fetch the current value of the state within the monad.
get :: (Monoid w, Monad m) => RWST r w s m s
get = RWST $ \ _ s -> return (s, s, mempty)
{-# INLINE get #-}

-- | @'put' s@ sets the state within the monad to @s@.
put :: (Monoid w, Monad m) => s -> RWST r w s m ()
put s = RWST $ \ _ _ -> return ((), s, mempty)
{-# INLINE put #-}

-- | @'modify' f@ is an action that updates the state to the result of
-- applying @f@ to the current state.
--
-- * @'modify' f = 'get' >>= ('put' . f)@
modify :: (Monoid w, Monad m) => (s -> s) -> RWST r w s m ()
modify f = RWST $ \ _ s -> return ((), f s, mempty)
{-# INLINE modify #-}

-- | Get a specific component of the state, using a projection function
-- supplied.
--
-- * @'gets' f = 'liftM' f 'get'@
gets :: (Monoid w, Monad m) => (s -> a) -> RWST r w s m a
gets f = RWST $ \ _ s -> return (f s, s, mempty)
{-# INLINE gets #-}

-- | Uniform lifting of a @callCC@ operation to the new monad.
-- This version rolls back to the original state on entering the
-- continuation.
liftCallCC :: (Monoid w) =>
    CallCC m (a,s,w) (b,s,w) -> CallCC (RWST r w s m) a b
liftCallCC callCC f = RWST $ \ r s ->
    callCC $ \ c ->
    runRWST (f (\ a -> RWST $ \ _ _ -> c (a, s, mempty))) r s
{-# INLINE liftCallCC #-}

-- | In-situ lifting of a @callCC@ operation to the new monad.
-- This version uses the current state on entering the continuation.
liftCallCC' :: (Monoid w) =>
    CallCC m (a,s,w) (b,s,w) -> CallCC (RWST r w s m) a b
liftCallCC' callCC f = RWST $ \ r s ->
    callCC $ \ c ->
    runRWST (f (\ a -> RWST $ \ _ s' -> c (a, s', mempty))) r s
{-# INLINE liftCallCC' #-}

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Catch e m (a,s,w) -> Catch e (RWST r w s m) a
liftCatch catchE m h =
    RWST $ \ r s -> runRWST m r s `catchE` \ e -> runRWST (h e) r s
{-# INLINE liftCatch #-}
