{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.State.Strict
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict state monads, passing an updatable state through a computation.
-- See below for examples.
--
-- Some computations may not require the full power of state transformers:
--
-- * For a read-only state, see "Control.Monad.Trans.Reader".
--
-- * To accumulate a value without using it on the way, see
--   "Control.Monad.Trans.Writer".
--
-- In this version, sequencing of computations is strict (but computations
-- are not strict in the state unless you force it with 'seq' or the like).
-- For a lazy version with the same interface, see
-- "Control.Monad.Trans.State.Lazy".
-----------------------------------------------------------------------------

module Control.Monad.Trans.State.Strict (
    -- * The State monad
    State,
    state,
    runState,
    evalState,
    execState,
    mapState,
    withState,
    -- * The StateT monad transformer
    StateT(..),
    evalStateT,
    execStateT,
    mapStateT,
    withStateT,
    -- * State operations
    get,
    put,
    modify,
    modify',
    gets,
    -- * Lifting other operations
    liftCallCC,
    liftCallCC',
    liftCatch,
    liftListen,
    liftPass,
    -- * Examples
    -- ** State monads
    -- $examples

    -- ** Counting
    -- $counting

    -- ** Labelling trees
    -- $labelling
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

-- ---------------------------------------------------------------------------
-- | A state monad parameterized by the type @s@ of the state to carry.
--
-- The 'return' function leaves the state unchanged, while @>>=@ uses
-- the final state of the first computation as the initial state of
-- the second.
type State s = StateT s Identity

-- | Construct a state monad computation from a function.
-- (The inverse of 'runState'.)
state :: (Monad m)
      => (s -> (a, s))  -- ^pure state transformer
      -> StateT s m a   -- ^equivalent state-passing computation
state f = StateT (return . f)
{-# INLINE state #-}

-- | Unwrap a state monad computation as a function.
-- (The inverse of 'state'.)
runState :: State s a   -- ^state-passing computation to execute
         -> s           -- ^initial state
         -> (a, s)      -- ^return value and final state
runState m = runIdentity . runStateT m
{-# INLINE runState #-}

-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
--
-- * @'evalState' m s = 'fst' ('runState' m s)@
evalState :: State s a  -- ^state-passing computation to execute
          -> s          -- ^initial value
          -> a          -- ^return value of the state computation
evalState m s = fst (runState m s)
{-# INLINE evalState #-}

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
--
-- * @'execState' m s = 'snd' ('runState' m s)@
execState :: State s a  -- ^state-passing computation to execute
          -> s          -- ^initial value
          -> s          -- ^final state
execState m s = snd (runState m s)
{-# INLINE execState #-}

-- | Map both the return value and final state of a computation using
-- the given function.
--
-- * @'runState' ('mapState' f m) = f . 'runState' m@
mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f = mapStateT (Identity . f . runIdentity)
{-# INLINE mapState #-}

-- | @'withState' f m@ executes action @m@ on a state modified by
-- applying @f@.
--
-- * @'withState' f m = 'modify' f >> m@
withState :: (s -> s) -> State s a -> State s a
withState = withStateT
{-# INLINE withState #-}

-- ---------------------------------------------------------------------------
-- | A state transformer monad parameterized by:
--
--   * @s@ - The state.
--
--   * @m@ - The inner monad.
--
-- The 'return' function leaves the state unchanged, while @>>=@ uses
-- the final state of the first computation as the initial state of
-- the second.
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
--
-- * @'evalStateT' m s = 'liftM' 'fst' ('runStateT' m s)@
evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
    (a, _) <- runStateT m s
    return a
{-# INLINE evalStateT #-}

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
--
-- * @'execStateT' m s = 'liftM' 'snd' ('runStateT' m s)@
execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s = do
    (_, s') <- runStateT m s
    return s'
{-# INLINE execStateT #-}

-- | Map both the return value and final state of a computation using
-- the given function.
--
-- * @'runStateT' ('mapStateT' f m) = f . 'runStateT' m@
mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ f . runStateT m
{-# INLINE mapStateT #-}

-- | @'withStateT' f m@ executes action @m@ on a state modified by
-- applying @f@.
--
-- * @'withStateT' f m = 'modify' f >> m@
withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT f m = StateT $ runStateT m . f
{-# INLINE withStateT #-}

instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ \ s ->
        fmap (\ (a, s') -> (f a, s')) $ runStateT m s
    {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (a, s)
    {-# INLINE pure #-}
    StateT mf <*> StateT mx = StateT $ \ s -> do
        (f, s') <- mf s
        (x, s'') <- mx s'
        return (f x, s'')
    {-# INLINE (<*>) #-}
    m *> k = m >>= \_ -> k
    {-# INLINE (*>) #-}

instance (Functor m, MonadPlus m) => Alternative (StateT s m) where
    empty = StateT $ \ _ -> mzero
    {-# INLINE empty #-}
    StateT m <|> StateT n = StateT $ \ s -> m s `mplus` n s
    {-# INLINE (<|>) #-}

instance (Monad m) => Monad (StateT s m) where
#if !(MIN_VERSION_base(4,8,0))
    return a = StateT $ \ s -> return (a, s)
    {-# INLINE return #-}
#endif
    m >>= k  = StateT $ \ s -> do
        (a, s') <- runStateT m s
        runStateT (k a) s'
    {-# INLINE (>>=) #-}
    fail str = StateT $ \ _ -> fail str
    {-# INLINE fail #-}

#if MIN_VERSION_base(4,9,0)
instance (Fail.MonadFail m) => Fail.MonadFail (StateT s m) where
    fail str = StateT $ \ _ -> Fail.fail str
    {-# INLINE fail #-}
#endif

instance (MonadPlus m) => MonadPlus (StateT s m) where
    mzero       = StateT $ \ _ -> mzero
    {-# INLINE mzero #-}
    StateT m `mplus` StateT n = StateT $ \ s -> m s `mplus` n s
    {-# INLINE mplus #-}

instance (MonadFix m) => MonadFix (StateT s m) where
    mfix f = StateT $ \ s -> mfix $ \ ~(a, _) -> runStateT (f a) s
    {-# INLINE mfix #-}

instance MonadTrans (StateT s) where
    lift m = StateT $ \ s -> do
        a <- m
        return (a, s)
    {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

-- | Fetch the current value of the state within the monad.
get :: (Monad m) => StateT s m s
get = state $ \ s -> (s, s)
{-# INLINE get #-}

-- | @'put' s@ sets the state within the monad to @s@.
put :: (Monad m) => s -> StateT s m ()
put s = state $ \ _ -> ((), s)
{-# INLINE put #-}

-- | @'modify' f@ is an action that updates the state to the result of
-- applying @f@ to the current state.
--
-- * @'modify' f = 'get' >>= ('put' . f)@
modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = state $ \ s -> ((), f s)
{-# INLINE modify #-}

-- | A variant of 'modify' in which the computation is strict in the
-- new state.
--
-- * @'modify'' f = 'get' >>= (('$!') 'put' . f)@
modify' :: (Monad m) => (s -> s) -> StateT s m ()
modify' f = do
    s <- get
    put $! f s
{-# INLINE modify' #-}

-- | Get a specific component of the state, using a projection function
-- supplied.
--
-- * @'gets' f = 'liftM' f 'get'@
gets :: (Monad m) => (s -> a) -> StateT s m a
gets f = state $ \ s -> (f s, s)
{-# INLINE gets #-}

-- | Uniform lifting of a @callCC@ operation to the new monad.
-- This version rolls back to the original state on entering the
-- continuation.
liftCallCC :: CallCC m (a,s) (b,s) -> CallCC (StateT s m) a b
liftCallCC callCC f = StateT $ \ s ->
    callCC $ \ c ->
    runStateT (f (\ a -> StateT $ \ _ -> c (a, s))) s
{-# INLINE liftCallCC #-}

-- | In-situ lifting of a @callCC@ operation to the new monad.
-- This version uses the current state on entering the continuation.
-- It does not satisfy the uniformity property (see "Control.Monad.Signatures").
liftCallCC' :: CallCC m (a,s) (b,s) -> CallCC (StateT s m) a b
liftCallCC' callCC f = StateT $ \ s ->
    callCC $ \ c ->
    runStateT (f (\ a -> StateT $ \ s' -> c (a, s'))) s
{-# INLINE liftCallCC' #-}

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Catch e m (a,s) -> Catch e (StateT s m) a
liftCatch catchE m h =
    StateT $ \ s -> runStateT m s `catchE` \ e -> runStateT (h e) s
{-# INLINE liftCatch #-}

-- | Lift a @listen@ operation to the new monad.
liftListen :: (Monad m) => Listen w m (a,s) -> Listen w (StateT s m) a
liftListen listen m = StateT $ \ s -> do
    ((a, s'), w) <- listen (runStateT m s)
    return ((a, w), s')
{-# INLINE liftListen #-}

-- | Lift a @pass@ operation to the new monad.
liftPass :: (Monad m) => Pass w m (a,s) -> Pass w (StateT s m) a
liftPass pass m = StateT $ \ s -> pass $ do
    ((a, f), s') <- runStateT m s
    return ((a, s'), f)
{-# INLINE liftPass #-}

{- $examples

Parser from ParseLib with Hugs:

> type Parser a = StateT String [] a
>    ==> StateT (String -> [(a,String)])

For example, item can be written as:

> item = do (x:xs) <- get
>        put xs
>        return x
>
> type BoringState s a = StateT s Identity a
>      ==> StateT (s -> Identity (a,s))
>
> type StateWithIO s a = StateT s IO a
>      ==> StateT (s -> IO (a,s))
>
> type StateWithErr s a = StateT s Maybe a
>      ==> StateT (s -> Maybe (a,s))

-}

{- $counting

A function to increment a counter.
Taken from the paper \"Generalising Monads to Arrows\",
John Hughes (<http://www.cse.chalmers.se/~rjmh/>), November 1998:

> tick :: State Int Int
> tick = do n <- get
>           put (n+1)
>           return n

Add one to the given number using the state monad:

> plusOne :: Int -> Int
> plusOne n = execState tick n

A contrived addition example. Works only with positive numbers:

> plus :: Int -> Int -> Int
> plus n x = execState (sequence $ replicate n tick) x

-}

{- $labelling

An example from /The Craft of Functional Programming/, Simon
Thompson (<http://www.cs.kent.ac.uk/people/staff/sjt/>),
Addison-Wesley 1999: \"Given an arbitrary tree, transform it to a
tree of integers in which the original elements are replaced by
natural numbers, starting from 0.  The same element has to be
replaced by the same number at every occurrence, and when we meet
an as-yet-unvisited element we have to find a \'new\' number to match
it with:\"

> data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Eq)
> type Table a = [a]

> numberTree :: Eq a => Tree a -> State (Table a) (Tree Int)
> numberTree Nil = return Nil
> numberTree (Node x t1 t2) = do
>     num <- numberNode x
>     nt1 <- numberTree t1
>     nt2 <- numberTree t2
>     return (Node num nt1 nt2)
>   where
>     numberNode :: Eq a => a -> State (Table a) Int
>     numberNode x = do
>         table <- get
>         case elemIndex x table of
>             Nothing -> do
>                 put (table ++ [x])
>                 return (length table)
>             Just i -> return i

numTree applies numberTree with an initial state:

> numTree :: (Eq a) => Tree a -> Tree Int
> numTree t = evalState (numberTree t) []

> testTree = Node "Zero" (Node "One" (Node "Two" Nil Nil) (Node "One" (Node "Zero" Nil Nil) Nil)) Nil
> numTree testTree => Node 0 (Node 1 (Node 2 Nil Nil) (Node 1 (Node 0 Nil Nil) Nil)) Nil

-}
