{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators #-}

module Util(
    Action(..), act, actM, actMT,
    (:->)(..), -- GHC bug that I can't just export (:->)
    (!), one,
    Cache, runCache, cache, askCache,
    T
    ) where

import Control.Monad.State
import Data.Tuple.Extra


newtype T = T Int deriving (Eq,Ord)

instance Enum T where
    succ (T x) = T (x + 1)
    toEnum = error "toEnum T"
    fromEnum = error "fromEnum T"


---------------------------------------------------------------------
-- ACTION

data Action k v = Done v
                | Need k (v -> Action k v)

act :: (k -> v) -> Action k v -> v
act f (Done v) = v
act f (Need k c) = act f $ c $ f k

actM :: Monad m => (k -> m v) -> Action k v -> m v
actM f (Done v) = pure v
actM f (Need k c) = actM f . c =<< f k

actMT :: Monad m => (k -> m v) -> Action k v -> m (v, [(k,v)])
actMT f (Done v) = pure (v, [])
actMT f (Need k c) = do
    v <- f k
    second ((k,v):) `liftM` actMT f (c v)


---------------------------------------------------------------------
-- ASSOCIATION

newtype k :-> v = Assoc [(k,v)]
    deriving (Semigroup, Monoid)


(!) :: Eq k => (k :-> v) -> k -> Maybe v
(!) (Assoc xs) k = lookup k xs

one :: k -> v -> (k :-> v)
one k v = Assoc [(k,v)]


---------------------------------------------------------------------
-- CACHE

newtype Cache k v a = Cache (State (k :-> v) a)
    deriving (Functor, Applicative, Monad)

askCache :: Eq k => k -> Cache k v (Maybe v)
askCache k = Cache $ gets (! k)

cache :: Eq k => k -> Cache k v v -> Cache k v v
cache k act = do
    v <- askCache k
    case v of
        Just v -> pure v
        Nothing -> do
            v <- act
            Cache $ modify $ mappend $ one k v
            pure v

runCache :: Cache k v a -> (k :-> v) -> (a, k :-> v)
runCache (Cache s) = runState s
