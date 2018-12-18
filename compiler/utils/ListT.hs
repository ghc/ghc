{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.Logic
-- Copyright   : (c) Dan Doel
-- License     : BSD3
--
-- Maintainer  : dan.doel@gmail.com
-- Stability   : experimental
-- Portability : non-portable (multi-parameter type classes)
--
-- A backtracking, logic programming monad.
--
--    Adapted from the paper
--    /Backtracking, Interleaving, and Terminating
--        Monad Transformers/, by
--    Oleg Kiselyov, Chung-chieh Shan, Daniel P. Friedman, Amr Sabry
--    (<http://www.cs.rutgers.edu/~ccshan/logicprog/ListT-icfp2005.pdf>).
-------------------------------------------------------------------------

module ListT (
    ListT(..),
    runListT,
    select,
    fold
  ) where

import GhcPrelude

import Control.Applicative

import Control.Monad
import Control.Monad.Fail as MonadFail

-------------------------------------------------------------------------
-- | A monad transformer for performing backtracking computations
-- layered over another monad 'm'
newtype ListT m a =
    ListT { unListT :: forall r. (a -> m r -> m r) -> m r -> m r }

select :: Monad m => [a] -> ListT m a
select xs = foldr (<|>) mzero (map pure xs)

fold :: ListT m a -> (a -> m r -> m r) -> m r -> m r
fold = runListT

-------------------------------------------------------------------------
-- | Runs a ListT computation with the specified initial success and
-- failure continuations.
runListT :: ListT m a -> (a -> m r -> m r) -> m r -> m r
runListT = unListT

instance Functor (ListT f) where
    fmap f lt = ListT $ \sk fk -> unListT lt (sk . f) fk

instance Applicative (ListT f) where
    pure a = ListT $ \sk fk -> sk a fk
    f <*> a = ListT $ \sk fk -> unListT f (\g fk' -> unListT a (sk . g) fk') fk

instance Alternative (ListT f) where
    empty = ListT $ \_ fk -> fk
    f1 <|> f2 = ListT $ \sk fk -> unListT f1 sk (unListT f2 sk fk)

instance Monad (ListT m) where
    m >>= f = ListT $ \sk fk -> unListT m (\a fk' -> unListT (f a) sk fk') fk
#if !MIN_VERSION_base(4,13,0)
    fail = MonadFail.fail
#endif

instance MonadFail.MonadFail (ListT m) where
    fail _ = ListT $ \_ fk -> fk

instance MonadPlus (ListT m) where
    mzero = ListT $ \_ fk -> fk
    m1 `mplus` m2 = ListT $ \sk fk -> unListT m1 sk (unListT m2 sk fk)
