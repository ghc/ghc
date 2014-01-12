{-# LANGUAGE KindSignatures, GADTs, TypeFamilies, MultiParamTypeClasses,  FlexibleContexts, ScopedTypeVariables, TypeSynonymInstances,  FlexibleInstances #-}  
module T5303( showContextSeries ) where

import Control.Monad.State.Strict( StateT )  
import Control.Monad.Trans ( lift )

data Tree (m :: * -> *) = Tree {}

data FL (a :: * -> * -> *) x z where
    (:>:) :: a x y -> FL a y z -> FL a x z
    NilFL :: FL a x x

class (Functor m, Monad m) => ApplyMonad m (state :: (* -> *) -> *)

class Apply (p :: * -> * -> *) where
    type ApplyState p :: (* -> *) -> *
    apply :: ApplyMonad m (ApplyState p) => p x y -> m ()

class (Functor m, Monad m, ApplyMonad (ApplyMonadOver m state) state)
      => ApplyMonadTrans m (state :: (* -> *) -> *) where
  type ApplyMonadOver m state :: * -> *
  runApplyMonad :: (ApplyMonadOver m state) x -> state m -> m (x, state m)

instance (Functor m, Monad m) => ApplyMonadTrans m Tree where
  type ApplyMonadOver m Tree = TreeMonad m
  runApplyMonad = virtualTreeMonad

instance (Functor m, Monad m) => ApplyMonad (TreeMonad m) Tree

-- | Internal state of the 'TreeIO' monad. Keeps track of the current Tree
-- content, unsync'd changes and a current working directory (of the  monad).
data TreeState m = TreeState { tree :: !(Tree m) }  
type TreeMonad m = StateT (TreeState m) m  
type TreeIO = TreeMonad IO

virtualTreeMonad :: (Functor m, Monad m) => TreeMonad m a -> Tree m -> m  (a, Tree m)  
virtualTreeMonad action t = undefined

applyToState :: forall p m x y. (Apply p, ApplyMonadTrans m (ApplyState p))
             => p x y -> (ApplyState p) m -> m ((ApplyState p) m)  
applyToState _ _ = snd `fmap` runApplyMonad undefined undefined

showContextSeries :: (Apply p, ApplyState p ~ Tree) => FL p x y -> TreeIO ()
showContextSeries (p:>:_) = (undefined >>= lift . applyToState p) >>  return ()
