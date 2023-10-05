{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, KindSignatures, 
    GADTs, FlexibleInstances, FlexibleContexts #-}
module T5591b where

import Data.Kind (Type)

class Monad m => Effect p e r m | p e m -> r where
  fin :: p e m -> e -> m r

data ErrorEff :: Type -> (Type -> Type) -> Type where
  CatchError :: (e -> m a) -> ErrorEff ((e -> m a) -> m a) m

instance Monad m => Effect ErrorEff ((e -> m a) -> m a) a m where
  fin (CatchError h) = \f -> f h
