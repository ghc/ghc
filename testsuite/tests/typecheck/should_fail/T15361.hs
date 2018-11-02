{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module T15361 where

import Data.Kind
import Data.Type.Equality

-- Don't report (* ~ *) here
foo :: forall (a :: Type) (b :: Type) (c :: Type).
       a :~~: b -> a :~~: c
foo HRefl = HRefl

data Chumbawamba :: Type -> Type where
  IGetKnockedDown :: (Eq a, Ord a) => a -> Chumbawamba a

-- Don't report (Eq a) here
goo :: Chumbawamba a -> String
goo (IGetKnockedDown x) = show x
