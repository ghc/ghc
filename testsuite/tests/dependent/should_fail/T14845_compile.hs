{-# Language PolyKinds, DataKinds, KindSignatures, GADTs, ConstraintKinds #-}
{-# Language FlexibleContexts #-}
{-# Language RankNTypes #-}
{-# Language TypeOperators #-}
module T14845 where

import Data.Kind
import Data.Type.Equality

data A :: Type -> Type where
  MkA1 :: a ~  Int => A a
  MkA2 :: a ~~ Int => A a

data SA :: forall a. A a -> Type where
  SMkA1 :: SA MkA1
  SMkA2 :: SA MkA2
