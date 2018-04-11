{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module T13938 where

import T13938a
import Data.Kind
import Data.Type.Equality
import GHC.TypeLits

type family Length (l :: [a]) :: Nat where {}
type family Map (f :: a ~> b) (l :: [a]) :: [b] where {}

type WhyMapPreservesLength (f :: x ~> y) (l :: [x])
  = Length l :~: Length (Map f l)
data WhyMapPreservesLengthSym1 (f :: x ~> y) :: [x] ~> Type
type instance Apply (WhyMapPreservesLengthSym1 f) l = WhyMapPreservesLength f l

mapPreservesLength :: forall (x :: Type) (y :: Type) (f :: x ~> y) (l :: [x]).
                      Length l :~: Length (Map f l)
mapPreservesLength
  = elimListTyFun @x @(WhyMapPreservesLengthSym1 f) @l undefined undefined undefined
