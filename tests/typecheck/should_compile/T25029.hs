{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}

module T25029 where

import           Data.Kind    (Type)
import           GHC.TypeLits (Nat)

type family RankedOf2 (f :: Type -> Type) :: Type -> Type
type family PrimalOf2 (f :: Type -> Type) :: Type -> Type

rrev :: forall r r1 u. (Floating r, Floating r1)
     => (forall f. ( RankedOf2 (PrimalOf2 f) ~ PrimalOf2 f
                   , (forall r2. Floating r2 => Floating (f r2))
--                   , f r1 ~ f u
                   )
         => f r -> f r1)
     -> ()
rrev f = ()

-- fails
testSin0RrevPP1 :: ()
testSin0RrevPP1 = rrev @Double sin

-- works
testSin0RrevPP2 :: ()
testSin0RrevPP2 = rrev @Double @Double sin
