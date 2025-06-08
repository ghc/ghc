{-# Language ConstraintKinds          #-}
{-# Language FlexibleContexts         #-}
{-# Language FlexibleInstances        #-}
{-# Language GADTs                    #-}
{-# Language ImpredicativeTypes       #-}
{-# Language InstanceSigs             #-}
{-# Language MultiParamTypeClasses    #-}
{-# Language PolyKinds                #-}
{-# Language PolyKinds                #-}
{-# Language PolyKinds                #-}
{-# Language QuantifiedConstraints    #-}
{-# Language ScopedTypeVariables      #-}
{-# Language StandaloneKindSignatures #-}
{-# Language TypeApplications         #-}
{-# Language TypeFamilies             #-}

module T21323 where

import Data.Kind
import Data.Coerce
import Data.Type.Coercion
import GHC.Generics

data Dict cls where
 Dict :: cls => Dict cls

type    ViaRep :: Type -> (k -> Type) -> Type
newtype a `ViaRep` rep = ViaRep a

class    Coercible (Rep a x) (rep x) => AuxCoercible a rep x
instance Coercible (Rep a x) (rep x) => AuxCoercible a rep x

type ForallAuxCoercible :: Type -> (Type -> Type) -> Constraint
type ForallAuxCoercible a rep = forall x. AuxCoercible a rep x

instance (Generic a, ForallAuxCoercible a rep) => Generic (ViaRep a rep) where
 type Rep (ViaRep a rep) = rep

 from :: forall x. ViaRep a rep -> rep x
 from (ViaRep a) = co undefined (from @a @x a) where -- coerce (from @a @x a) where

  co :: Coercion (Rep a x) (rep x) -> Rep a x -> rep x
  co = undefined

  c :: Dict (ForallAuxCoercible a rep)
  c = Dict

-- This caused a Core Lint error on GHC 9.0:
--
--    Out of scope: irred_a1LI :: cls_a1LH[tau:3]
--                  [LclId]
--    In the RHS of $cfrom_a1KZ :: forall a (rep :: * -> *) x.
--                                 (Generic a, ForallAuxCoercible a rep) =>
--                                 ViaRep a rep -> Rep (ViaRep a rep) x

 to = undefined
