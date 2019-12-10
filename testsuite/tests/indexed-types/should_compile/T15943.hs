{-# Language RankNTypes             #-}
{-# Language DataKinds              #-}
{-# Language KindSignatures         #-}
{-# Language PolyKinds              #-}
{-# Language TypeFamilyDependencies #-}
{-# Language GADTs                  #-}
{-# Language TypeSynonymInstances   #-}
{-# Language FlexibleInstances      #-}
{-# Language QuantifiedConstraints  #-}
{-# Language FlexibleContexts       #-}

module T15943 where

import Data.Type.Equality
import Data.Coerce
import Data.Type.Coercion
import Data.Kind

newtype WrapFalse a b = WrapFalse (Hom False a b)
newtype WrapTrue  a b = WrapTrue  (Hom True  a b)

class
  (forall (x :: ob) (y :: ob). Coercible (WrapFalse x y) (WrapTrue y x))
  =>
  Ríki ob where

  type Hom (or::Bool) = (res :: ob -> ob -> Type) | res -> or

instance Ríki Type where
  type Hom False = (->)
  type Hom True  = Op

newtype Op :: Type -> Type -> Type where
  Op :: (b -> a) -> Op a b
