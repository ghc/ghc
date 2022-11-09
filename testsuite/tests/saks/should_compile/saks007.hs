{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies, GADTs, PolyKinds, DataKinds, ExplicitForAll #-}

-- See also: saks007_fail.hs
module SAKS_007 where

import Data.Kind (Type, Constraint)

type family F a where { F Type = True; F _ = False }
type family G a where { G Type = False; G _ = True }

type X :: forall k1 k2. (F k1 ~ G k2) => k1 -> k2 -> Type
data X a b where
  MkX :: X Integer Maybe   -- OK: F Type ~ G (Type -> Type)
                           --     True ~ True


{-
Let   co :: F Type ~ G (Type->Type)

Wrapper data con type:
  $WMkX :: X @Type @(Type->Type) @(Eq# co) Integer Maybe

Worker data con's type:
  MkX :: forall k1 k2 (c :: F k1 ~ G k2) (a :: k1) (b :: k2)
      -> forall .   -- No existentials
         ( k1 ~# Type, k2 ~# Type->Type     -- EqSpec
         , a ~# Integer, b ~# Maybe )
      => X k1 k2 c a b

f :: forall k. (k ~ Type) => forall (a::k). a->a


f :: forall (cv :: a ~# b) => ....ty|>co....


X @kk1 @kk2 @(d :: F kk1 ~ G kk2) Integer Maybe


-}