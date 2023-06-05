{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module T23454 where

import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Type.Equality
import GHC.TypeNats

bogus :: forall a b . KnownNat a => a :~: b
bogus = case testEquality (SNat @a) (coerce (SNat @a) :: SNat b) of
          Just r  -> r
          Nothing -> error "bug fixed"

type G :: Nat -> Type -> Type -> Type
type family G n s t where
  G 0 s _ = s
  G _ _ t = t

newtype N n s t = MkN { unN :: G n s t }

oops :: forall b s t . N 0 s t -> N b s t
oops x = gcastWith (bogus @0 @b) x

unsafeCoerce :: s -> t
unsafeCoerce x = unN (oops @1 (MkN x))
