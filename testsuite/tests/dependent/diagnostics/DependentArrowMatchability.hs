{-# LANGUAGE DataKinds, GADTs, PolyKinds, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module DependentArrowMatchability where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import qualified DependentArrowProvider as P
import GHC.Exts (FUN, M, U, Matchability, Multiplicity(Many))

-- | Fix the written arrow to M from a datatype constructor.
type ImportedM = P.D (Type -> Type) Maybe
-- | Fix it to U from the imported abstract family.
type ImportedU = P.D (Type -> Type) P.Abstract

-- | Compare the inferred type with its explicit M form.
importedM :: ImportedM :~: P.D (FUN @M 'Many Type Type) Maybe
importedM = Refl

-- | Compare the inferred type with its explicit U form.
importedU :: ImportedU :~: P.D (FUN @U 'Many Type Type) P.Abstract
importedU = Refl

-- | A complete value signature checks the dependent telescope directly.
ordinary :: Proxy (P.D (Type -> Type) Maybe) -> Proxy ImportedM
ordinary = id

-- | Check both matchabilities in H98 constructor fields.
data Fields = Fields (P.D (Type -> Type) Maybe) (P.D (Type -> Type) P.Abstract)

-- | Preserve an explicitly quantified matchability in the GADT field.
data Scoped where
  Scoped :: forall (m :: Matchability).
            P.D (Type -> Type) (P.AbstractAny :: FUN @m 'Many Type Type) -> Scoped

-- | This use fixes the constructor's field at an arbitrary rigid m. The
-- declaration must not commit that field to either concrete matchability.
scope :: forall (m :: Matchability). Proxy m ->
         P.D (FUN @m 'Many Type Type) (P.AbstractAny :: FUN @m 'Many Type Type) -> Scoped
scope _ = Scoped @m

-- | A different declaration with no local m must keep the old U default.
type Outside = P.D (Type -> Type) P.AbstractAny
-- | Check the legacy default outside a quantified field.
outside :: Outside :~: P.D (FUN @U 'Many Type Type) P.AbstractAny
outside = Refl

-- | An unconstrained arrow in a quantified field keeps its ordinary U default.
data Quantified where
  Quantified :: forall a. Proxy a -> P.D (Type -> Type) P.AbstractAny -> Quantified

-- | Use the default from inside the constructor telescope.
quantified :: forall a. Proxy a -> P.D (FUN @U 'Many Type Type) P.AbstractAny -> Quantified
quantified = Quantified @a

-- | Decompose equality of two applications at a matchable head.
inverseM :: forall (f :: FUN @M 'Many Type Type) a b.
            Proxy f -> f a :~: f b -> a :~: b
inverseM _ Refl = Refl
