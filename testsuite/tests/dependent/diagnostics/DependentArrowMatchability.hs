{-# LANGUAGE DataKinds, GADTs, Modifiers, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
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

-- | Apply the callback at both arrows selected by its provider.
type CallbackResults = (P.Callback False Int, P.Callback True Int)

-- | Compare the inferred type with its explicit M form.
importedM :: ImportedM :~: P.D (FUN @M 'Many Type Type) Maybe
importedM = Refl

-- | Compare the inferred type with its explicit U form.
importedU :: ImportedU :~: P.D (FUN @U 'Many Type Type) P.Abstract
importedU = Refl

-- | A complete value signature checks the dependent telescope directly.
ordinary :: Proxy (P.D (Type -> Type) Maybe) -> Proxy ImportedM
ordinary = id

-- | Infer the callback's kind from its bare use, in either order.
type AppliedFirst f = (f Bool, P.D (Type %U -> Type) f)
type BareFirst f = (P.D (Type %U -> Type) f, f Bool)

mixedUses :: AppliedFirst P.Abstract :~: (P.Abstract Bool, P.D (Type %U -> Type) P.Abstract)
mixedUses = Refl

mixedUsesReversed :: BareFirst P.Abstract :~: (P.D (Type %U -> Type) P.Abstract, P.Abstract Bool)
mixedUsesReversed = Refl

-- | Infer the callback index with unsaturation. Otherwise it defaults to M.
type AppliedDefault f = (Proxy f, f Bool)

appliedDefault :: AppliedDefault Maybe :~: (Proxy Maybe, Maybe Bool)
appliedDefault = Refl

-- | Use the known kind of a scoped variable in its annotation.
ascribedLocalU :: forall (f :: Type %U -> Type). Proxy f -> Proxy (f :: Type -> Type)
ascribedLocalU = id

-- | Fix the bare kind arrow to U from the annotated subject.
ascribedImportedU :: Proxy (P.Abstract :: Type -> Type) -> Proxy P.Abstract
ascribedImportedU = id

-- | Fix the same kind arrow to M from a datatype constructor.
ascribedImportedM :: Proxy (Maybe :: Type -> Type) -> Proxy Maybe
ascribedImportedM = id

-- | Keep the M default when the subject has no known arrow.
ascribedDefault :: forall f. Proxy (f :: Type -> Type) -> Proxy f
ascribedDefault = id

ascribedDefaultM :: Proxy Maybe -> Proxy Maybe
ascribedDefaultM = ascribedDefault @Maybe

-- | Let the body of the complete signature select the binder's arrow.
laterBinder :: forall (f :: Type -> Type). Proxy (P.D (Type %U -> Type) f) -> Proxy f -> Proxy f
laterBinder _ = id

-- | Keep the M default when the signature leaves the binder's arrow open.
binderDefault :: forall (f :: Type -> Type). Proxy f -> Proxy f
binderDefault = id

binderDefaultM :: Proxy Maybe -> Proxy Maybe
binderDefaultM = binderDefault @Maybe

-- | A callback application selects U for the inner arrow only.
nestedBinder :: forall (g :: (Type -> Type) -> Type).
                Proxy (g P.Abstract) -> Proxy g -> Proxy g
nestedBinder _ = id

-- | A constructor uses the outer M default.
data NestedCode (f :: Type %U -> Type)

nestedDefault :: Proxy (NestedCode P.Abstract) -> Proxy NestedCode -> Proxy NestedCode
nestedDefault = nestedBinder @NestedCode

-- | A U outer arrow can also be inferred when a use requires it.
nestedBoth :: forall (g :: (Type -> Type) -> Type).
              Proxy (P.D ((Type %U -> Type) %U -> Type) g) -> Proxy g -> Proxy g
nestedBoth _ = id

-- | Result arrows use the same inference as domain arrows.
nestedResult :: forall (g :: Type -> Type -> Type).
                Proxy (P.D (Type %M -> Type %U -> Type) g) -> Proxy g -> Proxy g
nestedResult _ = id

-- | An earlier binder can also fix the arrow in the same telescope.
scopedBinder :: forall (m :: Matchability) (f :: Type -> Type).
                Proxy (P.D (FUN @m 'Many Type Type) f) -> Proxy f -> Proxy f
scopedBinder _ = id

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

-- | Distinguish an encoded arrow from a runtime arrow.
data ArrowCode (a :: Type) (b :: Type)

-- | Use a matchable arrow for an encoded callback.
type a ~> b = ArrowCode a b %M -> Type

-- | Store the encoded callback.
data Encoded a = Encode (a ~> Type)

-- | Store the ordinary callback.
data Decoded a = Decode (a -> Type)

-- | Demote the kind of a type.
type family Demote (k :: Type) :: Type

-- | Demote a type at its demoted kind.
type family DemoteType (x :: k) :: Demote k

type instance Demote Type = Type
type instance DemoteType (a :: Type) = Demote a
type instance Demote (a ~> b) = DemoteType a -> DemoteType b
type instance Demote (Encoded a) = Decoded (DemoteType a)

-- The decoded field fixes the callback arrow to U. Its domain must
-- agree with the parameter in the demoted wrapper kind.
type instance DemoteType ('Encode x :: Encoded a) = 'Decode (DemoteType x)
