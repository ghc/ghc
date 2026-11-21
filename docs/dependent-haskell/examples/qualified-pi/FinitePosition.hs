{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneKindSignatures, TypeApplications, TypeOperators #-}
module FinitePosition (FinView(..), viewFin, constructedViews, zeroView, zeroViewInlinable) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import PackedVecLibrary (Nat(..))
import ParameterizedIndexLibrary (Fin(..))

-- | Distinguish zero from the successor of the actual stored position.
type FinView :: forall n. Fin n -> Type
data FinView i where
    AtZero :: FinView 'FZ
    AtSucc :: forall n (i :: Fin ('S n)).
              (some (p :: Fin n) | 'FS p :~: i) -> FinView i

-- | Expose one position constructor without demanding its predecessor.
{-# INLINE viewFin #-}
viewFin :: forall n. foreach (i :: Fin n) -> FinView i
viewFin i = case i of
    FZ -> AtZero
    FS p -> let proof :: 'FS p :~: 'FS p
                proof = Refl
            in AtSucc @_ @('FS p) (Pair p proof)

-- | View the first position and the successor of the supplied position.
{-# INLINE constructedViews #-}
constructedViews :: forall n. foreach (p :: Fin n) -> (FinView ('FZ @n), FinView ('FS p))
constructedViews p = (viewFin @('S n) FZ, viewFin @('S n) (FS p))

-- | Keep a retained call in both exported templates.
{-# NOINLINE inspectPosition #-}
inspectPosition :: forall n. foreach (i :: Fin n) -> FinView i
inspectPosition i = viewFin i

-- | Expose the retained call in an ordinary INLINE template.
{-# INLINE zeroView #-}
zeroView :: forall n. FinView ('FZ @n)
zeroView = inspectPosition @('S n) FZ

-- | Keep size guidance for the same retained call.
{-# INLINABLE zeroViewInlinable #-}
zeroViewInlinable :: forall n. FinView ('FZ @n)
zeroViewInlinable = inspectPosition @('S n) FZ
