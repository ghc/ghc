{-# LANGUAGE ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
-- | Symmetry and transitivity from the PLFH Isomorphism chapter.
module BookIsoComposition (isoSym, isoTrans) where

import BookIsomorphism (Iso(..))
import Data.Type.Equality ((:~:), trans)
import HaskLib (cong)

-- | Reverse an isomorphism by exchanging the functions and their proofs.
isoSym :: Iso a b -> Iso b a
isoSym (MkIso f g gf fg) = MkIso g f fg gf

-- | Cancel the inner round trip before applying the outer inverse law.
composeLeft :: foreach (f :: a -> b) -> foreach (g :: b -> a)
            -> foreach (h :: b -> c) -> foreach (k :: c -> b)
            -> (foreach (x :: a) -> g (f x) :~: x)
            -> (foreach (y :: b) -> k (h y) :~: y)
            -> foreach (x :: a) -> g (k (h (f x))) :~: x
composeLeft f g _ _ gf kh x = trans (cong @g (kh (f x))) (gf x)

-- | Compose two isomorphisms and their round-trip proofs.
isoTrans :: forall a b c. Iso a b -> Iso b c -> Iso a c
isoTrans (MkIso f g gf fg) (MkIso h k kh hk) =
    MkIso @a @c (h . f) (g . k)
        (composeLeft f g h k gf kh)
        (composeLeft k h g f hk fg)
