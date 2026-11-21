{-# LANGUAGE CPP, DataKinds, TypeFamilies, ExperimentalDependentTypes, GADTs, KindSignatures, TypeOperators #-}
module RetainedTuple (observe, keep, reflexive) where
import Data.Type.Equality ((:~:)(Refl))
import TupleProvider

observe :: foreach (t :: Tuple Bool Bool) -> (At t, (Bool, Bool))
observe t = (At, fields t)

keep :: foreach (t :: Tuple Bool Bool) -> At t
keep _ = At

reflexive :: foreach (t :: Tuple Bool Bool) -> t :~: t
reflexive _ = Refl

-- Opaque identity does not depend on a selected family instance.
data family Open a

keepOpen :: foreach (t :: Open Bool) -> t :~: t
keepOpen _ = Refl

#ifdef WRONG_IDENTITY
wrong :: foreach (x :: Tuple Bool Bool) -> foreach (y :: Tuple Bool Bool) -> x :~: y
wrong _ _ = Refl
#endif

#ifdef STUCK_TYPE_FAMILY
type family Reduce a
unresolved :: foreach (t :: Reduce Bool) -> t :~: t
unresolved _ = Refl
#endif
