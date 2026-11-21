{-# LANGUAGE CPP, DataKinds, TypeFamilies, ExperimentalDependentTypes #-}
module RetainedConstructor (emitted, lazy, withNatural) where

import Numeric.Natural (Natural)
import RetainedTuple (keep, observe)
import TupleProvider

{-# INLINE emitted #-}
emitted :: () -> (At ('Tup2 'True 'False), (Bool, Bool))
emitted () = observe (Tup2 True False)

{-# INLINE lazy #-}
lazy :: Bool -> Bool
lazy x = case keep (Tup2 x False) of At -> True

observeNatural :: foreach (t :: Tuple Natural Bool) -> (At t, (Natural, Bool))
observeNatural t = (At, fields t)

{-# INLINE withNatural #-}
withNatural :: () -> (At ('Tup2 0 'True), (Natural, Bool))
withNatural () = observeNatural (Tup2 0 True)

#ifdef WRONG_INDEX
wrong :: At ('Tup2 'True 'False)
wrong = keep (Tup2 False True)
#endif

#ifdef STRICT_FIELD
data family StrictTuple a
data instance StrictTuple Bool = StrictTuple !Bool

keepStrict :: foreach (t :: StrictTuple Bool) -> At t
keepStrict _ = At

strict = keepStrict (StrictTuple True)
#endif
