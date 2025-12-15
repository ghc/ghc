{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}

module UnlDataInvalidResKind2 where

import GHC.Exts
import GHC.Types

type Interpret :: Bool -> RuntimeRep
type family Interpret b = r | r -> b where
  Interpret True  = BoxedRep Lifted
  Interpret False = BoxedRep Unlifted

-- Not allowed, although well-typed after type fam reduction
type A :: TYPE (Interpret b)
data A = MkA Int
