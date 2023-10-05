{-# LANGUAGE PatternSynonyms, ViewPatterns, ScopedTypeVariables #-}

module T23038 where

import GHC.Types( Any )
import Unsafe.Coerce( unsafeCoerce )

pattern N1 :: forall a. () => forall. () => a -> Any
pattern N1 { fld1 } <- ( unsafeCoerce -> fld1 )
  where N1 = unsafeCoerce

pattern N2 :: forall. () => forall a. () => a -> Any
pattern N2 { fld2 } <- ( unsafeCoerce -> fld2 )
  where N2 = unsafeCoerce

test1, test2 :: forall a. Any -> a

test1 = fld1  -- Should be OK
test2 = fld2  -- Should be rejected
