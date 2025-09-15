{-# LANGUAGE ViewPatterns, GADTs #-}

module T30243( getUL ) where

import Data.Kind
import Unsafe.Coerce

newtype AsUnitLoop a (b :: Type) (c :: Type) = UnsafeUL a

data SafeUnitLoop a b c where
  SafeUnitLoop :: !a -> SafeUnitLoop a () ()

mkSafeUnitLoop :: AsUnitLoop a b c -> SafeUnitLoop a b c
mkSafeUnitLoop (UnsafeUL a) = unsafeCoerce (SafeUnitLoop a)

getUL :: AsUnitLoop a b c -> a
getUL (mkSafeUnitLoop -> SafeUnitLoop a) = a

-- There should be no unsafeEqualityProof in the output
-- when compiled with -O
