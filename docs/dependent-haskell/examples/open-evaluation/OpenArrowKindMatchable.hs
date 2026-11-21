{-# LANGUAGE Modifiers, TypeApplications, TypeFamilies #-}
module OpenArrowKindMatchable () where
import OpenArrowKind (OpenKind)
import Data.Kind (Type)
import GHC.Matchability (M)
type instance OpenKind @M = Type %M -> Type
