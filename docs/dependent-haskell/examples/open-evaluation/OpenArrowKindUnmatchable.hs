{-# LANGUAGE Modifiers, TypeApplications, TypeFamilies #-}
module OpenArrowKindUnmatchable () where
import OpenArrowKind (OpenKind)
import GHC.Matchability (U)
type instance OpenKind @U = Bool %U -> Bool
