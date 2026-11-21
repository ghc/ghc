{-# LANGUAGE DataKinds, StandaloneKindSignatures, TypeAbstractions, TypeFamilies #-}
module OpenArrowKind (OpenKind) where
import Data.Kind (Type)
import GHC.Matchability (Matchability)

-- | Supply a kind selected by an invisible argument of the family.
type OpenKind :: forall (m :: Matchability). Type
type family OpenKind @m
