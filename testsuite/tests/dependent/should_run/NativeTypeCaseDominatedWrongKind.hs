{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers, PolyKinds, TypeFamilies #-}
module NativeTypeCaseDominatedWrongKind where
import Data.Kind (Type)
import GHC.Exts (U)

type Bad = ((\d -> case d of
  Just _ -> Int
  Just (Just _) -> 'True
  Nothing -> Char) :: forall k. @U Maybe (Maybe k) %U -> Type)
