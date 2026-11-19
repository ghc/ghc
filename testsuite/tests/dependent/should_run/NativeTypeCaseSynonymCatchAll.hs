{-# LANGUAGE ExperimentalDependentTypes, KindSignatures, Modifiers, TypeFamilies #-}
module NativeTypeCaseSynonymCatchAll where
import Data.Kind (Type)
import GHC.Exts (U)
import NativeTypeCaseHeadProvider (Forget)
type Identity a = a
type Indirect a = Identity a
type Direct (a :: Type) = case a of { Identity x -> Bool }
type Nested (a :: Type) = case a of { Indirect x -> Bool }
type Lambda = ((\a -> case a of { Identity x -> Bool }) :: Type %U -> Type)
type Imported (a :: Type) = case a of { Forget x -> Bool }
