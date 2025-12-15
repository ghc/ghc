{-# LANGUAGE DataKinds, GADTs, TypeFamilies
#-}

module T12045c where
import Data.Kind

type family F a where
  F @Type a = Bool
  F @(Type -> Type) b = Char
