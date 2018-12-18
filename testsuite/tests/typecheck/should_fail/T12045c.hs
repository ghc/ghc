{-# LANGUAGE PolyKinds, TypeApplications, KindSignatures, DataKinds, GADTs
             , TypeFamilies, RankNTypes #-}

module T12045c where
import Data.Kind

type family F a where
  F @Type a = Bool
  F @(Type -> Type) b = Char
