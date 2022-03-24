{-# language Haskell2010
           , AllowAmbiguousTypes
           , DataKinds
           , PolyKinds
           , ScopedTypeVariables
           , StandaloneKindSignatures
           , TypeFamilies #-}

module T20855b where

import Data.Kind ( Constraint, Type )
import GHC.Exts ( TYPE, RuntimeRep(..), LiftedRep )

type R :: Type -> RuntimeRep
type family R a where
  R () = LiftedRep

type C :: Type -> Constraint
class C a where
  type T a :: TYPE (R a)
  foo :: () -> T a

instance C () where
  type T () = Int
  foo _ = head $ [ a | a <- [ 12345 ] ]
