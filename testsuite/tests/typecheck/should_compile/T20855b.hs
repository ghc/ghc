{-# language Haskell2010
           , AllowAmbiguousTypes
           , DataKinds
           , ParallelListComp
           , PolyKinds
           , ScopedTypeVariables
           , StandaloneKindSignatures
           , TypeApplications
           , TypeFamilies
           , UnliftedNewtypes #-}

module T20855b where

import Data.Kind ( Constraint, Type )
import GHC.Exts ( TYPE, RuntimeRep(..), LiftedRep )

type R :: Type -> RuntimeRep
type family R a where
  R Bool = IntRep
  R ()   = LiftedRep

type C :: Type -> Constraint
class C a where
  type T a :: TYPE (R a)
  foo :: [D a] -> T a

instance C () where
  type T () = Int
  foo ds = sum . take 5 . drop (10^7)
        $ [ (a + b)
          | a <- [ bar () .. ]
          | MkD b <- ds ]

data family D a
data instance D () = MkD (T ())

bar :: () -> T ()
bar _ = 10

main = print $ foo @() (map MkD [ bar () .. ])
