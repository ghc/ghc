{-# LANGUAGE PolyKinds, TypeFamilies, TypeFamilyDependencies,
             ScopedTypeVariables, TypeOperators, GADTs,
             DataKinds #-}

module T15141 where

import Data.Type.Equality
import Data.Proxy

type family F a = r | r -> a where
  F () = Bool

data Wumpus where
  Unify :: k1 ~ F k2 => k1 -> k2 -> Wumpus

f :: forall k (a :: k). k :~: Bool -> ()
f Refl = let x :: Proxy ('Unify a b)
             x = undefined
         in ()

{-
We want this situation:

forall[1] k[1].
  [G] k ~ Bool
  forall [2] ... . [W] k ~ F kappa[2]

where the inner wanted can be solved only by taking the outer
given into account. This means that the wanted needs to be floated out.
More germane to this bug, we need *not* to generalize over kappa.

The code above builds this scenario fairly exactly, and indeed fails
without the logic in kindGeneralize that excludes constrained variables
from generalization.
-}
