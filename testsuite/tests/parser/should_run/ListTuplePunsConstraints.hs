{-# language DataKinds #-}

module ListTuplePunsConstraints where

import Data.Tuple.Experimental (CTuple2, Constraints)

type C1 m = Constraints (Monad m, Monad m)
type C2 m = CTuple2 (Monad m) (Monad m)

f ::
  Monad m =>
  C1 m =>
  C2 m =>
  CTuple2 (Monad m) (Monad m) =>
  (Monad m, Constraints (Monad m, Monad m)) =>
  m Int
f = pure 5
