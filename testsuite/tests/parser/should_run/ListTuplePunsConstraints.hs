{-# language DataKinds #-}

module ListTuplePunsConstraints where

-- import Data.Tuple.Experimental (Constraints)
import GHC.Classes (CTuple2)

-- type C1 m = Constraints (Monad m, Monad m)
type C2 m = CTuple2 (Monad m) (Monad m)

f ::
  Monad m =>
  -- C1 m =>
  C2 m =>
  CTuple2 (Monad m) (Monad m) =>
  -- (Monad m, Constraints (Monad m, Monad m)) =>
  m Int
f = pure 5
