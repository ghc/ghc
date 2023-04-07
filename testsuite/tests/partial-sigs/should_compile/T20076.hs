{-# LANGUAGE FlexibleContexts, PartialTypeSignatures #-}

module Bug where

f :: Eq [a] => a -> _
f x = [x] == [x]

-- See Note [Constraints in partial type signatures] in
-- GHC.Tc.Solver, in particular the bullet about (P2).