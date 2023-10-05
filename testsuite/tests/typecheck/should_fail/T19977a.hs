{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE FlexibleContexts #-}

module T19977a where

-- See Note [Inferring principal types] in Ghc.Tc.Solver

f x = show [x]

g :: Show [a] => a -> String
g x = f x
