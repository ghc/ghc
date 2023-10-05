{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE
  FlexibleContexts,
  FlexibleInstances,
  UndecidableInstances,
  NoMonomorphismRestriction
#-}

module T19977b where

-- See Note [Inferring principal types] in Ghc.Tc.Solver

class C a
class D a where
  d :: a
instance C a => D a where
  d = undefined
h = d

g :: D a => a
g = h
