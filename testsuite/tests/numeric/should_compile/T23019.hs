module T23019
  (
  eexponent
  ) where

-- spine lazy, value strict list of doubles
data List
  = Nil
  | {-# UNPACK #-} !Double :! List

infixr 5 :!

newtype TowerDouble = Tower { getTower :: List }

primal :: TowerDouble -> Double
primal (Tower (x:!_)) = x
primal _ = 0

eexponent :: TowerDouble -> Int
eexponent = exponent . primal

