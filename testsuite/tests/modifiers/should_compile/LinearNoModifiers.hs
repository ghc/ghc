{-# LANGUAGE LinearTypes, NoModifiers #-}

module LinearNoModifiers where

import GHC.Types (Multiplicity)

f :: a %m -> b
f = undefined

data D m = D { d %m :: () }

g :: forall (m :: Multiplicity) . ()
g = let %m a = () in a
