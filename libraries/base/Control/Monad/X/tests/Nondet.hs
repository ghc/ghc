import Control.Monad.X.Nondet

assoc1 a b c = (a `mplus` b) `mplus` c
assoc2 a b c = a `mplus` (b `mplus` c)

