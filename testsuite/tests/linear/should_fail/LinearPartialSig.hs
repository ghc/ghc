{-# LANGUAGE LinearTypes #-}
module LinearPartialSig where

import GHC.Types (Multiplicity)

-- We should suggest that _ :: Multiplicity
f :: a %_ -> a
f x = x

g :: a %(_ :: k) -> a
g x = x

h :: a %(_ :: Multiplicity) -> a
h x = x
