{-# LANGUAGE LinearTypes #-}
module Linear1Rule where

import GHC.Types (Multiplicity)

-- Test the 1 <= p rule
f :: a %1 -> b
f = f

g :: a %(p :: Multiplicity) -> b
g x = f x
