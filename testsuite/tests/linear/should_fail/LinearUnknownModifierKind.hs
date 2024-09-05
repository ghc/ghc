{-# LANGUAGE LinearTypes #-}
module LinearUnknownModifierKind where

import GHC.Types (Multiplicity)

-- We should suggest that _ :: Multiplicity
f :: a %m -> a
f x = x

g :: a %(m :: k) -> a
g x = x
