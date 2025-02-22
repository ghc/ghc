{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE KindSignatures #-}
module LinearTypes where

import GHC.Types (Multiplicity)

-- | Does something unrestricted.
unrestricted :: a -> b
unrestricted = undefined

-- | Does something linear.
linear :: a %1 -> b
linear = linear

-- | Does something polymorphic.
poly :: a %(m :: Multiplicity) -> b
poly = poly
