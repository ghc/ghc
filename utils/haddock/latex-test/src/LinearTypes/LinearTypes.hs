{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE LinearTypes #-}
module LinearTypes where

-- | Does something unrestricted.
unrestricted :: a -> b
unrestricted = undefined

-- | Does something linear.
linear :: a %1 -> b
linear = linear

-- | Does something polymorphic.
poly :: a %m -> b
poly = poly
