{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module LinearTypes where

import GHC.Exts (Multiplicity(..))

-- | Does something unrestricted.
unrestricted :: a -> b
unrestricted = undefined

-- | Does something linear.
linear :: a %1 -> b
linear = linear

-- | Does something polymorphic.
poly :: a %m -> b
poly = poly

-- | A record with non-linear fields.
data C m = C { linC %1 :: Int, urC %Many :: Char, varC %m :: String, noC :: Bool }

-- | A GADT record with non-linear fields.
data G mult where
  G :: { linG %1 :: Int, urG %Many :: Char, varG %m :: String, noG :: Bool } -> G m
