{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Safe, DerivingVia #-}

-- | Tests that Safe disables DerivingVia (#19786)
module SafeLang19 where

f :: Int
f = 1
