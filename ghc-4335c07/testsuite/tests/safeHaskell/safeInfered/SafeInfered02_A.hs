{-# LANGUAGE DeriveDataTypeable #-}
-- | Only uses deriving of Typeable so should be considered safe
module SafeInfered02_A where

import Data.Typeable

data G = G Int deriving (Typeable)

f :: Int
f = 1

