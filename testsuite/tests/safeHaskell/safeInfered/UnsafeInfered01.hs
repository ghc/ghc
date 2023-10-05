{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Safe #-}
module UnsafeInfered01 where

import safe UnsafeInfered01_A

g :: IO a -> a
g = f

