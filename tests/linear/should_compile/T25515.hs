{-# LANGUAGE LinearTypes #-}

module T25515 where

data C = MkC { unc :: Int }

f :: Int %1 -> C
f x = MkC { unc = x }
