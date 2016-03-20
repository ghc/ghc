{-# LANGUAGE Safe, Unsafe, Trustworthy #-}
-- | Basic test to see that incompatible flags give a nice error
-- message and ghc do not panic (see issue #11580).
module SafeFlags30 where

f :: Int
f = 1
