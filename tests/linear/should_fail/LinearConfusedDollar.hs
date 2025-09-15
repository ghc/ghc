{-# LANGUAGE LinearTypes #-}
module LinearConfusedDollar where

-- When ($) becomes polymorphic in the multiplicity, then, this test case won't
-- hold anymore. But, as it stands, it produces untyped desugared code, hence
-- must be rejected.

f :: a %1 -> a
f x = x

g :: a %1 -> a
g x = f $ x
