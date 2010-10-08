{-# LANGUAGE ImplicitParams #-}

-- The defn of foo should be rejected; it's monomorphic, but
-- the implicit paramter escapes

module Foo where

baz = let ?x = 5 in print foo

foo = woggle 3

woggle :: (?x :: Int) => Int -> Int
woggle y = ?x + y



