{-# OPTIONS -fglasgow-exts #-}

-- Crashed GHC 5.04 with tcTyC
--	panic: tcSplitTyConApp forall x{-r6S-} :: *.
--				Main.L{-rr-} x{-r6S-}

module Main where

newtype FA c = FA (forall x . c x)
newtype L x = L [x]

my_nil = FA (L []) :: FA L

sample :: String
sample = case my_nil of FA (L x) -> "foo"++x 

-- -- but this works fine
-- sample = case my_nil of FA x -> case x of L y -> "foo"++y 

main = print sample
