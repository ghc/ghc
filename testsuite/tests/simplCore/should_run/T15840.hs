module Main (main) where

data T = MkT !Bool

f :: T -> Bool
f _ = False
{-# NOINLINE f #-}

{-# RULES "non-det" [1] forall x. f (MkT x) = x #-}

main :: IO ()
main = print (f (MkT True))
-- Prints `True` if the rule fires, or `False` is the wrapper for `MkT` inlines
-- in phase 2, preventing the rule from being triggered in phase 1.
