module Main (main) where

data T = MkT !Bool

f :: Bool -> IO ()
f _ = putStrLn "The rule triggered before case-of-known-constructor could take effect (bad!)"
{-# NOINLINE f #-}

g :: IO ()
g = putStrLn "Case-of-known-constructor triggered (good!)"

{-# RULES "non-det" [~0] f True = g #-}

main :: IO ()
main =
  case MkT True of
    MkT x -> f x
-- What we want to see is case-of-known-constructor triggering before phase 0
-- (when the wrapper for MkT is allowed to be inlined). If it is, then the rule
-- will see `f True` and trigger, and `g` will be run. If it isn't then `f True`
-- will only appear at phase 0, when the rule cannot trigger, hence `f` will be
-- run.
