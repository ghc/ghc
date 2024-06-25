{-# LANGUAGE LinearTypes #-}

module Main where

-- These rules are clearly nonsensical, so that we can observe the result of
-- their firing.
{-# RULES "test/match" forall f. mark (f True) = (False, False) #-}
{-# RULES "test/core" forall f. mark (f False) = ensure_many f #-}

-- Tests that constructors are matched by higher-order rules (as originally
-- reported)
g = mark (True, True)

-- Tests that linear functions are matched by higher-order rules (as was
-- understood to be the root cause of the issue)
h = mark (d True)

-- Tests that a matched linear function can be used where a non-linear function
-- is expected, and that the result passes the linter. This wasn't part of the
-- original report, but a first fix to #23586 was incorrect because this rule
-- produced Core which was rejected by the linter.
-- See https://gitlab.haskell.org/ghc/ghc/-/merge_requests/12648#note_565803 .
i = mark (d False)

main :: IO ()
main = do
  print g
  print h
  print i


-- Helpers below

mark :: a -> a
mark x = x
{-# NOINLINE  mark #-}

d :: Bool %1 -> (Bool, Bool)
d True = (True, True)
d False = (False, False)
{-# NOINLINE d #-}

ensure_many :: (Bool -> (Bool, Bool)) -> (Bool, Bool)
ensure_many f = (False, True)
{-# NOINLINE ensure_many #-}
