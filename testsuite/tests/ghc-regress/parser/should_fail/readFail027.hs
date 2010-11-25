{-# LANGUAGE NoRelaxedLayout #-}

module ShouldFail where

-- Erroneously allowed by GHC 6.2.x
f x = case x of
         False -> do
    { return x; }
-- this line should close the 'case' context and cause the 'do' to be empty.

-- Update: arguably this should be allowed.  The fix to the Haskell
-- layout rule to allow it is simple: in Section 9.3 in the rules that
-- govern the introduction of the <n> and {n} psuedo-tokens, we need
-- to prevent <n> being inserted before {.  This could be a simple
-- side-condition on the rule that introduces <n>.
