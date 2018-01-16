module ShouldFail where

sig_without_a_defn :: a -> b

-- We don't even refer to the variable.  This compiled without error
-- in ghc-4.08.
