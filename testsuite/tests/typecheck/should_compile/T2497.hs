{-# OPTIONS_GHC -fwarn-unused-binds #-}

module ShouldCompile() where

foo x = x
{-# NOINLINE [1] foo #-}

-- Trac #2497; test should compile without language
--             pragmas to switch on the forall
{-# RULES "id" forall (x :: a). foo x = x #-}



-- Trac #2213; eq should not be reported as unused

eq,beq :: Eq a => a -> a -> Bool
{-# NOINLINE [0] eq #-}
-- The pragma and [~1] in the RULE are to prevent an infinite loop
-- in the simplifier, where the RULE fires infinitely in its
-- own RHS
eq = (==)    -- Used
beq = (==)   -- Unused

{-# RULES
    "rule 1" [~1] forall x y. x == y = y `eq` x
  #-}
