{-# OPTIONS_GHC -fwarn-unused-binds #-}

module ShouldCompile() where

foo x = x
{-# NOINLINE [1] foo #-}

-- Trac #2497; test should compile without language
-- 	       pragmas to swith on the forall
{-# RULES "id" forall (x :: a). foo x = x #-}



-- Trac #2213; eq should not be reported as unused

eq,beq :: Eq a => a -> a -> Bool
eq = (==)    -- Used
beq = (==)   -- Unused

{-# RULES
    "rule 1" forall x y. x == y = y `eq` x
  #-}
