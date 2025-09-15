-- Ensure that tuple instances are brought into scope
-- See #1385

module ShouldCompile where

foo = (1,True) == (2,False)
