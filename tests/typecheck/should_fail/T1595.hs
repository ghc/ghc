--
-- Check that we produce only one error message for each type
-- signature. See ticket #1595.
--

module T1595 where

foo1, bar1 :: DoesNotExist
foo1 = undefined
bar1 = undefined

class Test a where
  foo2, bar2 :: a -> DoesNotExist
