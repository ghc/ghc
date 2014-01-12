{-# LANGUAGE TypeFamilies  #-}

-- Trac #3346

module Foo where

class EP a where
  type Result a
  from :: a -> Result a
  to   :: Result a -> a

{-# RULES "rule1"   forall x. to (from x) = x #-}
{-# RULES "rule2"   forall x. from (to x) = x #-}

foo :: EP a => a -> a
-- This is typed in a way rather similarly to RULE rule1
foo x = to (from x)

-- 'bar' has an ambiguous type and is rightly rejected
-- bar :: forall a. Result a -> Result a
-- bar x = from (to x :: a)
