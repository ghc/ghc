{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

-- Trac #1795

-- This should fail.  It used to fail saying it can't unify
--   'a' with '(String, a)'
-- but now it simply says that it can't deduce 
-- 	(MkA (String, a) a) from the context ()
-- which is fair enough


module ShouldCompile where

data A a = A

class MkA a b | a -> b where
   mkA :: a -> A b

instance MkA a a where

translate :: (String, a) -> A a
translate a = mkA a
