{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

-- Trac #1795

module ShouldCompile where

data A a = A

class MkA a b | a -> b where
   mkA :: a -> A b

instance MkA a a where

translate :: (String, a) -> A a
translate a = mkA a
