{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

-- #1795

module ShouldCompile where

data A a = A

class MkA a b | a -> b where
   mkA :: a -> A b

instance MkA a a where

translate :: (String, a) -> A a
translate a = mkA a

{- From the call to mkA

[W] MkA alpha beta
[W] alpha ~ (String,a)
[W] A beta ~ A a

==>  beta:=a, alpha:=(String,a)
-}
