{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Ambiguity where

class C1 a b | b -> a
class (C1 a b) => C2 a b where
  foo :: b -> b

data A = A
data B = B
instance C1 A B 
instance C2 A B where foo = error "urk"

-- this is accepted by both 6.12.3 and 7  
runFoo1 :: C2 a b => b -> b  
runFoo1 = foo

-- this is accepted by 6.12.3, but not by 7
runFoo2 :: B -> B
runFoo2 = foo
