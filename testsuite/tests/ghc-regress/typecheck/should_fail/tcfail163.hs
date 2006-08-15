{-# OPTIONS -fglasgow-exts -fgenerics #-} 

-- Derivable type class with a higher-rank method
-- Currently this does not work, but it crashed GHC 6.5, so
-- this tests that the error message is civilised.
  
module Foo where

import Data.Generics 
 
class ChurchEncode k where 
 
  match :: k 
      -> z 
       -> (forall a b z. a -> b -> z)  {- product -} 
       -> (forall a   z. a -> z)       {- left -} 
       -> (forall a   z. a -> z)       {- right -} 
       -> z 
  
  match {| Unit    |} Unit      unit prod left right = unit 
  match {| a :*: b |} (x :*: y) unit prod left right = prod x y 
  match {| a :+: b |} (Inl l)   unit prod left right = left l 
  match {| a :+: b |} (Inr r)   unit prod left right = right r 
  
  
instance ChurchEncode Bool 


