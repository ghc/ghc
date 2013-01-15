{-# LANGUAGE FlexibleInstances, UndecidableInstances,
             MultiParamTypeClasses, FunctionalDependencies #-}

-- Trac #1564

module Foo where

import Text.PrettyPrint
import Prelude hiding(head,tail)

class FooBar m k l | m -> k l where 
 a :: m graphtype

instance FooBar [] Bool Bool where
  a = error "urk"

instance FooBar Maybe Int Int where
  a = error "urk"

class (Monad m)=>Gr g ep m where 
 x:: m Int
 v:: m Int

instance (Monad m,  FooBar m x z) =>  Gr g ep m  where
  x = error "urk"
  v = error "urk"  

-- Old GHC claims for y:  y :: (Monad m, FooBar m GHC.Prim.Any GHC.Prim.Any)
--			    => m Int (which is wrong)
-- The uses in foo and bar show if that happens
y () = x

foo :: [Int]
foo = y ()

bar :: Maybe Int
bar = y ()


