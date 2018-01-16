{-# LANGUAGE TypeFamilies, FunctionalDependencies, RankNTypes, MultiParamTypeClasses #-}
module T4254 where

class FD a b | a -> b where 
  op :: a -> b; 
  op = undefined 

instance FD Int Bool

ok1   :: forall a b. (a~Int,FD a b) => a -> b
ok1    = op
-- Should be OK: op has the right type

ok2   :: forall a b. (a~Int,FD a b,b~Bool) => a -> Bool
ok2    = op
-- Should be OK: needs the b~Bool

fails :: forall a b. (a~Int,FD a b) => a -> Bool 
fails  = op
-- Could fail: no proof that b~Bool
-- But can also succeed; it's not a *wanted* constraint
