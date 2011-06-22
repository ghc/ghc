{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module SilentParametersOverlapping where 

class C a where 
  c :: a -> () 

class C a => B a where 
  b :: a -> () 

instance C [a] where 
  c x = () 

instance {- silent: C [(a,b)] => -} B [(a,b)] where 
  b x = c [(undefined,undefined)]
  -- We get wanted: C [(gamma, delta)], 
  -- and gamma,delta are unconstrained
  -- But we can apply the C [a] instance without difficulty
  --  (except in the old days when we had silent dfun parameters)
