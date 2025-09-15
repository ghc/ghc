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
  -- We can apply the C [a] instance without difficulty, but
  --  that fails due to silent dfun parameters
