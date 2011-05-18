{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module SilentParametersOverlapping where 

class C a where 
  c :: a -> () 

class C a => B a where 
  b :: a -> () 

instance C [a] where 
  c x = () 

instance B [(a,b)] where 
  -- Silent: C [(a,b)] 
  b x = c [(undefined,undefined)]

-- DV: The silent parameter should not give us a failure to apply the instance!








