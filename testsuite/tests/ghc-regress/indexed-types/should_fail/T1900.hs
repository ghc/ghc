{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Class4 where

class (Eq (Depend s))=> Bug s where
  type Depend s 
  trans :: Depend s -> Depend s
  
instance Bug Int where
  type Depend Int = ()
  trans = (+1)
  
check :: (Bug s) => Depend s -> Bool
check d = d == trans d