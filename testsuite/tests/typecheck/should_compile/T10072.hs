module T0072 where
{-# RULES
"map/empty" forall (f :: a -> _). map f [] = []
  #-}
