module T2354(test) where 
 
class AsInt a where 
  {-# NOINLINE toInt #-} 
  toInt   :: a -> Int 
  {-# NOINLINE fromInt #-} 
  fromInt :: Int -> a 
