{-# LANGUAGE TypeFamilies #-}

module Rules1 where

class C a where
  data T a

instance (C a, C b) => C (a,b) where
  data T (a,b) = TPair (T a) (T b)

mapT :: (C a, C b) => (a -> b) -> T a -> T b
mapT = undefined

zipT :: (C a, C b) => T a -> T b -> T (a,b)
{-# NOINLINE [1] zipT #-}
zipT = undefined

{-# RULES

"zipT/mapT" forall f x y.
  zipT (mapT f x) y = mapT (\(x,y) -> (f x, y)) (zipT x y)

 #-}

