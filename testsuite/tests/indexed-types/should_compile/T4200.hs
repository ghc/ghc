{-# LANGUAGE TypeFamilies #-}

module T4200 where

class C a where
  type In a :: *
  op :: In a -> Int

-- Should be ok; no -XUndecidableInstances required
instance (In c ~ Int) => C [c] where 
  type In [c] = In c
  op x = 3
