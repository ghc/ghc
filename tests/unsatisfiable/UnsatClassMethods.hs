{-# LANGUAGE DataKinds #-}

module UnsatClassMethods where

import GHC.TypeError

-- Easy version

class Cls a where
  method :: a -> a -> a

instance Unsatisfiable (Text "Not allowed for Bool") => (Cls Bool)


-- Trickier version

class C a where
  {-# MINIMAL (method1, method3, method4) | (method2, method3, method4) | (method1, method2, method4) #-}
  method1 :: a -> a
  method1 = method2
  method2 :: a -> a
  method2 = method1
  method3 :: a -> a
  method3 = method2 . method1

  method4 :: a -> a -> a

instance Unsatisfiable (Text "Not allowed for Int") => (C Int) where
  method3 = error "not implemented"
