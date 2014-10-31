{-# LANGUAGE MultiParamTypeClasses #-}
module T9739 where

class Class3 a => Class1 a where

class Class2 t a where
  class2 :: (Class3 t) => a -> m

class (Class1 t, Class2 t t) => Class3 t where
