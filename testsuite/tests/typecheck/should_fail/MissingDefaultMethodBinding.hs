{-# LANGUAGE DefaultSignatures #-} 

module MissingDefaultMethodBinding where

class C a where
  meth :: a
  default meth :: Num a => a
