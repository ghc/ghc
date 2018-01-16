{-# LANGUAGE FunctionalDependencies #-}

class X a b | a -> b where
  to :: a -> b
