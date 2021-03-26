{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE UndecidableInstances #-}

module T19187 where

data T

instance Eq Int => Eq T
