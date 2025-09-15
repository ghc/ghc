{-# LANGUAGE Haskell2010 #-}

module RnUnexpectedStandaloneDeriving where

data Foo = Foo

deriving instance Eq Foo
