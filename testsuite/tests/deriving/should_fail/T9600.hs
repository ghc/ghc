{-# LANGUAGE Haskell2010 #-}
import Control.Applicative

newtype Foo a = Foo (a -> a) deriving Applicative
