{-# LANGUAGE Haskell2010 #-}


module ShouldFail where

newtype (f <.> g) a = Compose (f (g a))
