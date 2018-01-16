{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Foo ( B(P) ) where

class (f ~ A) => C f a where
  build :: a -> f a
  destruct :: f a -> a

data A a = A a

data B a = B a

instance C A Int where
  build n = A n
  destruct (A n) = n


pattern P :: C f a => a -> f a
pattern P x <- (destruct -> x)
  where
        P x = build x
