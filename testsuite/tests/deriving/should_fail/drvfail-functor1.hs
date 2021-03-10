{-# LANGUAGE Haskell2010 #-}
module ShouldFail where

-- Derive Functor without a DeriveFunctor language pragma

data List a = Nil | Cons a (List a)
    deriving Functor
