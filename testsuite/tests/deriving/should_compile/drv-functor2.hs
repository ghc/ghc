{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShouldCompile where

-- Deriving Functor should still work with GeneralizedNewtypeDeriving instead of DeriveFunctor

newtype List a = List [a]
  deriving (Functor)

