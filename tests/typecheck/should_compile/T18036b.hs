-- https://gitlab.haskell.org/ghc/ghc/-/issues/19865
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}

module Main where

-- declare class
class Mode t where
  type Scalar t
  myf :: Fractional (Scalar t) => t -> Scalar t -> t


-- declare data type + instance
data ReverseDouble a

instance Mode (ReverseDouble s) where
  type Scalar (ReverseDouble s) = Double
  myf = undefined


-- declare a newtype and derive the instance from the above data type
newtype GDouble s a = GDouble (ReverseDouble s)
  deriving newtype Mode



main :: IO ()
main = pure ()
