{-# LANGUAGE DeriveDataTypeable #-}

module ShouldCompile where
import Data.Typeable

data A a b c d e f g h i j = A deriving (Typeable)
  -- Many args

data B a b = B (a b) deriving (Typeable)
  -- Non type-kind args