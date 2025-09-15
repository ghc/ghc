{-# LANGUAGE Haskell2010 #-}
-- Test #2114 (error message)

module ShouldFail where

data T = forall a. MkT a

