module OverloadedRecFldsFailDeprecatedFieldsWerrordeprecation_A where

{-# DEPRECATED foo "Deprecated foo" #-}
{-# DEPRECATED bar "Deprecated bar" #-}
data T = MkT { foo :: Int, bar :: Int }
