module OverloadedRecFldsFail12_A where

{-# WARNING foo "Deprecated foo" #-}
{-# WARNING bar "Deprecated bar" #-}
data T = MkT { foo :: Int, bar :: Int }
