module T19168 where

import GHC.Exts (SpecConstrAnnotation(..))

{-# ANN type List NoSpecConstr #-}
newtype List a = List { unList :: [a] }
