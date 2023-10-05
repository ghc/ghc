{-# LANGUAGE TemplateHaskell #-}
module T4124 where

class Storable a where
data X = X
[d| instance Storable $( [t| X |] ) where |]
