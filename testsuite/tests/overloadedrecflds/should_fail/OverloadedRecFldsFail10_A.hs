{-# LANGUAGE TypeFamilies #-}
module OverloadedRecFldsFail10_A where

data family F a
data instance F Int = MkFInt { foo :: Int }
