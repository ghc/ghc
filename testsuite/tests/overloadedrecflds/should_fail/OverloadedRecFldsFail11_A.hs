{-# LANGUAGE DuplicateRecordFields #-}
module OverloadedRecFldsFail11_A where

{-# WARNING foo "Warning on a record field" #-}
data S = MkS { foo :: Bool }
data T = MkT { foo :: Int }
