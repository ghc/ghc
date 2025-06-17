{-# LANGUAGE GHC2021 #-}
module T13568 where

import T13568a

data S = A

foo :: A -> ()
foo = undefined
