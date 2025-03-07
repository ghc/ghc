{-# LANGUAGE DuplicateRecordFields #-}
module T24035_aux (R1(..), R2(..)) where

data R1 = MkR1 {ra :: Int, rb :: Bool}
data R2 = MkR2 {ra :: Int, rb :: Bool}
