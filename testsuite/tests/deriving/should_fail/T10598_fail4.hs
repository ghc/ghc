{-# LANGUAGE GHC2021 #-}
module T10598_fail4 where

data Bar = Bar
  deriving stock Eq
