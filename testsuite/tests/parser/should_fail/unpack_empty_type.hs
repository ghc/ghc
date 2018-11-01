module UnpackEmptyType where

data T = T { t :: {-# UNPACK #-} }
