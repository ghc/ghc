module UnpackInsideType where

data T = T { t :: Maybe {-# UNPACK #-} Int }
