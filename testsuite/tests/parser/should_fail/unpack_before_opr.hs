{-# LANGUAGE TypeOperators #-}

module UnpackBeforeOperator where

data a + b
data T = T { t :: {-# UNPACK #-} + Int }
