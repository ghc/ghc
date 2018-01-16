{-# LANGUAGE PartialTypeSignatures, NamedWildCards #-}
module ExpressionSigNamed where

bar :: _a -> _a
bar True  = (False :: _a)
bar False = (True :: _a)
