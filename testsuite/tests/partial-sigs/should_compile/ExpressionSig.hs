{-# LANGUAGE PartialTypeSignatures #-}
module ExpressionSig where

bar :: Bool -> Bool
bar x = (x :: _)
