{-# LANGUAGE DeriveAnyClass, MultiParamTypeClasses #-}

module T9968 where

class C a b
data X = X deriving (C Int)

