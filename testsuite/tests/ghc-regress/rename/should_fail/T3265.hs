{-# LANGUAGE MultiParamTypeClasses #-}

-- Test Trac #3265

module T3265 where

data a :+: b = Left a | Right b

class a :*: b where {}
