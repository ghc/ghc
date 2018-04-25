{-# LANGUAGE TypeFamilies #-}

-- Trac #2334

module Test where

data family F r

newtype instance F ()  = F () () deriving Eq
newtype instance F Int  = H deriving Eq

data instance F Bool = K1
data instance F Bool = K2



