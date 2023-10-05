{-# LANGUAGE TypeFamilies #-}

module T9156_DF where

data X = MkX

data family D a
data instance D Int
   = D1 { f1 :: X }
   | D2 { f1 :: X, f2 :: X, f1 :: X }
