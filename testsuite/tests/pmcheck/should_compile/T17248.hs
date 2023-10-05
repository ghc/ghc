{-# LANGUAGE BangPatterns #-}
module Lib where

data T1 a = T1 a
newtype T2 a = T2 a

f :: T1 a -> Bool -> ()
f _      True = ()
f (T1 _) True = ()
f _      _    = ()

g :: T2 a -> Bool -> ()
g _      True = ()
g (T2 _) True = () -- redundant
g !_     True = () -- inaccessible
g _      _    = ()

