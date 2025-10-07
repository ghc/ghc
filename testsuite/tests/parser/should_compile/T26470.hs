{-# LANGUAGE Haskell2010 #-}
module T26470 where

role :: Int
role = 42

f :: Int -> Int
f role = role + 1

data T = MkT { role :: String }

newtype T2 role = MkT2 Int
