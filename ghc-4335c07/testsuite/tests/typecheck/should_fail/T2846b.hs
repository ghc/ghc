{-# LANGUAGE ImpredicativeTypes, FlexibleContexts #-}
module T2846 where

f :: String
f = show ([1,2,3] :: [Num a => a])

