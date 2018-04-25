{-# LANGUAGE ImpredicativeTypes, FlexibleContexts #-}
module T2846 where

x = [1,2,3] :: [Num a => a]
