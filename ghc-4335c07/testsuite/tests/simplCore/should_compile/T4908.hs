{-# LANGUAGE MagicHash #-}

module T4908 where
import GHC.Base

f :: Int -> (Int,Int) -> Bool
f 0 x = True 
f n x = case x of (a,b) -> case b of 
                             I# 0# -> True
                             I# _  -> f (n-1) x
