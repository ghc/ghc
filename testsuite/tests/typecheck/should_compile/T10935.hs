{-# OPTIONS_GHC -fwarn-monomorphism-restriction #-}

module T10935 where

f x = let y = 1+1 in (y,y)
