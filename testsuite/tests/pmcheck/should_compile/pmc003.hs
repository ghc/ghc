{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}

module PMC003 where

f :: Bool -> Bool -> ()
f _    False = ()
f True False = ()
f _    _     = ()

