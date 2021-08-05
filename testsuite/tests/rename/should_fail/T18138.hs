module Misplaced where

instance Eq (T a)
{-# SPECIALISE instance Eq (T Int) #-}
 -- A mis-placed signature

data T a = T a



