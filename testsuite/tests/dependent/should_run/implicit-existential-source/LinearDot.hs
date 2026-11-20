{-# LANGUAGE LinearTypes #-}

module LinearDot where

import BarProvider (Bar)
import Filter (Hidden)
import Vector (Vec)

data BarBox a = BarBox (Bar a)

payload :: Vec n a %1 -> Vec n a
payload xs = xs

repack :: Hidden a %1 -> Hidden a
repack p = payload p
