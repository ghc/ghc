{-# OPTIONS_GHC -fallow-overlapping-instances #-}

module C where

import A
import B

class Class1 a => Class2 a

instance Class2 B
