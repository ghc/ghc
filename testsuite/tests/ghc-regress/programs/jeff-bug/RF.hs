module RF where

import Prelude hiding (read)
import LazyST
import Utils

import Hawk

type RF s a b = STArray s a b

new   :: (Register a,Num b) => ST c (RF c a b)
read  :: Register a => RF s a b -> a -> ST s b
write :: Register a => RF s a b -> a -> b -> ST s ()

new         = newSTArray (minBound,maxBound) 0
read        = readSTArray 
write f x z = if readOnly x then return () 
              else writeSTArray f x z
