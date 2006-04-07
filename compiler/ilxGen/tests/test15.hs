
{-# OPTIONS -fglasgow-exts -fno-implicit-prelude #-}


import PrelBase
import PrelList
import PrelEnum
import PrelShow
import PrelIO


bbuild 	:: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
{-# INLINE 2 bbuild #-}
bbuild g = g (:) []

main = putStr "hello world\n"


