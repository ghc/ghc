module Main(main,a) where

import B
import C

main = print (a 42)

a x = b x + c x
