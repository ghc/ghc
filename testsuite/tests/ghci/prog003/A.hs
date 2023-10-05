module A(main,a) where

import B
import C(c)

main = print (a 42)

a x = b x + c x
