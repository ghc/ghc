-- Trac #1072

module ShouldCompile where

import Tc239_Help

f1 :: Show a => WrapIO e a
f1 = return undefined

f2 :: Show a => WrapIO2 a
f2 = f1
