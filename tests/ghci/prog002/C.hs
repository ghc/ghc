module C (module B, module C) where

import B 

k x = x `mod` 11
