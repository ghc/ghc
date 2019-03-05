module A (caf, c_two) where

import Debug.Trace (trace)

data C = C Int Int

caf :: C
caf = C 3 (trace "value forced" 4)

c_two :: C -> Int
c_two (C _ b) = b
