--!!! Testing export of class members

module T7 where

import T6

p :: (W a,X a, Y a, Z a) => [a]
p = [y,z]
