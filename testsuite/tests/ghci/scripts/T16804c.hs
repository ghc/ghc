module T16804c where

import T16804a

a :: Test
a = A

localScope :: Test -> Bool
localScope arg = testFunction a arg

localScope2 :: Test -> Bool
localScope2 a = testFunction B a

localScope3 :: Test -> Bool
localScope3 a = testFunction a A
