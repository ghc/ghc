module ShouldCompile where

import MyList

myLength :: MyList a -> Int
myLength Empty = 0
myLength (x ::: xs) = 1 + myLength xs

