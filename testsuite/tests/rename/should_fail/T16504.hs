-- Type signature and definition with name typo
module M where

-- Both in global scope
simpleFuntcion :: Int -> Bool
simpleFunction i = i > 5
simpleFunction2 i = i < 5

-- Both in local scope
f x = anotherFunction x
  where anotherFunction :: Int -> Bool
        anotherFuntcion i = i > 5

-- Global signature, local definition
nonexistentFuntcion :: Int -> Bool
g x = nonexistentFunction x
  where nonexistentFunction i = i > 5
