module T17752 where

-- All occurences of elem should be optimized away.
-- For strings these should result in loops after inlining foldCString.
-- For lists it should result in a case expression.

-- Should compile to a pattern match if the rules fire
isElemList    x = x `elem` ['a','b','c']
isNotElemList x = x `elem` ['x','y','z']

isOneOfThese x = x `elem` [1,2,3,4,5::Int]
isNotOneOfThese x = x `notElem` [1,2,3,4,5::Int]

isElemString x = elem x "foo"
isNotElemString x = notElem x "bar"

isElemStringUtf x = elem x "foö"
isNotElemStringUtf x = notElem x "bär"
