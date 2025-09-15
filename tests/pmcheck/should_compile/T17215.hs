module Lib where

foo :: Bool -> String
foo True = "True"
foo i = case i of
  False -> "False"

bar :: Bool -> String
bar True = "True"
bar i = bir i
  where
    bir False = "False"

baz :: Bool -> String
baz i = case i of
  True -> "True"
  _ -> case i of
    False -> "False"

-- Amazingly this does not trigger a warning
baz' :: Bool -> String
baz' i = case i of
  True -> "True"
  False -> case i of
    False -> "False"


bur :: Bool -> String
bur True = "True"
bur i = case i of
  True -> "True"
  _ -> boz i
    where
      boz False = "False"


-- This should not fail
-- That's a proof that all function are total
test :: [String]
test = [foo, bar, baz, baz', bur] <*> [minBound..maxBound]

