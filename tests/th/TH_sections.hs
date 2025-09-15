
-- Test for trac #2956

module TH_sections where

two :: Int
two = $( [| (1 +) 1 |] )

three :: Int
three = $( [| (+ 2) 1 |] )

