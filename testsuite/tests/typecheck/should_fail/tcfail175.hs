
-- Crashed GHC 6.6!
-- #1153
 
module ShouldFail where

eval :: Int -> String -> String ->  String
eval 0 root actual = evalRHS 0 root actual

evalRHS :: Int -> a
evalRHS 0 root actual =  eval 0 root actual

