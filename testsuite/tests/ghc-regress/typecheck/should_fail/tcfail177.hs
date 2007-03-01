module ShouldFail where

-- This is really a pretty-printer test, not a typechecker test
-- The more infix ops we have, the worse fsep works

allTests :: Bool
allTests = foo
           [a ~?= b
           ,"Three" ~?= "3"
           ,"Four" ~?= "4"
           ,"Five" ~?= "5"
           ,"Five" ~?= "5"
           ,"Five" ~?= "5"
           ,"Five" ~?= "5"
           ,"Five" ~?= "5"
           ,"Five" ~?= "5"
           ,"Two", "Two", "Two" 
           ,"Two", "Two", "Two" 
           ,"Two", "Two", "Two" 
           ,"Two", "Two", "Two" 
           ,"Two", "Two", "Two" 
           ,"Two", "Two", "Two"] 

a=""
b=""

(~?=) :: a -> a -> Bool
(~?=) = error "urk" 

foo :: a -> Int
foo x = 0
