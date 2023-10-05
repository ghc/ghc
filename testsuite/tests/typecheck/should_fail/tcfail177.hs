module ShouldFail where

-- See #1176
-- This is really a pretty-printer test, not a typechecker test
--
-- Before ghc-7.2 the error messages looked like this (notice the wrong
-- indentation):

{-
tcfail177.hs:9:12:
    Couldn't match expected type `Bool' with actual type `Int'
    In the return type of a call of `foo'
    In the expression:
      foo
        ["One........" ~?= "1", "Two" ~?= "2", "Thre........." ~?= "3",
     "Four" ~?= "4", ....]
    In an equation for `allTest1':
        allTest1
          = foo
              ["One........" ~?= "1", "Two" ~?= "2", "Thre........." ~?= "3",
         ....]

tcfail177.hs:18:12:
    Couldn't match expected type `Bool' with actual type `Int'
    In the return type of a call of `foo'
    In the expression:
      foo
        ["One........" ~?= "1", "Two.................." ~?= "2",
       "Thre........." ~?= "3", "Four" ~?= "4", ....]
    In an equation for `allTest2':
        allTest2
          = foo
              ["One........" ~?= "1", "Two.................." ~?= "2",
             "Thre........." ~?= "3", ....]
-}

allTest1 :: Bool
allTest1 = foo
           ["One........" ~?= "1"
           ,"Two" ~?= "2"
           ,"Thre........." ~?= "3"
           ,"Four" ~?= "4"
           ,"Five" ~?= "5"
           ]

allTest2 :: Bool
allTest2 = foo
           ["One........" ~?= "1"
           ,"Two.................." ~?= "2"
           ,"Thre........." ~?= "3"
           ,"Four" ~?= "4"
           ,"Five" ~?= "5"
           ]

(~?=) :: a -> a -> Bool
(~?=) = error "urk"

foo :: a -> Int
foo x = 0
