{-# OPTIONS -fglasgow-exts #-}

module GRead (tests) where

{-

The following examples achieve branch coverage for the various
productions in the definition of gread. Also, negative test cases are
provided; see str2 and str3. Also, the potential of heading or
trailing spaces as well incomplete parsing of the input is exercised;
see str5.

-}

import Test.Tasty.HUnit

import Data.Generics

str1 = "(True)"     -- reads fine as a Bool
str2 = "(Treu)"     -- invalid constructor
str3 = "True"       -- lacks parentheses
str4 = "(1)"        -- could be an Int
str5 = "( 2 ) ..."  -- could be an Int with some trailing left-over
str6 = "([])"       -- test empty list
str7 = "((:)" ++ " " ++ str4 ++ " " ++ str6 ++ ")"

tests = show ( ( [ gread str1,
                   gread str2,
                   gread str3
                 ]
               , [ gread str4,
                   gread str5
                 ]
               , [ gread str6,
                   gread str7
                 ]
               )
             :: ( [[(Bool,  String)]]
                , [[(Int,   String)]]
                , [[([Int], String)]]
                )
             ) @=? output

output = show
           ([[(True,"")],[],[]],[[(1,"")],[(2,"...")]],[[([],"")],[([1],"")]])
