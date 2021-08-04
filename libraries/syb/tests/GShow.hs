{-# OPTIONS -fglasgow-exts #-}

module GShow (tests) where

{-

The generic show example from the 2nd boilerplate paper.
(There were some typos in the ICFP 2004 paper.)
Also check out Data.Generics.Text.

-}

import Test.Tasty.HUnit

import Data.Generics hiding (gshow)
import Prelude hiding (showString)


gshow :: Data a => a -> String
gshow = gshow_help `extQ` showString

gshow_help :: Data a => a -> String
gshow_help t
     =  "("
     ++ showConstr (toConstr t)
     ++ concat (intersperse " " (gmapQ gshow t))
     ++ ")"

showString :: String -> String
showString s = "\"" ++ concat (map escape s) ++ "\""
               where
                 escape '\n' = "\\n"
                 escape other_char = [other_char]

gshowList :: Data b => [b] -> String
gshowList xs
    = "[" ++ concat (intersperse "," (map gshow xs)) ++ "]"

gshow' :: Data a => a -> String
gshow' = gshow_help `ext1Q` gshowList
                    `extQ`  showString

intersperse :: a -> [a] -> [a]
intersperse _ []     = []
intersperse x [e]    = [e]
intersperse x (e:es) = (e:(x:intersperse x es))

tests = ( gshow' "foo"
        , gshow' [True,False]
        ) @=? output

output = ("\"foo\"","[(True),(False)]")
