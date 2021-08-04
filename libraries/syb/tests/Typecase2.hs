{-# OPTIONS -fglasgow-exts #-}

module Typecase2 (tests) where

{-

This test provides a variation on typecase1.hs.
This time, we use generic show as defined for all instances of Data.
Thereby, we get rid of the Show constraint in our functions.
So we only keep a single constraint: the one for class Data.

-}

import Test.Tasty.HUnit

import Data.Generics
import Data.Maybe

-- Some datatype.
data MyData = MyCons String deriving (Typeable, Data)

--
-- Some function that performs type case.
--
f :: Data a => a -> String
f a = (maybe (maybe (maybe others
              mytys (cast a) )
              float (cast a) )
              int   (cast a) )

 where

  -- do something with ints
  int :: Int -> String
  int a =  "got an int, incremented: " ++ show (a + 1)

  -- do something with floats
  float :: Double -> String
  float a = "got a float, multiplied by .42: " ++ show (a * 0.42)

  -- do something with my data
  mytys :: MyData -> String
  mytys a = "got my data: " ++ gshow a

  -- do something with all other data
  others = "got something else: " ++ gshow a


--
-- Test the type case
--
tests = ( f (41::Int)
        , f (88::Double)
        , f (MyCons "42")
        , f True) @=? output

output = ( "got an int, incremented: 42"
         , "got a float, multiplied by .42: 36.96"
         , "got my data: (MyCons \"42\")"
         , "got something else: (True)")

