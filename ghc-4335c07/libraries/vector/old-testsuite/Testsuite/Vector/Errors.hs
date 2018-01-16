module Testsuite.Vector.Errors ( tests )
where

import           Data.Vector  ( Vector )
import qualified Data.Vector as V

import Testsuite.Utils.Test
import Testsuite.Utils.Property
import Test.QuickCheck.Batch

prop_headOfEmpty = isBottom (V.head V.empty)
prop_lastOfEmpty = isBottom (V.last V.empty)
prop_indexNegative = isBottom (V.singleton 5 V.! (-1))
prop_indexOutOfRange = isBottom (V.singleton 5 V.! 2)
prop_sliceNegativeIndex = isBottom (V.slice V.empty (-1) 0)
prop_sliceIndexOutOfRange = isBottom (V.slice (V.singleton 5) 2 1)
prop_sliceNegativeLength = isBottom (V.slice V.empty 0 (-1))
prop_sliceLengthOutOfRange = isBottom (V.slice (V.singleton 5) 0 2)
prop_initOfEmpty = isBottom (V.init V.empty)
prop_tailOfEmpty = isBottom (V.tail V.empty)
prop_foldl1OfEmpty = isBottom (V.foldl1 (\_ _ -> ()) V.empty)
prop_foldl1'OfEmpty = isBottom (V.foldl1' (\_ _ -> ()) V.empty)
prop_foldr1OfEmpty  = isBottom (V.foldr1 (\_ _ -> ()) V.empty)
prop_scanl1OfEmpty  = isBottom (V.scanl1 (\x _ -> x) V.empty)
prop_scanl1'OfEmpty = isBottom (V.scanl1' (\x _ -> x) V.empty)


tests = "errors" $$? [
                      "head of empty"           $? prop_headOfEmpty
                    , "last of empty"           $? prop_lastOfEmpty
                    , "negative index"          $? prop_indexNegative
                    , "index out of range"      $? prop_indexOutOfRange
                    , "slice (negative index)"  $? prop_sliceNegativeIndex
                    , "slice (index out of range)"
                                                $? prop_sliceIndexOutOfRange
                    , "slice (negative length)" $? prop_sliceNegativeLength
                    , "slice (length out of range)"
                                                $? prop_sliceLengthOutOfRange
                    , "init of empty"           $? prop_initOfEmpty
                    , "tail of empty"           $? prop_tailOfEmpty
                    , "foldl1 of empty"         $? prop_foldl1OfEmpty
                    , "foldl1' of empty"        $? prop_foldl1'OfEmpty
                    , "foldr1 of empty"         $? prop_foldr1OfEmpty
                    , "scanl1 of empty"         $? prop_scanl1OfEmpty
                    , "scanl1' of empty"        $? prop_scanl1'OfEmpty
                     ]

