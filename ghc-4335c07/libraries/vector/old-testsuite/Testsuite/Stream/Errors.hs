module Testsuite.Stream.Errors ( tests )
where

import           Data.Vector.Fusion.Stream  ( Stream )
import qualified Data.Vector.Fusion.Stream as Stream

import Testsuite.Utils.Test
import Testsuite.Utils.Property
import Test.QuickCheck.Batch

consume :: Stream a -> ()
consume = Stream.foldl' (\_ _ -> ()) ()

prop_headOfEmpty = isBottom (Stream.head (Stream.empty :: Stream A))
prop_lastOfEmpty = isBottom (Stream.last (Stream.empty :: Stream A))
prop_indexNegative = isBottom (Stream.singleton 5 Stream.!! (-1))
prop_indexOutOfRange = isBottom (Stream.singleton 5 Stream.!! 2)
prop_initOfEmpty = isBottom (consume (Stream.init Stream.empty))
prop_tailOfEmpty = isBottom (consume (Stream.tail Stream.empty))
prop_foldl1OfEmpty = isBottom (Stream.foldl1 (\_ _ -> ()) Stream.empty)
prop_foldl1'OfEmpty = isBottom (Stream.foldl1' (\_ _ -> ()) Stream.empty)
prop_foldr1OfEmpty  = isBottom (Stream.foldr1 (\_ _ -> ()) Stream.empty)
prop_scanl1OfEmpty
  = isBottom (consume (Stream.scanl1 (\x _ -> x) Stream.empty))
prop_scanl1'OfEmpty
  = isBottom (consume (Stream.scanl1' (\x _ -> x) Stream.empty))

tests = "errors" $$? [
                      "head of empty"           $? prop_headOfEmpty
                    , "last of empty"           $? prop_lastOfEmpty
                    , "negative index"          $? prop_indexNegative
                    , "index out of range"      $? prop_indexOutOfRange
                    , "init of empty"           $? prop_initOfEmpty
                    , "tail of empty"           $? prop_tailOfEmpty
                    , "foldl1 of empty"         $? prop_foldl1OfEmpty
                    , "foldl1' of empty"        $? prop_foldl1'OfEmpty
                    , "foldr1 of empty"         $? prop_foldr1OfEmpty
                    , "scanl1 of empty"         $? prop_scanl1OfEmpty
                    , "scanl1' of empty"        $? prop_scanl1'OfEmpty
                    ]

