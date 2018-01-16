{-# LANGUAGE TypeFamilies #-}
module Testsuite.Vector.AsList ( tests ) where

import Data.Vector ( Vector )
import qualified Data.Vector as V
import Data.List

import Testsuite.Utils.List
import Testsuite.Utils.Property
import Testsuite.Utils.Test
import Testsuite.Utils.Generator
import Test.QuickCheck
import Text.Show.Functions ()

instance Modelled (Vector a) where
  type Model (Vector a) = [a]

  model = V.toList
  unmodel = V.fromList

prop_convert = arg_ty [A] $ \xs -> V.fromList xs ==? xs

prop_eq      = ((==) :: Vector A -> Vector A -> Bool) ==? (==)
prop_compare = (compare :: Vector A -> Vector A -> Ordering) ==? compare

prop_length = arg_ty [A] $ V.length ==? length
prop_null   = arg_ty [A] $ V.null   ==? null

prop_empty = (V.empty :: Vector A) ==? []
prop_singleton = arg_ty A $ V.singleton ==? (\x -> [x])
prop_replicate = arg2_ty A $ V.replicate ==? replicate
prop_cons = arg_ty A   $ V.cons ==? (:)
prop_snoc = arg_ty [A] $ V.snoc ==? (\xs x -> xs ++ [x])
prop_append = arg_ty [A] $ (V.++) ==? (++)
prop_copy   = arg_ty [A] $ V.copy ==? id

prop_head  = arg (not . null) $ arg_ty [A] $ V.head ==? head
prop_last  = arg (not . null) $ arg_ty [A] $ V.last ==? last
prop_index = args2 (\xs i -> i >= 0 && i < length xs)
           $ arg_ty [A] $ (V.!) ==? (!!)

prop_slice = forAll arbitrary $ \xs ->
             forAll (choose (0,length xs)) $ \i ->
             forAll (choose (0,length xs - i)) $ \n ->
             V.slice (V.fromList (xs :: [A])) i n ==? take n (drop i xs)
prop_init    = arg (not . null) $ arg_ty [A] $ V.init ==? init
prop_tail    = arg (not . null) $ arg_ty [A] $ V.tail ==? tail
prop_take    = arg2_ty [A] $ V.take ==? take
prop_drop    = arg2_ty [A] $ V.drop ==? drop

prop_accum  = forAll arbitrary $ \f ->
              forAll arbitrary $ \xs ->
              forAll (index_value_pairs (length xs)) $ \ps ->
              (V.accum f (V.fromList (xs :: [A])) (ps :: [(Int,B)]))
                ==? accum f xs ps
prop_upd    = forAll arbitrary $ \xs ->
              forAll (index_value_pairs (length xs)) $ \ps ->
              (V.fromList (xs :: [A]) V.// ps) ==? (xs // ps)
prop_update = forAll arbitrary $ \xs ->
              forAll (index_value_pairs (length xs)) $ \ps ->
              (V.update (V.fromList (xs :: [A])) (V.fromList ps))
                ==? (xs // ps)
prop_backpermute = forAll arbitrary $ \xs ->
                   forAll (indices (length xs)) $ \is ->
                   V.backpermute (V.fromList (xs :: [A])) (V.fromList is)
                     ==? map (xs!!) is
prop_reverse = arg_ty [A] $ V.reverse ==? reverse

prop_map = arg_ty (A :-> B) $ V.map ==? map
prop_zipWith = arg_ty (A :-> B :-> C) $ V.zipWith ==? zipWith
prop_zip     = arg_ty [A] $ arg2_ty [B] $ V.zip ==? zip

prop_filter = arg_ty (A :-> Bool) $ V.filter ==? filter
prop_takeWhile = arg_ty (A :-> Bool) $ V.takeWhile ==? takeWhile
prop_dropWhile = arg_ty (A :-> Bool) $ V.dropWhile ==? dropWhile

prop_elem      = arg_ty A $ V.elem ==? elem
prop_notElem   = arg_ty A $ V.notElem ==? notElem
prop_find      = arg_ty (A :-> Bool) $ V.find ==? find
prop_findIndex = arg_ty (A :-> Bool) $ V.findIndex ==? findIndex

prop_foldl     = arg_ty (A :-> B :-> A) V.foldl ==? foldl
prop_foldl1    = arg2 (not . null) $
                 arg_ty (A :-> A :-> A) V.foldl1 ==? foldl1
prop_foldl'    = arg_ty (A :-> B :-> A) V.foldl' ==? foldl'
prop_foldl1'   = arg2 (not . null) $
                 arg_ty (A :-> A :-> A) V.foldl1' ==? foldl1'
prop_foldr     = arg_ty (A :-> B :-> B) V.foldr ==? foldr
prop_foldr1    = arg2 (not . null) $
                 arg_ty (A :-> A :-> A) V.foldr1 ==? foldr1

prop_prescanl  = arg_ty (A :-> B :-> A)
                 V.prescanl ==? (\f z -> init . scanl f z)
prop_prescanl' = arg_ty (A :-> B :-> A)
                 V.prescanl' ==? (\f z -> init . scanl f z)
prop_postscanl  = arg_ty (A :-> B :-> A)
                  V.postscanl ==? (\f z -> tail . scanl f z)
prop_postscanl' = arg_ty (A :-> B :-> A)
                  V.postscanl' ==? (\f z -> tail . scanl f z)
prop_scanl    = arg_ty (A :-> B :-> A)
                V.scanl ==? scanl
prop_scanl'   = arg_ty (A :-> B :-> A)
                V.scanl' ==? scanl
prop_scanl1   = arg2 (not . null) $
                arg_ty (A :-> A :-> A)
                V.scanl1 ==? scanl1
prop_scanl1'  = arg2 (not . null) $
                arg_ty (A :-> A :-> A)
                V.scanl1' ==? scanl1


tests = "vs. list" $$? [
                      "convert"         $? prop_convert

                    , "(==)"            $? prop_eq
                    , "compare"         $? prop_compare

                    , "length"          $? prop_length
                    , "null"            $? prop_null

                    , "empty"           $? prop_empty
                    , "singleton"       $? prop_singleton
                    , "replicate"       $? prop_replicate
                    , "cons"            $? prop_cons
                    , "snoc"            $? prop_snoc
                    , "(++)"            $? prop_append
                    , "copy"            $? prop_copy

                    , "head"            $? prop_head
                    , "last"            $? prop_last
                    , "(!)"             $? prop_index

                    , "slice"           $? prop_slice
                    , "init"            $? prop_init
                    , "tail"            $? prop_tail
                    , "take"            $? prop_take
                    , "drop"            $? prop_drop

                    , "accum"           $? prop_accum
                    , "(//)"            $? prop_upd
                    , "update"          $? prop_update
                    , "backpermute"     $? prop_backpermute
                    , "reverse"         $? prop_reverse

                    , "map"             $? prop_map
                    , "zipWith"         $? prop_zipWith
                    , "zip"             $? prop_zip

                    , "filter"          $? prop_filter
                    , "takeWhile"       $? prop_takeWhile
                    , "dropWhile"       $? prop_dropWhile

                    , "elem"            $? prop_elem
                    , "notElem"         $? prop_notElem
                    , "find"            $? prop_find
                    , "findIndex"       $? prop_findIndex

                    , "foldl"           $? prop_foldl
                    , "foldl1"          $? prop_foldl1
                    , "foldl'"          $? prop_foldl'
                    , "foldl1'"         $? prop_foldl1'
                    , "foldr"           $? prop_foldr
                    , "foldr1"          $? prop_foldr1

                    , "prescanl"        $? prop_prescanl
                    , "prescanl'"       $? prop_prescanl'
                    , "postscanl"       $? prop_postscanl
                    , "postscanl'"      $? prop_postscanl'
                    , "scanl"           $? prop_scanl
                    , "scanl'"          $? prop_scanl'
                    , "scanl1"          $? prop_scanl1
                    , "scanl1'"         $? prop_scanl1'
                    ]

