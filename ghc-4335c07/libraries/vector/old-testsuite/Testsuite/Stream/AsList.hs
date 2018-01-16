{-# LANGUAGE TypeSynonymInstances, TypeFamilies #-}
module Testsuite.Stream.AsList ( tests ) where

import Data.Vector.Fusion.Stream ( Stream )
import qualified Data.Vector.Fusion.Stream as Stream
import Data.List

import Testsuite.Utils.Property
import Testsuite.Utils.Test
import Test.QuickCheck
import Text.Show.Functions ()

instance Modelled (Stream a) where
  type Model (Stream a) = [a]

  model = Stream.toList
  unmodel = Stream.fromList

prop_convert = arg_ty [A] $ \xs -> Stream.fromList xs ==? xs

prop_length = arg_ty [A] $ Stream.length ==? length
prop_null   = arg_ty [A] $ Stream.null   ==? null

prop_empty = (Stream.empty :: Stream A) ==? []
prop_singleton = arg_ty A $ Stream.singleton ==? (\x -> [x])
prop_replicate = arg2_ty A $ Stream.replicate ==? replicate
prop_cons = arg_ty A   $ Stream.cons ==? (:)
prop_snoc = arg_ty [A] $ Stream.snoc ==? (\xs x -> xs ++ [x])
prop_append = arg_ty [A] $ (Stream.++) ==? (++)

prop_head  = arg (not . null) $ arg_ty [A] $ Stream.head ==? head
prop_last  = arg (not . null) $ arg_ty [A] $ Stream.last ==? last
prop_index = args2 (\xs i -> i >= 0 && i < length xs)
           $ arg_ty [A] $ (Stream.!!) ==? (!!)

prop_extract = arg_ty [A] $ Stream.extract ==? (\xs i j -> take j (drop i xs))
prop_init    = arg (not . null) $ arg_ty [A] $ Stream.init ==? init
prop_tail    = arg (not . null) $ arg_ty [A] $ Stream.tail ==? tail
prop_take    = arg2_ty [A] $ Stream.take ==? take
prop_drop    = arg2_ty [A] $ Stream.drop ==? drop

prop_map = arg_ty (A :-> B) $ Stream.map ==? map
prop_zipWith = arg_ty (A :-> B :-> C) $ Stream.zipWith ==? zipWith

prop_filter = arg_ty (A :-> Bool) $ Stream.filter ==? filter
prop_takeWhile = arg_ty (A :-> Bool) $ Stream.takeWhile ==? takeWhile
prop_dropWhile = arg_ty (A :-> Bool) $ Stream.dropWhile ==? dropWhile

prop_elem      = arg_ty A $ Stream.elem ==? elem
prop_notElem   = arg_ty A $ Stream.notElem ==? notElem
prop_find      = arg_ty (A :-> Bool) $ Stream.find ==? find
prop_findIndex = arg_ty (A :-> Bool) $ Stream.findIndex ==? findIndex

prop_foldl     = arg_ty (A :-> B :-> A) Stream.foldl ==? foldl
prop_foldl1    = arg2 (not . null) $
                 arg_ty (A :-> A :-> A) Stream.foldl1 ==? foldl1
prop_foldl'    = arg_ty (A :-> B :-> A) Stream.foldl' ==? foldl'
prop_foldl1'   = arg2 (not . null) $
                 arg_ty (A :-> A :-> A) Stream.foldl1' ==? foldl1'
prop_foldr     = arg_ty (A :-> B :-> B) Stream.foldr ==? foldr
prop_foldr1    = arg2 (not . null) $
                 arg_ty (A :-> A :-> A) Stream.foldr1 ==? foldr1

prop_prescanl  = arg_ty (A :-> B :-> A)
                 Stream.prescanl ==? (\f z -> init . scanl f z)
prop_prescanl' = arg_ty (A :-> B :-> A)
                 Stream.prescanl' ==? (\f z -> init . scanl f z)
prop_postscanl  = arg_ty (A :-> B :-> A)
                  Stream.postscanl ==? (\f z -> tail . scanl f z)
prop_postscanl' = arg_ty (A :-> B :-> A)
                  Stream.postscanl' ==? (\f z -> tail . scanl f z)
prop_scanl      = arg_ty (A :-> B :-> A)
                  Stream.scanl ==? scanl
prop_scanl'     = arg_ty (A :-> B :-> A)
                  Stream.scanl' ==? scanl
prop_scanl1     = arg2 (not . null) $
                  arg_ty (A :-> A :-> A)
                  Stream.scanl1 ==? scanl1
prop_scanl1'    = arg2 (not . null) $
                  arg_ty (A :-> A :-> A)
                  Stream.scanl1' ==? scanl1

tests = "vs. list" $$? [
                      "convert"         $? prop_convert

                    , "length"          $? prop_length
                    , "null"            $? prop_null

                    , "empty"           $? prop_empty
                    , "singleton"       $? prop_singleton
                    , "replicate"       $? prop_replicate
                    , "cons"            $? prop_cons
                    , "snoc"            $? prop_snoc
                    , "(++)"            $? prop_append
 
                    , "head"            $? prop_head
                    , "last"            $? prop_last
                    , "(!!)"            $? prop_index

                    , "extract"         $? prop_extract
                    , "init"            $? prop_init
                    , "tail"            $? prop_tail
                    , "take"            $? prop_take
                    , "drop"            $? prop_drop

                    , "map"             $? prop_map
                    , "zipWith"         $? prop_zipWith

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

