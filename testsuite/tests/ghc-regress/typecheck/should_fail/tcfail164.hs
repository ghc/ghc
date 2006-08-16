{-# OPTIONS_GHC -fglasgow-exts #-}

-- Tests tagToEnum# hacks

module ShouldFail where

import GHC.Base

-- Test 1: Polymorphic
f :: a
f = tagToEnum# 0#

-- Test 2: Int value (not an Enumeration TyCon)
class Unboxable value where
    readUnboxable  :: Int -> value
instance Unboxable Int where
    readUnboxable (I# value#) =  tagToEnum# value# 

