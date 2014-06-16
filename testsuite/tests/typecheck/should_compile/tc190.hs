{-# LANGUAGE CPP, KindSignatures #-}

-- The record update triggered a kind error in GHC 6.2

module Foo where

data HT (ref :: * -> *)
  = HT { kcount :: Int }

set_kcount :: Int -> HT s -> HT s
set_kcount kc ht = ht{kcount=kc}
