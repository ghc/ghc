{-# LANGUAGE TypeFamilies #-}
-- Test Trac #2944

module T2944 where

type family T a :: *

f1 :: T a ~ () => a
f1 = f2

f2 :: T a ~ () => a
f2 = f1
