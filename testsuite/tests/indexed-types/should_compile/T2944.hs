{-# LANGUAGE TypeFamilies #-}
-- Test #2944

module T2944 where

import Data.Kind (Type)

type family T a :: Type

f1 :: T a ~ () => a
f1 = f2

f2 :: T a ~ () => a
f2 = f1
