{-# LANGUAGE TypeFamilies #-}

module ShouldFail where

-- must fail: C6 has no ATs S3 and Map
class C6 a

instance C6 Integer where
  data Map Integer v = MapInteger
  data S3 Integer = S3Integer
