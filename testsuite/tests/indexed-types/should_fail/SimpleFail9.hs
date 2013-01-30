{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module ShouldFail where

class C7 a b where
  data S7 b :: *

instance C7 Char (a, Bool) where
  data S7 (a, Bool) = S7_1

-- Used to fail, but now passes: 
-- type indexes don't match the instance types by name
-- but do by structure
instance C7 Char (a, Int) where
  data S7 (b, Int) = S7_2
