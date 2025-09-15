{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module ShouldFail where

import Data.Kind

class C7 a b where
  data S7 b :: Type

instance C7 Char (a, Bool) where
  data S7 (a, Bool) = S7_1

-- Fails because the arg to S7 should be the
-- same as that to C7
instance C7 Char (a, Int) where
  data S7 (b, Int) = S7_2
