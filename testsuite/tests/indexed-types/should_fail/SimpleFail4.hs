{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module ShouldFail where

import Data.Kind (Type)

-- must fail: defaults have no patterns
class C2 a b where
  type S2 a :: Type
  type S2 Int = Char
