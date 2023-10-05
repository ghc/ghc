{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module ShouldCompile where

import Data.Kind (Type)

class C7 a b where
  data S7 b :: Type

instance C7 Char (a, Bool) where
  data S7 (a, Bool) = S7_1
