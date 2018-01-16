{-# LANGUAGE TypeFamilies #-}

module ShouldFail where

class C8 a where
  data S8 a :: * -> *

instance C8 Int where
  data S8 Int a = S8Int a

-- Extra argument is not a variable; this is fine
instance C8 Bool where
  data S8 Bool Char = S8Bool
