{-# LANGUAGE TypeFamilies #-}

module ShouldCompile where

class C8 a where
  data S8 a :: * -> *

instance C8 Int where
  data S8 Int a = S8Int a
