{-# LANGUAGE TypeFamilies #-}

module ShouldCompile where

import Data.Kind (Type)

class C a where
  data Sd a :: Type
  data Sn a :: Type
  type St a :: Type

instance C Int where
  data    Sd Int = SdC Char
  newtype Sn Int = SnC Char
  type    St Int = Char
