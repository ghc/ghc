{-# LANGUAGE DataKinds, TypeFamilies #-}

module T12088c where

import Data.Kind

type family K a

class C a where   -- C:def
  type F a :: K a -- F:sig
  type G a :: K a -- G:sig

data T

type instance K T = Type

instance C T where    -- C:inst
  type F T = Bool     -- F:def
  type G T = String   -- G:def