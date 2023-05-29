{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wterm-variable-capture #-}
module T23434 where

import GHC.Types (Type)

k = 12

class C k a where
  type AT a :: k -> Type
