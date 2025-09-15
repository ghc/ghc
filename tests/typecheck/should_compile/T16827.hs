{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T16827 where

import Data.Kind
import GHC.Exts

data family Foo (a :: Type) :: TYPE 'IntRep
