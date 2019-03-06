{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T16391b where

import GHC.Exts

type family T (r :: RuntimeRep) :: TYPE r

foo :: T r
foo = foo
