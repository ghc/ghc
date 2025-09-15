{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module T19495 where
-- This test is a copy of T16391b, but run with -ddump-tc-trace
-- which (in #19495) triggered a panic

import GHC.Exts

type family T (r :: RuntimeRep) :: TYPE r

foo :: T r
foo = foo
