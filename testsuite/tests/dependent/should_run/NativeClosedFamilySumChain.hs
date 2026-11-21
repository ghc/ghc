{-# LANGUAGE DataKinds, TypeFamilies, UndecidableInstances #-}

module NativeClosedFamilySumChain (Loop, Outer) where

import Data.Kind (Type)
import NativeClosedFamilySumProvider (Pack)

type family Outer (select :: Bool) (a :: Type) :: Type where
    Outer select a = Pack select a

type family Loop (a :: Type) :: Type where
    Loop a = Loop [a]
