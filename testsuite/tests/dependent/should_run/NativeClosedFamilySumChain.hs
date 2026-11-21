{-# LANGUAGE DataKinds, TypeFamilies, UndecidableInstances #-}

module NativeClosedFamilySumChain (Loop, OpenLoop, OpenPackage, Outer) where

import Data.Kind (Type)
import NativeClosedFamilySumProvider (Pack)

type family Outer (select :: Bool) (a :: Type) :: Type where
    Outer select a = Pack select a

type family Loop (a :: Type) :: Type where
    Loop a = Loop [a]

type family OpenPackage (a :: Type) :: Type
type instance OpenPackage Bool = Outer True Char
type instance OpenPackage Int = WrappedOpenPackage

newtype WrappedOpenPackage = WrappedOpenPackage (Pack True Char)

type family OpenLoop (a :: Type) :: Type
type instance OpenLoop a = OpenLoop [a]
