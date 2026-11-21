{-# LANGUAGE CPP, DataKinds, ExplicitForAll, ExperimentalDependentTypes, PolyKinds, TypeFamilies, UndecidableInstances #-}

module NativeClosedFamilySumChain (Loop, OpenLoop, OpenPackage, Outer) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import NativeClosedFamilySumProvider (KindOpen, Pack, Tagged)
import qualified NativeClosedFamilySumProvider as Provider

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

type instance Provider.DirectOpen [a] = some (tag :: Bool) | Tagged tag a
type instance Provider.DirectOpen Char = some (tag :: Bool) | Tagged tag Bool

type instance forall (k :: Type) (item :: k). KindOpen k item =
    some (tag :: Bool) | (Proxy tag, Proxy item)

#ifdef BAD_OPEN_CAPTURE
type instance Provider.DirectOpen Bool = some (tag :: Bool) | Tagged tag missing
#endif

#ifdef BAD_OPEN_BINDER
type instance Provider.DirectOpen Bool = some (tag :: Proxy tag) | Char
#endif

#ifdef BAD_DATA_CATEGORY
data family DataOpen a
type instance DataOpen Bool = some (tag :: Bool) | Tagged tag Bool
#endif
