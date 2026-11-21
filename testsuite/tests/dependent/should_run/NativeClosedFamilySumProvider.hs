{-# LANGUAGE CPP, DataKinds, ExplicitForAll, ExperimentalDependentTypes, GADTs, PolyKinds, TypeFamilies #-}

module NativeClosedFamilySumProvider (KindCapture, Pack, PackViaAlias, Tagged (..)) where

import Data.Kind (Type)
import Data.Proxy (Proxy)

data Tagged (tag :: Bool) a where
    TrueTag :: a -> Tagged True a
    FalseTag :: a -> Tagged False a

type family Pack (select :: Bool) (a :: Type) :: Type where
    Pack False unused = some (tag :: Bool) | Tagged tag Bool
#ifdef BAD_SCOPE
    Pack True a = some (tag :: Bool) | Tagged tag unused
#else
    Pack True a = some (tag :: Bool) | Tagged tag a
#endif

type NamedSum a = some (tag :: Bool) | Tagged tag a

type family PackViaAlias (select :: Bool) a where
    PackViaAlias True a = NamedSum a

type family KindCapture (select :: Bool) (k :: Type) (item :: k) :: Type where
    forall (k :: Type) (item :: k). KindCapture True k item =
        some (tag :: Bool) | (Proxy tag, Proxy item)
