{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies #-}

module NativeClosedFamilySumProvider (Pack, Tagged (..)) where

import Data.Kind (Type)

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
