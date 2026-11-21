{-# LANGUAGE CPP, DataKinds, ExplicitForAll, ExperimentalDependentTypes, GADTs, MultiParamTypeClasses, PolyKinds, TypeFamilies, UndecidableInstances #-}

module NativeAssociatedSumProvider (DefaultSum (..), Tagged (..)) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import NativeAssociatedSumClass (Build (..), Shape (..))

data Tagged (tag :: Bool) a where
    Yes :: a -> Tagged True a
    No :: a -> Tagged False a

#ifdef USE_NAMED
type Private a = some (tag :: Bool) | Tagged tag a
type KindPrivate a (k :: Type) (p :: k) =
    some (tag :: Bool) | (Proxy tag, Proxy a, Proxy p)
#endif

instance forall a. Build [a] where
#ifdef USE_NAMED
    type forall b. Result [a] b = Private (a, b)
#else
    type forall b. Result [a] b = some (tag :: Bool) | Tagged tag (a, b)
#endif

instance forall (j :: Type) (a :: j). Shape (Proxy a) where
#ifdef USE_NAMED
    type forall (k :: Type) (p :: k). Keep (Proxy a) k p = KindPrivate a k p
#else
    type forall (k :: Type) (p :: k). Keep (Proxy a) k p =
        some (tag :: Bool) | (Proxy tag, Proxy a, Proxy p)
#endif

class DefaultSum (a :: Type) where
    type Defaulted a
#if defined(BAD_DEFAULT_SCOPE)
    type Defaulted b = some (tag :: Bool) | Tagged tag a
#elif defined(BAD_DEFAULT_PATTERN)
    type Defaulted [b] = some (tag :: Bool) | Tagged tag b
#elif defined(USE_NAMED)
    type Defaulted b = Private b
#else
    type Defaulted b = some (tag :: Bool) | Tagged tag b
#endif

#ifdef BAD_INSTANCE_SCOPE
class Invalid a b where
    type Missing a

instance forall a b. Invalid [a] b where
    type Missing [a] = some (tag :: Bool) | Tagged tag b
#endif
