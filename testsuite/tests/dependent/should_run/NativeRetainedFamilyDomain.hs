{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, ExplicitNamespaces, FlexibleContexts, RankNTypes, RequiredTypeArguments, ScopedTypeVariables, TypeFamilies #-}
module NativeRetainedFamilyDomain where

import ConstrainedElements (Collects(..), FieldKey)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))

#ifdef REQUIRED_KINDS
project :: forall k -> forall (c :: k) -> Collects c => Proxy (Elem c)
project (type _) (type _) = Proxy

ok :: Proxy Bool
ok = project (type Type) (type [Bool])
#elif defined(WRONG_TAG)
different :: foreach (a :: Bool) -> foreach (b :: Bool)
          -> Collects (FieldKey a) => Proxy '(a, Elem (FieldKey b))
different _ _ = Proxy
#elif defined(LATE_GIVEN)
earlier :: Proxy (Elem (FieldKey 'False))
        -> foreach (b :: Bool) -> Collects (FieldKey 'False) => Proxy b
earlier _ _ = Proxy
#elif defined(CALLBACK_ESCAPE)
escape :: forall a. (Collects a => Proxy (Elem a)) -> Proxy (Elem a)
escape _ = Proxy
#elif defined(CALLBACK_MISSING)
unqualified :: (forall c. Proxy c -> Elem c -> Elem c) -> Bool
unqualified _ = True
#elif defined(REQUIRED_MISSING)
required :: forall (tag :: Bool) -> Proxy (Elem (FieldKey tag))
required (type _) = Proxy
#else
missing :: foreach (tag :: Bool) -> Proxy (Elem (FieldKey tag))
missing _ = Proxy
#endif
