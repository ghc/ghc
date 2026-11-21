{-# LANGUAGE CPP, DerivingStrategies, DerivingVia, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoExperimentalDependentTypes, StandaloneDeriving, TypeFamilies, UndecidableInstances #-}
module NativeDerivedFamilyScope where

import NativeConstrainedFamilyProvider (Allowed, ListBacking, ListSource)

#if !defined(VIA_ONLY) && !defined(INFERRED_CONTEXT)
newtype Wrapped a = Wrapped [a]
#ifdef NO_PROJECTION
type instance ListBacking (Wrapped a) = ()
#else
type instance ListBacking (Wrapped a) = ListSource [a]
#endif

deriving newtype instance Allowed a => ListSource (Wrapped a)
#endif

#if !defined(NEWTYPE_ONLY) && !defined(INFERRED_CONTEXT)
newtype ViaList a = ViaList [a]
#ifdef NO_PROJECTION
type instance ListBacking (ViaList a) = ()
#else
type instance ListBacking (ViaList a) = ListSource [a]
#endif

deriving via [a] instance Allowed a => ListSource (ViaList a)
#endif

#ifdef INFERRED_CONTEXT
newtype Inferred a = Inferred [a]
    deriving newtype ListSource

type instance ListBacking (Inferred a) = ()
#endif
