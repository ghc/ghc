{- --------------------------------------------------------------------------
// Dirty CPP hackery for CTypes/CTypesISO
//
// (c) The FFI task force, 2000
// --------------------------------------------------------------------------
-}

#pragma once

{-
// As long as there is no automatic derivation of classes for newtypes we resort
// to extremely dirty cpp-hackery.   :-P   Some care has to be taken when the
// macros below are modified, otherwise the layout rule will bite you.
-}

--  // GHC can derive any class for a newtype, so we make use of that here...

#define ARITHMETIC_CLASSES  Eq,Ord,Num,Enum,Storable,Real
#define INTEGRAL_CLASSES Bounded,Integral,Bits,FiniteBits
#define FLOATING_CLASSES Fractional,Floating,RealFrac,RealFloat
#define OPAQUE_CLASSES Eq,Ord,Storable

#define ARITHMETIC_TYPE(T,B) \
newtype T = T B deriving (ARITHMETIC_CLASSES) \
                deriving newtype (Read, Show);

#define INTEGRAL_TYPE(T,B) \
newtype T = T B deriving (ARITHMETIC_CLASSES, INTEGRAL_CLASSES) \
                deriving newtype (Read, Show);

#define INTEGRAL_TYPE_WITH_CTYPE(T,THE_CTYPE,B) \
newtype {-# CTYPE "THE_CTYPE" #-} T = T B \
    deriving (ARITHMETIC_CLASSES, INTEGRAL_CLASSES) \
    deriving newtype (Read, Show);

#define FLOATING_TYPE(T,B) \
newtype T = T B deriving (ARITHMETIC_CLASSES, FLOATING_CLASSES) \
                deriving newtype (Read, Show);

#define FLOATING_TYPE_WITH_CTYPE(T,THE_CTYPE,B) \
newtype {-# CTYPE "THE_CTYPE" #-} T = T B \
    deriving (ARITHMETIC_CLASSES, FLOATING_CLASSES) \
    deriving newtype (Read, Show);

#define OPAQUE_TYPE_WITH_CTYPE(T,THE_CTYPE,B) \
newtype {-# CTYPE "THE_CTYPE" #-} T = T (B) \
    deriving (OPAQUE_CLASSES) \
    deriving newtype Show;
