{- --------------------------------------------------------------------------
// Dirty CPP hackery for CTypes/CTypesISO
//
// (c) The FFI task force, 2000
// --------------------------------------------------------------------------
-}

#ifndef CTYPES__H
#define CTYPES__H

#include "Typeable.h"

{-
// As long as there is no automatic derivation of classes for newtypes we resort
// to extremely dirty cpp-hackery.   :-P   Some care has to be taken when the
// macros below are modified, otherwise the layout rule will bite you.
-}

--  // GHC can derive any class for a newtype, so we make use of that here...

#define ARITHMETIC_CLASSES  Eq,Ord,Num,Enum,Storable,Real
#define INTEGRAL_CLASSES Bounded,Integral,Bits
#define FLOATING_CLASSES Fractional,Floating,RealFrac,RealFloat

#define ARITHMETIC_TYPE(T,C,S,B) \
newtype T = T B deriving (ARITHMETIC_CLASSES); \
INSTANCE_READ(T,B); \
INSTANCE_SHOW(T,B); \
INSTANCE_TYPEABLE0(T,C,S) ;

#define INTEGRAL_TYPE(T,C,S,B) \
newtype T = T B deriving (ARITHMETIC_CLASSES, INTEGRAL_CLASSES); \
INSTANCE_READ(T,B); \
INSTANCE_SHOW(T,B); \
INSTANCE_TYPEABLE0(T,C,S) ;

#define INTEGRAL_TYPE_WITH_CTYPE(T,THE_CTYPE,C,S,B) \
newtype {-# CTYPE "THE_CTYPE" #-} T = T B deriving (ARITHMETIC_CLASSES, INTEGRAL_CLASSES); \
INSTANCE_READ(T,B); \
INSTANCE_SHOW(T,B); \
INSTANCE_TYPEABLE0(T,C,S) ;

#define FLOATING_TYPE(T,C,S,B) \
newtype T = T B deriving (ARITHMETIC_CLASSES, FLOATING_CLASSES); \
INSTANCE_READ(T,B); \
INSTANCE_SHOW(T,B); \
INSTANCE_TYPEABLE0(T,C,S) ;

#define INSTANCE_READ(T,B) \
instance Read T where { \
   readsPrec            = unsafeCoerce# (readsPrec :: Int -> ReadS B); \
   readList             = unsafeCoerce# (readList  :: ReadS [B]); }

#define INSTANCE_SHOW(T,B) \
instance Show T where { \
   showsPrec            = unsafeCoerce# (showsPrec :: Int -> B -> ShowS); \
   show                 = unsafeCoerce# (show :: B -> String); \
   showList             = unsafeCoerce# (showList :: [B] -> ShowS); }

#endif
