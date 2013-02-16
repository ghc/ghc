{-# LANGUAGE Unsafe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Typeable.Internal
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- The representations of the types TyCon and TypeRep, and the
-- function mkTyCon which is used by derived instances of Typeable to
-- construct a TyCon.
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP
           , NoImplicitPrelude
           , OverlappingInstances
           , ScopedTypeVariables
           , FlexibleInstances
           , MagicHash #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif

module Data.OldTypeable.Internal {-# DEPRECATED "Use Data.Typeable.Internal instead" #-} ( -- deprecated in 7.8
    TypeRep(..),
    TyCon(..),
    mkTyCon,
    mkTyCon3,
    mkTyConApp,
    mkAppTy,
    typeRepTyCon,
    typeOfDefault,
    typeOf1Default,
    typeOf2Default,
    typeOf3Default,
    typeOf4Default,
    typeOf5Default,
    typeOf6Default,
    Typeable(..),
    Typeable1(..),
    Typeable2(..),
    Typeable3(..),
    Typeable4(..),
    Typeable5(..),
    Typeable6(..),
    Typeable7(..),
    mkFunTy,
    splitTyConApp,
    funResultTy,
    typeRepArgs,
    showsTypeRep,
    tyConString,
#if defined(__GLASGOW_HASKELL__)
    listTc, funTc
#endif
  ) where

import GHC.Base
import GHC.Word
import GHC.Show
import GHC.Err          (undefined)
import Data.Maybe
import Data.List
import GHC.Num
import GHC.Real
import GHC.IORef
import GHC.IOArray
import GHC.MVar
import GHC.ST           ( ST )
import GHC.STRef        ( STRef )
import GHC.Ptr          ( Ptr, FunPtr )
import GHC.Stable
import GHC.Arr          ( Array, STArray )
import Data.Int

import GHC.Fingerprint.Type
import {-# SOURCE #-} GHC.Fingerprint
   -- loop: GHC.Fingerprint -> Foreign.Ptr -> Data.Typeable
   -- Better to break the loop here, because we want non-SOURCE imports
   -- of Data.Typeable as much as possible so we can optimise the derived
   -- instances.

-- | A concrete representation of a (monomorphic) type.  'TypeRep'
-- supports reasonably efficient equality.
data TypeRep = TypeRep {-# UNPACK #-} !Fingerprint TyCon [TypeRep]

-- Compare keys for equality
instance Eq TypeRep where
  (TypeRep k1 _ _) == (TypeRep k2 _ _) = k1 == k2

instance Ord TypeRep where
  (TypeRep k1 _ _) <= (TypeRep k2 _ _) = k1 <= k2

-- | An abstract representation of a type constructor.  'TyCon' objects can
-- be built using 'mkTyCon'.
data TyCon = TyCon {
   tyConHash    :: {-# UNPACK #-} !Fingerprint,
   tyConPackage :: String,
   tyConModule  :: String,
   tyConName    :: String
 }

instance Eq TyCon where
  (TyCon t1 _ _ _) == (TyCon t2 _ _ _) = t1 == t2

instance Ord TyCon where
  (TyCon k1 _ _ _) <= (TyCon k2 _ _ _) = k1 <= k2

----------------- Construction --------------------

#include "MachDeps.h"

-- mkTyCon is an internal function to make it easier for GHC to
-- generate derived instances.  GHC precomputes the MD5 hash for the
-- TyCon and passes it as two separate 64-bit values to mkTyCon.  The
-- TyCon for a derived Typeable instance will end up being statically
-- allocated.

#if WORD_SIZE_IN_BITS < 64
mkTyCon :: Word64# -> Word64# -> String -> String -> String -> TyCon
#else
mkTyCon :: Word#   -> Word#   -> String -> String -> String -> TyCon
#endif
mkTyCon high# low# pkg modl name
  = TyCon (Fingerprint (W64# high#) (W64# low#)) pkg modl name

-- | Applies a type constructor to a sequence of types
mkTyConApp  :: TyCon -> [TypeRep] -> TypeRep
mkTyConApp tc@(TyCon tc_k _ _ _) []
  = TypeRep tc_k tc [] -- optimisation: all derived Typeable instances
                       -- end up here, and it helps generate smaller
                       -- code for derived Typeable.
mkTyConApp tc@(TyCon tc_k _ _ _) args
  = TypeRep (fingerprintFingerprints (tc_k : arg_ks)) tc args
  where
    arg_ks = [k | TypeRep k _ _ <- args]

-- | A special case of 'mkTyConApp', which applies the function 
-- type constructor to a pair of types.
mkFunTy  :: TypeRep -> TypeRep -> TypeRep
mkFunTy f a = mkTyConApp funTc [f,a]

-- | Splits a type constructor application
splitTyConApp :: TypeRep -> (TyCon,[TypeRep])
splitTyConApp (TypeRep _ tc trs) = (tc,trs)

-- | Applies a type to a function type.  Returns: @'Just' u@ if the
-- first argument represents a function of type @t -> u@ and the
-- second argument represents a function of type @t@.  Otherwise,
-- returns 'Nothing'.
funResultTy :: TypeRep -> TypeRep -> Maybe TypeRep
funResultTy trFun trArg
  = case splitTyConApp trFun of
      (tc, [t1,t2]) | tc == funTc && t1 == trArg -> Just t2
      _ -> Nothing

-- | Adds a TypeRep argument to a TypeRep.
mkAppTy :: TypeRep -> TypeRep -> TypeRep
mkAppTy (TypeRep _ tc trs) arg_tr = mkTyConApp tc (trs ++ [arg_tr])
   -- Notice that we call mkTyConApp to construct the fingerprint from tc and
   -- the arg fingerprints.  Simply combining the current fingerprint with
   -- the new one won't give the same answer, but of course we want to 
   -- ensure that a TypeRep of the same shape has the same fingerprint!
   -- See Trac #5962

-- | Builds a 'TyCon' object representing a type constructor.  An
-- implementation of "Data.Typeable" should ensure that the following holds:
--
-- >  A==A' ^ B==B' ^ C==C' ==> mkTyCon A B C == mkTyCon A' B' C'
--

--
mkTyCon3 :: String       -- ^ package name
         -> String       -- ^ module name
         -> String       -- ^ the name of the type constructor
         -> TyCon        -- ^ A unique 'TyCon' object
mkTyCon3 pkg modl name =
  TyCon (fingerprintString (unwords [pkg, modl, name])) pkg modl name

----------------- Observation ---------------------

-- | Observe the type constructor of a type representation
typeRepTyCon :: TypeRep -> TyCon
typeRepTyCon (TypeRep _ tc _) = tc

-- | Observe the argument types of a type representation
typeRepArgs :: TypeRep -> [TypeRep]
typeRepArgs (TypeRep _ _ args) = args

-- | Observe string encoding of a type representation
{-# DEPRECATED tyConString "renamed to tyConName; tyConModule and tyConPackage are also available." #-} -- deprecated in 7.4
tyConString :: TyCon   -> String
tyConString = tyConName

-------------------------------------------------------------
--
--      The Typeable class and friends
--
-------------------------------------------------------------

{- Note [Memoising typeOf]
~~~~~~~~~~~~~~~~~~~~~~~~~~
IMPORTANT: we don't want to recalculate the type-rep once per
call to the dummy argument.  This is what went wrong in Trac #3245
So we help GHC by manually keeping the 'rep' *outside* the value 
lambda, thus
    
    typeOfDefault :: forall t a. (Typeable1 t, Typeable a) => t a -> TypeRep
    typeOfDefault = \_ -> rep
      where
        rep = typeOf1 (undefined :: t a) `mkAppTy` 
              typeOf  (undefined :: a)

Notice the crucial use of scoped type variables here!
-}

-- | The class 'Typeable' allows a concrete representation of a type to
-- be calculated.
class Typeable a where
  typeOf :: a -> TypeRep
  -- ^ Takes a value of type @a@ and returns a concrete representation
  -- of that type.  The /value/ of the argument should be ignored by
  -- any instance of 'Typeable', so that it is safe to pass 'undefined' as
  -- the argument.

-- | Variant for unary type constructors
class Typeable1 t where
  typeOf1 :: t a -> TypeRep

#ifdef __GLASGOW_HASKELL__
-- | For defining a 'Typeable' instance from any 'Typeable1' instance.
typeOfDefault :: forall t a. (Typeable1 t, Typeable a) => t a -> TypeRep
typeOfDefault = \_ -> rep
 where
   rep = typeOf1 (undefined :: t a) `mkAppTy` 
         typeOf  (undefined :: a)
   -- Note [Memoising typeOf]
#else
-- | For defining a 'Typeable' instance from any 'Typeable1' instance.
typeOfDefault :: (Typeable1 t, Typeable a) => t a -> TypeRep
typeOfDefault x = typeOf1 x `mkAppTy` typeOf (argType x)
 where
   argType :: t a -> a
   argType = undefined
#endif

-- | Variant for binary type constructors
class Typeable2 t where
  typeOf2 :: t a b -> TypeRep

#ifdef __GLASGOW_HASKELL__
-- | For defining a 'Typeable1' instance from any 'Typeable2' instance.
typeOf1Default :: forall t a b. (Typeable2 t, Typeable a) => t a b -> TypeRep
typeOf1Default = \_ -> rep 
 where
   rep = typeOf2 (undefined :: t a b) `mkAppTy` 
         typeOf  (undefined :: a)
   -- Note [Memoising typeOf]
#else
-- | For defining a 'Typeable1' instance from any 'Typeable2' instance.
typeOf1Default :: (Typeable2 t, Typeable a) => t a b -> TypeRep
typeOf1Default x = typeOf2 x `mkAppTy` typeOf (argType x)
 where
   argType :: t a b -> a
   argType = undefined
#endif

-- | Variant for 3-ary type constructors
class Typeable3 t where
  typeOf3 :: t a b c -> TypeRep

#ifdef __GLASGOW_HASKELL__
-- | For defining a 'Typeable2' instance from any 'Typeable3' instance.
typeOf2Default :: forall t a b c. (Typeable3 t, Typeable a) => t a b c -> TypeRep
typeOf2Default = \_ -> rep 
 where
   rep = typeOf3 (undefined :: t a b c) `mkAppTy` 
         typeOf  (undefined :: a)
   -- Note [Memoising typeOf]
#else
-- | For defining a 'Typeable2' instance from any 'Typeable3' instance.
typeOf2Default :: (Typeable3 t, Typeable a) => t a b c -> TypeRep
typeOf2Default x = typeOf3 x `mkAppTy` typeOf (argType x)
 where
   argType :: t a b c -> a
   argType = undefined
#endif

-- | Variant for 4-ary type constructors
class Typeable4 t where
  typeOf4 :: t a b c d -> TypeRep

#ifdef __GLASGOW_HASKELL__
-- | For defining a 'Typeable3' instance from any 'Typeable4' instance.
typeOf3Default :: forall t a b c d. (Typeable4 t, Typeable a) => t a b c d -> TypeRep
typeOf3Default = \_ -> rep
 where
   rep = typeOf4 (undefined :: t a b c d) `mkAppTy` 
         typeOf  (undefined :: a)
   -- Note [Memoising typeOf]
#else
-- | For defining a 'Typeable3' instance from any 'Typeable4' instance.
typeOf3Default :: (Typeable4 t, Typeable a) => t a b c d -> TypeRep
typeOf3Default x = typeOf4 x `mkAppTy` typeOf (argType x)
 where
   argType :: t a b c d -> a
   argType = undefined
#endif
   
-- | Variant for 5-ary type constructors
class Typeable5 t where
  typeOf5 :: t a b c d e -> TypeRep

#ifdef __GLASGOW_HASKELL__
-- | For defining a 'Typeable4' instance from any 'Typeable5' instance.
typeOf4Default :: forall t a b c d e. (Typeable5 t, Typeable a) => t a b c d e -> TypeRep
typeOf4Default = \_ -> rep 
 where
   rep = typeOf5 (undefined :: t a b c d e) `mkAppTy` 
         typeOf  (undefined :: a)
   -- Note [Memoising typeOf]
#else
-- | For defining a 'Typeable4' instance from any 'Typeable5' instance.
typeOf4Default :: (Typeable5 t, Typeable a) => t a b c d e -> TypeRep
typeOf4Default x = typeOf5 x `mkAppTy` typeOf (argType x)
 where
   argType :: t a b c d e -> a
   argType = undefined
#endif

-- | Variant for 6-ary type constructors
class Typeable6 t where
  typeOf6 :: t a b c d e f -> TypeRep

#ifdef __GLASGOW_HASKELL__
-- | For defining a 'Typeable5' instance from any 'Typeable6' instance.
typeOf5Default :: forall t a b c d e f. (Typeable6 t, Typeable a) => t a b c d e f -> TypeRep
typeOf5Default = \_ -> rep
 where
   rep = typeOf6 (undefined :: t a b c d e f) `mkAppTy` 
         typeOf  (undefined :: a)
   -- Note [Memoising typeOf]
#else
-- | For defining a 'Typeable5' instance from any 'Typeable6' instance.
typeOf5Default :: (Typeable6 t, Typeable a) => t a b c d e f -> TypeRep
typeOf5Default x = typeOf6 x `mkAppTy` typeOf (argType x)
 where
   argType :: t a b c d e f -> a
   argType = undefined
#endif

-- | Variant for 7-ary type constructors
class Typeable7 t where
  typeOf7 :: t a b c d e f g -> TypeRep

#ifdef __GLASGOW_HASKELL__
-- | For defining a 'Typeable6' instance from any 'Typeable7' instance.
typeOf6Default :: forall t a b c d e f g. (Typeable7 t, Typeable a) => t a b c d e f g -> TypeRep
typeOf6Default = \_ -> rep
 where
   rep = typeOf7 (undefined :: t a b c d e f g) `mkAppTy` 
         typeOf  (undefined :: a)
   -- Note [Memoising typeOf]
#else
-- | For defining a 'Typeable6' instance from any 'Typeable7' instance.
typeOf6Default :: (Typeable7 t, Typeable a) => t a b c d e f g -> TypeRep
typeOf6Default x = typeOf7 x `mkAppTy` typeOf (argType x)
 where
   argType :: t a b c d e f g -> a
   argType = undefined
#endif

#ifdef __GLASGOW_HASKELL__
-- Given a @Typeable@/n/ instance for an /n/-ary type constructor,
-- define the instances for partial applications.
-- Programmers using non-GHC implementations must do this manually
-- for each type constructor.
-- (The INSTANCE_TYPEABLE/n/ macros in Typeable.h include this.)

-- | One Typeable instance for all Typeable1 instances
instance (Typeable1 s, Typeable a)
       => Typeable (s a) where
  typeOf = typeOfDefault

-- | One Typeable1 instance for all Typeable2 instances
instance (Typeable2 s, Typeable a)
       => Typeable1 (s a) where
  typeOf1 = typeOf1Default

-- | One Typeable2 instance for all Typeable3 instances
instance (Typeable3 s, Typeable a)
       => Typeable2 (s a) where
  typeOf2 = typeOf2Default

-- | One Typeable3 instance for all Typeable4 instances
instance (Typeable4 s, Typeable a)
       => Typeable3 (s a) where
  typeOf3 = typeOf3Default

-- | One Typeable4 instance for all Typeable5 instances
instance (Typeable5 s, Typeable a)
       => Typeable4 (s a) where
  typeOf4 = typeOf4Default

-- | One Typeable5 instance for all Typeable6 instances
instance (Typeable6 s, Typeable a)
       => Typeable5 (s a) where
  typeOf5 = typeOf5Default

-- | One Typeable6 instance for all Typeable7 instances
instance (Typeable7 s, Typeable a)
       => Typeable6 (s a) where
  typeOf6 = typeOf6Default

#endif /* __GLASGOW_HASKELL__ */

----------------- Showing TypeReps --------------------

instance Show TypeRep where
  showsPrec p (TypeRep _ tycon tys) =
    case tys of
      [] -> showsPrec p tycon
      [x]   | tycon == listTc -> showChar '[' . shows x . showChar ']'
      [a,r] | tycon == funTc  -> showParen (p > 8) $
                                 showsPrec 9 a .
                                 showString " -> " .
                                 showsPrec 8 r
      xs | isTupleTyCon tycon -> showTuple xs
         | otherwise         ->
            showParen (p > 9) $
            showsPrec p tycon . 
            showChar ' '      . 
            showArgs tys

showsTypeRep :: TypeRep -> ShowS
showsTypeRep = shows

instance Show TyCon where
  showsPrec _ t = showString (tyConName t)

isTupleTyCon :: TyCon -> Bool
isTupleTyCon (TyCon _ _ _ ('(':',':_)) = True
isTupleTyCon _                         = False

-- Some (Show.TypeRep) helpers:

showArgs :: Show a => [a] -> ShowS
showArgs [] = id
showArgs [a] = showsPrec 10 a
showArgs (a:as) = showsPrec 10 a . showString " " . showArgs as 

showTuple :: [TypeRep] -> ShowS
showTuple args = showChar '('
               . (foldr (.) id $ intersperse (showChar ',') 
                               $ map (showsPrec 10) args)
               . showChar ')'

#if defined(__GLASGOW_HASKELL__)
listTc :: TyCon
listTc = typeRepTyCon (typeOf [()])

funTc :: TyCon
funTc = mkTyCon3 "ghc-prim" "GHC.Types" "->"
#endif

-------------------------------------------------------------
--
--      Instances of the Typeable classes for Prelude types
--
-------------------------------------------------------------

#include "OldTypeable.h"

INSTANCE_TYPEABLE0((),unitTc,"()")
INSTANCE_TYPEABLE1([],listTc,"[]")
INSTANCE_TYPEABLE1(Maybe,maybeTc,"Maybe")
INSTANCE_TYPEABLE1(Ratio,ratioTc,"Ratio")
#if defined(__GLASGOW_HASKELL__)
{-
TODO: Deriving this instance fails with:
libraries/base/Data/Typeable.hs:589:1:
    Can't make a derived instance of `Typeable2 (->)':
      The last argument of the instance must be a data or newtype application
    In the stand-alone deriving instance for `Typeable2 (->)'
-}
instance Typeable2 (->) where { typeOf2 _ = mkTyConApp funTc [] }
#else
INSTANCE_TYPEABLE2((->),funTc,"->")
#endif
INSTANCE_TYPEABLE1(IO,ioTc,"IO")

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
-- Types defined in GHC.MVar
INSTANCE_TYPEABLE1(MVar,mvarTc,"MVar" )
#endif

INSTANCE_TYPEABLE2(Array,arrayTc,"Array")
INSTANCE_TYPEABLE2(IOArray,iOArrayTc,"IOArray")

#ifdef __GLASGOW_HASKELL__
-- Hugs has these too, but their Typeable<n> instances are defined
-- elsewhere to keep this module within Haskell 98.
-- This is important because every invocation of runhugs or ffihugs
-- uses this module via Data.Dynamic.
INSTANCE_TYPEABLE2(ST,stTc,"ST")
INSTANCE_TYPEABLE2(STRef,stRefTc,"STRef")
INSTANCE_TYPEABLE3(STArray,sTArrayTc,"STArray")
#endif

INSTANCE_TYPEABLE2((,),pairTc,"(,)")
INSTANCE_TYPEABLE3((,,),tup3Tc,"(,,)")
INSTANCE_TYPEABLE4((,,,),tup4Tc,"(,,,)")
INSTANCE_TYPEABLE5((,,,,),tup5Tc,"(,,,,)")
INSTANCE_TYPEABLE6((,,,,,),tup6Tc,"(,,,,,)")
INSTANCE_TYPEABLE7((,,,,,,),tup7Tc,"(,,,,,,)")

INSTANCE_TYPEABLE1(Ptr,ptrTc,"Ptr")
INSTANCE_TYPEABLE1(FunPtr,funPtrTc,"FunPtr")
#ifndef __GLASGOW_HASKELL__
INSTANCE_TYPEABLE1(ForeignPtr,foreignPtrTc,"ForeignPtr")
#endif
INSTANCE_TYPEABLE1(StablePtr,stablePtrTc,"StablePtr")
INSTANCE_TYPEABLE1(IORef,iORefTc,"IORef")

-------------------------------------------------------
--
-- Generate Typeable instances for standard datatypes
--
-------------------------------------------------------

INSTANCE_TYPEABLE0(Bool,boolTc,"Bool")
INSTANCE_TYPEABLE0(Char,charTc,"Char")
INSTANCE_TYPEABLE0(Float,floatTc,"Float")
INSTANCE_TYPEABLE0(Double,doubleTc,"Double")
INSTANCE_TYPEABLE0(Int,intTc,"Int")
INSTANCE_TYPEABLE0(Word,wordTc,"Word" )
INSTANCE_TYPEABLE0(Integer,integerTc,"Integer")
INSTANCE_TYPEABLE0(Ordering,orderingTc,"Ordering")
#ifndef __GLASGOW_HASKELL__
INSTANCE_TYPEABLE0(Handle,handleTc,"Handle")
#endif

INSTANCE_TYPEABLE0(Int8,int8Tc,"Int8")
INSTANCE_TYPEABLE0(Int16,int16Tc,"Int16")
INSTANCE_TYPEABLE0(Int32,int32Tc,"Int32")
INSTANCE_TYPEABLE0(Int64,int64Tc,"Int64")

INSTANCE_TYPEABLE0(Word8,word8Tc,"Word8" )
INSTANCE_TYPEABLE0(Word16,word16Tc,"Word16")
INSTANCE_TYPEABLE0(Word32,word32Tc,"Word32")
INSTANCE_TYPEABLE0(Word64,word64Tc,"Word64")

INSTANCE_TYPEABLE0(TyCon,tyconTc,"TyCon")
INSTANCE_TYPEABLE0(TypeRep,typeRepTc,"TypeRep")

#ifdef __GLASGOW_HASKELL__
{-
TODO: This can't be derived currently:
libraries/base/Data/Typeable.hs:674:1:
    Can't make a derived instance of `Typeable RealWorld':
      The last argument of the instance must be a data or newtype application
    In the stand-alone deriving instance for `Typeable RealWorld'
-}
realWorldTc :: TyCon; \
realWorldTc = mkTyCon3 "ghc-prim" "GHC.Types" "RealWorld"; \
instance Typeable RealWorld where { typeOf _ = mkTyConApp realWorldTc [] }

#endif
