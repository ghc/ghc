{-# LANGUAGE Unsafe             #-}

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
           , MagicHash
           , KindSignatures
           , PolyKinds
           , ConstraintKinds
           , DeriveDataTypeable
           , StandaloneDeriving #-}

module Data.Typeable.Internal (
    Proxy (..),
    TypeRep(..),
    Fingerprint(..),
    typeOf, typeOf1, typeOf2, typeOf3, typeOf4, typeOf5, typeOf6, typeOf7,
    Typeable1, Typeable2, Typeable3, Typeable4, Typeable5, Typeable6, Typeable7,
    TyCon(..),
    typeRep,
    mkTyCon,
    mkTyCon3,
    mkTyConApp,
    mkAppTy,
    typeRepTyCon,
    Typeable(..),
    mkFunTy,
    splitTyConApp,
    funResultTy,
    typeRepArgs,
    showsTypeRep,
    tyConString,
    listTc, funTc
  ) where

import GHC.Base
import GHC.Word
import GHC.Show
import Data.Maybe
import Data.Proxy
import GHC.Num
import GHC.Real
-- import GHC.IORef
-- import GHC.IOArray
-- import GHC.MVar
import GHC.ST           ( ST )
import GHC.STRef        ( STRef )
import GHC.Ptr          ( Ptr, FunPtr )
-- import GHC.Stable
import GHC.Arr          ( Array, STArray )
import Data.Type.Coercion
import Data.Type.Equality
-- import Data.Int

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
   tyConPackage :: String, -- ^ /Since: 4.5.0.0/
   tyConModule  :: String, -- ^ /Since: 4.5.0.0/
   tyConName    :: String  -- ^ /Since: 4.5.0.0/
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
  TyCon (fingerprintString (pkg ++ (' ':modl) ++ (' ':name))) pkg modl name

----------------- Observation ---------------------

-- | Observe the type constructor of a type representation
typeRepTyCon :: TypeRep -> TyCon
typeRepTyCon (TypeRep _ tc _) = tc

-- | Observe the argument types of a type representation
typeRepArgs :: TypeRep -> [TypeRep]
typeRepArgs (TypeRep _ _ args) = args

-- | Observe string encoding of a type representation
{-# DEPRECATED tyConString "renamed to 'tyConName'; 'tyConModule' and 'tyConPackage' are also available." #-} -- deprecated in 7.4
tyConString :: TyCon   -> String
tyConString = tyConName

-------------------------------------------------------------
--
--      The Typeable class and friends
--
-------------------------------------------------------------

-- | The class 'Typeable' allows a concrete representation of a type to
-- be calculated.
class Typeable a where
  typeRep# :: Proxy# a -> TypeRep

--  Takes a value of type @a@ and returns a concrete representation
-- of that type.
--
-- /Since: 4.7.0.0/
typeRep :: forall proxy a. Typeable a => proxy a -> TypeRep
typeRep _ = typeRep# (proxy# :: Proxy# a)
{-# INLINE typeRep #-}

-- Keeping backwards-compatibility
typeOf :: forall a. Typeable a => a -> TypeRep
typeOf _ = typeRep (Proxy :: Proxy a)

typeOf1 :: forall t (a :: *). Typeable t => t a -> TypeRep
typeOf1 _ = typeRep (Proxy :: Proxy t)

typeOf2 :: forall t (a :: *) (b :: *). Typeable t => t a b -> TypeRep
typeOf2 _ = typeRep (Proxy :: Proxy t)

typeOf3 :: forall t (a :: *) (b :: *) (c :: *). Typeable t
        => t a b c -> TypeRep
typeOf3 _ = typeRep (Proxy :: Proxy t)

typeOf4 :: forall t (a :: *) (b :: *) (c :: *) (d :: *). Typeable t
        => t a b c d -> TypeRep
typeOf4 _ = typeRep (Proxy :: Proxy t)

typeOf5 :: forall t (a :: *) (b :: *) (c :: *) (d :: *) (e :: *). Typeable t
        => t a b c d e -> TypeRep
typeOf5 _ = typeRep (Proxy :: Proxy t)

typeOf6 :: forall t (a :: *) (b :: *) (c :: *) (d :: *) (e :: *) (f :: *).
                Typeable t => t a b c d e f -> TypeRep
typeOf6 _ = typeRep (Proxy :: Proxy t)

typeOf7 :: forall t (a :: *) (b :: *) (c :: *) (d :: *) (e :: *) (f :: *)
                (g :: *). Typeable t => t a b c d e f g -> TypeRep
typeOf7 _ = typeRep (Proxy :: Proxy t)

type Typeable1 (a :: * -> *)                               = Typeable a
type Typeable2 (a :: * -> * -> *)                          = Typeable a
type Typeable3 (a :: * -> * -> * -> *)                     = Typeable a
type Typeable4 (a :: * -> * -> * -> * -> *)                = Typeable a
type Typeable5 (a :: * -> * -> * -> * -> * -> *)           = Typeable a
type Typeable6 (a :: * -> * -> * -> * -> * -> * -> *)      = Typeable a
type Typeable7 (a :: * -> * -> * -> * -> * -> * -> * -> *) = Typeable a

{-# DEPRECATED Typeable1 "renamed to 'Typeable'" #-} -- deprecated in 7.8
{-# DEPRECATED Typeable2 "renamed to 'Typeable'" #-} -- deprecated in 7.8
{-# DEPRECATED Typeable3 "renamed to 'Typeable'" #-} -- deprecated in 7.8
{-# DEPRECATED Typeable4 "renamed to 'Typeable'" #-} -- deprecated in 7.8
{-# DEPRECATED Typeable5 "renamed to 'Typeable'" #-} -- deprecated in 7.8
{-# DEPRECATED Typeable6 "renamed to 'Typeable'" #-} -- deprecated in 7.8
{-# DEPRECATED Typeable7 "renamed to 'Typeable'" #-} -- deprecated in 7.8

-- | Kind-polymorphic Typeable instance for type application
instance (Typeable s, Typeable a) => Typeable (s a) where
  typeRep# _ = typeRep# (proxy# :: Proxy# s) `mkAppTy` typeRep# (proxy# :: Proxy# a)

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
            showArgs (showChar ' ') tys

showsTypeRep :: TypeRep -> ShowS
showsTypeRep = shows

instance Show TyCon where
  showsPrec _ t = showString (tyConName t)

isTupleTyCon :: TyCon -> Bool
isTupleTyCon (TyCon _ _ _ ('(':',':_)) = True
isTupleTyCon _                         = False

-- Some (Show.TypeRep) helpers:

showArgs :: Show a => ShowS -> [a] -> ShowS
showArgs _   []     = id
showArgs _   [a]    = showsPrec 10 a
showArgs sep (a:as) = showsPrec 10 a . sep . showArgs sep as 

showTuple :: [TypeRep] -> ShowS
showTuple args = showChar '('
               . showArgs (showChar ',') args
               . showChar ')'

listTc :: TyCon
listTc = typeRepTyCon (typeOf [()])

funTc :: TyCon
funTc = typeRepTyCon (typeRep (Proxy :: Proxy (->)))

-------------------------------------------------------------
--
--      Instances of the Typeable classes for Prelude types
--
-------------------------------------------------------------

deriving instance Typeable ()
deriving instance Typeable []
deriving instance Typeable Maybe
deriving instance Typeable Ratio
deriving instance Typeable (->)
deriving instance Typeable IO

deriving instance Typeable Array

deriving instance Typeable ST
deriving instance Typeable STRef
deriving instance Typeable STArray

deriving instance Typeable (,)
deriving instance Typeable (,,)
deriving instance Typeable (,,,)
deriving instance Typeable (,,,,)
deriving instance Typeable (,,,,,)
deriving instance Typeable (,,,,,,)

deriving instance Typeable Ptr
deriving instance Typeable FunPtr

-------------------------------------------------------
--
-- Generate Typeable instances for standard datatypes
--
-------------------------------------------------------

deriving instance Typeable Bool
deriving instance Typeable Char
deriving instance Typeable Float
deriving instance Typeable Double
deriving instance Typeable Int
deriving instance Typeable Word
deriving instance Typeable Integer
deriving instance Typeable Ordering

deriving instance Typeable Word8
deriving instance Typeable Word16
deriving instance Typeable Word32
deriving instance Typeable Word64

deriving instance Typeable TyCon
deriving instance Typeable TypeRep

deriving instance Typeable RealWorld
deriving instance Typeable Proxy
deriving instance Typeable (:~:)
deriving instance Typeable Coercion
