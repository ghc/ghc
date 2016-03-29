{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

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

module Data.Typeable.Internal (
    Proxy (..),
    Fingerprint(..),

    -- * Typeable class
    typeOf, typeOf1, typeOf2, typeOf3, typeOf4, typeOf5, typeOf6, typeOf7,
    Typeable1, Typeable2, Typeable3, Typeable4, Typeable5, Typeable6, Typeable7,

    -- * Module
    Module,  -- Abstract
    moduleName, modulePackage,

    -- * TyCon
    TyCon,   -- Abstract
    tyConPackage, tyConModule, tyConName, tyConString, tyConFingerprint,
    mkTyCon3, mkTyCon3#,
    rnfTyCon,

    -- * TypeRep
    TypeRep(..), KindRep,
    typeRep,
    mkTyConApp,
    mkPolyTyConApp,
    mkAppTy,
    typeRepTyCon,
    Typeable(..),
    mkFunTy,
    splitTyConApp,
    splitPolyTyConApp,
    funResultTy,
    typeRepArgs,
    typeRepFingerprint,
    rnfTypeRep,
    showsTypeRep,
    typeRepKinds,
    typeSymbolTypeRep, typeNatTypeRep
  ) where

import GHC.Base
import GHC.Types (TYPE)
import GHC.Word
import GHC.Show
import Data.Proxy
import GHC.TypeLits( KnownNat, KnownSymbol, natVal', symbolVal' )

import GHC.Fingerprint.Type
import {-# SOURCE #-} GHC.Fingerprint
   -- loop: GHC.Fingerprint -> Foreign.Ptr -> Data.Typeable
   -- Better to break the loop here, because we want non-SOURCE imports
   -- of Data.Typeable as much as possible so we can optimise the derived
   -- instances.

#include "MachDeps.h"

{- *********************************************************************
*                                                                      *
                The TyCon type
*                                                                      *
********************************************************************* -}

modulePackage :: Module -> String
modulePackage (Module p _) = trNameString p

moduleName :: Module -> String
moduleName (Module _ m) = trNameString m

tyConPackage :: TyCon -> String
tyConPackage (TyCon _ _ m _) = modulePackage m

tyConModule :: TyCon -> String
tyConModule (TyCon _ _ m _) = moduleName m

tyConName :: TyCon -> String
tyConName (TyCon _ _ _ n) = trNameString n

trNameString :: TrName -> String
trNameString (TrNameS s) = unpackCString# s
trNameString (TrNameD s) = s

-- | Observe string encoding of a type representation
{-# DEPRECATED tyConString "renamed to 'tyConName'; 'tyConModule' and 'tyConPackage' are also available." #-}
-- deprecated in 7.4
tyConString :: TyCon   -> String
tyConString = tyConName

tyConFingerprint :: TyCon -> Fingerprint
tyConFingerprint (TyCon hi lo _ _)
  = Fingerprint (W64# hi) (W64# lo)

mkTyCon3# :: Addr#       -- ^ package name
          -> Addr#       -- ^ module name
          -> Addr#       -- ^ the name of the type constructor
          -> TyCon       -- ^ A unique 'TyCon' object
mkTyCon3# pkg modl name
  | Fingerprint (W64# hi) (W64# lo) <- fingerprint
  = TyCon hi lo (Module (TrNameS pkg) (TrNameS modl)) (TrNameS name)
  where
    fingerprint :: Fingerprint
    fingerprint = fingerprintString (unpackCString# pkg
                                    ++ (' ': unpackCString# modl)
                                    ++ (' ' : unpackCString# name))

mkTyCon3 :: String       -- ^ package name
         -> String       -- ^ module name
         -> String       -- ^ the name of the type constructor
         -> TyCon        -- ^ A unique 'TyCon' object
-- Used when the strings are dynamically allocated,
-- eg from binary deserialisation
mkTyCon3 pkg modl name
  | Fingerprint (W64# hi) (W64# lo) <- fingerprint
  = TyCon hi lo (Module (TrNameD pkg) (TrNameD modl)) (TrNameD name)
  where
    fingerprint :: Fingerprint
    fingerprint = fingerprintString (pkg ++ (' ':modl) ++ (' ':name))

isTupleTyCon :: TyCon -> Bool
isTupleTyCon tc
  | ('(':',':_) <- tyConName tc = True
  | otherwise                   = False

-- | Helper to fully evaluate 'TyCon' for use as @NFData(rnf)@ implementation
--
-- @since 4.8.0.0
rnfModule :: Module -> ()
rnfModule (Module p m) = rnfTrName p `seq` rnfTrName m

rnfTrName :: TrName -> ()
rnfTrName (TrNameS _) = ()
rnfTrName (TrNameD n) = rnfString n

rnfTyCon :: TyCon -> ()
rnfTyCon (TyCon _ _ m n) = rnfModule m `seq` rnfTrName n

rnfString :: [Char] -> ()
rnfString [] = ()
rnfString (c:cs) = c `seq` rnfString cs


{- *********************************************************************
*                                                                      *
                The TypeRep type
*                                                                      *
********************************************************************* -}

-- | A concrete representation of a (monomorphic) type.
-- 'TypeRep' supports reasonably efficient equality.
data TypeRep = TypeRep {-# UNPACK #-} !Fingerprint TyCon [KindRep] [TypeRep]
     -- NB: For now I've made this lazy so that it's easy to
     -- optimise code that constructs and deconstructs TypeReps
     -- perf/should_run/T9203 is a good example
     -- Also note that mkAppTy does discards the fingerprint,
     -- so it's a waste to compute it

type KindRep = TypeRep

-- Compare keys for equality
instance Eq TypeRep where
  TypeRep x _ _ _ == TypeRep y _ _ _ = x == y

instance Ord TypeRep where
  TypeRep x _ _ _ <= TypeRep y _ _ _ = x <= y

-- | Observe the 'Fingerprint' of a type representation
--
-- @since 4.8.0.0
typeRepFingerprint :: TypeRep -> Fingerprint
typeRepFingerprint (TypeRep fpr _ _ _) = fpr

-- | Applies a kind-polymorphic type constructor to a sequence of kinds and
-- types
mkPolyTyConApp :: TyCon -> [KindRep] -> [TypeRep] -> TypeRep
{-# INLINE mkPolyTyConApp #-}
mkPolyTyConApp tc kinds types
  = TypeRep (fingerprintFingerprints sub_fps) tc kinds types
  where
    !kt_fps = typeRepFingerprints kinds types
    sub_fps = tyConFingerprint tc : kt_fps

typeRepFingerprints :: [KindRep] -> [TypeRep] -> [Fingerprint]
-- Builds no thunks
typeRepFingerprints kinds types
  = go1 [] kinds
  where
    go1 acc []     = go2 acc types
    go1 acc (k:ks) = let !fp = typeRepFingerprint k
                     in go1 (fp:acc) ks
    go2 acc []     = acc
    go2 acc (t:ts) = let !fp = typeRepFingerprint t
                     in go2 (fp:acc) ts

-- | Applies a kind-monomorphic type constructor to a sequence of types
mkTyConApp  :: TyCon -> [TypeRep] -> TypeRep
mkTyConApp tc = mkPolyTyConApp tc []

-- | A special case of 'mkTyConApp', which applies the function
-- type constructor to a pair of types.
mkFunTy  :: TypeRep -> TypeRep -> TypeRep
mkFunTy f a = mkTyConApp tcFun [f,a]

-- | Splits a type constructor application.
-- Note that if the type constructor is polymorphic, this will
-- not return the kinds that were used.
-- See 'splitPolyTyConApp' if you need all parts.
splitTyConApp :: TypeRep -> (TyCon,[TypeRep])
splitTyConApp (TypeRep _ tc _ trs) = (tc,trs)

-- | Split a type constructor application
splitPolyTyConApp :: TypeRep -> (TyCon,[KindRep],[TypeRep])
splitPolyTyConApp (TypeRep _ tc ks trs) = (tc,ks,trs)

-- | Applies a type to a function type.  Returns: @'Just' u@ if the
-- first argument represents a function of type @t -> u@ and the
-- second argument represents a function of type @t@.  Otherwise,
-- returns 'Nothing'.
funResultTy :: TypeRep -> TypeRep -> Maybe TypeRep
funResultTy trFun trArg
  = case splitTyConApp trFun of
      (tc, [t1,t2]) | tc == tcFun && t1 == trArg -> Just t2
      _ -> Nothing

tyConOf :: Typeable a => Proxy a -> TyCon
tyConOf = typeRepTyCon . typeRep

tcFun :: TyCon
tcFun = tyConOf (Proxy :: Proxy (Int -> Int))

-- | Adds a TypeRep argument to a TypeRep.
mkAppTy :: TypeRep -> TypeRep -> TypeRep
{-# INLINE mkAppTy #-}
mkAppTy (TypeRep _ tc ks trs) arg_tr = mkPolyTyConApp tc ks (trs ++ [arg_tr])
   -- Notice that we call mkTyConApp to construct the fingerprint from tc and
   -- the arg fingerprints.  Simply combining the current fingerprint with
   -- the new one won't give the same answer, but of course we want to
   -- ensure that a TypeRep of the same shape has the same fingerprint!
   -- See Trac #5962

----------------- Observation ---------------------

-- | Observe the type constructor of a type representation
typeRepTyCon :: TypeRep -> TyCon
typeRepTyCon (TypeRep _ tc _ _) = tc

-- | Observe the argument types of a type representation
typeRepArgs :: TypeRep -> [TypeRep]
typeRepArgs (TypeRep _ _ _ tys) = tys

-- | Observe the argument kinds of a type representation
typeRepKinds :: TypeRep -> [KindRep]
typeRepKinds (TypeRep _ _ ks _) = ks


{- *********************************************************************
*                                                                      *
                The Typeable class
*                                                                      *
********************************************************************* -}

-------------------------------------------------------------
--
--      The Typeable class and friends
--
-------------------------------------------------------------

-- | The class 'Typeable' allows a concrete representation of a type to
-- be calculated.
class Typeable a where
  typeRep# :: Proxy# a -> TypeRep

-- | Takes a value of type @a@ and returns a concrete representation
-- of that type.
--
-- @since 4.7.0.0
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


----------------- Showing TypeReps --------------------

instance Show TypeRep where
  showsPrec p (TypeRep _ tycon kinds tys) =
    case tys of
      [] -> showsPrec p tycon
      [x]
        | tycon == tcList -> showChar '[' . shows x . showChar ']'
        where
          tcList = tyConOf @[] Proxy
      [TypeRep _ ptrRepCon _ []]
        | tycon == tcTYPE && ptrRepCon == tc'PtrRepLifted
          -> showChar '*'
        | tycon == tcTYPE && ptrRepCon == tc'PtrRepUnlifted
          -> showChar '#'
        where
          tcTYPE            = tyConOf @TYPE            Proxy
          tc'PtrRepLifted   = tyConOf @'PtrRepLifted   Proxy
          tc'PtrRepUnlifted = tyConOf @'PtrRepUnlifted Proxy
      [a,r] | tycon == tcFun  -> showParen (p > 8) $
                                 showsPrec 9 a .
                                 showString " -> " .
                                 showsPrec 8 r
      xs | isTupleTyCon tycon -> showTuple xs
         | otherwise         ->
            showParen (p > 9) $
            showsPrec p tycon .
            showChar ' '      .
            showArgs (showChar ' ') (kinds ++ tys)

showsTypeRep :: TypeRep -> ShowS
showsTypeRep = shows

-- | Helper to fully evaluate 'TypeRep' for use as @NFData(rnf)@ implementation
--
-- @since 4.8.0.0
rnfTypeRep :: TypeRep -> ()
rnfTypeRep (TypeRep _ tyc krs tyrs) = rnfTyCon tyc `seq` go krs `seq` go tyrs
  where
    go [] = ()
    go (x:xs) = rnfTypeRep x `seq` go xs

-- Some (Show.TypeRep) helpers:

showArgs :: Show a => ShowS -> [a] -> ShowS
showArgs _   []     = id
showArgs _   [a]    = showsPrec 10 a
showArgs sep (a:as) = showsPrec 10 a . sep . showArgs sep as

showTuple :: [TypeRep] -> ShowS
showTuple args = showChar '('
               . showArgs (showChar ',') args
               . showChar ')'

{- *********************************************************
*                                                          *
*       TyCon/TypeRep definitions for type literals        *
*              (Symbol and Nat)                            *
*                                                          *
********************************************************* -}


mkTypeLitTyCon :: String -> TyCon
mkTypeLitTyCon name = mkTyCon3 "base" "GHC.TypeLits" name

-- | Used to make `'Typeable' instance for things of kind Nat
typeNatTypeRep :: KnownNat a => Proxy# a -> TypeRep
typeNatTypeRep p = typeLitTypeRep (show (natVal' p))

-- | Used to make `'Typeable' instance for things of kind Symbol
typeSymbolTypeRep :: KnownSymbol a => Proxy# a -> TypeRep
typeSymbolTypeRep p = typeLitTypeRep (show (symbolVal' p))

-- | An internal function, to make representations for type literals.
typeLitTypeRep :: String -> TypeRep
typeLitTypeRep nm = mkTyConApp (mkTypeLitTyCon nm) []
