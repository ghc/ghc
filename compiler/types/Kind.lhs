%
% (c) The University of Glasgow 2006
%

\begin{code}
module Kind (
        -- * Main data type
        Kind, typeKind,

	-- Kinds
	liftedTypeKind, unliftedTypeKind, openTypeKind,
        argTypeKind, ubxTupleKind, constraintKind,
        mkArrowKind, mkArrowKinds,

        -- Kind constructors...
        liftedTypeKindTyCon, openTypeKindTyCon, unliftedTypeKindTyCon,
        argTypeKindTyCon, ubxTupleKindTyCon, constraintKindTyCon,

        -- Super Kinds
	tySuperKind, tySuperKindTyCon, 
        
	pprKind, pprParendKind,

        -- ** Deconstructing Kinds
        kindFunResult, kindAppResult, synTyConResKind,
        splitKindFunTys, splitKindFunTysN, splitKindFunTy_maybe,

        -- ** Predicates on Kinds
        isLiftedTypeKind, isUnliftedTypeKind, isOpenTypeKind,
        isUbxTupleKind, isArgTypeKind, isConstraintKind, isKind, isTySuperKind,
        isSuperKind, 
        isLiftedTypeKindCon, isConstraintKindCon,

        isSubArgTypeKind, isSubOpenTypeKind, isSubKind, defaultKind,
        isSubKindCon,

       ) where

#include "HsVersions.h"

import {-# SOURCE #-} Type (typeKind)

import TypeRep
import TysPrim
import TyCon
import PrelNames
import Outputable
\end{code}

%************************************************************************
%*									*
        Predicates over Kinds
%*									*
%************************************************************************

\begin{code}
isTySuperKind :: SuperKind -> Bool
isTySuperKind (TyConApp kc []) = kc `hasKey` tySuperKindTyConKey
isTySuperKind _                = False

-------------------
-- Lastly we need a few functions on Kinds

isLiftedTypeKindCon :: TyCon -> Bool
isLiftedTypeKindCon tc    = tc `hasKey` liftedTypeKindTyConKey
\end{code}

%************************************************************************
%*									*
	Functions over Kinds		
%*									*
%************************************************************************

\begin{code}
-- | Essentially 'funResultTy' on kinds
kindFunResult :: Kind -> Kind
kindFunResult (FunTy _ res) = res
kindFunResult k = pprPanic "kindFunResult" (ppr k)

kindAppResult :: Kind -> [arg] -> Kind
kindAppResult k []     = k
kindAppResult k (_:as) = kindAppResult (kindFunResult k) as

-- | Essentially 'splitFunTys' on kinds
splitKindFunTys :: Kind -> ([Kind],Kind)
splitKindFunTys (FunTy a r) = case splitKindFunTys r of
                              (as, k) -> (a:as, k)
splitKindFunTys k = ([], k)

splitKindFunTy_maybe :: Kind -> Maybe (Kind,Kind)
splitKindFunTy_maybe (FunTy a r) = Just (a,r)
splitKindFunTy_maybe _           = Nothing

-- | Essentially 'splitFunTysN' on kinds
splitKindFunTysN :: Int -> Kind -> ([Kind],Kind)
splitKindFunTysN 0 k           = ([], k)
splitKindFunTysN n (FunTy a r) = case splitKindFunTysN (n-1) r of
                                   (as, k) -> (a:as, k)
splitKindFunTysN n k = pprPanic "splitKindFunTysN" (ppr n <+> ppr k)

-- | Find the result 'Kind' of a type synonym, 
-- after applying it to its 'arity' number of type variables
-- Actually this function works fine on data types too, 
-- but they'd always return '*', so we never need to ask
synTyConResKind :: TyCon -> Kind
synTyConResKind tycon = kindAppResult (tyConKind tycon) (tyConTyVars tycon)

-- | See "Type#kind_subtyping" for details of the distinction between these 'Kind's
isUbxTupleKind, isOpenTypeKind, isArgTypeKind, isUnliftedTypeKind, isConstraintKind :: Kind -> Bool
isOpenTypeKindCon, isUbxTupleKindCon, isArgTypeKindCon,
        isUnliftedTypeKindCon, isSubArgTypeKindCon, isConstraintKindCon      :: TyCon -> Bool

isOpenTypeKindCon tc    = tyConUnique tc == openTypeKindTyConKey

isOpenTypeKind (TyConApp tc _) = isOpenTypeKindCon tc
isOpenTypeKind _               = False

isUbxTupleKindCon tc = tyConUnique tc == ubxTupleKindTyConKey

isUbxTupleKind (TyConApp tc _) = isUbxTupleKindCon tc
isUbxTupleKind _               = False

isArgTypeKindCon tc = tyConUnique tc == argTypeKindTyConKey

isArgTypeKind (TyConApp tc _) = isArgTypeKindCon tc
isArgTypeKind _               = False

isUnliftedTypeKindCon tc = tyConUnique tc == unliftedTypeKindTyConKey

isUnliftedTypeKind (TyConApp tc _) = isUnliftedTypeKindCon tc
isUnliftedTypeKind _               = False

isConstraintKindCon tc = tyConUnique tc == constraintKindTyConKey

isConstraintKind (TyConApp tc _) = isConstraintKindCon tc
isConstraintKind _               = False

isSubOpenTypeKind :: Kind -> Bool
-- ^ True of any sub-kind of OpenTypeKind (i.e. anything except arrow)
isSubOpenTypeKind (FunTy k1 k2)    = ASSERT2 ( isKind k1, text "isSubOpenTypeKind" <+> ppr k1 <+> text "::" <+> ppr (typeKind k1) ) 
                                     ASSERT2 ( isKind k2, text "isSubOpenTypeKind" <+> ppr k2 <+> text "::" <+> ppr (typeKind k2) ) 
                                     False
isSubOpenTypeKind (TyConApp kc []) = ASSERT( isKind (TyConApp kc []) ) True
isSubOpenTypeKind other            = ASSERT( isKind other ) False
         -- This is a conservative answer
         -- It matters in the call to isSubKind in
	 -- checkExpectedKind.

isSubArgTypeKindCon kc
  | isUnliftedTypeKindCon kc = True
  | isLiftedTypeKindCon kc   = True
  | isArgTypeKindCon kc      = True
  | isConstraintKindCon kc   = True
  | otherwise                = False

isSubArgTypeKind :: Kind -> Bool
-- ^ True of any sub-kind of ArgTypeKind 
isSubArgTypeKind (TyConApp kc []) = isSubArgTypeKindCon kc
isSubArgTypeKind _                = False

-- | Is this a super-kind (i.e. a type-of-kinds)?
isSuperKind :: Type -> Bool
isSuperKind (TyConApp (skc) []) = isSuperKindTyCon skc
isSuperKind _                   = False

-- | Is this a kind (i.e. a type-of-types)?
isKind :: Kind -> Bool
isKind k = isSuperKind (typeKind k)

isSubKind :: Kind -> Kind -> Bool
-- ^ @k1 \`isSubKind\` k2@ checks that @k1@ <: @k2@
isSubKind (TyConApp kc1 []) (TyConApp kc2 []) = kc1 `isSubKindCon` kc2
isSubKind (FunTy a1 r1) (FunTy a2 r2)	      = (a2 `isSubKind` a1) && (r1 `isSubKind` r2)
isSubKind _             _                     = False

isSubKindCon :: TyCon -> TyCon -> Bool
-- ^ @kc1 \`isSubKindCon\` kc2@ checks that @kc1@ <: @kc2@
isSubKindCon kc1 kc2
  | isLiftedTypeKindCon kc1   && isLiftedTypeKindCon kc2   = True
  | isUnliftedTypeKindCon kc1 && isUnliftedTypeKindCon kc2 = True
  | isUbxTupleKindCon kc1     && isUbxTupleKindCon kc2     = True
  | isConstraintKindCon kc1   && isConstraintKindCon kc2   = True
  | isOpenTypeKindCon kc2                                  = True 
                           -- we already know kc1 is not a fun, its a TyCon
  | isArgTypeKindCon kc2      && isSubArgTypeKindCon kc1   = True
  | otherwise                                              = False

defaultKind :: Kind -> Kind
-- ^ Used when generalising: default kind ? and ?? to *. See "Type#kind_subtyping" for more
-- information on what that means

-- When we generalise, we make generic type variables whose kind is
-- simple (* or *->* etc).  So generic type variables (other than
-- built-in constants like 'error') always have simple kinds.  This is important;
-- consider
--	f x = True
-- We want f to get type
--	f :: forall (a::*). a -> Bool
-- Not 
--	f :: forall (a::??). a -> Bool
-- because that would allow a call like (f 3#) as well as (f True),
--and the calling conventions differ.  This defaulting is done in TcMType.zonkTcTyVarBndr.
defaultKind k 
  | isSubOpenTypeKind k = liftedTypeKind
  | isSubArgTypeKind k  = liftedTypeKind
  | otherwise        = k
\end{code}