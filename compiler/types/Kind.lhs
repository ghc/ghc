%
% (c) The University of Glasgow 2006
%

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module Kind (
        -- * Main data type
        Kind, typeKind,

	-- Kinds
	anyKind, liftedTypeKind, unliftedTypeKind, openTypeKind,
        argTypeKind, ubxTupleKind, constraintKind,
        mkArrowKind, mkArrowKinds,

        -- Kind constructors...
        anyKindTyCon, liftedTypeKindTyCon, openTypeKindTyCon,
        unliftedTypeKindTyCon, argTypeKindTyCon, ubxTupleKindTyCon,
        constraintKindTyCon,

        -- Super Kinds
	tySuperKind, tySuperKindTyCon, 
        
	pprKind, pprParendKind,

        -- ** Deconstructing Kinds
        kindFunResult, kindAppResult, synTyConResKind,
        splitKindFunTys, splitKindFunTysN, splitKindFunTy_maybe,

        -- ** Predicates on Kinds
        isLiftedTypeKind, isUnliftedTypeKind, isOpenTypeKind,
        isUbxTupleKind, isArgTypeKind, isConstraintKind,
        isConstraintOrLiftedKind, isKind,
        isSuperKind, noHashInKind,
        isLiftedTypeKindCon, isConstraintKindCon,
        isAnyKind, isAnyKindCon,

        isSubArgTypeKind, tcIsSubArgTypeKind, 
        isSubOpenTypeKind, tcIsSubOpenTypeKind,
        isSubKind, tcIsSubKind, defaultKind,
        isSubKindCon, tcIsSubKindCon, isSubOpenTypeKindCon,

        -- ** Functions on variables
        isKiVar, splitKiTyVars, partitionKiTyVars,
        kiVarsOfKind, kiVarsOfKinds,

        -- ** Promotion related functions
        promoteType, isPromotableType, isPromotableKind,

       ) where

#include "HsVersions.h"

import {-# SOURCE #-} Type      ( typeKind, substKiWith, eqKind )

import TypeRep
import TysPrim
import TyCon
import Var
import VarSet
import PrelNames
import Outputable

import Data.List ( partition )
\end{code}

%************************************************************************
%*									*
        Predicates over Kinds
%*									*
%************************************************************************

\begin{code}
-------------------
-- Lastly we need a few functions on Kinds

isLiftedTypeKindCon :: TyCon -> Bool
isLiftedTypeKindCon tc    = tc `hasKey` liftedTypeKindTyConKey

-- This checks that its argument does not contain # or (#).
-- It is used in tcTyVarBndrs.
noHashInKind :: Kind -> Bool
noHashInKind (TyVarTy {}) = True
noHashInKind (FunTy k1 k2) = noHashInKind k1 && noHashInKind k2
noHashInKind (ForAllTy _ ki) = noHashInKind ki
noHashInKind (TyConApp kc kis)
  =  not (kc `hasKey` unliftedTypeKindTyConKey)
  && not (kc `hasKey` ubxTupleKindTyConKey)
  && all noHashInKind kis
noHashInKind _ = panic "noHashInKind"
\end{code}

%************************************************************************
%*									*
	Functions over Kinds		
%*									*
%************************************************************************

\begin{code}
-- | Essentially 'funResultTy' on kinds handling pi-types too
kindFunResult :: Kind -> KindOrType -> Kind
kindFunResult (FunTy _ res) _ = res
kindFunResult (ForAllTy kv res) arg = substKiWith [kv] [arg] res
kindFunResult k _ = pprPanic "kindFunResult" (ppr k)

kindAppResult :: Kind -> [Type] -> Kind
kindAppResult k []     = k
kindAppResult k (a:as) = kindAppResult (kindFunResult k a) as

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
synTyConResKind tycon = kindAppResult (tyConKind tycon) (map mkTyVarTy (tyConTyVars tycon))

-- | See "Type#kind_subtyping" for details of the distinction between these 'Kind's
isUbxTupleKind, isOpenTypeKind, isArgTypeKind, isUnliftedTypeKind,
  isConstraintKind, isAnyKind, isConstraintOrLiftedKind :: Kind -> Bool

isOpenTypeKindCon, isUbxTupleKindCon, isArgTypeKindCon,
  isUnliftedTypeKindCon, isSubArgTypeKindCon, tcIsSubArgTypeKindCon,
  isSubOpenTypeKindCon, tcIsSubOpenTypeKindCon, isConstraintKindCon,
  isAnyKindCon :: TyCon -> Bool

isAnyKindCon tc     = tyConUnique tc == anyKindTyConKey

isAnyKind (TyConApp tc _) = isAnyKindCon tc
isAnyKind _               = False

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

isConstraintOrLiftedKind (TyConApp tc _)
  = isConstraintKindCon tc || isLiftedTypeKindCon tc
isConstraintOrLiftedKind _ = False

-- Subkinding
-- The tc variants are used during type-checking, where we don't want the
-- Constraint kind to be a subkind of anything
-- After type-checking (in core), Constraint is a subkind of argTypeKind
isSubOpenTypeKind, tcIsSubOpenTypeKind :: Kind -> Bool
-- ^ True of any sub-kind of OpenTypeKind
isSubOpenTypeKind (TyConApp kc []) = isSubOpenTypeKindCon kc
isSubOpenTypeKind _                = False

-- ^ True of any sub-kind of OpenTypeKind
tcIsSubOpenTypeKind (TyConApp kc []) = tcIsSubOpenTypeKindCon kc
tcIsSubOpenTypeKind _                = False

isSubOpenTypeKindCon kc
  | isSubArgTypeKindCon kc   = True
  | isUbxTupleKindCon kc     = True
  | isOpenTypeKindCon kc     = True
  | otherwise                = False

tcIsSubOpenTypeKindCon kc
  | tcIsSubArgTypeKindCon kc = True
  | isUbxTupleKindCon kc     = True
  | isOpenTypeKindCon kc     = True
  | otherwise                = False

isSubArgTypeKindCon kc
  | isUnliftedTypeKindCon kc = True
  | isLiftedTypeKindCon kc   = True
  | isArgTypeKindCon kc      = True
  | isConstraintKindCon kc   = True
  | otherwise                = False

tcIsSubArgTypeKindCon kc
  | isConstraintKindCon kc   = False
  | otherwise                = isSubArgTypeKindCon kc

isSubArgTypeKind, tcIsSubArgTypeKind :: Kind -> Bool
-- ^ True of any sub-kind of ArgTypeKind 
isSubArgTypeKind (TyConApp kc []) = isSubArgTypeKindCon kc
isSubArgTypeKind _                = False

tcIsSubArgTypeKind (TyConApp kc []) = tcIsSubArgTypeKindCon kc
tcIsSubArgTypeKind _                = False

-- | Is this a super-kind (i.e. a type-of-kinds)?
isSuperKind :: Type -> Bool
isSuperKind (TyConApp (skc) []) = isSuperKindTyCon skc
isSuperKind _                   = False

-- | Is this a kind (i.e. a type-of-types)?
isKind :: Kind -> Bool
isKind k = isSuperKind (typeKind k)

isSubKind, tcIsSubKind :: Kind -> Kind -> Bool
isSubKind   = isSubKind' False
tcIsSubKind = isSubKind' True

-- The first argument denotes whether we are in the type-checking phase or not
isSubKind' :: Bool -> Kind -> Kind -> Bool
-- ^ @k1 \`isSubKind\` k2@ checks that @k1@ <: @k2@

isSubKind' duringTc (FunTy a1 r1) (FunTy a2 r2)
  = (isSubKind' duringTc a2 a1) && (isSubKind' duringTc r1 r2)

isSubKind' duringTc k1@(TyConApp kc1 k1s) k2@(TyConApp kc2 k2s)
  | isPromotedTypeTyCon kc1 || isPromotedTypeTyCon kc2
    -- handles promoted kinds (List *, Nat, etc.)
    = eqKind k1 k2

  | isSuperKindTyCon kc1 || isSuperKindTyCon kc2
    -- handles BOX
    = ASSERT2( isSuperKindTyCon kc2 && null k1s && null k2s, ppr kc1 <+> ppr kc2 )
      True

  | otherwise = -- handles usual kinds (*, #, (#), etc.)
                ASSERT2( null k1s && null k2s, ppr k1 <+> ppr k2 )
                if duringTc then kc1 `tcIsSubKindCon` kc2
                else kc1 `isSubKindCon` kc2

isSubKind' _duringTc k1 k2 = eqKind k1 k2

isSubKindCon :: TyCon -> TyCon -> Bool
-- ^ @kc1 \`isSubKindCon\` kc2@ checks that @kc1@ <: @kc2@
isSubKindCon kc1 kc2
  | kc1 == kc2                                             = True
  | isSubArgTypeKindCon  kc1  && isArgTypeKindCon  kc2     = True
  | isSubOpenTypeKindCon kc1  && isOpenTypeKindCon kc2     = True
  | otherwise                                              = False

tcIsSubKindCon :: TyCon -> TyCon -> Bool
tcIsSubKindCon kc1 kc2
  | kc1 == kc2                                         = True
  | isConstraintKindCon kc1 || isConstraintKindCon kc2 = False
  | otherwise                                          = isSubKindCon kc1 kc2

defaultKind :: Kind -> Kind
-- ^ Used when generalising: default OpenKind and ArgKind to *.
-- See "Type#kind_subtyping" for more information on what that means

-- When we generalise, we make generic type variables whose kind is
-- simple (* or *->* etc).  So generic type variables (other than
-- built-in constants like 'error') always have simple kinds.  This is important;
-- consider
--	f x = True
-- We want f to get type
--	f :: forall (a::*). a -> Bool
-- Not 
--	f :: forall (a::ArgKind). a -> Bool
-- because that would allow a call like (f 3#) as well as (f True),
-- and the calling conventions differ.
-- This defaulting is done in TcMType.zonkTcTyVarBndr.
defaultKind k
  | tcIsSubOpenTypeKind k = liftedTypeKind
  | otherwise             = k

splitKiTyVars :: [TyVar] -> ([KindVar], [TyVar])
-- Precondition: kind variables should precede type variables
-- Postcondition: appending the two result lists gives the input!
splitKiTyVars = span (isSuperKind . tyVarKind)

partitionKiTyVars :: [TyVar] -> ([KindVar], [TyVar])
partitionKiTyVars = partition (isSuperKind . tyVarKind)

-- Checks if this "type or kind" variable is a kind variable
isKiVar :: TyVar -> Bool
isKiVar v = isSuperKind (varType v)

-- Returns the free kind variables in a kind
kiVarsOfKind :: Kind -> VarSet
kiVarsOfKind = tyVarsOfType

kiVarsOfKinds :: [Kind] -> VarSet
kiVarsOfKinds = tyVarsOfTypes

-- Datatype promotion
isPromotableType :: Type -> Bool
isPromotableType = go emptyVarSet
  where
    go vars (TyConApp tc tys) = ASSERT( not (isPromotedDataTyCon tc) ) all (go vars) tys
    go vars (FunTy arg res) = all (go vars) [arg,res]
    go vars (TyVarTy tvar) = tvar `elemVarSet` vars
    go vars (ForAllTy tvar ty) = isPromotableTyVar tvar && go (vars `extendVarSet` tvar) ty
    go _ _ = panic "isPromotableType"  -- argument was not kind-shaped

isPromotableTyVar :: TyVar -> Bool
isPromotableTyVar = isLiftedTypeKind . varType

-- | Promotes a type to a kind. Assumes the argument is promotable.
promoteType :: Type -> Kind
promoteType (TyConApp tc tys) = mkTyConApp (mkPromotedTypeTyCon tc) 
                                           (map promoteType tys)
  -- T t1 .. tn  ~~>  'T k1 .. kn  where  ti ~~> ki
promoteType (FunTy arg res) = mkArrowKind (promoteType arg) (promoteType res)
  -- t1 -> t2  ~~>  k1 -> k2  where  ti ~~> ki
promoteType (TyVarTy tvar) = mkTyVarTy (promoteTyVar tvar)
  -- a :: *  ~~>  a :: BOX
promoteType (ForAllTy tvar ty) = ForAllTy (promoteTyVar tvar) (promoteType ty)
  -- forall (a :: *). t  ~~> forall (a :: BOX). k  where  t ~~> k
promoteType _ = panic "promoteType"  -- argument was not kind-shaped

promoteTyVar :: TyVar -> KindVar
promoteTyVar tvar = mkKindVar (tyVarName tvar) tySuperKind

-- If kind is [ *^n -> * ] returns [ Just n ], else returns [ Nothing ]
isPromotableKind :: Kind -> Maybe Int
isPromotableKind kind =
  let (args, res) = splitKindFunTys kind in
  if all isLiftedTypeKind (res:args)
  then Just $ length args
  else Nothing

{- Note [Promoting a Type to a Kind]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We only promote the followings.
- Type variables: a
- Fully applied arrow types: tau -> sigma
- Fully applied type constructors of kind:
     n >= 0
  /-----------\
  * -> ... -> * -> *
- Polymorphic types over type variables of kind star:
  forall (a::*). tau
-}
\end{code}
