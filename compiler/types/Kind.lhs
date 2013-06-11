%
% (c) The University of Glasgow 2006-2012
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
        SuperKind, Kind, typeKind,

	-- Kinds
	anyKind, liftedTypeKind, unliftedTypeKind, openTypeKind, constraintKind,
        mkArrowKind, mkArrowKinds,

        -- Kind constructors...
        anyKindTyCon, liftedTypeKindTyCon, openTypeKindTyCon,
        unliftedTypeKindTyCon, constraintKindTyCon,

        -- Super Kinds
	superKind, superKindTyCon, 
        
	pprKind, pprParendKind,

        -- ** Deconstructing Kinds
        kindAppResult, synTyConResKind,
        splitKindFunTys, splitKindFunTysN, splitKindFunTy_maybe,

        -- ** Predicates on Kinds
        isLiftedTypeKind, isUnliftedTypeKind, isOpenTypeKind,
        isConstraintKind, isConstraintOrLiftedKind, returnsConstraintKind,
        isKind, isKindVar,
        isSuperKind, isSuperKindTyCon,
        isLiftedTypeKindCon, isConstraintKindCon,
        isAnyKind, isAnyKindCon,
        okArrowArgKind, okArrowResultKind,

        isSubOpenTypeKind, isSubOpenTypeKindKey,
        isSubKind, isSubKindCon, 
        tcIsSubKind, tcIsSubKindCon,
        defaultKind, defaultKind_maybe,

        -- ** Functions on variables
        kiVarsOfKind, kiVarsOfKinds

       ) where

#include "HsVersions.h"

import {-# SOURCE #-} Type      ( typeKind, substKiWith, eqKind )

import TypeRep
import TysPrim
import TyCon
import VarSet
import PrelNames
import Outputable
import Maybes( orElse )
import Util
\end{code}

%************************************************************************
%*									*
	Functions over Kinds		
%*									*
%************************************************************************

Note [Kind Constraint and kind *]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The kind Constraint is the kind of classes and other type constraints.
The special thing about types of kind Constraint is that 
 * They are displayed with double arrow:
     f :: Ord a => a -> a
 * They are implicitly instantiated at call sites; so the type inference
   engine inserts an extra argument of type (Ord a) at every call site
   to f.

However, once type inference is over, there is *no* distinction between 
Constraint and *.  Indeed we can have coercions between the two. Consider
   class C a where
     op :: a -> a
For this single-method class we may generate a newtype, which in turn 
generates an axiom witnessing
    Ord a ~ (a -> a)
so on the left we have Constraint, and on the right we have *.
See Trac #7451.

Bottom line: although '*' and 'Constraint' are distinct TyCons, with
distinct uniques, they are treated as equal at all times except 
during type inference.  Hence cmpTc treats them as equal.

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
isOpenTypeKind, isUnliftedTypeKind,
  isConstraintKind, isAnyKind, isConstraintOrLiftedKind :: Kind -> Bool

isOpenTypeKindCon, isUnliftedTypeKindCon,
  isSubOpenTypeKindCon, isConstraintKindCon,
  isLiftedTypeKindCon, isAnyKindCon, isSuperKindTyCon :: TyCon -> Bool


isLiftedTypeKindCon   tc = tyConUnique tc == liftedTypeKindTyConKey
isAnyKindCon          tc = tyConUnique tc == anyKindTyConKey
isOpenTypeKindCon     tc = tyConUnique tc == openTypeKindTyConKey
isUnliftedTypeKindCon tc = tyConUnique tc == unliftedTypeKindTyConKey
isConstraintKindCon   tc = tyConUnique tc == constraintKindTyConKey
isSuperKindTyCon      tc = tyConUnique tc == superKindTyConKey

isAnyKind (TyConApp tc _) = isAnyKindCon tc
isAnyKind _               = False

isOpenTypeKind (TyConApp tc _) = isOpenTypeKindCon tc
isOpenTypeKind _               = False

isUnliftedTypeKind (TyConApp tc _) = isUnliftedTypeKindCon tc
isUnliftedTypeKind _               = False

isConstraintKind (TyConApp tc _) = isConstraintKindCon tc
isConstraintKind _               = False

isConstraintOrLiftedKind (TyConApp tc _)
  = isConstraintKindCon tc || isLiftedTypeKindCon tc
isConstraintOrLiftedKind _ = False

returnsConstraintKind :: Kind -> Bool
returnsConstraintKind (ForAllTy _ k)  = returnsConstraintKind k
returnsConstraintKind (FunTy _ k)     = returnsConstraintKind k
returnsConstraintKind (TyConApp tc _) = isConstraintKindCon tc
returnsConstraintKind _               = False

--------------------------------------------
--            Kinding for arrow (->)
-- Says when a kind is acceptable on lhs or rhs of an arrow
--     arg -> res

okArrowArgKindCon, okArrowResultKindCon :: TyCon -> Bool
okArrowArgKindCon    = isSubOpenTypeKindCon
okArrowResultKindCon = isSubOpenTypeKindCon

okArrowArgKind, okArrowResultKind :: Kind -> Bool
okArrowArgKind    (TyConApp kc []) = okArrowArgKindCon kc
okArrowArgKind    _                = False

okArrowResultKind (TyConApp kc []) = okArrowResultKindCon kc
okArrowResultKind _                = False

-----------------------------------------
--              Subkinding
-- The tc variants are used during type-checking, where we don't want the
-- Constraint kind to be a subkind of anything
-- After type-checking (in core), Constraint is a subkind of openTypeKind

isSubOpenTypeKind :: Kind -> Bool
-- ^ True of any sub-kind of OpenTypeKind
isSubOpenTypeKind (TyConApp kc []) = isSubOpenTypeKindCon kc
isSubOpenTypeKind _                = False

isSubOpenTypeKindCon kc = isSubOpenTypeKindKey (tyConUnique kc)

isSubOpenTypeKindKey :: Unique -> Bool
isSubOpenTypeKindKey uniq
  =  uniq == openTypeKindTyConKey
  || uniq == unliftedTypeKindTyConKey
  || uniq == liftedTypeKindTyConKey
  || uniq == constraintKindTyConKey  -- Needed for error (Num a) "blah"
                                     -- and so that (Ord a -> Eq a) is well-kinded
                                     -- and so that (# Eq a, Ord b #) is well-kinded
                              	     -- See Note [Kind Constraint and kind *]

-- | Is this a kind (i.e. a type-of-types)?
isKind :: Kind -> Bool
isKind k = isSuperKind (typeKind k)

isSubKind :: Kind -> Kind -> Bool
-- ^ @k1 \`isSubKind\` k2@ checks that @k1@ <: @k2@
-- Sub-kinding is extremely simple and does not look
-- under arrrows or type constructors

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.lhs
isSubKind k1@(TyConApp kc1 k1s) k2@(TyConApp kc2 k2s)
  | isPromotedTyCon kc1 || isPromotedTyCon kc2
    -- handles promoted kinds (List *, Nat, etc.)
  = eqKind k1 k2

  | otherwise -- handles usual kinds (*, #, (#), etc.)
  = ASSERT2( null k1s && null k2s, ppr k1 <+> ppr k2 )
    kc1 `isSubKindCon` kc2

isSubKind k1 k2 = eqKind k1 k2

isSubKindCon :: TyCon -> TyCon -> Bool
-- ^ @kc1 \`isSubKindCon\` kc2@ checks that @kc1@ <: @kc2@

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.lhs
isSubKindCon kc1 kc2
  | kc1 == kc2              = True
  | isOpenTypeKindCon kc2   = isSubOpenTypeKindCon kc1 
  | isConstraintKindCon kc1 = isLiftedTypeKindCon kc2
  | isLiftedTypeKindCon kc1 = isConstraintKindCon kc2
    -- See Note [Kind Constraint and kind *]
  | otherwise               = False

-------------------------
-- Hack alert: we need a tiny variant for the typechecker
-- Reason:     f :: Int -> (a~b)
--             g :: forall (c::Constraint). Int -> c
--             h :: Int => Int
-- We want to reject these, even though Constraint is
-- a sub-kind of OpenTypeKind.  It must be a sub-kind of OpenTypeKind
-- *after* the typechecker
--   a) So that (Ord a -> Eq a) is a legal type
--   b) So that the simplifer can generate (error (Eq a) "urk")
-- Moreover, after the type checker, Constraint and *
-- are identical; see Note [Kind Constraint and kind *]
--
-- Easiest way to reject is simply to make Constraint a compliete
-- below OpenTypeKind when type checking

tcIsSubKind :: Kind -> Kind -> Bool
tcIsSubKind k1 k2
  | isConstraintKind k1 = isConstraintKind k2
  | isConstraintKind k2 = isConstraintKind k1
  | otherwise           = isSubKind k1 k2

tcIsSubKindCon :: TyCon -> TyCon -> Bool
tcIsSubKindCon kc1 kc2
  | isConstraintKindCon kc1 = isConstraintKindCon kc2
  | isConstraintKindCon kc2 = isConstraintKindCon kc1
  | otherwise               = isSubKindCon kc1 kc2

-------------------------
defaultKind       :: Kind -> Kind
defaultKind_maybe :: Kind -> Maybe Kind
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
--
-- The test is really whether the kind is strictly above '*'
defaultKind_maybe (TyConApp kc _args)
  | isOpenTypeKindCon kc = ASSERT( null _args ) Just liftedTypeKind
defaultKind_maybe _      = Nothing

defaultKind k = defaultKind_maybe k `orElse` k

-- Returns the free kind variables in a kind
kiVarsOfKind :: Kind -> VarSet
kiVarsOfKind = tyVarsOfType

kiVarsOfKinds :: [Kind] -> VarSet
kiVarsOfKinds = tyVarsOfTypes
\end{code}
