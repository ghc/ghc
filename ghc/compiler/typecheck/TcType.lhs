%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcType]{Types used in the typechecker}

This module provides the Type interface for front-end parts of the 
compiler.  These parts 

	* treat "source types" as opaque: 
		newtypes, and predicates are meaningful. 
	* look through usage types

The "tc" prefix is for "typechechecker", because the type checker
is the principal client.

\begin{code}
module TcType (
  --------------------------------
  -- TyThing
  TyThing(..),	-- instance NamedThing

  --------------------------------
  -- Types 
  TcType, TcSigmaType, TcRhoType, TcTauType, TcPredType, TcThetaType, 
  TcTyVar, TcTyVarSet, TcKind, 

  --------------------------------
  -- TyVarDetails
  TyVarDetails(..), isUserTyVar, isSkolemTyVar, 
  tyVarBindingInfo,

  --------------------------------
  -- Builders
  mkPhiTy, mkSigmaTy, 

  --------------------------------
  -- Splitters  
  -- These are important because they do not look through newtypes
  tcSplitForAllTys, tcSplitPhiTy, 
  tcSplitFunTy_maybe, tcSplitFunTys, tcFunArgTy, tcFunResultTy,
  tcSplitTyConApp, tcSplitTyConApp_maybe, tcTyConAppTyCon, tcTyConAppArgs,
  tcSplitAppTy_maybe, tcSplitAppTy, tcSplitAppTys, tcSplitSigmaTy,
  tcSplitMethodTy, tcGetTyVar_maybe, tcGetTyVar,

  ---------------------------------
  -- Predicates. 
  -- Again, newtypes are opaque
  tcEqType, tcEqTypes, tcEqPred, tcCmpType, tcCmpTypes, tcCmpPred,
  isSigmaTy, isOverloadedTy, 
  isDoubleTy, isFloatTy, isIntTy,
  isIntegerTy, isAddrTy, isBoolTy, isUnitTy,
  isTauTy, tcIsTyVarTy, tcIsForAllTy,
  allDistinctTyVars,

  ---------------------------------
  -- Misc type manipulators
  deNoteType, classNamesOfTheta,
  tyClsNamesOfType, tyClsNamesOfDFunHead, 
  getDFunTyKey,

  ---------------------------------
  -- Predicate types  
  getClassPredTys_maybe, getClassPredTys, 
  isPredTy, isClassPred, isTyVarClassPred, 
  mkDictTy, tcSplitPredTy_maybe, 
  isDictTy, tcSplitDFunTy, predTyUnique, 
  mkClassPred, isInheritablePred, isLinearPred, isIPPred, mkPredName, 

  ---------------------------------
  -- Foreign import and export
  isFFIArgumentTy,     -- :: DynFlags -> Safety -> Type -> Bool
  isFFIImportResultTy, -- :: DynFlags -> Type -> Bool
  isFFIExportResultTy, -- :: Type -> Bool
  isFFIExternalTy,     -- :: Type -> Bool
  isFFIDynArgumentTy,  -- :: Type -> Bool
  isFFIDynResultTy,    -- :: Type -> Bool
  isFFILabelTy,        -- :: Type -> Bool
  isFFIDotnetTy,       -- :: DynFlags -> Type -> Bool
  isFFIDotnetObjTy,    -- :: Type -> Bool
  
  toDNType,            -- :: Type -> DNType

  ---------------------------------
  -- Unifier and matcher  
  unifyTysX, unifyTyListsX, unifyExtendTysX,
  matchTy, matchTys, match,

  --------------------------------
  -- Rexported from Type
  Kind, 	-- Stuff to do with kinds is insensitive to pre/post Tc
  unliftedTypeKind, liftedTypeKind, openTypeKind, mkArrowKind, mkArrowKinds, 
  superBoxity, liftedBoxity, hasMoreBoxityInfo, defaultKind, superKind,
  isTypeKind, isAnyTypeKind,

  Type, SourceType(..), PredType, ThetaType, 
  mkForAllTy, mkForAllTys, 
  mkFunTy, mkFunTys, zipFunTys, 
  mkTyConApp, mkGenTyConApp, mkAppTy, mkAppTys, mkSynTy, applyTy, applyTys,
  mkTyVarTy, mkTyVarTys, mkTyConTy, mkPredTy, mkPredTys, 

  isUnLiftedType,	-- Source types are always lifted
  isUnboxedTupleType,	-- Ditto
  isPrimitiveType, isTyVarTy,

  tidyTopType, tidyType, tidyPred, tidyTypes, tidyFreeTyVars, tidyOpenType, tidyOpenTypes,
  tidyTyVarBndr, tidyOpenTyVar, tidyOpenTyVars,
  typeKind, eqKind,

  tyVarsOfType, tyVarsOfTypes, tyVarsOfPred, tyVarsOfTheta
  ) where

#include "HsVersions.h"


import {-# SOURCE #-} PprType( pprType )
-- PprType imports TcType so that it can print intelligently

-- friends:
import TypeRep		( Type(..), TyNote(..), funTyCon )  -- friend

import Type		(	-- Re-exports
			  tyVarsOfType, tyVarsOfTypes, tyVarsOfPred,
			  tyVarsOfTheta, Kind, Type, SourceType(..),
			  PredType, ThetaType, unliftedTypeKind,
			  liftedTypeKind, openTypeKind, mkArrowKind,
			  mkArrowKinds, mkForAllTy, mkForAllTys,
			  defaultKind, isTypeKind, isAnyTypeKind,
			  mkFunTy, mkFunTys, zipFunTys, isTyVarTy,
			  mkTyConApp, mkGenTyConApp, mkAppTy,
			  mkAppTys, mkSynTy, applyTy, applyTys,
			  mkTyVarTy, mkTyVarTys, mkTyConTy, mkPredTy,
			  mkPredTys, isUnLiftedType,
			  isUnboxedTupleType, isPrimitiveType,
			  splitTyConApp_maybe,
			  tidyTopType, tidyType, tidyPred, tidyTypes,
			  tidyFreeTyVars, tidyOpenType, tidyOpenTypes,
			  tidyTyVarBndr, tidyOpenTyVar,
			  tidyOpenTyVars, eqKind, 
			  hasMoreBoxityInfo, liftedBoxity,
			  superBoxity, typeKind, superKind, repType
			)
import DataCon		( DataCon )
import TyCon		( TyCon, isUnLiftedTyCon, tyConUnique )
import Class		( classHasFDs, Class )
import Var		( TyVar, Id, tyVarKind, isMutTyVar, mutTyVarDetails )
import ForeignCall	( Safety, playSafe
			  , DNType(..)
			)
import VarEnv
import VarSet

-- others:
import CmdLineOpts	( DynFlags, DynFlag( Opt_GlasgowExts ), dopt )
import Name		( Name, NamedThing(..), mkInternalName, getSrcLoc )
import OccName		( OccName, mkDictOcc )
import NameSet
import PrelNames	-- Lots (e.g. in isFFIArgumentTy)
import TysWiredIn	( unitTyCon, charTyCon, listTyCon )
import BasicTypes	( IPName(..), ipNameName )
import Unique		( Unique, Uniquable(..) )
import SrcLoc		( SrcLoc )
import Util		( cmpList, thenCmp, equalLength, snocView )
import Maybes		( maybeToBool, expectJust )
import Outputable
\end{code}


%************************************************************************
%*									*
			TyThing
%*									*
%************************************************************************

\begin{code}
data TyThing = AnId     Id
	     | ADataCon DataCon
	     | ATyCon   TyCon
	     | AClass   Class

instance NamedThing TyThing where
  getName (AnId id)     = getName id
  getName (ATyCon tc)   = getName tc
  getName (AClass cl)   = getName cl
  getName (ADataCon dc) = getName dc
\end{code}


%************************************************************************
%*									*
\subsection{Types}
%*									*
%************************************************************************

The type checker divides the generic Type world into the 
following more structured beasts:

sigma ::= forall tyvars. phi
	-- A sigma type is a qualified type
	--
	-- Note that even if 'tyvars' is empty, theta
	-- may not be: e.g.   (?x::Int) => Int

	-- Note that 'sigma' is in prenex form:
	-- all the foralls are at the front.
	-- A 'phi' type has no foralls to the right of
	-- an arrow

phi :: theta => rho

rho ::= sigma -> rho
     |  tau

-- A 'tau' type has no quantification anywhere
-- Note that the args of a type constructor must be taus
tau ::= tyvar
     |  tycon tau_1 .. tau_n
     |  tau_1 tau_2
     |  tau_1 -> tau_2

-- In all cases, a (saturated) type synonym application is legal,
-- provided it expands to the required form.


\begin{code}
type SigmaType = Type
type RhoType   = Type
type TauType   = Type
\end{code}

\begin{code}
type TcTyVar    = TyVar		-- Might be a mutable tyvar
type TcTyVarSet = TyVarSet

type TcType = Type 		-- A TcType can have mutable type variables
	-- Invariant on ForAllTy in TcTypes:
	-- 	forall a. T
	-- a cannot occur inside a MutTyVar in T; that is,
	-- T is "flattened" before quantifying over a

type TcPredType     = PredType
type TcThetaType    = ThetaType
type TcSigmaType    = TcType
type TcRhoType      = TcType
type TcTauType      = TcType
type TcKind         = TcType
\end{code}


%************************************************************************
%*									*
\subsection{TyVarDetails}
%*									*
%************************************************************************

TyVarDetails gives extra info about type variables, used during type
checking.  It's attached to mutable type variables only.
It's knot-tied back to Var.lhs.  There is no reason in principle
why Var.lhs shouldn't actually have the definition, but it "belongs" here.

\begin{code}
data TyVarDetails
  = SigTv	-- Introduced when instantiating a type signature,
		-- prior to checking that the defn of a fn does 
		-- have the expected type.  Should not be instantiated.
		--
		-- 	f :: forall a. a -> a
		-- 	f = e
		-- When checking e, with expected type (a->a), we 
		-- should not instantiate a

   | ClsTv	-- Scoped type variable introduced by a class decl
		--	class C a where ...

   | InstTv	-- Ditto, but instance decl

   | PatSigTv	-- Scoped type variable, introduced by a pattern
		-- type signature
		--	\ x::a -> e

   | VanillaTv	-- Everything else

isUserTyVar :: TcTyVar -> Bool	-- Avoid unifying these if possible
isUserTyVar tv = case mutTyVarDetails tv of
		   VanillaTv -> False
		   other     -> True

isSkolemTyVar :: TcTyVar -> Bool
isSkolemTyVar tv = case mutTyVarDetails tv of
		      SigTv  -> True
		      ClsTv  -> True
		      InstTv -> True
		      oteher -> False

tyVarBindingInfo :: TyVar -> SDoc	-- Used in checkSigTyVars
tyVarBindingInfo tv
  | isMutTyVar tv
  = sep [ptext SLIT("is bound by the") <+> details (mutTyVarDetails tv),
	 ptext SLIT("at") <+> ppr (getSrcLoc tv)]
  | otherwise
  = empty
  where
    details SigTv     = ptext SLIT("type signature")
    details ClsTv     = ptext SLIT("class declaration")
    details InstTv    = ptext SLIT("instance declaration")
    details PatSigTv  = ptext SLIT("pattern type signature")
    details VanillaTv = ptext SLIT("//vanilla//")	-- Ditto
\end{code}


%************************************************************************
%*									*
\subsection{Tau, sigma and rho}
%*									*
%************************************************************************

\begin{code}
mkSigmaTy tyvars theta tau = mkForAllTys tyvars (mkPhiTy theta tau)

mkPhiTy :: [SourceType] -> Type -> Type
mkPhiTy theta ty = foldr (\p r -> FunTy (mkPredTy p) r) ty theta
\end{code}


@isTauTy@ tests for nested for-alls.

\begin{code}
isTauTy :: Type -> Bool
isTauTy (TyVarTy v)	 = True
isTauTy (TyConApp _ tys) = all isTauTy tys
isTauTy (AppTy a b)	 = isTauTy a && isTauTy b
isTauTy (FunTy a b)	 = isTauTy a && isTauTy b
isTauTy (SourceTy p)	 = True		-- Don't look through source types
isTauTy (NoteTy _ ty)	 = isTauTy ty
isTauTy other		 = False
\end{code}

\begin{code}
getDFunTyKey :: Type -> OccName	-- Get some string from a type, to be used to 
				-- construct a dictionary function name
getDFunTyKey (TyVarTy tv)    	     = getOccName tv
getDFunTyKey (TyConApp tc _) 	     = getOccName tc
getDFunTyKey (AppTy fun _)   	     = getDFunTyKey fun
getDFunTyKey (NoteTy _ t)    	     = getDFunTyKey t
getDFunTyKey (FunTy arg _)   	     = getOccName funTyCon
getDFunTyKey (ForAllTy _ t)  	     = getDFunTyKey t
getDFunTyKey (SourceTy (NType tc _)) = getOccName tc	-- Newtypes are quite reasonable
getDFunTyKey ty		     	     = pprPanic "getDFunTyKey" (pprType ty)
-- SourceTy shouldn't happen
\end{code}


%************************************************************************
%*									*
\subsection{Expanding and splitting}
%*									*
%************************************************************************

These tcSplit functions are like their non-Tc analogues, but
	a) they do not look through newtypes
	b) they do not look through PredTys
	c) [future] they ignore usage-type annotations

However, they are non-monadic and do not follow through mutable type
variables.  It's up to you to make sure this doesn't matter.

\begin{code}
tcSplitForAllTys :: Type -> ([TyVar], Type)
tcSplitForAllTys ty = split ty ty []
   where
     split orig_ty (ForAllTy tv ty) tvs = split ty ty (tv:tvs)
     split orig_ty (NoteTy n  ty)   tvs = split orig_ty ty tvs
     split orig_ty t		    tvs = (reverse tvs, orig_ty)

tcIsForAllTy (ForAllTy tv ty) = True
tcIsForAllTy (NoteTy n ty)    = tcIsForAllTy ty
tcIsForAllTy t		      = False

tcSplitPhiTy :: Type -> ([PredType], Type)
tcSplitPhiTy ty = split ty ty []
 where
  split orig_ty (FunTy arg res) ts = case tcSplitPredTy_maybe arg of
					Just p  -> split res res (p:ts)
					Nothing -> (reverse ts, orig_ty)
  split orig_ty (NoteTy n ty)	ts = split orig_ty ty ts
  split orig_ty ty		ts = (reverse ts, orig_ty)

tcSplitSigmaTy ty = case tcSplitForAllTys ty of
			(tvs, rho) -> case tcSplitPhiTy rho of
					(theta, tau) -> (tvs, theta, tau)

tcTyConAppTyCon :: Type -> TyCon
tcTyConAppTyCon ty = fst (tcSplitTyConApp ty)

tcTyConAppArgs :: Type -> [Type]
tcTyConAppArgs ty = snd (tcSplitTyConApp ty)

tcSplitTyConApp :: Type -> (TyCon, [Type])
tcSplitTyConApp ty = case tcSplitTyConApp_maybe ty of
			Just stuff -> stuff
			Nothing	   -> pprPanic "tcSplitTyConApp" (pprType ty)

tcSplitTyConApp_maybe :: Type -> Maybe (TyCon, [Type])
tcSplitTyConApp_maybe (TyConApp tc tys) 	= Just (tc, tys)
tcSplitTyConApp_maybe (FunTy arg res)   	= Just (funTyCon, [arg,res])
tcSplitTyConApp_maybe (NoteTy n ty)     	= tcSplitTyConApp_maybe ty
tcSplitTyConApp_maybe (SourceTy (NType tc tys)) = Just (tc,tys)
	-- Newtypes are opaque, so they may be split
	-- However, predicates are not treated
	-- as tycon applications by the type checker
tcSplitTyConApp_maybe other	        	= Nothing

tcSplitFunTys :: Type -> ([Type], Type)
tcSplitFunTys ty = case tcSplitFunTy_maybe ty of
			Nothing	       -> ([], ty)
			Just (arg,res) -> (arg:args, res')
				       where
					  (args,res') = tcSplitFunTys res

tcSplitFunTy_maybe :: Type -> Maybe (Type, Type)
tcSplitFunTy_maybe (FunTy arg res)  = Just (arg, res)
tcSplitFunTy_maybe (NoteTy n ty)    = tcSplitFunTy_maybe ty
tcSplitFunTy_maybe other	    = Nothing

tcFunArgTy    ty = case tcSplitFunTy_maybe ty of { Just (arg,res) -> arg }
tcFunResultTy ty = case tcSplitFunTy_maybe ty of { Just (arg,res) -> res }


tcSplitAppTy_maybe :: Type -> Maybe (Type, Type)
tcSplitAppTy_maybe (FunTy ty1 ty2)   	     = Just (TyConApp funTyCon [ty1], ty2)
tcSplitAppTy_maybe (AppTy ty1 ty2)   	     = Just (ty1, ty2)
tcSplitAppTy_maybe (NoteTy n ty)     	     = tcSplitAppTy_maybe ty
tcSplitAppTy_maybe (SourceTy (NType tc tys)) = tc_split_app tc tys	--- Don't forget that newtype!
tcSplitAppTy_maybe (TyConApp tc tys)	     = tc_split_app tc tys
tcSplitAppTy_maybe other	  	     = Nothing

tc_split_app tc tys = case snocView tys of
			Just (tys',ty') -> Just (TyConApp tc tys', ty')
			Nothing		-> Nothing

tcSplitAppTy ty = case tcSplitAppTy_maybe ty of
		    Just stuff -> stuff
		    Nothing    -> pprPanic "tcSplitAppTy" (pprType ty)

tcSplitAppTys :: Type -> (Type, [Type])
tcSplitAppTys ty
  = go ty []
  where
    go ty args = case tcSplitAppTy_maybe ty of
		   Just (ty', arg) -> go ty' (arg:args)
		   Nothing	   -> (ty,args)

tcGetTyVar_maybe :: Type -> Maybe TyVar
tcGetTyVar_maybe (TyVarTy tv) 	= Just tv
tcGetTyVar_maybe (NoteTy _ t) 	= tcGetTyVar_maybe t
tcGetTyVar_maybe other	        = Nothing

tcGetTyVar :: String -> Type -> TyVar
tcGetTyVar msg ty = expectJust msg (tcGetTyVar_maybe ty)

tcIsTyVarTy :: Type -> Bool
tcIsTyVarTy ty = maybeToBool (tcGetTyVar_maybe ty)
\end{code}

The type of a method for class C is always of the form:
	Forall a1..an. C a1..an => sig_ty
where sig_ty is the type given by the method's signature, and thus in general
is a ForallTy.  At the point that splitMethodTy is called, it is expected
that the outer Forall has already been stripped off.  splitMethodTy then
returns (C a1..an, sig_ty') where sig_ty' is sig_ty with any Notes stripped off.

\begin{code}
tcSplitMethodTy :: Type -> (PredType, Type)
tcSplitMethodTy ty = split ty
 where
  split (FunTy arg res) = case tcSplitPredTy_maybe arg of
			    Just p  -> (p, res)
			    Nothing -> panic "splitMethodTy"
  split (NoteTy n ty)	= split ty
  split _               = panic "splitMethodTy"

tcSplitDFunTy :: Type -> ([TyVar], [SourceType], Class, [Type])
-- Split the type of a dictionary function
tcSplitDFunTy ty 
  = case tcSplitSigmaTy ty       of { (tvs, theta, tau) ->
    case tcSplitPredTy_maybe tau of { Just (ClassP clas tys) -> 
    (tvs, theta, clas, tys) }}
\end{code}

(allDistinctTyVars tys tvs) = True 
	iff 
all the types tys are type variables, 
distinct from each other and from tvs.

This is useful when checking that unification hasn't unified signature
type variables.  For example, if the type sig is
	f :: forall a b. a -> b -> b
we want to check that 'a' and 'b' havn't 
	(a) been unified with a non-tyvar type
	(b) been unified with each other (all distinct)
	(c) been unified with a variable free in the environment

\begin{code}
allDistinctTyVars :: [Type] -> TyVarSet -> Bool

allDistinctTyVars []       acc
  = True
allDistinctTyVars (ty:tys) acc 
  = case tcGetTyVar_maybe ty of
	Nothing 		      -> False 	-- (a)
	Just tv | tv `elemVarSet` acc -> False	-- (b) or (c)
		| otherwise           -> allDistinctTyVars tys (acc `extendVarSet` tv)
\end{code}    


%************************************************************************
%*									*
\subsection{Predicate types}
%*									*
%************************************************************************

"Predicates" are particular source types, namelyClassP or IParams

\begin{code}
isPred :: SourceType -> Bool
isPred (ClassP _ _) = True
isPred (IParam _ _) = True
isPred (NType _ _)  = False

isPredTy :: Type -> Bool
isPredTy (NoteTy _ ty)  = isPredTy ty
isPredTy (SourceTy sty) = isPred sty
isPredTy _	        = False

tcSplitPredTy_maybe :: Type -> Maybe PredType
   -- Returns Just for predicates only
tcSplitPredTy_maybe (NoteTy _ ty)  	    = tcSplitPredTy_maybe ty
tcSplitPredTy_maybe (SourceTy p) | isPred p = Just p
tcSplitPredTy_maybe other	      	    = Nothing
	
predTyUnique :: PredType -> Unique
predTyUnique (IParam n _)      = getUnique (ipNameName n)
predTyUnique (ClassP clas tys) = getUnique clas

mkPredName :: Unique -> SrcLoc -> SourceType -> Name
mkPredName uniq loc (ClassP cls tys) = mkInternalName uniq (mkDictOcc (getOccName cls)) loc
mkPredName uniq loc (IParam ip ty)   = mkInternalName uniq (getOccName (ipNameName ip)) loc
\end{code}


--------------------- Dictionary types ---------------------------------

\begin{code}
mkClassPred clas tys = ClassP clas tys

isClassPred :: SourceType -> Bool
isClassPred (ClassP clas tys) = True
isClassPred other	      = False

isTyVarClassPred (ClassP clas tys) = all tcIsTyVarTy tys
isTyVarClassPred other		   = False

getClassPredTys_maybe :: SourceType -> Maybe (Class, [Type])
getClassPredTys_maybe (ClassP clas tys) = Just (clas, tys)
getClassPredTys_maybe _		        = Nothing

getClassPredTys :: PredType -> (Class, [Type])
getClassPredTys (ClassP clas tys) = (clas, tys)

mkDictTy :: Class -> [Type] -> Type
mkDictTy clas tys = mkPredTy (ClassP clas tys)

isDictTy :: Type -> Bool
isDictTy (SourceTy p)   = isClassPred p
isDictTy (NoteTy _ ty)	= isDictTy ty
isDictTy other		= False
\end{code}

--------------------- Implicit parameters ---------------------------------

\begin{code}
isIPPred :: SourceType -> Bool
isIPPred (IParam _ _) = True
isIPPred other	      = False

isInheritablePred :: PredType -> Bool
-- Can be inherited by a context.  For example, consider
--	f x = let g y = (?v, y+x)
--	      in (g 3 with ?v = 8, 
--		  g 4 with ?v = 9)
-- The point is that g's type must be quantifed over ?v:
--	g :: (?v :: a) => a -> a
-- but it doesn't need to be quantified over the Num a dictionary
-- which can be free in g's rhs, and shared by both calls to g
isInheritablePred (ClassP _ _) = True
isInheritablePred other	     = False

isLinearPred :: TcPredType -> Bool
isLinearPred (IParam (Linear n) _) = True
isLinearPred other		   = False
\end{code}


%************************************************************************
%*									*
\subsection{Comparison}
%*									*
%************************************************************************

Comparison, taking note of newtypes, predicates, etc,
But ignoring usage types

\begin{code}
tcEqType :: Type -> Type -> Bool
tcEqType ty1 ty2 = case ty1 `tcCmpType` ty2 of { EQ -> True; other -> False }

tcEqTypes :: [Type] -> [Type] -> Bool
tcEqTypes ty1 ty2 = case ty1 `tcCmpTypes` ty2 of { EQ -> True; other -> False }

tcEqPred :: PredType -> PredType -> Bool
tcEqPred p1 p2 = case p1 `tcCmpPred` p2 of { EQ -> True; other -> False }

-------------
tcCmpType :: Type -> Type -> Ordering
tcCmpType ty1 ty2 = cmpTy emptyVarEnv ty1 ty2

tcCmpTypes tys1 tys2 = cmpTys emptyVarEnv tys1 tys2

tcCmpPred p1 p2 = cmpSourceTy emptyVarEnv p1 p2
-------------
cmpTys env tys1 tys2 = cmpList (cmpTy env) tys1 tys2

-------------
cmpTy :: TyVarEnv TyVar -> Type -> Type -> Ordering
  -- The "env" maps type variables in ty1 to type variables in ty2
  -- So when comparing for-alls.. (forall tv1 . t1) (forall tv2 . t2)
  -- we in effect substitute tv2 for tv1 in t1 before continuing

    -- Look through NoteTy
cmpTy env (NoteTy _ ty1) ty2 = cmpTy env ty1 ty2
cmpTy env ty1 (NoteTy _ ty2) = cmpTy env ty1 ty2

    -- Deal with equal constructors
cmpTy env (TyVarTy tv1) (TyVarTy tv2) = case lookupVarEnv env tv1 of
					  Just tv1a -> tv1a `compare` tv2
					  Nothing   -> tv1  `compare` tv2

cmpTy env (SourceTy p1) (SourceTy p2) = cmpSourceTy env p1 p2
cmpTy env (AppTy f1 a1) (AppTy f2 a2) = cmpTy env f1 f2 `thenCmp` cmpTy env a1 a2
cmpTy env (FunTy f1 a1) (FunTy f2 a2) = cmpTy env f1 f2 `thenCmp` cmpTy env a1 a2
cmpTy env (TyConApp tc1 tys1) (TyConApp tc2 tys2) = (tc1 `compare` tc2) `thenCmp` (cmpTys env tys1 tys2)
cmpTy env (ForAllTy tv1 t1)   (ForAllTy tv2 t2)   = cmpTy (extendVarEnv env tv1 tv2) t1 t2
    
    -- Deal with the rest: TyVarTy < AppTy < FunTy < TyConApp < ForAllTy < SourceTy
cmpTy env (AppTy _ _) (TyVarTy _) = GT
    
cmpTy env (FunTy _ _) (TyVarTy _) = GT
cmpTy env (FunTy _ _) (AppTy _ _) = GT
    
cmpTy env (TyConApp _ _) (TyVarTy _) = GT
cmpTy env (TyConApp _ _) (AppTy _ _) = GT
cmpTy env (TyConApp _ _) (FunTy _ _) = GT
    
cmpTy env (ForAllTy _ _) (TyVarTy _)    = GT
cmpTy env (ForAllTy _ _) (AppTy _ _)    = GT
cmpTy env (ForAllTy _ _) (FunTy _ _)    = GT
cmpTy env (ForAllTy _ _) (TyConApp _ _) = GT

cmpTy env (SourceTy _)   t2		= GT

cmpTy env _ _ = LT
\end{code}

\begin{code}
cmpSourceTy :: TyVarEnv TyVar -> SourceType -> SourceType -> Ordering
cmpSourceTy env (IParam n1 ty1) (IParam n2 ty2) = (n1 `compare` n2) `thenCmp` (cmpTy env ty1 ty2)
	-- Compare types as well as names for implicit parameters
	-- This comparison is used exclusively (I think) for the
	-- finite map built in TcSimplify
cmpSourceTy env (IParam _ _)     sty		  = LT

cmpSourceTy env (ClassP _ _)     (IParam _ _)     = GT
cmpSourceTy env (ClassP c1 tys1) (ClassP c2 tys2) = (c1 `compare` c2) `thenCmp` (cmpTys env tys1 tys2)
cmpSourceTy env (ClassP _ _)     (NType _ _)      = LT

cmpSourceTy env (NType tc1 tys1) (NType tc2 tys2) = (tc1 `compare` tc2) `thenCmp` (cmpTys env tys1 tys2)
cmpSourceTy env (NType _ _)	 sty		  = GT
\end{code}

PredTypes are used as a FM key in TcSimplify, 
so we take the easy path and make them an instance of Ord

\begin{code}
instance Eq  SourceType where { (==)    = tcEqPred }
instance Ord SourceType where { compare = tcCmpPred }
\end{code}


%************************************************************************
%*									*
\subsection{Predicates}
%*									*
%************************************************************************

isSigmaTy returns true of any qualified type.  It doesn't *necessarily* have 
any foralls.  E.g.
	f :: (?x::Int) => Int -> Int

\begin{code}
isSigmaTy :: Type -> Bool
isSigmaTy (ForAllTy tyvar ty) = True
isSigmaTy (FunTy a b)	      = isPredTy a
isSigmaTy (NoteTy n ty)	      = isSigmaTy ty
isSigmaTy _		      = False

isOverloadedTy :: Type -> Bool
isOverloadedTy (ForAllTy tyvar ty) = isOverloadedTy ty
isOverloadedTy (FunTy a b)	   = isPredTy a
isOverloadedTy (NoteTy n ty)	   = isOverloadedTy ty
isOverloadedTy _		   = False
\end{code}

\begin{code}
isFloatTy      = is_tc floatTyConKey
isDoubleTy     = is_tc doubleTyConKey
isIntegerTy    = is_tc integerTyConKey
isIntTy        = is_tc intTyConKey
isAddrTy       = is_tc addrTyConKey
isBoolTy       = is_tc boolTyConKey
isUnitTy       = is_tc unitTyConKey

is_tc :: Unique -> Type -> Bool
-- Newtypes are opaque to this
is_tc uniq ty = case tcSplitTyConApp_maybe ty of
			Just (tc, _) -> uniq == getUnique tc
			Nothing	     -> False
\end{code}


%************************************************************************
%*									*
\subsection{Misc}
%*									*
%************************************************************************

\begin{code}
deNoteType :: Type -> Type
	-- Remove synonyms, but not source types
deNoteType ty@(TyVarTy tyvar)	= ty
deNoteType (TyConApp tycon tys) = TyConApp tycon (map deNoteType tys)
deNoteType (SourceTy p)		= SourceTy (deNoteSourceType p)
deNoteType (NoteTy _ ty)	= deNoteType ty
deNoteType (AppTy fun arg)	= AppTy (deNoteType fun) (deNoteType arg)
deNoteType (FunTy fun arg)	= FunTy (deNoteType fun) (deNoteType arg)
deNoteType (ForAllTy tv ty)	= ForAllTy tv (deNoteType ty)

deNoteSourceType :: SourceType -> SourceType
deNoteSourceType (ClassP c tys)   = ClassP c (map deNoteType tys)
deNoteSourceType (IParam n ty)    = IParam n (deNoteType ty)
deNoteSourceType (NType tc tys)   = NType tc (map deNoteType tys)
\end{code}

Find the free tycons and classes of a type.  This is used in the front
end of the compiler.

\begin{code}
tyClsNamesOfType :: Type -> NameSet
tyClsNamesOfType (TyVarTy tv)		    = emptyNameSet
tyClsNamesOfType (TyConApp tycon tys)	    = unitNameSet (getName tycon) `unionNameSets` tyClsNamesOfTypes tys
tyClsNamesOfType (NoteTy (SynNote ty1) ty2) = tyClsNamesOfType ty1
tyClsNamesOfType (NoteTy other_note    ty2) = tyClsNamesOfType ty2
tyClsNamesOfType (SourceTy (IParam n ty))   = tyClsNamesOfType ty
tyClsNamesOfType (SourceTy (ClassP cl tys)) = unitNameSet (getName cl) `unionNameSets` tyClsNamesOfTypes tys
tyClsNamesOfType (SourceTy (NType tc tys))  = unitNameSet (getName tc) `unionNameSets` tyClsNamesOfTypes tys
tyClsNamesOfType (FunTy arg res)	    = tyClsNamesOfType arg `unionNameSets` tyClsNamesOfType res
tyClsNamesOfType (AppTy fun arg)	    = tyClsNamesOfType fun `unionNameSets` tyClsNamesOfType arg
tyClsNamesOfType (ForAllTy tyvar ty)	    = tyClsNamesOfType ty

tyClsNamesOfTypes tys = foldr (unionNameSets . tyClsNamesOfType) emptyNameSet tys

tyClsNamesOfDFunHead :: Type -> NameSet
-- Find the free type constructors and classes 
-- of the head of the dfun instance type
-- The 'dfun_head_type' is because of
--	instance Foo a => Baz T where ...
-- The decl is an orphan if Baz and T are both not locally defined,
--	even if Foo *is* locally defined
tyClsNamesOfDFunHead dfun_ty 
  = case tcSplitSigmaTy dfun_ty of
	(tvs,_,head_ty) -> tyClsNamesOfType head_ty

classNamesOfTheta :: ThetaType -> [Name]
-- Looks just for ClassP things; maybe it should check
classNamesOfTheta preds = [ getName c | ClassP c _ <- preds ]
\end{code}


%************************************************************************
%*									*
\subsection[TysWiredIn-ext-type]{External types}
%*									*
%************************************************************************

The compiler's foreign function interface supports the passing of a
restricted set of types as arguments and results (the restricting factor
being the )

\begin{code}
isFFIArgumentTy :: DynFlags -> Safety -> Type -> Bool
-- Checks for valid argument type for a 'foreign import'
isFFIArgumentTy dflags safety ty 
   = checkRepTyCon (legalOutgoingTyCon dflags safety) ty

isFFIExternalTy :: Type -> Bool
-- Types that are allowed as arguments of a 'foreign export'
isFFIExternalTy ty = checkRepTyCon legalFEArgTyCon ty

isFFIImportResultTy :: DynFlags -> Type -> Bool
isFFIImportResultTy dflags ty 
  = checkRepTyCon (legalFIResultTyCon dflags) ty

isFFIExportResultTy :: Type -> Bool
isFFIExportResultTy ty = checkRepTyCon legalFEResultTyCon ty

isFFIDynArgumentTy :: Type -> Bool
-- The argument type of a foreign import dynamic must be Ptr, FunPtr, Addr,
-- or a newtype of either.
isFFIDynArgumentTy = checkRepTyConKey [ptrTyConKey, funPtrTyConKey, addrTyConKey]

isFFIDynResultTy :: Type -> Bool
-- The result type of a foreign export dynamic must be Ptr, FunPtr, Addr,
-- or a newtype of either.
isFFIDynResultTy = checkRepTyConKey [ptrTyConKey, funPtrTyConKey, addrTyConKey]

isFFILabelTy :: Type -> Bool
-- The type of a foreign label must be Ptr, FunPtr, Addr,
-- or a newtype of either.
isFFILabelTy = checkRepTyConKey [ptrTyConKey, funPtrTyConKey, addrTyConKey]

isFFIDotnetTy :: DynFlags -> Type -> Bool
isFFIDotnetTy dflags ty
  = checkRepTyCon (\ tc -> not (isByteArrayLikeTyCon tc) &&
  			   (legalFIResultTyCon dflags tc || 
			   isFFIDotnetObjTy ty || isStringTy ty)) ty

-- Support String as an argument or result from a .NET FFI call.
isStringTy ty = 
  case tcSplitTyConApp_maybe (repType ty) of
    Just (tc, [arg_ty])
      | tc == listTyCon ->
        case tcSplitTyConApp_maybe (repType arg_ty) of
	  Just (cc,[]) -> cc == charTyCon
	  _ -> False
    _ -> False

-- Support String as an argument or result from a .NET FFI call.
isFFIDotnetObjTy ty = 
  let
   (_, t_ty) = tcSplitForAllTys ty
  in
  case tcSplitTyConApp_maybe (repType t_ty) of
    Just (tc, [arg_ty]) | getName tc == objectTyConName -> True
    _ -> False

toDNType :: Type -> DNType
toDNType ty
  | isStringTy ty = DNString
  | isFFIDotnetObjTy ty = DNObject
  | Just (tc,argTys) <- tcSplitTyConApp_maybe ty = 
     case lookup (getUnique tc) dn_assoc of
       Just x  -> x
       Nothing 
         | tc `hasKey` ioTyConKey -> toDNType (head argTys)
	 | otherwise -> pprPanic ("toDNType: unsupported .NET type") (pprType ty <+> parens (hcat (map pprType argTys)) <+> ppr tc)
    where
      dn_assoc :: [ (Unique, DNType) ]
      dn_assoc = [ (unitTyConKey,   DNUnit)
      		 , (intTyConKey,    DNInt)
      	         , (int8TyConKey,   DNInt8)
		 , (int16TyConKey,  DNInt16)
		 , (int32TyConKey,  DNInt32)
		 , (int64TyConKey,  DNInt64)
		 , (wordTyConKey,   DNInt)
		 , (word8TyConKey,  DNWord8)
		 , (word16TyConKey, DNWord16)
		 , (word32TyConKey, DNWord32)
		 , (word64TyConKey, DNWord64)
		 , (floatTyConKey,  DNFloat)
		 , (doubleTyConKey, DNDouble)
		 , (addrTyConKey,   DNPtr)
		 , (ptrTyConKey,    DNPtr)
		 , (funPtrTyConKey, DNPtr)
		 , (charTyConKey,   DNChar)
		 , (boolTyConKey,   DNBool)
		 ]

checkRepTyCon :: (TyCon -> Bool) -> Type -> Bool
	-- Look through newtypes
	-- Non-recursive ones are transparent to splitTyConApp,
	-- but recursive ones aren't
checkRepTyCon check_tc ty 
  | Just (tc,_) <- splitTyConApp_maybe (repType ty) = check_tc tc
  | otherwise				  	    = False

checkRepTyConKey :: [Unique] -> Type -> Bool
-- Like checkRepTyCon, but just looks at the TyCon key
checkRepTyConKey keys
  = checkRepTyCon (\tc -> tyConUnique tc `elem` keys)
\end{code}

----------------------------------------------
These chaps do the work; they are not exported
----------------------------------------------

\begin{code}
legalFEArgTyCon :: TyCon -> Bool
-- It's illegal to return foreign objects and (mutable)
-- bytearrays from a _ccall_ / foreign declaration
-- (or be passed them as arguments in foreign exported functions).
legalFEArgTyCon tc
  | isByteArrayLikeTyCon tc
  = False
  -- It's also illegal to make foreign exports that take unboxed
  -- arguments.  The RTS API currently can't invoke such things.  --SDM 7/2000
  | otherwise
  = boxedMarshalableTyCon tc

legalFIResultTyCon :: DynFlags -> TyCon -> Bool
legalFIResultTyCon dflags tc
  | isByteArrayLikeTyCon tc = False
  | tc == unitTyCon         = True
  | otherwise	            = marshalableTyCon dflags tc

legalFEResultTyCon :: TyCon -> Bool
legalFEResultTyCon tc
  | isByteArrayLikeTyCon tc = False
  | tc == unitTyCon         = True
  | otherwise               = boxedMarshalableTyCon tc

legalOutgoingTyCon :: DynFlags -> Safety -> TyCon -> Bool
-- Checks validity of types going from Haskell -> external world
legalOutgoingTyCon dflags safety tc
  | playSafe safety && isByteArrayLikeTyCon tc
  = False
  | otherwise
  = marshalableTyCon dflags tc

marshalableTyCon dflags tc
  =  (dopt Opt_GlasgowExts dflags && isUnLiftedTyCon tc)
  || boxedMarshalableTyCon tc

boxedMarshalableTyCon tc
   = getUnique tc `elem` [ intTyConKey, int8TyConKey, int16TyConKey
			 , int32TyConKey, int64TyConKey
			 , wordTyConKey, word8TyConKey, word16TyConKey
			 , word32TyConKey, word64TyConKey
			 , floatTyConKey, doubleTyConKey
			 , addrTyConKey, ptrTyConKey, funPtrTyConKey
			 , charTyConKey
			 , stablePtrTyConKey
			 , byteArrayTyConKey, mutableByteArrayTyConKey
			 , boolTyConKey
			 ]

isByteArrayLikeTyCon :: TyCon -> Bool
isByteArrayLikeTyCon tc = 
  getUnique tc `elem` [byteArrayTyConKey, mutableByteArrayTyConKey]
\end{code}


%************************************************************************
%*									*
\subsection{Unification with an explicit substitution}
%*									*
%************************************************************************

Unify types with an explicit substitution and no monad.
Ignore usage annotations.

\begin{code}
type MySubst
   = (TyVarSet,		-- Set of template tyvars
      TyVarSubstEnv)	-- Not necessarily idempotent

unifyTysX :: TyVarSet		-- Template tyvars
	  -> Type
          -> Type
          -> Maybe TyVarSubstEnv
unifyTysX tmpl_tyvars ty1 ty2
  = uTysX ty1 ty2 (\(_,s) -> Just s) (tmpl_tyvars, emptySubstEnv)

unifyExtendTysX :: TyVarSet		-- Template tyvars
		-> TyVarSubstEnv	-- Substitution to start with
		-> Type
	        -> Type
        	-> Maybe TyVarSubstEnv	-- Extended substitution
unifyExtendTysX tmpl_tyvars subst ty1 ty2
  = uTysX ty1 ty2 (\(_,s) -> Just s) (tmpl_tyvars, subst)

unifyTyListsX :: TyVarSet -> [Type] -> [Type]
              -> Maybe TyVarSubstEnv
unifyTyListsX tmpl_tyvars tys1 tys2
  = uTyListsX tys1 tys2 (\(_,s) -> Just s) (tmpl_tyvars, emptySubstEnv)


uTysX :: Type
      -> Type
      -> (MySubst -> Maybe result)
      -> MySubst
      -> Maybe result

uTysX (NoteTy _ ty1) ty2 k subst = uTysX ty1 ty2 k subst
uTysX ty1 (NoteTy _ ty2) k subst = uTysX ty1 ty2 k subst

	-- Variables; go for uVar
uTysX (TyVarTy tyvar1) (TyVarTy tyvar2) k subst 
  | tyvar1 == tyvar2
  = k subst
uTysX (TyVarTy tyvar1) ty2 k subst@(tmpls,_)
  | tyvar1 `elemVarSet` tmpls
  = uVarX tyvar1 ty2 k subst
uTysX ty1 (TyVarTy tyvar2) k subst@(tmpls,_)
  | tyvar2 `elemVarSet` tmpls
  = uVarX tyvar2 ty1 k subst

	-- Predicates
uTysX (SourceTy (IParam n1 t1)) (SourceTy (IParam n2 t2)) k subst
  | n1 == n2 = uTysX t1 t2 k subst
uTysX (SourceTy (ClassP c1 tys1)) (SourceTy (ClassP c2 tys2)) k subst
  | c1 == c2 = uTyListsX tys1 tys2 k subst
uTysX (SourceTy (NType tc1 tys1)) (SourceTy (NType tc2 tys2)) k subst
  | tc1 == tc2 = uTyListsX tys1 tys2 k subst

	-- Functions; just check the two parts
uTysX (FunTy fun1 arg1) (FunTy fun2 arg2) k subst
  = uTysX fun1 fun2 (uTysX arg1 arg2 k) subst

	-- Type constructors must match
uTysX (TyConApp con1 tys1) (TyConApp con2 tys2) k subst
  | (con1 == con2 && equalLength tys1 tys2)
  = uTyListsX tys1 tys2 k subst

	-- Applications need a bit of care!
	-- They can match FunTy and TyConApp, so use splitAppTy_maybe
	-- NB: we've already dealt with type variables and Notes,
	-- so if one type is an App the other one jolly well better be too
uTysX (AppTy s1 t1) ty2 k subst
  = case tcSplitAppTy_maybe ty2 of
      Just (s2, t2) -> uTysX s1 s2 (uTysX t1 t2 k) subst
      Nothing       -> Nothing    -- Fail

uTysX ty1 (AppTy s2 t2) k subst
  = case tcSplitAppTy_maybe ty1 of
      Just (s1, t1) -> uTysX s1 s2 (uTysX t1 t2 k) subst
      Nothing       -> Nothing    -- Fail

	-- Not expecting for-alls in unification
#ifdef DEBUG
uTysX (ForAllTy _ _) ty2 k subst = panic "Unify.uTysX subst:ForAllTy (1st arg)"
uTysX ty1 (ForAllTy _ _) k subst = panic "Unify.uTysX subst:ForAllTy (2nd arg)"
#endif

	-- Anything else fails
uTysX ty1 ty2 k subst = Nothing


uTyListsX []         []         k subst = k subst
uTyListsX (ty1:tys1) (ty2:tys2) k subst = uTysX ty1 ty2 (uTyListsX tys1 tys2 k) subst
uTyListsX tys1	     tys2       k subst = Nothing   -- Fail if the lists are different lengths
\end{code}

\begin{code}
-- Invariant: tv1 is a unifiable variable
uVarX tv1 ty2 k subst@(tmpls, env)
  = case lookupSubstEnv env tv1 of
      Just (DoneTy ty1) ->    -- Already bound
		     uTysX ty1 ty2 k subst

      Nothing	     -- Not already bound
	       |  typeKind ty2 `eqKind` tyVarKind tv1
	       && occur_check_ok ty2
	       ->     -- No kind mismatch nor occur check
                  k (tmpls, extendSubstEnv env tv1 (DoneTy ty2))

	       | otherwise -> Nothing	-- Fail if kind mis-match or occur check
  where
    occur_check_ok ty = all occur_check_ok_tv (varSetElems (tyVarsOfType ty))
    occur_check_ok_tv tv | tv1 == tv = False
			 | otherwise = case lookupSubstEnv env tv of
				         Nothing	   -> True
					 Just (DoneTy ty)  -> occur_check_ok ty
\end{code}



%************************************************************************
%*									*
\subsection{Matching on types}
%*									*
%************************************************************************

Matching is a {\em unidirectional} process, matching a type against a
template (which is just a type with type variables in it).  The
matcher assumes that there are no repeated type variables in the
template, so that it simply returns a mapping of type variables to
types.  It also fails on nested foralls.

@matchTys@ matches corresponding elements of a list of templates and
types.  It and @matchTy@ both ignore usage annotations, unlike the
main function @match@.

\begin{code}
matchTy :: TyVarSet			-- Template tyvars
	-> Type  			-- Template
	-> Type				-- Proposed instance of template
	-> Maybe TyVarSubstEnv		-- Matching substitution
					

matchTys :: TyVarSet			-- Template tyvars
	 -> [Type]			-- Templates
	 -> [Type]			-- Proposed instance of template
	 -> Maybe (TyVarSubstEnv,		-- Matching substitution
		   [Type])		-- Left over instance types

matchTy tmpls ty1 ty2 = match ty1 ty2 tmpls (\ senv -> Just senv) emptySubstEnv

matchTys tmpls tys1 tys2 = match_list tys1 tys2 tmpls 
				      (\ (senv,tys) -> Just (senv,tys))
				      emptySubstEnv
\end{code}

@match@ is the main function.  It takes a flag indicating whether
usage annotations are to be respected.

\begin{code}
match :: Type -> Type    	    		-- Current match pair
      -> TyVarSet				-- Template vars
      -> (TyVarSubstEnv -> Maybe result)	-- Continuation
      -> TyVarSubstEnv				-- Current subst
      -> Maybe result

-- When matching against a type variable, see if the variable
-- has already been bound.  If so, check that what it's bound to
-- is the same as ty; if not, bind it and carry on.

match (TyVarTy v) ty tmpls k senv
  | v `elemVarSet` tmpls
  =     -- v is a template variable
    case lookupSubstEnv senv v of
	Nothing | typeKind ty `eqKind` tyVarKind v	
			-- We do a kind check, just as in the uVarX above
			-- The kind check is needed to avoid bogus matches
			-- of 	(a b) with (c d), where the kinds don't match
			-- An occur check isn't needed when matching.
		-> k (extendSubstEnv senv v (DoneTy ty))

		| otherwise  -> Nothing -- Fails

	Just (DoneTy ty')  | ty' `tcEqType` ty   -> k senv   -- Succeeds
			   | otherwise	         -> Nothing  -- Fails

  | otherwise
  =     -- v is not a template variable; ty had better match
        -- Can't use (==) because types differ
    case tcGetTyVar_maybe ty of
        Just v' | v == v' -> k senv    -- Success
        other    	  -> Nothing   -- Failure
    -- This tcGetTyVar_maybe is *required* because it must strip Notes.
    -- I guess the reason the Note-stripping case is *last* rather than first
    -- is to preserve type synonyms etc., so I'm not moving it to the
    -- top; but this means that (without the deNotetype) a type
    -- variable may not match the pattern (TyVarTy v') as one would
    -- expect, due to an intervening Note.  KSW 2000-06.

	-- Predicates
match (SourceTy (IParam n1 t1)) (SourceTy (IParam n2 t2)) tmpls k senv
  | n1 == n2 = match t1 t2 tmpls k senv
match (SourceTy (ClassP c1 tys1)) (SourceTy (ClassP c2 tys2)) tmpls k senv
  | c1 == c2 = match_list_exactly tys1 tys2 tmpls k senv
match (SourceTy (NType tc1 tys1)) (SourceTy (NType tc2 tys2)) tmpls k senv
  | tc1 == tc2 = match_list_exactly tys1 tys2 tmpls k senv

	-- Functions; just check the two parts
match (FunTy arg1 res1) (FunTy arg2 res2) tmpls k senv
  = match arg1 arg2 tmpls (match res1 res2 tmpls k) senv

match (AppTy fun1 arg1) ty2 tmpls k senv 
  = case tcSplitAppTy_maybe ty2 of
	Just (fun2,arg2) -> match fun1 fun2 tmpls (match arg1 arg2 tmpls k) senv
	Nothing 	 -> Nothing	-- Fail

match (TyConApp tc1 tys1) (TyConApp tc2 tys2) tmpls k senv
  | tc1 == tc2 = match_list_exactly tys1 tys2 tmpls k senv

-- Newtypes are opaque; other source types should not happen
match (SourceTy (NType tc1 tys1)) (SourceTy (NType tc2 tys2)) tmpls k senv
  | tc1 == tc2 = match_list_exactly tys1 tys2 tmpls k senv

	-- With type synonyms, we have to be careful for the exact
	-- same reasons as in the unifier.  Please see the
	-- considerable commentary there before changing anything
	-- here! (WDP 95/05)
match (NoteTy n1 ty1) ty2      tmpls k senv = match ty1 ty2 tmpls k senv
match ty1      (NoteTy n2 ty2) tmpls k senv = match ty1 ty2 tmpls k senv

-- Catch-all fails
match _ _ _ _ _ = Nothing

match_list_exactly tys1 tys2 tmpls k senv
  = match_list tys1 tys2 tmpls k' senv
  where
    k' (senv', tys2') | null tys2' = k senv'	-- Succeed
		      | otherwise  = Nothing	-- Fail	

match_list []         tys2       tmpls k senv = k (senv, tys2)
match_list (ty1:tys1) []         tmpls k senv = Nothing	-- Not enough arg tys => failure
match_list (ty1:tys1) (ty2:tys2) tmpls k senv
  = match ty1 ty2 tmpls (match_list tys1 tys2 tmpls k) senv
\end{code}
