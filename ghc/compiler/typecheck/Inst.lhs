%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Inst]{The @Inst@ type: dictionaries or method instances}

\begin{code}
module Inst ( 
	LIE, emptyLIE, unitLIE, plusLIE, consLIE, zonkLIE,
	plusLIEs, mkLIE, isEmptyLIE, lieToList, listToLIE,

	Inst, OverloadedLit(..),
	pprInst, pprInsts, pprInstsInFull, tidyInst, tidyInsts,

        InstanceMapper,

	newDictFromOld, newDicts, newClassDicts, newDictsAtLoc,
	newMethod, newMethodWithGivenTy, newOverloadedLit,
	newIPDict, instOverloadedFun,

	tyVarsOfInst, tyVarsOfInsts, tyVarsOfLIE, instLoc, getDictClassTys,
	getDictPred_maybe, getMethodTheta_maybe,
	getFunDeps, getFunDepsOfLIE,
	getIPs, getIPsOfLIE,
	getAllFunDeps, getAllFunDepsOfLIE,

	lookupInst, lookupSimpleInst, LookupInstResult(..),

	isDict, isClassDict, isMethod,
	isTyVarDict, isStdClassTyVarDict, isMethodFor, notFunDep,
	instBindingRequired, instCanBeGeneralised,

	zonkInst, zonkInsts, zonkFunDeps, zonkTvFunDeps,
	instToId, instToIdBndr, ipToId,

	InstOrigin(..), InstLoc, pprInstLoc
    ) where

#include "HsVersions.h"

import HsSyn	( HsLit(..), HsExpr(..) )
import RnHsSyn	( RenamedArithSeqInfo, RenamedHsExpr, RenamedPat )
import TcHsSyn	( TcExpr, TcId, 
		  mkHsTyApp, mkHsDictApp, mkHsConApp, zonkId
		)
import TcMonad
import TcEnv	( TcIdSet, tcLookupValueByKey, tcLookupTyConByKey )
import TcType	( TcThetaType,
		  TcType, TcTauType, TcTyVarSet,
		  zonkTcTyVars, zonkTcType, zonkTcTypes, 
		  zonkTcThetaType
		)
import Bag
import Class	( classInstEnv, Class, FunDep )
import FunDeps	( instantiateFdClassTys )
import Id	( Id, idFreeTyVars, idType, mkUserLocal, mkSysLocal )
import PrelInfo	( isStandardClass, isCcallishClass, isNoDictClass )
import Name	( OccName, Name, mkDictOcc, mkMethodOcc, mkIPOcc,
		  getOccName, nameUnique )
import PprType	( pprPred )	
import InstEnv	( InstEnv, lookupInstEnv )
import SrcLoc	( SrcLoc )
import Type	( Type, PredType(..), ThetaType,
		  mkTyVarTy, isTyVarTy, mkDictTy, mkPredTy,
		  splitForAllTys, splitSigmaTy,
		  splitRhoTy, tyVarsOfType, tyVarsOfTypes, tyVarsOfPred,
		  mkSynTy, tidyOpenType, tidyOpenTypes
		)
import InstEnv	( InstEnv )
import Subst	( emptyInScopeSet, mkSubst,
		  substTy, substClasses, mkTyVarSubst, mkTopTyVarSubst
		)
import TyCon	( TyCon )
import Literal	( inIntRange )
import Var	( TyVar )
import VarEnv	( lookupVarEnv, TidyEnv,
		  lookupSubstEnv, SubstResult(..)
		)
import VarSet	( elemVarSet, emptyVarSet, unionVarSet )
import TysPrim	  ( intPrimTy, floatPrimTy, doublePrimTy )
import TysWiredIn ( intDataCon, isIntTy,
		    floatDataCon, isFloatTy,
		    doubleDataCon, isDoubleTy,
		    integerTy, isIntegerTy
		  ) 
import Unique	( fromRationalClassOpKey, rationalTyConKey,
		  fromIntClassOpKey, fromIntegerClassOpKey, Unique
		)
import Maybes	( expectJust )
import Maybe	( catMaybes )
import Util	( thenCmp, zipWithEqual, mapAccumL )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[Inst-collections]{LIE: a collection of Insts}
%*									*
%************************************************************************

\begin{code}
type LIE = Bag Inst

isEmptyLIE	  = isEmptyBag
emptyLIE          = emptyBag
unitLIE inst 	  = unitBag inst
mkLIE insts	  = listToBag insts
plusLIE lie1 lie2 = lie1 `unionBags` lie2
consLIE inst lie  = inst `consBag` lie
plusLIEs lies	  = unionManyBags lies
lieToList	  = bagToList
listToLIE	  = listToBag

zonkLIE :: LIE -> NF_TcM s LIE
zonkLIE lie = mapBagNF_Tc zonkInst lie

pprInsts :: [Inst] -> SDoc
pprInsts insts = parens (sep (punctuate comma (map pprInst insts)))


pprInstsInFull insts
  = vcat (map go insts)
  where
    go inst = quotes (ppr inst) <+> pprInstLoc (instLoc inst)
\end{code}

%************************************************************************
%*									*
\subsection[Inst-types]{@Inst@ types}
%*									*
%************************************************************************

An @Inst@ is either a dictionary, an instance of an overloaded
literal, or an instance of an overloaded value.  We call the latter a
``method'' even though it may not correspond to a class operation.
For example, we might have an instance of the @double@ function at
type Int, represented by

	Method 34 doubleId [Int] origin

\begin{code}
data Inst
  = Dict
	Unique
	TcPredType
	InstLoc

  | Method
	Unique

	TcId	-- The overloaded function
			-- This function will be a global, local, or ClassOpId;
			--   inside instance decls (only) it can also be an InstId!
			-- The id needn't be completely polymorphic.
			-- You'll probably find its name (for documentation purposes)
			--	  inside the InstOrigin

	[TcType]	-- The types to which its polymorphic tyvars
			--	should be instantiated.
			-- These types must saturate the Id's foralls.

	TcThetaType	-- The (types of the) dictionaries to which the function
			-- must be applied to get the method

	TcTauType	-- The type of the method

	InstLoc

	-- INVARIANT: in (Method u f tys theta tau loc)
	--	type of (f tys dicts(from theta)) = tau

  | LitInst
	Unique
	OverloadedLit
	TcType		-- The type at which the literal is used
	InstLoc

  | FunDep
	Class		-- the class from which this arises
	[FunDep TcType]
	InstLoc

data OverloadedLit
  = OverloadedIntegral	 Integer	-- The number
  | OverloadedFractional Rational	-- The number
\end{code}

Ordering
~~~~~~~~
@Insts@ are ordered by their class/type info, rather than by their
unique.  This allows the context-reduction mechanism to use standard finite
maps to do their stuff.

\begin{code}
instance Ord Inst where
  compare = cmpInst

instance Eq Inst where
  (==) i1 i2 = case i1 `cmpInst` i2 of
	         EQ    -> True
		 other -> False

cmpInst (Dict _ pred1 _)     	  (Dict _ pred2 _)	    = (pred1 `compare` pred2)
cmpInst (Dict _ _ _)	     	  other 		    = LT

cmpInst (Method _ _ _ _ _ _) 	  (Dict _ _ _)	  	    = GT
cmpInst (Method _ id1 tys1 _ _ _) (Method _ id2 tys2 _ _ _) = (id1 `compare` id2) `thenCmp` (tys1 `compare` tys2)
cmpInst (Method _ _ _ _ _ _)      other			    = LT

cmpInst (LitInst _ lit1 ty1 _)	  (LitInst _ lit2 ty2 _)    = (lit1 `cmpOverLit` lit2) `thenCmp` (ty1 `compare` ty2)
cmpInst (LitInst _ _ _ _)	  (FunDep _ _ _)	    = LT
cmpInst (LitInst _ _ _ _)	  other 		    = GT

cmpInst (FunDep clas1 fds1 _)     (FunDep clas2 fds2 _)     = (clas1 `compare` clas2) `thenCmp` (fds1 `compare` fds2)
cmpInst (FunDep _ _ _)		  other			    = GT

cmpOverLit (OverloadedIntegral   i1) (OverloadedIntegral   i2) = i1 `compare` i2
cmpOverLit (OverloadedFractional f1) (OverloadedFractional f2) = f1 `compare` f2
cmpOverLit (OverloadedIntegral _)    (OverloadedFractional _)  = LT
cmpOverLit (OverloadedFractional _)  (OverloadedIntegral _)    = GT
\end{code}


Selection
~~~~~~~~~
\begin{code}
instLoc (Dict   u pred      loc) = loc
instLoc (Method u _ _ _ _   loc) = loc
instLoc (LitInst u lit ty   loc) = loc
instLoc (FunDep _ _	    loc) = loc

getDictPred_maybe (Dict _ p _) = Just p
getDictPred_maybe _	       = Nothing

getMethodTheta_maybe (Method _ _ _ theta _ _) = Just theta
getMethodTheta_maybe _			      = Nothing

getDictClassTys (Dict u (Class clas tys) _) = (clas, tys)

getFunDeps (FunDep clas fds _) = Just (clas, fds)
getFunDeps _ = Nothing

getFunDepsOfLIE lie = catMaybes (map getFunDeps (lieToList lie))

getIPsOfPred (IParam n ty) = [(n, ty)]
getIPsOfPred _             = []
getIPsOfTheta theta = concatMap getIPsOfPred theta

getIPs (Dict u (IParam n ty) loc) = [(n, ty)]
getIPs (Method u id _ theta t loc) = getIPsOfTheta theta
getIPs _ = []

getIPsOfLIE lie = concatMap getIPs (lieToList lie)

getAllFunDeps (FunDep clas fds _) = fds
getAllFunDeps inst = map (\(n,ty) -> ([], [ty])) (getIPs inst)

getAllFunDepsOfLIE lie = concat (map getAllFunDeps (lieToList lie))

tyVarsOfInst :: Inst -> TcTyVarSet
tyVarsOfInst (Dict _ pred _)         = tyVarsOfPred pred
tyVarsOfInst (Method _ id tys _ _ _) = tyVarsOfTypes tys `unionVarSet` idFreeTyVars id
					 -- The id might have free type variables; in the case of
					 -- locally-overloaded class methods, for example
tyVarsOfInst (LitInst _ _ ty _)      = tyVarsOfType  ty
tyVarsOfInst (FunDep _ fds _)
  = foldr unionVarSet emptyVarSet (map tyVarsOfFd fds)
  where tyVarsOfFd (ts1, ts2) =
	    tyVarsOfTypes ts1 `unionVarSet` tyVarsOfTypes ts2

tyVarsOfInsts insts
  = foldr unionVarSet emptyVarSet (map tyVarsOfInst insts)

tyVarsOfLIE lie
  = foldr unionVarSet emptyVarSet (map tyVarsOfInst insts)
  where insts = lieToList lie
\end{code}

Predicates
~~~~~~~~~~
\begin{code}
isDict :: Inst -> Bool
isDict (Dict _ _ _) = True
isDict other	    = False
isClassDict :: Inst -> Bool
isClassDict (Dict _ (Class _ _) _) = True
isClassDict other		   = False

isMethod :: Inst -> Bool
isMethod (Method _ _ _ _ _ _) = True
isMethod other		      = False

isMethodFor :: TcIdSet -> Inst -> Bool
isMethodFor ids (Method uniq id tys _ _ loc) 
  = id `elemVarSet` ids
isMethodFor ids inst 
  = False

isTyVarDict :: Inst -> Bool
isTyVarDict (Dict _ (Class _ tys) _) = all isTyVarTy tys
isTyVarDict other		     = False

isStdClassTyVarDict (Dict _ (Class clas [ty]) _)
  = isStandardClass clas && isTyVarTy ty
isStdClassTyVarDict other
  = False

notFunDep :: Inst -> Bool
notFunDep (FunDep _ _ _) = False
notFunDep other	         = True
\end{code}

Two predicates which deal with the case where class constraints don't
necessarily result in bindings.  The first tells whether an @Inst@
must be witnessed by an actual binding; the second tells whether an
@Inst@ can be generalised over.

\begin{code}
instBindingRequired :: Inst -> Bool
instBindingRequired (Dict _ (Class clas _) _) = not (isNoDictClass clas)
instBindingRequired (Dict _ (IParam _ _) _)   = False
instBindingRequired other		      = True

instCanBeGeneralised :: Inst -> Bool
instCanBeGeneralised (Dict _ (Class clas _) _) = not (isCcallishClass clas)
instCanBeGeneralised other		       = True
\end{code}


Construction
~~~~~~~~~~~~

\begin{code}
newDicts :: InstOrigin
	 -> TcThetaType
	 -> NF_TcM s (LIE, [TcId])
newDicts orig theta
  = tcGetInstLoc orig		`thenNF_Tc` \ loc ->
    newDictsAtLoc loc theta	`thenNF_Tc` \ (dicts, ids) ->
    returnNF_Tc (listToBag dicts, ids)

newClassDicts :: InstOrigin
	      -> [(Class,[TcType])]
	      -> NF_TcM s (LIE, [TcId])
newClassDicts orig theta
  = newDicts orig (map (uncurry Class) theta)

-- Local function, similar to newDicts, 
-- but with slightly different interface
newDictsAtLoc :: InstLoc
 	      -> TcThetaType
	      -> NF_TcM s ([Inst], [TcId])
newDictsAtLoc loc theta =
 tcGetUniques (length theta)		`thenNF_Tc` \ new_uniqs ->
 let
  mk_dict u pred = Dict u pred loc
  dicts = zipWithEqual "newDictsAtLoc" mk_dict new_uniqs theta
 in
 returnNF_Tc (dicts, map instToId dicts)

newDictFromOld :: Inst -> Class -> [TcType] -> NF_TcM s Inst
newDictFromOld (Dict _ _ loc) clas tys
  = tcGetUnique	      `thenNF_Tc` \ uniq ->
    returnNF_Tc (Dict uniq (Class clas tys) loc)


newMethod :: InstOrigin
	  -> TcId
	  -> [TcType]
	  -> NF_TcM s (LIE, TcId)
newMethod orig id tys
  =   	-- Get the Id type and instantiate it at the specified types
    let
	(tyvars, rho) = splitForAllTys (idType id)
	rho_ty	      = substTy (mkTyVarSubst tyvars tys) rho
	(theta, tau)  = splitRhoTy rho_ty
    in
    newMethodWithGivenTy orig id tys theta tau	`thenNF_Tc` \ meth_inst ->
    returnNF_Tc (unitLIE meth_inst, instToId meth_inst)

instOverloadedFun orig v arg_tys theta tau
-- This is where we introduce new functional dependencies into the LIE
  = newMethodWithGivenTy orig v arg_tys theta tau	`thenNF_Tc` \ inst ->
    instFunDeps orig theta				`thenNF_Tc` \ fds ->
    returnNF_Tc (instToId inst, mkLIE (inst : fds))

instFunDeps orig theta
  = tcGetInstLoc orig	`thenNF_Tc` \ loc ->
    let ifd (Class clas tys) =
	    let fds = instantiateFdClassTys clas tys in
	    if null fds then Nothing else Just (FunDep clas fds loc)
	ifd _ = Nothing
    in returnNF_Tc (catMaybes (map ifd theta))

newMethodWithGivenTy orig id tys theta tau
  = tcGetInstLoc orig	`thenNF_Tc` \ loc ->
    newMethodWith id tys theta tau loc

newMethodWith id tys theta tau loc
  = tcGetUnique		`thenNF_Tc` \ new_uniq ->
    returnNF_Tc (Method new_uniq id tys theta tau loc)

newMethodAtLoc :: InstLoc
	       -> Id -> [TcType]
	       -> NF_TcM s (Inst, TcId)
newMethodAtLoc loc real_id tys		-- Local function, similar to newMethod but with 
					-- slightly different interface
  =   	-- Get the Id type and instantiate it at the specified types
    tcGetUnique					`thenNF_Tc` \ new_uniq ->
    let
	(tyvars,rho) = splitForAllTys (idType real_id)
	rho_ty	      = ASSERT( length tyvars == length tys )
			substTy (mkTopTyVarSubst tyvars tys) rho
	(theta, tau)  = splitRhoTy rho_ty
	meth_inst     = Method new_uniq real_id tys theta tau loc
    in
    returnNF_Tc (meth_inst, instToId meth_inst)
\end{code}

In newOverloadedLit we convert directly to an Int or Integer if we
know that's what we want.  This may save some time, by not
temporarily generating overloaded literals, but it won't catch all
cases (the rest are caught in lookupInst).

\begin{code}
newOverloadedLit :: InstOrigin
		 -> OverloadedLit
		 -> TcType
		 -> NF_TcM s (TcExpr, LIE)
newOverloadedLit orig (OverloadedIntegral i) ty
  | isIntTy ty && inIntRange i		-- Short cut for Int
  = returnNF_Tc (int_lit, emptyLIE)

  | isIntegerTy ty 			-- Short cut for Integer
  = returnNF_Tc (integer_lit, emptyLIE)

  where
    intprim_lit    = HsLitOut (HsIntPrim i) intPrimTy
    integer_lit    = HsLitOut (HsInt i) integerTy
    int_lit        = mkHsConApp intDataCon [] [intprim_lit]

newOverloadedLit orig lit ty		-- The general case
  = tcGetInstLoc orig		`thenNF_Tc` \ loc ->
    tcGetUnique			`thenNF_Tc` \ new_uniq ->
    let
	lit_inst = LitInst new_uniq lit ty loc
    in
    returnNF_Tc (HsVar (instToId lit_inst), unitLIE lit_inst)
\end{code}

\begin{code}
newIPDict name ty loc
  = tcGetUnique		`thenNF_Tc` \ new_uniq ->
    let d = Dict new_uniq (IParam name ty) loc in
    returnNF_Tc d
\end{code}

\begin{code}
instToId :: Inst -> TcId
instToId inst = instToIdBndr inst

instToIdBndr :: Inst -> TcId
instToIdBndr (Dict u (Class clas tys) (_,loc,_))
  = mkUserLocal (mkDictOcc (getOccName clas)) u (mkDictTy clas tys) loc
instToIdBndr (Dict u (IParam n ty) (_,loc,_))
  = ipToId n ty loc

instToIdBndr (Method u id tys theta tau (_,loc,_))
  = mkUserLocal (mkMethodOcc (getOccName id)) u tau loc

instToIdBndr (LitInst u list ty loc)
  = mkSysLocal SLIT("lit") u ty

instToIdBndr (FunDep clas fds _)
  = panic "FunDep escaped!!!"

ipToId n ty loc
  = mkUserLocal (mkIPOcc (getOccName n)) (nameUnique n) (mkPredTy (IParam n ty)) loc
\end{code}


Zonking
~~~~~~~
Zonking makes sure that the instance types are fully zonked,
but doesn't do the same for the Id in a Method.  There's no
need, and it's a lot of extra work.

\begin{code}
zonkPred :: TcPredType -> NF_TcM s TcPredType
zonkPred (Class clas tys)
  = zonkTcTypes tys			`thenNF_Tc` \ new_tys ->
    returnNF_Tc (Class clas new_tys)
zonkPred (IParam n ty)
  = zonkTcType ty			`thenNF_Tc` \ new_ty ->
    returnNF_Tc (IParam n new_ty)

zonkInst :: Inst -> NF_TcM s Inst
zonkInst (Dict u pred loc)
  = zonkPred pred			`thenNF_Tc` \ new_pred ->
    returnNF_Tc (Dict u new_pred loc)

zonkInst (Method u id tys theta tau loc) 
  = zonkId id			`thenNF_Tc` \ new_id ->
	-- Essential to zonk the id in case it's a local variable
	-- Can't use zonkIdOcc because the id might itself be
	-- an InstId, in which case it won't be in scope

    zonkTcTypes tys		`thenNF_Tc` \ new_tys ->
    zonkTcThetaType theta	`thenNF_Tc` \ new_theta ->
    zonkTcType tau		`thenNF_Tc` \ new_tau ->
    returnNF_Tc (Method u new_id new_tys new_theta new_tau loc)

zonkInst (LitInst u lit ty loc)
  = zonkTcType ty			`thenNF_Tc` \ new_ty ->
    returnNF_Tc (LitInst u lit new_ty loc)

zonkInst (FunDep clas fds loc)
  = zonkFunDeps fds			`thenNF_Tc` \ fds' ->
    returnNF_Tc (FunDep clas fds' loc)

zonkPreds preds = mapNF_Tc zonkPred preds
zonkInsts insts = mapNF_Tc zonkInst insts

zonkFunDeps fds = mapNF_Tc zonkFd fds
  where
  zonkFd (ts1, ts2)
    = zonkTcTypes ts1			`thenNF_Tc` \ ts1' ->
      zonkTcTypes ts2			`thenNF_Tc` \ ts2' ->
      returnNF_Tc (ts1', ts2')

zonkTvFunDeps fds = mapNF_Tc zonkFd fds
  where
  zonkFd (tvs1, tvs2)
    = zonkTcTyVars tvs1			`thenNF_Tc` \ tvs1' ->
      zonkTcTyVars tvs2			`thenNF_Tc` \ tvs2' ->
      returnNF_Tc (tvs1', tvs2')
\end{code}


Printing
~~~~~~~~
ToDo: improve these pretty-printing things.  The ``origin'' is really only
relevant in error messages.

\begin{code}
instance Outputable Inst where
    ppr inst = pprInst inst

pprInst (LitInst u lit ty loc)
  = hsep [case lit of
	      OverloadedIntegral   i -> integer i
	      OverloadedFractional f -> rational f,
	   ptext SLIT("at"),
	   ppr ty,
	   show_uniq u]

pprInst (Dict u pred loc) = pprPred pred <+> show_uniq u

pprInst m@(Method u id tys theta tau loc)
  = hsep [ppr id, ptext SLIT("at"), 
	  brackets (interppSP tys) {- ,
	  ppr theta, ppr tau,
	  show_uniq u,
	  ppr (instToId m) -}]

pprInst (FunDep clas fds loc)
  = hsep [ppr clas, ppr fds]

tidyPred :: TidyEnv -> TcPredType -> (TidyEnv, TcPredType)
tidyPred env (Class clas tys)
  = (env', Class clas tys')
  where
    (env', tys') = tidyOpenTypes env tys
tidyPred env (IParam n ty)
  = (env', IParam n ty')
  where
    (env', ty') = tidyOpenType env ty

tidyInst :: TidyEnv -> Inst -> (TidyEnv, Inst)
tidyInst env (LitInst u lit ty loc)
  = (env', LitInst u lit ty' loc)
  where
    (env', ty') = tidyOpenType env ty

tidyInst env (Dict u pred loc)
  = (env', Dict u pred' loc)
  where
    (env', pred') = tidyPred env pred

tidyInst env (Method u id tys theta tau loc)
  = (env', Method u id tys' theta tau loc)
		-- Leave theta, tau alone cos we don't print them
  where
    (env', tys') = tidyOpenTypes env tys

-- this case shouldn't arise... (we never print fundeps)
tidyInst env fd@(FunDep clas fds loc)
  = (env, fd)

tidyInsts env insts = mapAccumL tidyInst env insts

show_uniq u = ifPprDebug (text "{-" <> ppr u <> text "-}")
\end{code}


%************************************************************************
%*									*
\subsection[InstEnv-types]{Type declarations}
%*									*
%************************************************************************

\begin{code}
type InstanceMapper = Class -> InstEnv
\end{code}

A @ClassInstEnv@ lives inside a class, and identifies all the instances
of that class.  The @Id@ inside a ClassInstEnv mapping is the dfun for
that instance.  

There is an important consistency constraint between the @MatchEnv@s
in and the dfun @Id@s inside them: the free type variables of the
@Type@ key in the @MatchEnv@ must be a subset of the universally-quantified
type variables of the dfun.  Thus, the @ClassInstEnv@ for @Eq@ might
contain the following entry:
@
	[a] ===> dfun_Eq_List :: forall a. Eq a => Eq [a]
@
The "a" in the pattern must be one of the forall'd variables in
the dfun type.

\begin{code}
data LookupInstResult s
  = NoInstance
  | SimpleInst TcExpr		-- Just a variable, type application, or literal
  | GenInst    [Inst] TcExpr	-- The expression and its needed insts

lookupInst :: Inst 
	   -> NF_TcM s (LookupInstResult s)

-- Dictionaries

lookupInst dict@(Dict _ (Class clas tys) loc)
  = case lookupInstEnv (ppr clas) (classInstEnv clas) tys of

      Just (tenv, dfun_id)
	-> let
		subst	      = mkSubst (tyVarsOfTypes tys) tenv
		(tyvars, rho) = splitForAllTys (idType dfun_id)
		ty_args	      = map subst_tv tyvars
		dfun_rho      = substTy subst rho
		(theta, tau)  = splitRhoTy dfun_rho
		ty_app        = mkHsTyApp (HsVar dfun_id) ty_args
		subst_tv tv   = case lookupSubstEnv tenv tv of
				   Just (DoneTy ty)  -> ty
					-- tenv should bind all the tyvars
	   in
	   if null theta then
		returnNF_Tc (SimpleInst ty_app)
	   else
	   newDictsAtLoc loc theta	`thenNF_Tc` \ (dicts, dict_ids) ->
	   let 
		rhs = mkHsDictApp ty_app dict_ids
	   in
	   returnNF_Tc (GenInst dicts rhs)

      Nothing	-> returnNF_Tc NoInstance
lookupInst dict@(Dict _ _ loc) = returnNF_Tc NoInstance

-- Methods

lookupInst inst@(Method _ id tys theta _ loc)
  = newDictsAtLoc loc theta		`thenNF_Tc` \ (dicts, dict_ids) ->
    returnNF_Tc (GenInst dicts (mkHsDictApp (mkHsTyApp (HsVar id) tys) dict_ids))

-- Literals

lookupInst inst@(LitInst u (OverloadedIntegral i) ty loc)
  | isIntTy ty && in_int_range			-- Short cut for Int
  = returnNF_Tc (GenInst [] int_lit)
	-- GenInst, not SimpleInst, because int_lit is actually a constructor application

  | isIntegerTy ty				-- Short cut for Integer
  = returnNF_Tc (GenInst [] integer_lit)

  | in_int_range				-- It's overloaded but small enough to fit into an Int
  = tcLookupValueByKey fromIntClassOpKey	`thenNF_Tc` \ from_int ->
    newMethodAtLoc loc from_int [ty]		`thenNF_Tc` \ (method_inst, method_id) ->
    returnNF_Tc (GenInst [method_inst] (HsApp (HsVar method_id) int_lit))

  | otherwise   				-- Alas, it is overloaded and a big literal!
  = tcLookupValueByKey fromIntegerClassOpKey	`thenNF_Tc` \ from_integer ->
    newMethodAtLoc loc from_integer [ty]	`thenNF_Tc` \ (method_inst, method_id) ->
    returnNF_Tc (GenInst [method_inst] (HsApp (HsVar method_id) integer_lit))
  where
    in_int_range   = inIntRange i
    intprim_lit    = HsLitOut (HsIntPrim i) intPrimTy
    integer_lit    = HsLitOut (HsInt i) integerTy
    int_lit        = mkHsConApp intDataCon [] [intprim_lit]

-- similar idea for overloaded floating point literals: if the literal is
-- *definitely* a float or a double, generate the real thing here.
-- This is essential  (see nofib/spectral/nucleic).

lookupInst inst@(LitInst u (OverloadedFractional f) ty loc)
  | isFloatTy ty    = returnNF_Tc (GenInst [] float_lit)
  | isDoubleTy ty   = returnNF_Tc (GenInst [] double_lit)

  | otherwise 
  = tcLookupValueByKey fromRationalClassOpKey	`thenNF_Tc` \ from_rational ->

	-- The type Rational isn't wired in so we have to conjure it up
    tcLookupTyConByKey rationalTyConKey	`thenNF_Tc` \ rational_tycon ->
    let
	rational_ty  = mkSynTy rational_tycon []
	rational_lit = HsLitOut (HsFrac f) rational_ty
    in
    newMethodAtLoc loc from_rational [ty]		`thenNF_Tc` \ (method_inst, method_id) ->
    returnNF_Tc (GenInst [method_inst] (HsApp (HsVar method_id) rational_lit))

  where
    floatprim_lit  = HsLitOut (HsFloatPrim f) floatPrimTy
    float_lit      = mkHsConApp floatDataCon [] [floatprim_lit]
    doubleprim_lit = HsLitOut (HsDoublePrim f) doublePrimTy
    double_lit     = mkHsConApp doubleDataCon [] [doubleprim_lit]

-- there are no `instances' of functional dependencies or implicit params

lookupInst _  = returnNF_Tc NoInstance

\end{code}

There is a second, simpler interface, when you want an instance of a
class at a given nullary type constructor.  It just returns the
appropriate dictionary if it exists.  It is used only when resolving
ambiguous dictionaries.

\begin{code}
lookupSimpleInst :: InstEnv
		 -> Class
		 -> [Type]				-- Look up (c,t)
	         -> NF_TcM s (Maybe [(Class,[Type])])	-- Here are the needed (c,t)s

lookupSimpleInst class_inst_env clas tys
  = case lookupInstEnv (ppr clas) class_inst_env tys of
      Nothing	 -> returnNF_Tc Nothing

      Just (tenv, dfun)
	-> returnNF_Tc (Just (substClasses (mkSubst emptyInScopeSet tenv) theta'))
        where
	   (_, theta, _) = splitSigmaTy (idType dfun)
	   theta' = map (\(Class clas tys) -> (clas,tys)) theta
\end{code}
