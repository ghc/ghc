%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Inst]{The @Inst@ type: dictionaries or method instances}

\begin{code}
module Inst ( 
	LIE, emptyLIE, unitLIE, plusLIE, consLIE, zonkLIE,
	plusLIEs, mkLIE, isEmptyLIE, lieToList, listToLIE,

	Inst, 
	pprInst, pprInsts, pprInstsInFull, tidyInst, tidyInsts,

	newDictFromOld, newDicts, newClassDicts, newDictsAtLoc,
	newMethod, newMethodWithGivenTy, newOverloadedLit,
	newIPDict, instOverloadedFun,
	instantiateFdClassTys, instFunDeps, instFunDepsOfTheta,
	newFunDepFromDict,

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

import HsSyn	( HsLit(..), HsOverLit(..), HsExpr(..) )
import RnHsSyn	( RenamedHsOverLit )
import TcHsSyn	( TcExpr, TcId, 
		  mkHsTyApp, mkHsDictApp, mkHsConApp, zonkId
		)
import TcMonad
import TcEnv	( TcIdSet, tcGetInstEnv, tcLookupGlobalId )
import TcInstUtil ( InstLookupResult(..), lookupInstEnv )
import TcType	( TcThetaType,
		  TcType, TcTauType, TcTyVarSet,
		  zonkTcTyVars, zonkTcType, zonkTcTypes, 
		  zonkTcThetaType
		)
import Bag
import Class	( Class, FunDep )
import FunDeps	( instantiateFdClassTys )
import Id	( Id, idFreeTyVars, idType, mkUserLocal, mkSysLocal )
import PrelInfo	( isStandardClass, isCcallishClass, isNoDictClass )
import Name	( mkDictOcc, mkMethodOcc, mkIPOcc, getOccName, nameUnique )
import PprType	( pprPred )	
import Type	( Type, PredType(..), 
		  isTyVarTy, mkDictTy, mkPredTy,
		  splitForAllTys, splitSigmaTy, funArgTy,
		  splitRhoTy, tyVarsOfType, tyVarsOfTypes, tyVarsOfPred,
		  tidyOpenType, tidyOpenTypes
		)
import Subst	( emptyInScopeSet, mkSubst, mkInScopeSet,
		  substTy, substClasses, mkTyVarSubst, mkTopTyVarSubst
		)
import Literal	( inIntRange )
import VarEnv	( TidyEnv, lookupSubstEnv, SubstResult(..) )
import VarSet	( elemVarSet, emptyVarSet, unionVarSet )
import TysWiredIn ( isIntTy,
		    floatDataCon, isFloatTy,
		    doubleDataCon, isDoubleTy,
		    isIntegerTy, voidTy
		  ) 
import PrelNames( Unique, hasKey, fromIntName, fromIntegerClassOpKey )
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

zonkLIE :: LIE -> NF_TcM LIE
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
	RenamedHsOverLit	-- The literal from the occurrence site
	TcType			-- The type at which the literal is used
	InstLoc

  | FunDep
	Unique
	Class		-- the class from which this arises
	[FunDep TcType]
	InstLoc
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

cmpInst (LitInst _ lit1 ty1 _)	  (LitInst _ lit2 ty2 _)    = (lit1 `compare` lit2) `thenCmp` (ty1 `compare` ty2)
cmpInst (LitInst _ _ _ _)	  (FunDep _ _ _ _)	    = LT
cmpInst (LitInst _ _ _ _)	  other 		    = GT

cmpInst (FunDep _ clas1 fds1 _)   (FunDep _ clas2 fds2 _)   = (clas1 `compare` clas2) `thenCmp` (fds1 `compare` fds2)
cmpInst (FunDep _ _ _ _)	  other			    = GT

-- and they can only have HsInt or HsFracs in them.
\end{code}


Selection
~~~~~~~~~
\begin{code}
instLoc (Dict   u pred      loc) = loc
instLoc (Method u _ _ _ _   loc) = loc
instLoc (LitInst u lit ty   loc) = loc
instLoc (FunDep _ _ _	    loc) = loc

getDictPred_maybe (Dict _ p _) = Just p
getDictPred_maybe _	       = Nothing

getMethodTheta_maybe (Method _ _ _ theta _ _) = Just theta
getMethodTheta_maybe _			      = Nothing

getDictClassTys (Dict u (Class clas tys) _) = (clas, tys)

getFunDeps (FunDep _ clas fds _) = Just (clas, fds)
getFunDeps _ = Nothing

getFunDepsOfLIE lie = catMaybes (map getFunDeps (lieToList lie))

getIPsOfPred (IParam n ty) = [(n, ty)]
getIPsOfPred _             = []
getIPsOfTheta theta = concatMap getIPsOfPred theta

getIPs (Dict u (IParam n ty) loc) = [(n, ty)]
getIPs (Method u id _ theta t loc) = getIPsOfTheta theta
getIPs _ = []

getIPsOfLIE lie = concatMap getIPs (lieToList lie)

getAllFunDeps (FunDep _ clas fds _) = fds
getAllFunDeps inst = map (\(n,ty) -> ([], [ty])) (getIPs inst)

getAllFunDepsOfLIE lie = concat (map getAllFunDeps (lieToList lie))

tyVarsOfInst :: Inst -> TcTyVarSet
tyVarsOfInst (Dict _ pred _)         = tyVarsOfPred pred
tyVarsOfInst (Method _ id tys _ _ _) = tyVarsOfTypes tys `unionVarSet` idFreeTyVars id
					 -- The id might have free type variables; in the case of
					 -- locally-overloaded class methods, for example
tyVarsOfInst (LitInst _ _ ty _)      = tyVarsOfType  ty
tyVarsOfInst (FunDep _ _ fds _)
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
isMethodFor ids (Method uniq id tys _ _ loc) = id `elemVarSet` ids
isMethodFor ids inst			     = False

isTyVarDict :: Inst -> Bool
isTyVarDict (Dict _ (Class _ tys) _) = all isTyVarTy tys
isTyVarDict other		     = False

isStdClassTyVarDict (Dict _ (Class clas [ty]) _)
  = isStandardClass clas && isTyVarTy ty
isStdClassTyVarDict other
  = False

notFunDep :: Inst -> Bool
notFunDep (FunDep _ _ _ _) = False
notFunDep other		   = True
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
	 -> NF_TcM (LIE, [TcId])
newDicts orig theta
  = tcGetInstLoc orig		`thenNF_Tc` \ loc ->
    newDictsAtLoc loc theta	`thenNF_Tc` \ (dicts, ids) ->
    returnNF_Tc (listToBag dicts, ids)

newClassDicts :: InstOrigin
	      -> [(Class,[TcType])]
	      -> NF_TcM (LIE, [TcId])
newClassDicts orig theta
  = newDicts orig (map (uncurry Class) theta)

-- Local function, similar to newDicts, 
-- but with slightly different interface
newDictsAtLoc :: InstLoc
 	      -> TcThetaType
	      -> NF_TcM ([Inst], [TcId])
newDictsAtLoc loc theta =
 tcGetUniques (length theta)		`thenNF_Tc` \ new_uniqs ->
 let
  mk_dict u pred = Dict u pred loc
  dicts = zipWithEqual "newDictsAtLoc" mk_dict new_uniqs theta
 in
 returnNF_Tc (dicts, map instToId dicts)

newDictFromOld :: Inst -> Class -> [TcType] -> NF_TcM Inst
newDictFromOld (Dict _ _ loc) clas tys
  = tcGetUnique	      `thenNF_Tc` \ uniq ->
    returnNF_Tc (Dict uniq (Class clas tys) loc)


newMethod :: InstOrigin
	  -> TcId
	  -> [TcType]
	  -> NF_TcM (LIE, TcId)
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
  = tcGetUnique		`thenNF_Tc` \ uniq ->
    tcGetInstLoc orig	`thenNF_Tc` \ loc ->
    let ifd (Class clas tys) =
	    let fds = instantiateFdClassTys clas tys in
	    if null fds then Nothing else Just (FunDep uniq clas fds loc)
	ifd _ = Nothing
    in returnNF_Tc (catMaybes (map ifd theta))

instFunDepsOfTheta theta
  = let ifd (Class clas tys) = instantiateFdClassTys clas tys
	ifd (IParam n ty)    = [([], [ty])]
    in concat (map ifd theta)

newMethodWithGivenTy orig id tys theta tau
  = tcGetInstLoc orig	`thenNF_Tc` \ loc ->
    newMethodWith id tys theta tau loc

newMethodWith id tys theta tau loc
  = tcGetUnique		`thenNF_Tc` \ new_uniq ->
    returnNF_Tc (Method new_uniq id tys theta tau loc)

newMethodAtLoc :: InstLoc
	       -> Id -> [TcType]
	       -> NF_TcM (Inst, TcId)
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
		 -> RenamedHsOverLit
		 -> TcType
		 -> NF_TcM (TcExpr, LIE)
newOverloadedLit orig (HsIntegral i _) ty
  | isIntTy ty && inIntRange i		-- Short cut for Int
  = returnNF_Tc (int_lit, emptyLIE)

  | isIntegerTy ty 			-- Short cut for Integer
  = returnNF_Tc (integer_lit, emptyLIE)

  where
    int_lit     = HsLit (HsInt i)
    integer_lit = HsLit (HsInteger i)

newOverloadedLit orig lit ty		-- The general case
  = tcGetInstLoc orig		`thenNF_Tc` \ loc ->
    tcGetUnique			`thenNF_Tc` \ new_uniq ->
    let
	lit_inst = LitInst new_uniq lit ty loc
    in
    returnNF_Tc (HsVar (instToId lit_inst), unitLIE lit_inst)
\end{code}

\begin{code}
newFunDepFromDict dict
  | isClassDict dict
  = tcGetUnique		`thenNF_Tc` \ uniq ->
    let (clas, tys) = getDictClassTys dict
	fds = instantiateFdClassTys clas tys
	inst = FunDep uniq clas fds (instLoc dict)
    in
	if null fds then returnNF_Tc Nothing else returnNF_Tc (Just inst)
  | otherwise
  = returnNF_Tc Nothing
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

instToIdBndr (FunDep u clas fds _)
  = mkSysLocal SLIT("FunDep") u voidTy

ipToId n ty loc
  = mkUserLocal (mkIPOcc (getOccName n)) (nameUnique n) (mkPredTy (IParam n ty)) loc
\end{code}


Zonking
~~~~~~~
Zonking makes sure that the instance types are fully zonked,
but doesn't do the same for the Id in a Method.  There's no
need, and it's a lot of extra work.

\begin{code}
zonkPred :: TcPredType -> NF_TcM TcPredType
zonkPred (Class clas tys)
  = zonkTcTypes tys			`thenNF_Tc` \ new_tys ->
    returnNF_Tc (Class clas new_tys)
zonkPred (IParam n ty)
  = zonkTcType ty			`thenNF_Tc` \ new_ty ->
    returnNF_Tc (IParam n new_ty)

zonkInst :: Inst -> NF_TcM Inst
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

zonkInst (FunDep u clas fds loc)
  = zonkFunDeps fds			`thenNF_Tc` \ fds' ->
    returnNF_Tc (FunDep u clas fds' loc)

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
  = hsep [ppr lit, ptext SLIT("at"), ppr ty, show_uniq u]

pprInst (Dict u pred loc) = pprPred pred <+> show_uniq u

pprInst m@(Method u id tys theta tau loc)
  = hsep [ppr id, ptext SLIT("at"), 
	  brackets (interppSP tys) {- ,
	  ppr theta, ppr tau,
	  show_uniq u,
	  ppr (instToId m) -}]

pprInst (FunDep _ clas fds loc)
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
tidyInst env fd@(FunDep _ clas fds loc)
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
data LookupInstResult s
  = NoInstance
  | SimpleInst TcExpr		-- Just a variable, type application, or literal
  | GenInst    [Inst] TcExpr	-- The expression and its needed insts

lookupInst :: Inst 
	   -> NF_TcM (LookupInstResult s)

-- Dictionaries

lookupInst dict@(Dict _ (Class clas tys) loc)
  = tcGetInstEnv		`thenNF_Tc` \ inst_env ->
    case lookupInstEnv inst_env clas tys of

      FoundInst tenv dfun_id
	-> let
		subst	      = mkSubst (mkInScopeSet (tyVarsOfTypes tys)) tenv
		(tyvars, rho) = splitForAllTys (idType dfun_id)
		ty_args	      = map subst_tv tyvars
		dfun_rho      = substTy subst rho
		(theta, _)    = splitRhoTy dfun_rho
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

      other	-> returnNF_Tc NoInstance
lookupInst dict@(Dict _ _ loc) = returnNF_Tc NoInstance

-- Methods

lookupInst inst@(Method _ id tys theta _ loc)
  = newDictsAtLoc loc theta		`thenNF_Tc` \ (dicts, dict_ids) ->
    returnNF_Tc (GenInst dicts (mkHsDictApp (mkHsTyApp (HsVar id) tys) dict_ids))

-- Literals

lookupInst inst@(LitInst u (HsIntegral i from_integer_name) ty loc)
  | isIntTy ty && in_int_range			-- Short cut for Int
  = returnNF_Tc (GenInst [] int_lit)
	-- GenInst, not SimpleInst, because int_lit is actually a constructor application

  | isIntegerTy ty				-- Short cut for Integer
  = returnNF_Tc (GenInst [] integer_lit)

  | in_int_range 				-- It's overloaded but small enough to fit into an Int
  && from_integer_name `hasKey` fromIntegerClassOpKey	-- And it's the built-in prelude fromInteger
							-- (i.e. no funny business with user-defined
							--  packages of numeric classes)
  =	-- So we can use the Prelude fromInt 
    tcLookupGlobalId fromIntName		`thenNF_Tc` \ from_int ->
    newMethodAtLoc loc from_int [ty]		`thenNF_Tc` \ (method_inst, method_id) ->
    returnNF_Tc (GenInst [method_inst] (HsApp (HsVar method_id) int_lit))

  | otherwise   				-- Alas, it is overloaded and a big literal!
  = tcLookupGlobalId from_integer_name		`thenNF_Tc` \ from_integer ->
    newMethodAtLoc loc from_integer [ty]	`thenNF_Tc` \ (method_inst, method_id) ->
    returnNF_Tc (GenInst [method_inst] (HsApp (HsVar method_id) integer_lit))
  where
    in_int_range   = inIntRange i
    integer_lit    = HsLit (HsInteger i)
    int_lit        = HsLit (HsInt i)

-- similar idea for overloaded floating point literals: if the literal is
-- *definitely* a float or a double, generate the real thing here.
-- This is essential  (see nofib/spectral/nucleic).

lookupInst inst@(LitInst u (HsFractional f from_rat_name) ty loc)
  | isFloatTy ty    = returnNF_Tc (GenInst [] float_lit)
  | isDoubleTy ty   = returnNF_Tc (GenInst [] double_lit)

  | otherwise 
  = tcLookupGlobalId from_rat_name		`thenNF_Tc` \ from_rational ->
    newMethodAtLoc loc from_rational [ty]	`thenNF_Tc` \ (method_inst, method_id) ->
    let
	rational_ty  = funArgTy (idType method_id)
	rational_lit = HsLit (HsRat f rational_ty)
    in
    returnNF_Tc (GenInst [method_inst] (HsApp (HsVar method_id) rational_lit))

  where
    floatprim_lit  = HsLit (HsFloatPrim f)
    float_lit      = mkHsConApp floatDataCon [] [floatprim_lit]
    doubleprim_lit = HsLit (HsDoublePrim f)
    double_lit     = mkHsConApp doubleDataCon [] [doubleprim_lit]

-- there are no `instances' of functional dependencies or implicit params

lookupInst _  = returnNF_Tc NoInstance

\end{code}

There is a second, simpler interface, when you want an instance of a
class at a given nullary type constructor.  It just returns the
appropriate dictionary if it exists.  It is used only when resolving
ambiguous dictionaries.

\begin{code}
lookupSimpleInst :: Class
		 -> [Type]				-- Look up (c,t)
	         -> NF_TcM (Maybe [(Class,[Type])])	-- Here are the needed (c,t)s

lookupSimpleInst clas tys
  = tcGetInstEnv		`thenNF_Tc` \ inst_env -> 
    case lookupInstEnv inst_env clas tys of
      FoundInst tenv dfun
	-> returnNF_Tc (Just (substClasses (mkSubst emptyInScopeSet tenv) theta'))
        where
	   (_, theta, _) = splitSigmaTy (idType dfun)
	   theta' = map (\(Class clas tys) -> (clas,tys)) theta

      other  -> returnNF_Tc Nothing
\end{code}


