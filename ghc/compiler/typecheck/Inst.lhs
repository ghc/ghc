%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Inst]{The @Inst@ type: dictionaries or method instances}

\begin{code}
module Inst ( 
	LIE, emptyLIE, unitLIE, plusLIE, consLIE, zonkLIE,
	plusLIEs, mkLIE, isEmptyLIE, lieToList, listToLIE,

	Inst, 
	pprInst, pprInsts, pprInstsInFull, tidyInsts, tidyMoreInsts,

	newDictsFromOld, newDicts, cloneDict,
	newMethod, newMethodFromName, newMethodWithGivenTy, newMethodAtLoc,
	newOverloadedLit, newIPDict, tcInstCall, tcInstDataCon,

	tyVarsOfInst, tyVarsOfInsts, tyVarsOfLIE, 
	ipNamesOfInst, ipNamesOfInsts, predsOfInst, predsOfInsts,
	instLoc, getDictClassTys, dictPred,

	lookupInst, lookupSimpleInst, LookupInstResult(..),

	isDict, isClassDict, isMethod, 
	isLinearInst, linearInstType,
	isTyVarDict, isStdClassTyVarDict, isMethodFor, 
	instBindingRequired, instCanBeGeneralised,

	zonkInst, zonkInsts,
	instToId, instName,

	InstOrigin(..), InstLoc, pprInstLoc
    ) where

#include "HsVersions.h"

import HsSyn	( HsLit(..), HsOverLit(..), HsExpr(..) )
import TcHsSyn	( TcExpr, TcId, TypecheckedHsExpr,
		  mkHsTyApp, mkHsDictApp, mkHsConApp, zonkId
		)
import TcMonad
import TcEnv	( TcIdSet, tcGetInstEnv, tcLookupId, tcLookupGlobalId )
import InstEnv	( InstLookupResult(..), lookupInstEnv )
import TcMType	( zonkTcType, zonkTcTypes, zonkTcPredType, zapToType,
		  zonkTcThetaType, tcInstTyVar, tcInstType, tcInstTyVars
		)
import TcType	( Type, TcType, TcThetaType, TcPredType, TcTauType, TcTyVarSet,
		  SourceType(..), PredType, ThetaType, TyVarDetails(VanillaTv),
		  tcSplitForAllTys, tcSplitForAllTys, mkTyConApp,
		  tcSplitMethodTy, tcSplitPhiTy, tcFunArgTy,
		  isIntTy,isFloatTy, isIntegerTy, isDoubleTy,
		  tcIsTyVarTy, mkPredTy, mkTyVarTy, mkTyVarTys,
		  tyVarsOfType, tyVarsOfTypes, tyVarsOfPred, tidyPred,
		  isClassPred, isTyVarClassPred, isLinearPred,
		  getClassPredTys, getClassPredTys_maybe, mkPredName,
		  tidyType, tidyTypes, tidyFreeTyVars,
		  tcCmpType, tcCmpTypes, tcCmpPred
		)
import CoreFVs	( idFreeTyVars )
import Class	( Class )
import DataCon	( dataConSig )
import Id	( Id, idName, idType, mkUserLocal, mkSysLocal, mkLocalId, setIdUnique )
import PrelInfo	( isStandardClass, isCcallishClass, isNoDictClass )
import Name	( Name, mkMethodOcc, getOccName )
import PprType	( pprPred, pprParendType )	
import Subst	( emptyInScopeSet, mkSubst, 
		  substTy, substTyWith, substTheta, mkTyVarSubst, mkTopTyVarSubst
		)
import Literal	( inIntRange )
import VarEnv	( TidyEnv, lookupSubstEnv, SubstResult(..) )
import VarSet	( elemVarSet, emptyVarSet, unionVarSet )
import TysWiredIn ( floatDataCon, doubleDataCon )
import PrelNames( fromIntegerName, fromRationalName )
import Util	( thenCmp, equalLength )
import BasicTypes( IPName(..), mapIPName, ipNameName )

import Bag
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
pprInsts insts  = parens (sep (punctuate comma (map pprInst insts)))


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
	Id
	TcPredType
	InstLoc

  | Method
	Id

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
	Id
	HsOverLit	-- The literal from the occurrence site
	TcType		-- The type at which the literal is used
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

cmpInst (Dict _ pred1 _)     	  (Dict _ pred2 _)	    = pred1 `tcCmpPred` pred2
cmpInst (Dict _ _ _)	     	  other 		    = LT

cmpInst (Method _ _ _ _ _ _) 	  (Dict _ _ _)	  	    = GT
cmpInst (Method _ id1 tys1 _ _ _) (Method _ id2 tys2 _ _ _) = (id1 `compare` id2) `thenCmp` (tys1 `tcCmpTypes` tys2)
cmpInst (Method _ _ _ _ _ _)      other			    = LT

cmpInst (LitInst _ lit1 ty1 _)	  (LitInst _ lit2 ty2 _)    = (lit1 `compare` lit2) `thenCmp` (ty1 `tcCmpType` ty2)
cmpInst (LitInst _ _ _ _)	  other 		    = GT

-- and they can only have HsInt or HsFracs in them.
\end{code}


Selection
~~~~~~~~~
\begin{code}
instName :: Inst -> Name
instName inst = idName (instToId inst)

instToId :: Inst -> TcId
instToId (Dict id _ _)	       = id
instToId (Method id _ _ _ _ _) = id
instToId (LitInst id _ _ _)    = id

instLoc (Dict _ _         loc) = loc
instLoc (Method _ _ _ _ _ loc) = loc
instLoc (LitInst _ _ _    loc) = loc

dictPred (Dict _ pred _ ) = pred
dictPred inst		  = pprPanic "dictPred" (ppr inst)

getDictClassTys (Dict _ pred _) = getClassPredTys pred

predsOfInsts :: [Inst] -> [PredType]
predsOfInsts insts = concatMap predsOfInst insts

predsOfInst (Dict _ pred _)          = [pred]
predsOfInst (Method _ _ _ theta _ _) = theta
predsOfInst (LitInst _ _ _ _)	     = []
	-- The last case is is really a big cheat
	-- LitInsts to give rise to a (Num a) or (Fractional a) predicate
	-- But Num and Fractional have only one parameter and no functional
	-- dependencies, so I think no caller of predsOfInst will care.

ipNamesOfInsts :: [Inst] -> [Name]
ipNamesOfInst  :: Inst   -> [Name]
-- Get the implicit parameters mentioned by these Insts
-- NB: ?x and %x get different Names

ipNamesOfInsts insts = [n | inst <- insts, n <- ipNamesOfInst inst]

ipNamesOfInst (Dict _ (IParam n _) _)  = [ipNameName n]
ipNamesOfInst (Method _ _ _ theta _ _) = [ipNameName n | IParam n _ <- theta]
ipNamesOfInst other		       = []

tyVarsOfInst :: Inst -> TcTyVarSet
tyVarsOfInst (LitInst _ _ ty _)      = tyVarsOfType  ty
tyVarsOfInst (Dict _ pred _)         = tyVarsOfPred pred
tyVarsOfInst (Method _ id tys _ _ _) = tyVarsOfTypes tys `unionVarSet` idFreeTyVars id
					 -- The id might have free type variables; in the case of
					 -- locally-overloaded class methods, for example

tyVarsOfInsts insts = foldr (unionVarSet . tyVarsOfInst) emptyVarSet insts
tyVarsOfLIE   lie   = tyVarsOfInsts (lieToList lie)
\end{code}

Predicates
~~~~~~~~~~
\begin{code}
isDict :: Inst -> Bool
isDict (Dict _ _ _) = True
isDict other	    = False

isClassDict :: Inst -> Bool
isClassDict (Dict _ pred _) = isClassPred pred
isClassDict other	    = False

isTyVarDict :: Inst -> Bool
isTyVarDict (Dict _ pred _) = isTyVarClassPred pred
isTyVarDict other	    = False

isMethod :: Inst -> Bool
isMethod (Method _ _ _ _ _ _) = True
isMethod other		      = False

isMethodFor :: TcIdSet -> Inst -> Bool
isMethodFor ids (Method uniq id tys _ _ loc) = id `elemVarSet` ids
isMethodFor ids inst			     = False

isLinearInst :: Inst -> Bool
isLinearInst (Dict _ pred _) = isLinearPred pred
isLinearInst other	     = False
	-- We never build Method Insts that have
	-- linear implicit paramters in them.
	-- Hence no need to look for Methods
	-- See TcExpr.tcId 

linearInstType :: Inst -> TcType	-- %x::t  -->  t
linearInstType (Dict _ (IParam _ ty) _) = ty


isStdClassTyVarDict (Dict _ pred _) = case getClassPredTys_maybe pred of
					Just (clas, [ty]) -> isStandardClass clas && tcIsTyVarTy ty
					other		  -> False
\end{code}

Two predicates which deal with the case where class constraints don't
necessarily result in bindings.  The first tells whether an @Inst@
must be witnessed by an actual binding; the second tells whether an
@Inst@ can be generalised over.

\begin{code}
instBindingRequired :: Inst -> Bool
instBindingRequired (Dict _ (ClassP clas _) _) = not (isNoDictClass clas)
instBindingRequired other		       = True

instCanBeGeneralised :: Inst -> Bool
instCanBeGeneralised (Dict _ (ClassP clas _) _) = not (isCcallishClass clas)
instCanBeGeneralised other		        = True
\end{code}


%************************************************************************
%*									*
\subsection{Building dictionaries}
%*									*
%************************************************************************

\begin{code}
newDicts :: InstOrigin
	 -> TcThetaType
	 -> NF_TcM [Inst]
newDicts orig theta
  = tcGetInstLoc orig		`thenNF_Tc` \ loc ->
    newDictsAtLoc loc theta

cloneDict :: Inst -> NF_TcM Inst
cloneDict (Dict id ty loc) = tcGetUnique	`thenNF_Tc` \ uniq ->
			     returnNF_Tc (Dict (setIdUnique id uniq) ty loc)

newDictsFromOld :: Inst -> TcThetaType -> NF_TcM [Inst]
newDictsFromOld (Dict _ _ loc) theta = newDictsAtLoc loc theta

-- Local function, similar to newDicts, 
-- but with slightly different interface
newDictsAtLoc :: InstLoc
 	      -> TcThetaType
	      -> NF_TcM [Inst]
newDictsAtLoc inst_loc@(_,loc,_) theta
  = tcGetUniques 			`thenNF_Tc` \ new_uniqs ->
    returnNF_Tc (zipWith mk_dict new_uniqs theta)
  where
    mk_dict uniq pred = Dict (mkLocalId (mkPredName uniq loc pred) (mkPredTy pred)) pred inst_loc

-- For vanilla implicit parameters, there is only one in scope
-- at any time, so we used to use the name of the implicit parameter itself
-- But with splittable implicit parameters there may be many in 
-- scope, so we make up a new name.
newIPDict :: InstOrigin -> IPName Name -> Type 
	  -> NF_TcM (IPName Id, Inst)
newIPDict orig ip_name ty
  = tcGetInstLoc orig			`thenNF_Tc` \ inst_loc@(_,loc,_) ->
    tcGetUnique				`thenNF_Tc` \ uniq ->
    let
	pred = IParam ip_name ty
	id   = mkLocalId (mkPredName uniq loc pred) (mkPredTy pred)
    in
    returnNF_Tc (mapIPName (\n -> id) ip_name, Dict id pred inst_loc)
\end{code}


%************************************************************************
%*									*
\subsection{Building methods (calls of overloaded functions)}
%*									*
%************************************************************************


\begin{code}
tcInstCall :: InstOrigin  -> TcType -> NF_TcM (TypecheckedHsExpr -> TypecheckedHsExpr, LIE, TcType)
tcInstCall orig fun_ty	-- fun_ty is usually a sigma-type
  = tcInstType VanillaTv fun_ty	`thenNF_Tc` \ (tyvars, theta, tau) ->
    newDicts orig theta		`thenNF_Tc` \ dicts ->
    let
	inst_fn e = mkHsDictApp (mkHsTyApp e (mkTyVarTys tyvars)) (map instToId dicts)
    in
    returnNF_Tc (inst_fn, mkLIE dicts, tau)

tcInstDataCon orig data_con
  = let 
	(tvs, stupid_theta, ex_tvs, ex_theta, arg_tys, tycon) = dataConSig data_con
	     -- We generate constraints for the stupid theta even when 
	     -- pattern matching (as the Report requires)
    in
    tcInstTyVars VanillaTv (tvs ++ ex_tvs)	`thenNF_Tc` \ (all_tvs', ty_args', tenv) ->
    let
	stupid_theta' = substTheta tenv stupid_theta
	ex_theta'     = substTheta tenv ex_theta
	arg_tys'      = map (substTy tenv) arg_tys

	n_normal_tvs  = length tvs
	ex_tvs'       = drop n_normal_tvs all_tvs'
	result_ty     = mkTyConApp tycon (take n_normal_tvs ty_args')
    in
    newDicts orig stupid_theta'	`thenNF_Tc` \ stupid_dicts ->
    newDicts orig ex_theta'	`thenNF_Tc` \ ex_dicts ->

	-- Note that we return the stupid theta *only* in the LIE;
	-- we don't otherwise use it at all
    returnNF_Tc (ty_args', map instToId ex_dicts, arg_tys', result_ty,
		 mkLIE stupid_dicts, mkLIE ex_dicts, ex_tvs')


newMethodFromName :: InstOrigin -> TcType -> Name -> NF_TcM Inst
newMethodFromName origin ty name
  = tcLookupGlobalId name		`thenNF_Tc` \ id ->
    newMethod origin id [ty]

newMethod :: InstOrigin
	  -> TcId
	  -> [TcType]
	  -> NF_TcM Inst
newMethod orig id tys
  =   	-- Get the Id type and instantiate it at the specified types
    let
	(tyvars, rho) = tcSplitForAllTys (idType id)
	rho_ty	      = substTyWith tyvars tys rho
	(pred, tau)   = tcSplitMethodTy rho_ty
    in
    newMethodWithGivenTy orig id tys [pred] tau

newMethodWithGivenTy orig id tys theta tau
  = tcGetInstLoc orig	`thenNF_Tc` \ loc ->
    newMethodWith loc id tys theta tau

newMethodWith inst_loc@(_,loc,_) id tys theta tau
  = tcGetUnique		`thenNF_Tc` \ new_uniq ->
    let
	meth_id	= mkUserLocal (mkMethodOcc (getOccName id)) new_uniq tau loc
    in
    returnNF_Tc (Method meth_id id tys theta tau inst_loc)

newMethodAtLoc :: InstLoc
	       -> Id -> [TcType]
	       -> NF_TcM (Inst, TcId)
newMethodAtLoc inst_loc real_id tys
	-- This actually builds the Inst
  =   	-- Get the Id type and instantiate it at the specified types
    let
	(tyvars,rho)  = tcSplitForAllTys (idType real_id)
	rho_ty	      = ASSERT( equalLength tyvars tys )
			substTy (mkTopTyVarSubst tyvars tys) rho
	(theta, tau)  = tcSplitPhiTy rho_ty
    in
    newMethodWith inst_loc real_id tys theta tau	`thenNF_Tc` \ meth_inst ->
    returnNF_Tc (meth_inst, instToId meth_inst)
\end{code}

In newOverloadedLit we convert directly to an Int or Integer if we
know that's what we want.  This may save some time, by not
temporarily generating overloaded literals, but it won't catch all
cases (the rest are caught in lookupInst).

\begin{code}
newOverloadedLit :: InstOrigin
		 -> HsOverLit
		 -> TcType
		 -> NF_TcM (TcExpr, LIE)
newOverloadedLit orig lit expected_ty
  | Just expr <- shortCutLit lit expected_ty
  = returnNF_Tc (expr, emptyLIE)

  | otherwise
  = tcGetInstLoc orig		`thenNF_Tc` \ loc ->
    tcGetUnique			`thenNF_Tc` \ new_uniq ->
    zapToType expected_ty	`thenNF_Tc_` 
	-- The expected type might be a 'hole' type variable, 
	-- in which case we must zap it to an ordinary type variable
    let
	lit_inst = LitInst lit_id lit expected_ty loc
	lit_id   = mkSysLocal FSLIT("lit") new_uniq expected_ty
    in
    returnNF_Tc (HsVar (instToId lit_inst), unitLIE lit_inst)

shortCutLit :: HsOverLit -> TcType -> Maybe TcExpr
shortCutLit (HsIntegral i fi) ty
  | isIntTy ty && inIntRange i && fi == fromIntegerName		-- Short cut for Int
  = Just (HsLit (HsInt i))
  | isIntegerTy ty && fi == fromIntegerName			-- Short cut for Integer
  = Just (HsLit (HsInteger i))

shortCutLit (HsFractional f fr) ty
  | isFloatTy ty  && fr == fromRationalName 
  = Just (mkHsConApp floatDataCon [] [HsLit (HsFloatPrim f)])
  | isDoubleTy ty && fr == fromRationalName 
  = Just (mkHsConApp doubleDataCon [] [HsLit (HsDoublePrim f)])

shortCutLit lit ty
  = Nothing
\end{code}


%************************************************************************
%*									*
\subsection{Zonking}
%*									*
%************************************************************************

Zonking makes sure that the instance types are fully zonked,
but doesn't do the same for any of the Ids in an Inst.  There's no
need, and it's a lot of extra work.

\begin{code}
zonkInst :: Inst -> NF_TcM Inst
zonkInst (Dict id pred loc)
  = zonkTcPredType pred			`thenNF_Tc` \ new_pred ->
    returnNF_Tc (Dict id new_pred loc)

zonkInst (Method m id tys theta tau loc) 
  = zonkId id			`thenNF_Tc` \ new_id ->
	-- Essential to zonk the id in case it's a local variable
	-- Can't use zonkIdOcc because the id might itself be
	-- an InstId, in which case it won't be in scope

    zonkTcTypes tys		`thenNF_Tc` \ new_tys ->
    zonkTcThetaType theta	`thenNF_Tc` \ new_theta ->
    zonkTcType tau		`thenNF_Tc` \ new_tau ->
    returnNF_Tc (Method m new_id new_tys new_theta new_tau loc)

zonkInst (LitInst id lit ty loc)
  = zonkTcType ty			`thenNF_Tc` \ new_ty ->
    returnNF_Tc (LitInst id lit new_ty loc)

zonkInsts insts = mapNF_Tc zonkInst insts
\end{code}


%************************************************************************
%*									*
\subsection{Printing}
%*									*
%************************************************************************

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
	  brackets (sep (map pprParendType tys)) {- ,
	  ptext SLIT("theta"), ppr theta,
	  ptext SLIT("tau"), ppr tau
	  show_uniq u,
	  ppr (instToId m) -}]

show_uniq u = ifPprDebug (text "{-" <> ppr u <> text "-}")

tidyInst :: TidyEnv -> Inst -> Inst
tidyInst env (LitInst u lit ty loc) 	     = LitInst u lit (tidyType env ty) loc
tidyInst env (Dict u pred loc)     	     = Dict u (tidyPred env pred) loc
tidyInst env (Method u id tys theta tau loc) = Method u id (tidyTypes env tys) theta tau loc

tidyMoreInsts :: TidyEnv -> [Inst] -> (TidyEnv, [Inst])
-- This function doesn't assume that the tyvars are in scope
-- so it works like tidyOpenType, returning a TidyEnv
tidyMoreInsts env insts
  = (env', map (tidyInst env') insts)
  where
    env' = tidyFreeTyVars env (tyVarsOfInsts insts)

tidyInsts :: [Inst] -> (TidyEnv, [Inst])
tidyInsts insts = tidyMoreInsts emptyTidyEnv insts
\end{code}


%************************************************************************
%*									*
\subsection{Looking up Insts}
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

lookupInst dict@(Dict _ (ClassP clas tys) loc)
  = getDOptsTc			`thenNF_Tc` \ dflags ->
    tcGetInstEnv		`thenNF_Tc` \ inst_env ->
    case lookupInstEnv dflags inst_env clas tys of

      FoundInst tenv dfun_id
	->	-- It's possible that not all the tyvars are in
		-- the substitution, tenv. For example:
		--	instance C X a => D X where ...
		-- (presumably there's a functional dependency in class C)
		-- Hence the mk_ty_arg to instantiate any un-substituted tyvars.	
	   let
		(tyvars, rho) = tcSplitForAllTys (idType dfun_id)
		mk_ty_arg tv  = case lookupSubstEnv tenv tv of
				   Just (DoneTy ty) -> returnNF_Tc ty
				   Nothing 	    -> tcInstTyVar VanillaTv tv `thenNF_Tc` \ tc_tv ->
						       returnTc (mkTyVarTy tc_tv)
	   in
	   mapNF_Tc mk_ty_arg tyvars	`thenNF_Tc` \ ty_args ->
	   let
		dfun_rho   = substTy (mkTyVarSubst tyvars ty_args) rho
		(theta, _) = tcSplitPhiTy dfun_rho
		ty_app     = mkHsTyApp (HsVar dfun_id) ty_args
	   in
	   if null theta then
		returnNF_Tc (SimpleInst ty_app)
	   else
	   newDictsAtLoc loc theta	`thenNF_Tc` \ dicts ->
	   let 
		rhs = mkHsDictApp ty_app (map instToId dicts)
	   in
	   returnNF_Tc (GenInst dicts rhs)

      other	-> returnNF_Tc NoInstance

lookupInst dict@(Dict _ _ loc) = returnNF_Tc NoInstance

-- Methods

lookupInst inst@(Method _ id tys theta _ loc)
  = newDictsAtLoc loc theta		`thenNF_Tc` \ dicts ->
    returnNF_Tc (GenInst dicts (mkHsDictApp (mkHsTyApp (HsVar id) tys) (map instToId dicts)))

-- Literals

-- Look for short cuts first: if the literal is *definitely* a 
-- int, integer, float or a double, generate the real thing here.
-- This is essential  (see nofib/spectral/nucleic).
-- [Same shortcut as in newOverloadedLit, but we
--  may have done some unification by now] 		

lookupInst inst@(LitInst u lit ty loc)
  | Just expr <- shortCutLit lit ty
  = returnNF_Tc (GenInst [] expr)	-- GenInst, not SimpleInst, because 
					-- expr may be a constructor application

lookupInst inst@(LitInst u (HsIntegral i from_integer_name) ty loc)
  = tcLookupId from_integer_name		`thenNF_Tc` \ from_integer ->
    newMethodAtLoc loc from_integer [ty]	`thenNF_Tc` \ (method_inst, method_id) ->
    returnNF_Tc (GenInst [method_inst] 
			 (HsApp (HsVar method_id) (HsLit (HsInteger i))))


lookupInst inst@(LitInst u (HsFractional f from_rat_name) ty loc)
  = tcLookupId from_rat_name			`thenNF_Tc` \ from_rational ->
    newMethodAtLoc loc from_rational [ty]	`thenNF_Tc` \ (method_inst, method_id) ->
    let
	rational_ty  = tcFunArgTy (idType method_id)
	rational_lit = HsLit (HsRat f rational_ty)
    in
    returnNF_Tc (GenInst [method_inst] (HsApp (HsVar method_id) rational_lit))
\end{code}

There is a second, simpler interface, when you want an instance of a
class at a given nullary type constructor.  It just returns the
appropriate dictionary if it exists.  It is used only when resolving
ambiguous dictionaries.

\begin{code}
lookupSimpleInst :: Class
		 -> [Type]			-- Look up (c,t)
	         -> NF_TcM (Maybe ThetaType)	-- Here are the needed (c,t)s

lookupSimpleInst clas tys
  = getDOptsTc			`thenNF_Tc` \ dflags ->
    tcGetInstEnv		`thenNF_Tc` \ inst_env -> 
    case lookupInstEnv dflags inst_env clas tys of
      FoundInst tenv dfun
	-> returnNF_Tc (Just (substTheta (mkSubst emptyInScopeSet tenv) theta))
        where
	   (_, rho)  = tcSplitForAllTys (idType dfun)
	   (theta,_) = tcSplitPhiTy rho

      other  -> returnNF_Tc Nothing
\end{code}
