%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Inst]{The @Inst@ type: dictionaries or method instances}

\begin{code}
module Inst ( 
	Inst, 

	pprInstances, pprDictsTheta, pprDictsInFull,	-- User error messages
	showLIE, pprInst, pprInsts, pprInstInFull,	-- Debugging messages

	tidyInsts, tidyMoreInsts,

	newDictBndr, newDictBndrs, newDictBndrsO,
	instCall, instStupidTheta,
	cloneDict, 
	shortCutFracLit, shortCutIntLit, newIPDict, 
	newMethod, newMethodFromName, newMethodWithGivenTy, 
	tcInstClassOp, 
	tcSyntaxName, isHsVar,

	tyVarsOfInst, tyVarsOfInsts, tyVarsOfLIE, 
	ipNamesOfInst, ipNamesOfInsts, fdPredsOfInst, fdPredsOfInsts,
	instLoc, getDictClassTys, dictPred,

	lookupInst, LookupInstResult(..), lookupPred, 
	tcExtendLocalInstEnv, tcGetInstEnvs, getOverlapFlag,

	isDict, isClassDict, isMethod, 
	isLinearInst, linearInstType, isIPDict, isInheritableInst,
	isTyVarDict, isMethodFor, 

	zonkInst, zonkInsts,
	instToId, instToVar, instName,

	InstOrigin(..), InstLoc(..), pprInstLoc
    ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcPolyExpr )
import {-# SOURCE #-}	TcUnify( unifyType )

import HsSyn	( HsLit(..), HsOverLit(..), HsExpr(..), LHsExpr, mkHsApp,
		  ExprCoFn(..), (<.>), mkCoTyApps, idCoercion,
		  nlHsLit, nlHsVar )
import TcHsSyn	( zonkId )
import TcRnMonad
import TcEnv	( tcLookupId, checkWellStaged, topIdLvl, tcMetaTy )
import InstEnv	( DFunId, InstEnv, Instance(..), OverlapFlag(..),
		  lookupInstEnv, extendInstEnv, pprInstances, 
		  instanceHead, instanceDFunId, setInstanceDFunId )
import FunDeps	( checkFunDeps )
import TcMType	( zonkTcType, zonkTcTypes, zonkTcPredType, zonkTcThetaType, 
		  tcInstTyVar, tcInstSkolType
		)
import TcType	( Type, TcType, TcThetaType, TcTyVarSet, TcPredType,
		  BoxyRhoType,
		  PredType(..), SkolemInfo(..), typeKind, mkSigmaTy,
		  tcSplitForAllTys, applyTys, 
		  tcSplitPhiTy, tcSplitDFunHead,
		  isIntTy,isFloatTy, isIntegerTy, isDoubleTy,
		  mkPredTy, mkTyVarTys,
		  tyVarsOfType, tyVarsOfTypes, tyVarsOfPred, tidyPred,
		  isClassPred, isTyVarClassPred, isLinearPred, 
		  getClassPredTys, mkPredName,
		  isInheritablePred, isIPPred, 
		  tidyType, tidyTypes, tidyFreeTyVars, tcSplitSigmaTy, 
		  pprPred, pprParendType, pprTheta 
		)
import Type	( TvSubst, substTy, substTyVar, substTyWith,
		  notElemTvSubst, extendTvSubstList )
import Unify	( tcMatchTys )
import Module	( modulePackageId )
import {- Kind parts of -} Type	( isSubKind )
import Coercion ( isEqPred )
import HscTypes	( ExternalPackageState(..), HscEnv(..) )
import CoreFVs	( idFreeTyVars )
import DataCon	( dataConWrapId )
import Id	( Id, idName, idType, mkUserLocal, mkLocalId, isId )
import Name	( Name, mkMethodOcc, getOccName, getSrcLoc, nameModule,
		  isInternalName, setNameUnique )
import NameSet	( addOneToNameSet )
import Literal	( inIntRange )
import Var	( Var, TyVar, tyVarKind, setIdType, isId, mkTyVar )
import VarEnv	( TidyEnv, emptyTidyEnv )
import VarSet	( elemVarSet, emptyVarSet, unionVarSet, mkVarSet )
import TysWiredIn ( floatDataCon, doubleDataCon )
import PrelNames	( integerTyConName, fromIntegerName, fromRationalName, rationalTyConName )
import BasicTypes( IPName(..), mapIPName, ipNameName )
import SrcLoc	( mkSrcSpan, noLoc, unLoc, Located(..) )
import DynFlags	( DynFlag(..), DynFlags(..), dopt )
import Maybes	( isJust )
import Outputable
\end{code}


Selection
~~~~~~~~~
\begin{code}
instName :: Inst -> Name
instName inst = idName (instToId inst)

instToId :: Inst -> TcId
instToId inst = ASSERT2( isId id, ppr inst ) id 
	      where
		id = instToVar inst

instToVar :: Inst -> Var
instToVar (LitInst nm _ ty _) = mkLocalId nm ty
instToVar (Method id _ _ _ _) = id
instToVar (Dict nm pred _)    
  | isEqPred pred = mkTyVar nm (mkPredTy pred)
  | otherwise	  = mkLocalId nm (mkPredTy pred)

instLoc (Dict _ _       loc) = loc
instLoc (Method _ _ _ _ loc) = loc
instLoc (LitInst _ _ _  loc) = loc

dictPred (Dict _ pred _ ) = pred
dictPred inst		  = pprPanic "dictPred" (ppr inst)

getDictClassTys (Dict _ pred _) = getClassPredTys pred

-- fdPredsOfInst is used to get predicates that contain functional 
-- dependencies *or* might do so.  The "might do" part is because
-- a constraint (C a b) might have a superclass with FDs
-- Leaving these in is really important for the call to fdPredsOfInsts
-- in TcSimplify.inferLoop, because the result is fed to 'grow',
-- which is supposed to be conservative
fdPredsOfInst (Dict _ pred _) 	     = [pred]
fdPredsOfInst (Method _ _ _ theta _) = theta
fdPredsOfInst other		     = []	-- LitInsts etc

fdPredsOfInsts :: [Inst] -> [PredType]
fdPredsOfInsts insts = concatMap fdPredsOfInst insts

isInheritableInst (Dict _ pred _) 	 = isInheritablePred pred
isInheritableInst (Method _ _ _ theta _) = all isInheritablePred theta
isInheritableInst other			 = True


ipNamesOfInsts :: [Inst] -> [Name]
ipNamesOfInst  :: Inst   -> [Name]
-- Get the implicit parameters mentioned by these Insts
-- NB: ?x and %x get different Names
ipNamesOfInsts insts = [n | inst <- insts, n <- ipNamesOfInst inst]

ipNamesOfInst (Dict _ (IParam n _) _) = [ipNameName n]
ipNamesOfInst (Method _ _ _ theta _)  = [ipNameName n | IParam n _ <- theta]
ipNamesOfInst other		      = []

tyVarsOfInst :: Inst -> TcTyVarSet
tyVarsOfInst (LitInst _ _ ty _)    = tyVarsOfType  ty
tyVarsOfInst (Dict _ pred _)       = tyVarsOfPred pred
tyVarsOfInst (Method _ id tys _ _) = tyVarsOfTypes tys `unionVarSet` idFreeTyVars id
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

isIPDict :: Inst -> Bool
isIPDict (Dict _ pred _) = isIPPred pred
isIPDict other		 = False

isMethod :: Inst -> Bool
isMethod (Method {}) = True
isMethod other	     = False

isMethodFor :: TcIdSet -> Inst -> Bool
isMethodFor ids (Method uniq id tys _ loc) = id `elemVarSet` ids
isMethodFor ids inst			   = False

isLinearInst :: Inst -> Bool
isLinearInst (Dict _ pred _) = isLinearPred pred
isLinearInst other	     = False
	-- We never build Method Insts that have
	-- linear implicit paramters in them.
	-- Hence no need to look for Methods
	-- See TcExpr.tcId 

linearInstType :: Inst -> TcType	-- %x::t  -->  t
linearInstType (Dict _ (IParam _ ty) _) = ty
\end{code}



%************************************************************************
%*									*
\subsection{Building dictionaries}
%*									*
%************************************************************************

-- newDictBndrs makes a dictionary at a binding site
-- instCall makes a dictionary at an occurrence site
--	and throws it into the LIE

\begin{code}
----------------
newDictBndrsO :: InstOrigin -> TcThetaType -> TcM [Inst]
newDictBndrsO orig theta = do { loc <- getInstLoc orig
			      ; newDictBndrs loc theta }

newDictBndrs :: InstLoc -> TcThetaType -> TcM [Inst]
newDictBndrs inst_loc theta = mapM (newDictBndr inst_loc) theta

newDictBndr :: InstLoc -> TcPredType -> TcM Inst
newDictBndr inst_loc pred
  = do 	{ uniq <- newUnique 
	; let name = mkPredName uniq (instLocSrcLoc inst_loc) pred 
	; return (Dict name pred inst_loc) }

----------------
instCall :: InstOrigin -> [TcType] -> TcThetaType -> TcM ExprCoFn
-- Instantiate the constraints of a call
--	(instCall o tys theta)
-- (a) Makes fresh dictionaries as necessary for the constraints (theta)
-- (b) Throws these dictionaries into the LIE
-- (c) Eeturns an ExprCoFn ([.] tys dicts)

instCall orig tys theta 
  = do	{ loc <- getInstLoc orig
	; (dicts, dict_app) <- instCallDicts loc theta
	; extendLIEs dicts
	; return (dict_app <.> mkCoTyApps tys) }

----------------
instStupidTheta :: InstOrigin -> TcThetaType -> TcM ()
-- Similar to instCall, but only emit the constraints in the LIE
-- Used exclusively for the 'stupid theta' of a data constructor
instStupidTheta orig theta
  = do	{ loc <- getInstLoc orig
	; (dicts, _) <- instCallDicts loc theta
	; extendLIEs dicts }

----------------
instCallDicts :: InstLoc -> TcThetaType -> TcM ([Inst], ExprCoFn)
-- This is the key place where equality predicates 
-- are unleashed into the world
instCallDicts loc [] = return ([], idCoercion)

instCallDicts loc (EqPred ty1 ty2 : preds)
  = do  { unifyType ty1 ty2	-- For now, we insist that they unify right away 
				-- Later on, when we do associated types, 
				-- unifyType :: Type -> Type -> TcM ([Inst], Coercion)
	; (dicts, co_fn) <- instCallDicts loc preds
	; return (dicts, co_fn <.> CoTyApp ty1) }
	-- We use type application to apply the function to the 
	-- coercion; here ty1 *is* the appropriate identity coercion

instCallDicts loc (pred : preds)
  = do	{ uniq <- newUnique
	; let name = mkPredName uniq (instLocSrcLoc loc) pred 
	      dict = Dict name pred loc
	; (dicts, co_fn) <- instCallDicts loc preds
	; return (dict:dicts, co_fn <.> CoApp (instToId dict)) }

-------------
cloneDict :: Inst -> TcM Inst	-- Only used for linear implicit params
cloneDict (Dict nm ty loc) = newUnique	`thenM` \ uniq ->
			     returnM (Dict (setNameUnique nm uniq) ty loc)

-- For vanilla implicit parameters, there is only one in scope
-- at any time, so we used to use the name of the implicit parameter itself
-- But with splittable implicit parameters there may be many in 
-- scope, so we make up a new namea.
newIPDict :: InstOrigin -> IPName Name -> Type 
	  -> TcM (IPName Id, Inst)
newIPDict orig ip_name ty
  = getInstLoc orig			`thenM` \ inst_loc ->
    newUnique				`thenM` \ uniq ->
    let
	pred = IParam ip_name ty
        name = mkPredName uniq (instLocSrcLoc inst_loc) pred 
	dict = Dict name pred inst_loc
    in
    returnM (mapIPName (\n -> instToId dict) ip_name, dict)
\end{code}



%************************************************************************
%*									*
\subsection{Building methods (calls of overloaded functions)}
%*									*
%************************************************************************


\begin{code}
newMethodFromName :: InstOrigin -> BoxyRhoType -> Name -> TcM TcId
newMethodFromName origin ty name
  = tcLookupId name		`thenM` \ id ->
	-- Use tcLookupId not tcLookupGlobalId; the method is almost
	-- always a class op, but with -fno-implicit-prelude GHC is
	-- meant to find whatever thing is in scope, and that may
	-- be an ordinary function. 
    getInstLoc origin		`thenM` \ loc ->
    tcInstClassOp loc id [ty]	`thenM` \ inst ->
    extendLIE inst		`thenM_`
    returnM (instToId inst)

newMethodWithGivenTy orig id tys
  = getInstLoc orig		`thenM` \ loc ->
    newMethod loc id tys	`thenM` \ inst ->
    extendLIE inst		`thenM_`
    returnM (instToId inst)

--------------------------------------------
-- tcInstClassOp, and newMethod do *not* drop the 
-- Inst into the LIE; they just returns the Inst
-- This is important because they are used by TcSimplify
-- to simplify Insts

-- NB: the kind of the type variable to be instantiated
--     might be a sub-kind of the type to which it is applied,
--     notably when the latter is a type variable of kind ??
--     Hence the call to checkKind
-- A worry: is this needed anywhere else?
tcInstClassOp :: InstLoc -> Id -> [TcType] -> TcM Inst
tcInstClassOp inst_loc sel_id tys
  = let
	(tyvars, _rho) = tcSplitForAllTys (idType sel_id)
    in
    zipWithM_ checkKind tyvars tys	`thenM_` 
    newMethod inst_loc sel_id tys

checkKind :: TyVar -> TcType -> TcM ()
-- Ensure that the type has a sub-kind of the tyvar
checkKind tv ty
  = do	{ let ty1 = ty 
		-- ty1 <- zonkTcType ty
	; if typeKind ty1 `isSubKind` tyVarKind tv
	  then return ()
	  else 

    pprPanic "checkKind: adding kind constraint" 
	     (vcat [ppr tv <+> ppr (tyVarKind tv), 
	            ppr ty <+> ppr ty1 <+> ppr (typeKind ty1)])
	}
--    do	{ tv1 <- tcInstTyVar tv
--	; unifyType ty1 (mkTyVarTy tv1) } }


---------------------------
newMethod inst_loc id tys
  = newUnique		`thenM` \ new_uniq ->
    let
	(theta,tau) = tcSplitPhiTy (applyTys (idType id) tys)
	meth_id	    = mkUserLocal (mkMethodOcc (getOccName id)) new_uniq tau loc
	inst        = Method meth_id id tys theta inst_loc
	loc         = instLocSrcLoc inst_loc
    in
    returnM inst
\end{code}

\begin{code}
shortCutIntLit :: Integer -> TcType -> Maybe (HsExpr TcId)
shortCutIntLit i ty
  | isIntTy ty && inIntRange i 		-- Short cut for Int
  = Just (HsLit (HsInt i))
  | isIntegerTy ty 			-- Short cut for Integer
  = Just (HsLit (HsInteger i ty))
  | otherwise = Nothing

shortCutFracLit :: Rational -> TcType -> Maybe (HsExpr TcId)
shortCutFracLit f ty
  | isFloatTy ty 
  = Just (mk_lit floatDataCon (HsFloatPrim f))
  | isDoubleTy ty
  = Just (mk_lit doubleDataCon (HsDoublePrim f))
  | otherwise = Nothing
  where
    mk_lit con lit = HsApp (nlHsVar (dataConWrapId con)) (nlHsLit lit)

mkIntegerLit :: Integer -> TcM (LHsExpr TcId)
mkIntegerLit i
  = tcMetaTy integerTyConName 	`thenM` \ integer_ty ->
    getSrcSpanM			`thenM` \ span -> 
    returnM (L span $ HsLit (HsInteger i integer_ty))

mkRatLit :: Rational -> TcM (LHsExpr TcId)
mkRatLit r
  = tcMetaTy rationalTyConName 	`thenM` \ rat_ty ->
    getSrcSpanM			`thenM` \ span -> 
    returnM (L span $ HsLit (HsRat r rat_ty))

isHsVar :: HsExpr Name -> Name -> Bool
isHsVar (HsVar f) g = f==g
isHsVar other 	  g = False
\end{code}


%************************************************************************
%*									*
\subsection{Zonking}
%*									*
%************************************************************************

Zonking makes sure that the instance types are fully zonked.

\begin{code}
zonkInst :: Inst -> TcM Inst
zonkInst (Dict name pred loc)
  = zonkTcPredType pred			`thenM` \ new_pred ->
    returnM (Dict name new_pred loc)

zonkInst (Method m id tys theta loc) 
  = zonkId id			`thenM` \ new_id ->
	-- Essential to zonk the id in case it's a local variable
	-- Can't use zonkIdOcc because the id might itself be
	-- an InstId, in which case it won't be in scope

    zonkTcTypes tys		`thenM` \ new_tys ->
    zonkTcThetaType theta	`thenM` \ new_theta ->
    returnM (Method m new_id new_tys new_theta loc)

zonkInst (LitInst nm lit ty loc)
  = zonkTcType ty			`thenM` \ new_ty ->
    returnM (LitInst nm lit new_ty loc)

zonkInsts insts = mappM zonkInst insts
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

pprDictsTheta :: [Inst] -> SDoc
-- Print in type-like fashion (Eq a, Show b)
pprDictsTheta dicts = pprTheta (map dictPred dicts)

pprDictsInFull :: [Inst] -> SDoc
-- Print in type-like fashion, but with source location
pprDictsInFull dicts 
  = vcat (map go dicts)
  where
    go dict = sep [quotes (ppr (dictPred dict)), nest 2 (pprInstLoc (instLoc dict))]

pprInsts :: [Inst] -> SDoc
-- Debugging: print the evidence :: type
pprInsts insts  = brackets (interpp'SP insts)

pprInst, pprInstInFull :: Inst -> SDoc
-- Debugging: print the evidence :: type
pprInst (LitInst nm lit ty loc) = ppr nm <+> dcolon <+> ppr ty
pprInst (Dict nm pred loc)      = ppr nm <+> dcolon <+> pprPred pred

pprInst m@(Method inst_id id tys theta loc)
  = ppr inst_id <+> dcolon <+> 
	braces (sep [ppr id <+> ptext SLIT("at"),
		     brackets (sep (map pprParendType tys))])

pprInstInFull inst
  = sep [quotes (pprInst inst), nest 2 (pprInstLoc (instLoc inst))]

tidyInst :: TidyEnv -> Inst -> Inst
tidyInst env (LitInst nm lit ty loc) 	 = LitInst nm lit (tidyType env ty) loc
tidyInst env (Dict nm pred loc)     	 = Dict nm (tidyPred env pred) loc
tidyInst env (Method u id tys theta loc) = Method u id (tidyTypes env tys) theta loc

tidyMoreInsts :: TidyEnv -> [Inst] -> (TidyEnv, [Inst])
-- This function doesn't assume that the tyvars are in scope
-- so it works like tidyOpenType, returning a TidyEnv
tidyMoreInsts env insts
  = (env', map (tidyInst env') insts)
  where
    env' = tidyFreeTyVars env (tyVarsOfInsts insts)

tidyInsts :: [Inst] -> (TidyEnv, [Inst])
tidyInsts insts = tidyMoreInsts emptyTidyEnv insts

showLIE :: SDoc -> TcM ()	-- Debugging
showLIE str
  = do { lie_var <- getLIEVar ;
	 lie <- readMutVar lie_var ;
	 traceTc (str <+> vcat (map pprInstInFull (lieToList lie))) }
\end{code}


%************************************************************************
%*									*
	Extending the instance environment
%*									*
%************************************************************************

\begin{code}
tcExtendLocalInstEnv :: [Instance] -> TcM a -> TcM a
  -- Add new locally-defined instances
tcExtendLocalInstEnv dfuns thing_inside
 = do { traceDFuns dfuns
      ; env <- getGblEnv
      ; inst_env' <- foldlM addLocalInst (tcg_inst_env env) dfuns
      ; let env' = env { tcg_insts = dfuns ++ tcg_insts env,
			 tcg_inst_env = inst_env' }
      ; setGblEnv env' thing_inside }

addLocalInst :: InstEnv -> Instance -> TcM InstEnv
-- Check that the proposed new instance is OK, 
-- and then add it to the home inst env
addLocalInst home_ie ispec
  = do	{ 	-- Instantiate the dfun type so that we extend the instance
		-- envt with completely fresh template variables
		-- This is important because the template variables must
		-- not overlap with anything in the things being looked up
		-- (since we do unification).  
		-- We use tcInstSkolType because we don't want to allocate fresh
		--  *meta* type variables.  
	  let dfun = instanceDFunId ispec
	; (tvs', theta', tau') <- tcInstSkolType (InstSkol dfun) (idType dfun)
	; let	(cls, tys') = tcSplitDFunHead tau'
		dfun' 	    = setIdType dfun (mkSigmaTy tvs' theta' tau')	    
	  	ispec'      = setInstanceDFunId ispec dfun'

		-- Load imported instances, so that we report
		-- duplicates correctly
	; eps <- getEps
	; let inst_envs = (eps_inst_env eps, home_ie)

		-- Check functional dependencies
	; case checkFunDeps inst_envs ispec' of
		Just specs -> funDepErr ispec' specs
		Nothing    -> return ()

		-- Check for duplicate instance decls
	; let { (matches, _) = lookupInstEnv inst_envs cls tys'
	      ;	dup_ispecs = [ dup_ispec 
			     | (_, dup_ispec) <- matches
			     , let (_,_,_,dup_tys) = instanceHead dup_ispec
			     , isJust (tcMatchTys (mkVarSet tvs') tys' dup_tys)] }
		-- Find memebers of the match list which ispec itself matches.
		-- If the match is 2-way, it's a duplicate
	; case dup_ispecs of
	    dup_ispec : _ -> dupInstErr ispec' dup_ispec
	    []            -> return ()

		-- OK, now extend the envt
	; return (extendInstEnv home_ie ispec') }

getOverlapFlag :: TcM OverlapFlag
getOverlapFlag 
  = do 	{ dflags <- getDOpts
	; let overlap_ok    = dopt Opt_AllowOverlappingInstances dflags
	      incoherent_ok = dopt Opt_AllowIncoherentInstances  dflags
	      overlap_flag | incoherent_ok = Incoherent
			   | overlap_ok    = OverlapOk
			   | otherwise     = NoOverlap
			   
	; return overlap_flag }

traceDFuns ispecs
  = traceTc (hang (text "Adding instances:") 2 (vcat (map pp ispecs)))
  where
    pp ispec = ppr (instanceDFunId ispec) <+> colon <+> ppr ispec
	-- Print the dfun name itself too

funDepErr ispec ispecs
  = addDictLoc ispec $
    addErr (hang (ptext SLIT("Functional dependencies conflict between instance declarations:"))
	       2 (pprInstances (ispec:ispecs)))
dupInstErr ispec dup_ispec
  = addDictLoc ispec $
    addErr (hang (ptext SLIT("Duplicate instance declarations:"))
	       2 (pprInstances [ispec, dup_ispec]))

addDictLoc ispec thing_inside
  = setSrcSpan (mkSrcSpan loc loc) thing_inside
  where
   loc = getSrcLoc ispec
\end{code}
    

%************************************************************************
%*									*
\subsection{Looking up Insts}
%*									*
%************************************************************************

\begin{code}
data LookupInstResult
  = NoInstance
  | SimpleInst (LHsExpr TcId)		-- Just a variable, type application, or literal
  | GenInst    [Inst] (LHsExpr TcId)	-- The expression and its needed insts

lookupInst :: Inst -> TcM LookupInstResult
-- It's important that lookupInst does not put any new stuff into
-- the LIE.  Instead, any Insts needed by the lookup are returned in
-- the LookupInstResult, where they can be further processed by tcSimplify


-- Methods

lookupInst inst@(Method _ id tys theta loc)
  = do	{ (dicts, dict_app) <- instCallDicts loc theta
	; let co_fn = dict_app <.> mkCoTyApps tys
	; return (GenInst dicts (L span $ HsCoerce co_fn (HsVar id))) }
  where
    span = instLocSrcSpan loc

-- Literals

-- Look for short cuts first: if the literal is *definitely* a 
-- int, integer, float or a double, generate the real thing here.
-- This is essential (see nofib/spectral/nucleic).
-- [Same shortcut as in newOverloadedLit, but we
--  may have done some unification by now] 		

lookupInst inst@(LitInst _nm (HsIntegral i from_integer_name) ty loc)
  | Just expr <- shortCutIntLit i ty
  = returnM (GenInst [] (noLoc expr))	-- GenInst, not SimpleInst, because 
					-- expr may be a constructor application
  | otherwise
  = ASSERT( from_integer_name `isHsVar` fromIntegerName )	-- A LitInst invariant
    tcLookupId fromIntegerName			`thenM` \ from_integer ->
    tcInstClassOp loc from_integer [ty]		`thenM` \ method_inst ->
    mkIntegerLit i				`thenM` \ integer_lit ->
    returnM (GenInst [method_inst]
		     (mkHsApp (L (instLocSrcSpan loc)
			   	 (HsVar (instToId method_inst))) integer_lit))

lookupInst inst@(LitInst _nm (HsFractional f from_rat_name) ty loc)
  | Just expr <- shortCutFracLit f ty
  = returnM (GenInst [] (noLoc expr))

  | otherwise
  = ASSERT( from_rat_name `isHsVar` fromRationalName )	-- A LitInst invariant
    tcLookupId fromRationalName			`thenM` \ from_rational ->
    tcInstClassOp loc from_rational [ty]	`thenM` \ method_inst ->
    mkRatLit f					`thenM` \ rat_lit ->
    returnM (GenInst [method_inst] (mkHsApp (L (instLocSrcSpan loc) 
					       (HsVar (instToId method_inst))) rat_lit))

-- Dictionaries
lookupInst (Dict _ pred loc)
  = do 	{ mb_result <- lookupPred pred
	; case mb_result of {
	    Nothing -> return NoInstance ;
	    Just (tenv, dfun_id) -> do

    -- tenv is a substitution that instantiates the dfun_id 
    -- to match the requested result type.   
    -- 
    -- We ASSUME that the dfun is quantified over the very same tyvars 
    -- that are bound by the tenv.
    -- 
    -- However, the dfun
    -- might have some tyvars that *only* appear in arguments
    --	dfun :: forall a b. C a b, Ord b => D [a]
    -- We instantiate b to a flexi type variable -- it'll presumably
    -- become fixed later via functional dependencies
    { use_stage <- getStage
    ; checkWellStaged (ptext SLIT("instance for") <+> quotes (ppr pred))
    		      (topIdLvl dfun_id) use_stage

 	-- It's possible that not all the tyvars are in
	-- the substitution, tenv. For example:
	--	instance C X a => D X where ...
	-- (presumably there's a functional dependency in class C)
	-- Hence the open_tvs to instantiate any un-substituted tyvars.	
    ; let (tyvars, rho) = tcSplitForAllTys (idType dfun_id)
	  open_tvs      = filter (`notElemTvSubst` tenv) tyvars
    ; open_tvs' <- mappM tcInstTyVar open_tvs
    ; let
 	tenv' = extendTvSubstList tenv open_tvs (mkTyVarTys open_tvs')
		-- Since the open_tvs' are freshly made, they cannot possibly be captured by
		-- any nested for-alls in rho.  So the in-scope set is unchanged
    	dfun_rho   = substTy tenv' rho
    	(theta, _) = tcSplitPhiTy dfun_rho
	src_loc	   = instLocSrcSpan loc
	dfun	   = HsVar dfun_id
	tys	   = map (substTyVar tenv') tyvars
    ; if null theta then
    	returnM (SimpleInst (L src_loc $ HsCoerce (mkCoTyApps tys) dfun))
      else do
    { (dicts, dict_app) <- instCallDicts loc theta
    ; let co_fn = dict_app <.> mkCoTyApps tys
    ; returnM (GenInst dicts (L src_loc $ HsCoerce co_fn dfun))
    }}}}

---------------
lookupPred :: TcPredType -> TcM (Maybe (TvSubst, DFunId))
-- Look up a class constraint in the instance environment
lookupPred pred@(ClassP clas tys)
  = do	{ eps     <- getEps
	; tcg_env <- getGblEnv
	; let inst_envs = (eps_inst_env eps, tcg_inst_env tcg_env)
	; case lookupInstEnv inst_envs clas tys of {
	    ([(tenv, ispec)], []) 
		-> do	{ let dfun_id = is_dfun ispec
			; traceTc (text "lookupInst success" <+> 
				   vcat [text "dict" <+> ppr pred, 
				         text "witness" <+> ppr dfun_id
					 <+> ppr (idType dfun_id) ])
				-- Record that this dfun is needed
			; record_dfun_usage dfun_id
			; return (Just (tenv, dfun_id)) } ;

     	    (matches, unifs)
		-> do	{ traceTc (text "lookupInst fail" <+> 
				   vcat [text "dict" <+> ppr pred,
				   	 text "matches" <+> ppr matches,
				   	 text "unifs" <+> ppr unifs])
		-- In the case of overlap (multiple matches) we report
		-- NoInstance here.  That has the effect of making the 
		-- context-simplifier return the dict as an irreducible one.
		-- Then it'll be given to addNoInstanceErrs, which will do another
		-- lookupInstEnv to get the detailed info about what went wrong.
			; return Nothing }
	}}

lookupPred ip_pred = return Nothing

record_dfun_usage dfun_id 
  = do	{ hsc_env <- getTopEnv
	; let  dfun_name = idName dfun_id
	       dfun_mod  = nameModule dfun_name
	; if isInternalName dfun_name ||    -- Internal name => defined in this module
	     modulePackageId dfun_mod /= thisPackage (hsc_dflags hsc_env)
	  then return () -- internal, or in another package
	   else do { tcg_env <- getGblEnv
	  	   ; updMutVar (tcg_inst_uses tcg_env)
			       (`addOneToNameSet` idName dfun_id) }}


tcGetInstEnvs :: TcM (InstEnv, InstEnv)
-- Gets both the external-package inst-env
-- and the home-pkg inst env (includes module being compiled)
tcGetInstEnvs = do { eps <- getEps; env <- getGblEnv;
		     return (eps_inst_env eps, tcg_inst_env env) }
\end{code}



%************************************************************************
%*									*
		Re-mappable syntax
%*									*
%************************************************************************

Suppose we are doing the -fno-implicit-prelude thing, and we encounter
a do-expression.  We have to find (>>) in the current environment, which is
done by the rename. Then we have to check that it has the same type as
Control.Monad.(>>).  Or, more precisely, a compatible type. One 'customer' had
this:

  (>>) :: HB m n mn => m a -> n b -> mn b

So the idea is to generate a local binding for (>>), thus:

	let then72 :: forall a b. m a -> m b -> m b
	    then72 = ...something involving the user's (>>)...
	in
	...the do-expression...

Now the do-expression can proceed using then72, which has exactly
the expected type.

In fact tcSyntaxName just generates the RHS for then72, because we only
want an actual binding in the do-expression case. For literals, we can 
just use the expression inline.

\begin{code}
tcSyntaxName :: InstOrigin
	     -> TcType			-- Type to instantiate it at
	     -> (Name, HsExpr Name)	-- (Standard name, user name)
	     -> TcM (Name, HsExpr TcId)	-- (Standard name, suitable expression)
--	*** NOW USED ONLY FOR CmdTop (sigh) ***
-- NB: tcSyntaxName calls tcExpr, and hence can do unification.
-- So we do not call it from lookupInst, which is called from tcSimplify

tcSyntaxName orig ty (std_nm, HsVar user_nm)
  | std_nm == user_nm
  = newMethodFromName orig ty std_nm	`thenM` \ id ->
    returnM (std_nm, HsVar id)

tcSyntaxName orig ty (std_nm, user_nm_expr)
  = tcLookupId std_nm		`thenM` \ std_id ->
    let	
	-- C.f. newMethodAtLoc
	([tv], _, tau)  = tcSplitSigmaTy (idType std_id)
 	sigma1		= substTyWith [tv] [ty] tau
	-- Actually, the "tau-type" might be a sigma-type in the
	-- case of locally-polymorphic methods.
    in
    addErrCtxtM (syntaxNameCtxt user_nm_expr orig sigma1)	$

	-- Check that the user-supplied thing has the
	-- same type as the standard one.  
	-- Tiresome jiggling because tcCheckSigma takes a located expression
    getSrcSpanM					`thenM` \ span -> 
    tcPolyExpr (L span user_nm_expr) sigma1	`thenM` \ expr ->
    returnM (std_nm, unLoc expr)

syntaxNameCtxt name orig ty tidy_env
  = getInstLoc orig		`thenM` \ inst_loc ->
    let
	msg = vcat [ptext SLIT("When checking that") <+> quotes (ppr name) <+> 
				ptext SLIT("(needed by a syntactic construct)"),
		    nest 2 (ptext SLIT("has the required type:") <+> ppr (tidyType tidy_env ty)),
		    nest 2 (pprInstLoc inst_loc)]
    in
    returnM (tidy_env, msg)
\end{code}
