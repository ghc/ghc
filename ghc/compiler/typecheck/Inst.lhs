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

	newDicts, newDictAtLoc, newDictsAtLoc, cloneDict, 
	tcOverloadedLit, newIPDict, 
	newMethod, newMethodFromName, newMethodWithGivenTy, 
	tcInstClassOp, tcInstCall, tcInstStupidTheta,
	tcSyntaxName, 

	tyVarsOfInst, tyVarsOfInsts, tyVarsOfLIE, 
	ipNamesOfInst, ipNamesOfInsts, fdPredsOfInst, fdPredsOfInsts,
	instLoc, getDictClassTys, dictPred,

	lookupInst, LookupInstResult(..), lookupPred, 
	tcExtendLocalInstEnv, tcGetInstEnvs, getOverlapFlag,

	isDict, isClassDict, isMethod, 
	isLinearInst, linearInstType, isIPDict, isInheritableInst,
	isTyVarDict, isStdClassTyVarDict, isMethodFor, 
	instBindingRequired,

	zonkInst, zonkInsts,
	instToId, instName,

	InstOrigin(..), InstLoc(..), pprInstLoc
    ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcCheckSigma, tcSyntaxOp )
import {-# SOURCE #-}	TcUnify	( unifyTauTy )	-- Used in checkKind (sigh)

import HsSyn	( HsLit(..), HsOverLit(..), HsExpr(..), LHsExpr, mkHsApp,
		  nlHsLit, nlHsVar )
import TcHsSyn	( TcId, TcIdSet, 
		  mkHsTyApp, mkHsDictApp, zonkId, 
		  mkCoercion, ExprCoFn
		)
import TcRnMonad
import TcEnv	( tcLookupId, checkWellStaged, topIdLvl, tcMetaTy )
import InstEnv	( DFunId, InstEnv, Instance(..), OverlapFlag(..),
		  lookupInstEnv, extendInstEnv, pprInstances, 
		  instanceHead, instanceDFunId, setInstanceDFunId )
import FunDeps	( checkFunDeps )
import TcMType	( zonkTcType, zonkTcTypes, zonkTcPredType, zonkTcThetaType, 
		  tcInstTyVar, tcInstType, tcSkolType
		)
import TcType	( Type, TcType, TcThetaType, TcTyVarSet, TcTyVar, TcPredType,
		  PredType(..), SkolemInfo(..), typeKind, mkSigmaTy,
		  tcSplitForAllTys, mkFunTy,
		  tcSplitPhiTy, tcIsTyVarTy, tcSplitDFunHead,
		  isIntTy,isFloatTy, isIntegerTy, isDoubleTy,
		  mkPredTy, mkTyVarTy, mkTyVarTys,
		  tyVarsOfType, tyVarsOfTypes, tyVarsOfPred, tidyPred,
		  isClassPred, isTyVarClassPred, isLinearPred, 
		  getClassPredTys, getClassPredTys_maybe, mkPredName,
		  isInheritablePred, isIPPred, 
		  tidyType, tidyTypes, tidyFreeTyVars, tcSplitSigmaTy, 
		  pprPred, pprParendType, pprTheta 
		)
import Type	( TvSubst, substTy, substTyVar, substTyWith, substTheta, zipTopTvSubst,
		  notElemTvSubst, extendTvSubstList )
import Unify	( tcMatchTys )
import Kind	( isSubKind )
import Packages	( isHomeModule )
import HscTypes	( ExternalPackageState(..) )
import CoreFVs	( idFreeTyVars )
import DataCon	( DataCon, dataConTyVars, dataConStupidTheta, dataConName, dataConWrapId )
import Id	( Id, idName, idType, mkUserLocal, mkLocalId )
import PrelInfo	( isStandardClass, isNoDictClass )
import Name	( Name, mkMethodOcc, getOccName, getSrcLoc, nameModule,
		  isInternalName, setNameUnique, mkSystemVarNameEncoded )
import NameSet	( addOneToNameSet )
import Literal	( inIntRange )
import Var	( TyVar, tyVarKind, setIdType )
import VarEnv	( TidyEnv, emptyTidyEnv )
import VarSet	( elemVarSet, emptyVarSet, unionVarSet, mkVarSet )
import TysWiredIn ( floatDataCon, doubleDataCon )
import PrelNames	( integerTyConName, fromIntegerName, fromRationalName, rationalTyConName )
import BasicTypes( IPName(..), mapIPName, ipNameName )
import UniqSupply( uniqsFromSupply )
import SrcLoc	( mkSrcSpan, noLoc, unLoc, Located(..) )
import DynFlags	( DynFlag(..), dopt )
import Maybes	( isJust )
import Outputable
\end{code}


Selection
~~~~~~~~~
\begin{code}
instName :: Inst -> Name
instName inst = idName (instToId inst)

instToId :: Inst -> TcId
instToId (LitInst nm _ ty _)   = mkLocalId nm ty
instToId (Dict nm pred _)      = mkLocalId nm (mkPredTy pred)
instToId (Method id _ _ _ _ _) = id

instLoc (Dict _ _         loc) = loc
instLoc (Method _ _ _ _ _ loc) = loc
instLoc (LitInst _ _ _    loc) = loc

dictPred (Dict _ pred _ ) = pred
dictPred inst		  = pprPanic "dictPred" (ppr inst)

getDictClassTys (Dict _ pred _) = getClassPredTys pred

-- fdPredsOfInst is used to get predicates that contain functional 
-- dependencies *or* might do so.  The "might do" part is because
-- a constraint (C a b) might have a superclass with FDs
-- Leaving these in is really important for the call to fdPredsOfInsts
-- in TcSimplify.inferLoop, because the result is fed to 'grow',
-- which is supposed to be conservative
fdPredsOfInst (Dict _ pred _) 	       = [pred]
fdPredsOfInst (Method _ _ _ theta _ _) = theta
fdPredsOfInst other		       = []	-- LitInsts etc

fdPredsOfInsts :: [Inst] -> [PredType]
fdPredsOfInsts insts = concatMap fdPredsOfInst insts

isInheritableInst (Dict _ pred _) 	   = isInheritablePred pred
isInheritableInst (Method _ _ _ theta _ _) = all isInheritablePred theta
isInheritableInst other			   = True


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

isIPDict :: Inst -> Bool
isIPDict (Dict _ pred _) = isIPPred pred
isIPDict other		 = False

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
\end{code}


%************************************************************************
%*									*
\subsection{Building dictionaries}
%*									*
%************************************************************************

\begin{code}
newDicts :: InstOrigin
	 -> TcThetaType
	 -> TcM [Inst]
newDicts orig theta
  = getInstLoc orig		`thenM` \ loc ->
    newDictsAtLoc loc theta

cloneDict :: Inst -> TcM Inst
cloneDict (Dict nm ty loc) = newUnique	`thenM` \ uniq ->
			     returnM (Dict (setNameUnique nm uniq) ty loc)

newDictAtLoc :: InstLoc -> TcPredType -> TcM Inst
newDictAtLoc inst_loc pred
  = do	{ uniq <- newUnique
  	; return (mkDict inst_loc uniq pred) }

newDictsAtLoc :: InstLoc -> TcThetaType -> TcM [Inst]
newDictsAtLoc inst_loc theta
  = newUniqueSupply		`thenM` \ us ->
    returnM (zipWith (mkDict inst_loc) (uniqsFromSupply us) theta)

mkDict inst_loc uniq pred
  = Dict name pred inst_loc
  where
    name = mkPredName uniq (instLocSrcLoc inst_loc) pred 

-- For vanilla implicit parameters, there is only one in scope
-- at any time, so we used to use the name of the implicit parameter itself
-- But with splittable implicit parameters there may be many in 
-- scope, so we make up a new name.
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
tcInstCall :: InstOrigin -> TcType -> TcM (ExprCoFn, [TcTyVar], TcType)
tcInstCall orig fun_ty	-- fun_ty is usually a sigma-type
  = do	{ (tyvars, theta, tau) <- tcInstType fun_ty
	; dicts <- newDicts orig theta
	; extendLIEs dicts
	; let inst_fn e = unLoc (mkHsDictApp (mkHsTyApp (noLoc e) (mkTyVarTys tyvars)) 
				 	     (map instToId dicts))
	; return (mkCoercion inst_fn, tyvars, tau) }

tcInstStupidTheta :: DataCon -> [TcType] -> TcM ()
-- Instantiate the "stupid theta" of the data con, and throw 
-- the constraints into the constraint set
tcInstStupidTheta data_con inst_tys
  | null stupid_theta
  = return ()
  | otherwise
  = do	{ stupid_dicts <- newDicts (OccurrenceOf (dataConName data_con))
				   (substTheta tenv stupid_theta)
	; extendLIEs stupid_dicts }
  where
    stupid_theta = dataConStupidTheta data_con
    tenv = zipTopTvSubst (dataConTyVars data_con) inst_tys

newMethodFromName :: InstOrigin -> TcType -> Name -> TcM TcId
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

newMethodWithGivenTy orig id tys theta tau
  = getInstLoc orig			`thenM` \ loc ->
    newMethod loc id tys theta tau	`thenM` \ inst ->
    extendLIE inst			`thenM_`
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
	(tyvars,rho) = tcSplitForAllTys (idType sel_id)
	rho_ty	     = ASSERT( length tyvars == length tys )
		       substTyWith tyvars tys rho
	(preds,tau)  = tcSplitPhiTy rho_ty
    in
    zipWithM_ checkKind tyvars tys	`thenM_` 
    newMethod inst_loc sel_id tys preds tau

checkKind :: TyVar -> TcType -> TcM ()
-- Ensure that the type has a sub-kind of the tyvar
checkKind tv ty
  = do	{ ty1 <- zonkTcType ty
	; if typeKind ty1 `isSubKind` tyVarKind tv
	  then return ()
	  else do
	{ traceTc (text "checkKind: adding kind constraint" <+> ppr tv <+> ppr ty)
	; tv1 <- tcInstTyVar tv
	; unifyTauTy (mkTyVarTy tv1) ty1 }}


---------------------------
newMethod inst_loc id tys theta tau
  = newUnique		`thenM` \ new_uniq ->
    let
	meth_id	= mkUserLocal (mkMethodOcc (getOccName id)) new_uniq tau loc
	inst    = Method meth_id id tys theta tau inst_loc
	loc     = instLocSrcLoc inst_loc
    in
    returnM inst
\end{code}

In tcOverloadedLit we convert directly to an Int or Integer if we
know that's what we want.  This may save some time, by not
temporarily generating overloaded literals, but it won't catch all
cases (the rest are caught in lookupInst).

\begin{code}
tcOverloadedLit :: InstOrigin
		 -> HsOverLit Name
		 -> TcType
		 -> TcM (HsOverLit TcId)
tcOverloadedLit orig lit@(HsIntegral i fi) expected_ty
  | not (fi `isHsVar` fromIntegerName)	-- Do not generate a LitInst for rebindable syntax.  
	-- Reason: If we do, tcSimplify will call lookupInst, which
	--	   will call tcSyntaxName, which does unification, 
	--	   which tcSimplify doesn't like
	-- ToDo: noLoc sadness
  = do	{ integer_ty <- tcMetaTy integerTyConName
	; fi' <- tcSyntaxOp orig fi (mkFunTy integer_ty expected_ty)
	; return (HsIntegral i (HsApp (noLoc fi') (nlHsLit (HsInteger i integer_ty)))) }

  | Just expr <- shortCutIntLit i expected_ty 
  = return (HsIntegral i expr)

  | otherwise
  = do 	{ expr <- newLitInst orig lit expected_ty
	; return (HsIntegral i expr) }

tcOverloadedLit orig lit@(HsFractional r fr) expected_ty
  | not (fr `isHsVar` fromRationalName)	-- c.f. HsIntegral case
  = do	{ rat_ty <- tcMetaTy rationalTyConName
	; fr' <- tcSyntaxOp orig fr (mkFunTy rat_ty expected_ty)
	; return (HsFractional r (HsApp (noLoc fr') (nlHsLit (HsRat r rat_ty)))) }

  | Just expr <- shortCutFracLit r expected_ty 
  = return (HsFractional r expr)

  | otherwise
  = do 	{ expr <- newLitInst orig lit expected_ty
	; return (HsFractional r expr) }

newLitInst :: InstOrigin -> HsOverLit Name -> TcType -> TcM (HsExpr TcId)
newLitInst orig lit expected_ty	-- Make a LitInst
  = do 	{ loc <- getInstLoc orig
	; new_uniq <- newUnique
	; let
		lit_nm   = mkSystemVarNameEncoded new_uniq FSLIT("lit")
		-- The "encoded" bit means that we don't need to
		-- z-encode the string every time we call this!
		lit_inst = LitInst lit_nm lit expected_ty loc
	; extendLIE lit_inst
	; return (HsVar (instToId lit_inst)) }

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

zonkInst (Method m id tys theta tau loc) 
  = zonkId id			`thenM` \ new_id ->
	-- Essential to zonk the id in case it's a local variable
	-- Can't use zonkIdOcc because the id might itself be
	-- an InstId, in which case it won't be in scope

    zonkTcTypes tys		`thenM` \ new_tys ->
    zonkTcThetaType theta	`thenM` \ new_theta ->
    zonkTcType tau		`thenM` \ new_tau ->
    returnM (Method m new_id new_tys new_theta new_tau loc)

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

pprInst m@(Method inst_id id tys theta tau loc)
  = ppr inst_id <+> dcolon <+> 
	braces (sep [ppr id <+> ptext SLIT("at"),
		     brackets (sep (map pprParendType tys))])

pprInstInFull inst
  = sep [quotes (pprInst inst), nest 2 (pprInstLoc (instLoc inst))]

tidyInst :: TidyEnv -> Inst -> Inst
tidyInst env (LitInst nm lit ty loc) 	     = LitInst nm lit (tidyType env ty) loc
tidyInst env (Dict nm pred loc)     	     = Dict nm (tidyPred env pred) loc
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
		-- We use tcSkolType because we don't want to allocate fresh
		--  *meta* type variables.  
	  let dfun = instanceDFunId ispec
	; (tvs', theta', tau') <- tcSkolType (InstSkol dfun) (idType dfun)
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

lookupInst inst@(Method _ id tys theta _ loc)
  = newDictsAtLoc loc theta		`thenM` \ dicts ->
    returnM (GenInst dicts (mkHsDictApp (mkHsTyApp (L span (HsVar id)) tys) (map instToId dicts)))
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
    	ty_app     = mkHsTyApp (L (instLocSrcSpan loc) (HsVar dfun_id)) 
			       (map (substTyVar tenv') tyvars)
    ; if null theta then
    	returnM (SimpleInst ty_app)
      else do
    { dicts <- newDictsAtLoc loc theta
    ; let rhs = mkHsDictApp ty_app (map instToId dicts)
    ; returnM (GenInst dicts rhs)
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
  = do	{ dflags <- getDOpts
	; let  dfun_name = idName dfun_id
	       dfun_mod  = nameModule dfun_name
	; if isInternalName dfun_name ||    -- Internal name => defined in this module
	     not (isHomeModule dflags dfun_mod)
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
    tcCheckSigma (L span user_nm_expr) sigma1	`thenM` \ expr ->
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
