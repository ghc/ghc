%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Inst]{The @Inst@ type: dictionaries or method instances}

\begin{code}
module Inst ( 
	showLIE,

	Inst, 
	pprInst, pprInsts, pprInstsInFull, pprDFuns,
	tidyInsts, tidyMoreInsts,

	newDictsFromOld, newDicts, cloneDict, 
	newOverloadedLit, newIPDict, 
	newMethod, newMethodFromName, newMethodWithGivenTy, 
	tcInstClassOp, tcInstCall, tcInstDataCon, 
	tcSyntaxName, tcStdSyntaxName,

	tyVarsOfInst, tyVarsOfInsts, tyVarsOfLIE, 
	ipNamesOfInst, ipNamesOfInsts, fdPredsOfInst, fdPredsOfInsts,
	instLoc, getDictClassTys, dictPred,

	lookupInst, LookupInstResult(..),
	tcExtendLocalInstEnv, tcGetInstEnvs,

	isDict, isClassDict, isMethod, 
	isLinearInst, linearInstType, isIPDict, isInheritableInst,
	isTyVarDict, isStdClassTyVarDict, isMethodFor, 
	instBindingRequired,

	zonkInst, zonkInsts,
	instToId, instName,

	InstOrigin(..), InstLoc(..), pprInstLoc
    ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcCheckSigma )

import HsSyn	( HsLit(..), HsOverLit(..), HsExpr(..), LHsExpr, nlHsVar, mkHsApp )
import TcHsSyn	( TcId, TcIdSet, 
		  mkHsTyApp, mkHsDictApp, mkHsConApp, zonkId, 
		  mkCoercion, ExprCoFn
		)
import TcRnMonad
import TcEnv	( tcLookupId, checkWellStaged, topIdLvl, tcMetaTy )
import InstEnv	( DFunId, InstEnv, lookupInstEnv, checkFunDeps, extendInstEnv )
import TcIface	( loadImportedInsts )
import TcMType	( zonkTcType, zonkTcTypes, zonkTcPredType, 
		  zonkTcThetaType, tcInstTyVar, tcInstType, tcInstTyVars
		)
import TcType	( Type, TcType, TcThetaType, TcTyVarSet,
		  PredType(..), TyVarDetails(VanillaTv),
		  tcSplitForAllTys, tcSplitForAllTys, mkTyConApp,
		  tcSplitPhiTy, tcIsTyVarTy, tcSplitDFunTy,
		  isIntTy,isFloatTy, isIntegerTy, isDoubleTy,
		  tcIsTyVarTy, mkPredTy, mkTyVarTy, mkTyVarTys,
		  tyVarsOfType, tyVarsOfTypes, tyVarsOfPred, tidyPred,
		  isClassPred, isTyVarClassPred, isLinearPred, 
		  getClassPredTys, getClassPredTys_maybe, mkPredName,
		  isInheritablePred, isIPPred, matchTys,
		  tidyType, tidyTypes, tidyFreeTyVars, tcSplitSigmaTy, 
		  pprPred, pprParendType, pprThetaArrow, pprClassPred
		)
import HscTypes	( ExternalPackageState(..) )
import CoreFVs	( idFreeTyVars )
import DataCon	( DataCon,dataConSig )
import Id	( Id, idName, idType, mkUserLocal, mkSysLocal, mkLocalId, setIdUnique )
import PrelInfo	( isStandardClass, isNoDictClass )
import Name	( Name, mkMethodOcc, getOccName, getSrcLoc, isHomePackageName, isInternalName )
import NameSet	( addOneToNameSet )
import Subst	( substTy, substTyWith, substTheta, mkTyVarSubst )
import Literal	( inIntRange )
import Var	( TyVar )
import VarEnv	( TidyEnv, emptyTidyEnv, lookupSubstEnv, SubstResult(..) )
import VarSet	( elemVarSet, emptyVarSet, unionVarSet, mkVarSet )
import TysWiredIn ( floatDataCon, doubleDataCon )
import PrelNames	( integerTyConName, fromIntegerName, fromRationalName, rationalTyConName )
import BasicTypes( IPName(..), mapIPName, ipNameName )
import UniqSupply( uniqsFromSupply )
import SrcLoc	( mkSrcSpan, noLoc, unLoc, Located(..) )
import CmdLineOpts( DynFlags, DynFlag( Opt_AllowUndecidableInstances ), dopt )
import Maybes	( isJust )
import Outputable
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
cloneDict (Dict id ty loc) = newUnique	`thenM` \ uniq ->
			     returnM (Dict (setIdUnique id uniq) ty loc)

newDictsFromOld :: Inst -> TcThetaType -> TcM [Inst]
newDictsFromOld (Dict _ _ loc) theta = newDictsAtLoc loc theta

-- Local function, similar to newDicts, 
-- but with slightly different interface
newDictsAtLoc :: InstLoc
 	      -> TcThetaType
	      -> TcM [Inst]
newDictsAtLoc inst_loc theta
  = newUniqueSupply		`thenM` \ us ->
    returnM (zipWith mk_dict (uniqsFromSupply us) theta)
  where
    mk_dict uniq pred = Dict (mkLocalId (mkPredName uniq loc pred) (mkPredTy pred))
			     pred inst_loc
    loc = instLocSrcLoc inst_loc

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
	id   = mkLocalId name (mkPredTy pred)
    in
    returnM (mapIPName (\n -> id) ip_name, Dict id pred inst_loc)
\end{code}



%************************************************************************
%*									*
\subsection{Building methods (calls of overloaded functions)}
%*									*
%************************************************************************


\begin{code}
tcInstCall :: InstOrigin  -> TcType -> TcM (ExprCoFn, TcType)
tcInstCall orig fun_ty	-- fun_ty is usually a sigma-type
  = tcInstType VanillaTv fun_ty	`thenM` \ (tyvars, theta, tau) ->
    newDicts orig theta		`thenM` \ dicts ->
    extendLIEs dicts		`thenM_`
    let
	inst_fn e = DictApp (mkHsTyApp (noLoc e) (mkTyVarTys tyvars)) (map instToId dicts)
    in
    returnM (mkCoercion inst_fn, tau)

tcInstDataCon :: InstOrigin -> DataCon
	      -> TcM ([TcType],	-- Types to instantiate at
		      [Inst],	-- Existential dictionaries to apply to
		      [TcType],	-- Argument types of constructor
		      TcType,	-- Result type
		      [TyVar])	-- Existential tyvars
tcInstDataCon orig data_con
  = let 
	(tvs, stupid_theta, ex_tvs, ex_theta, arg_tys, tycon) = dataConSig data_con
	     -- We generate constraints for the stupid theta even when 
	     -- pattern matching (as the Report requires)
    in
    tcInstTyVars VanillaTv (tvs ++ ex_tvs)	`thenM` \ (all_tvs', ty_args', tenv) ->
    let
	stupid_theta' = substTheta tenv stupid_theta
	ex_theta'     = substTheta tenv ex_theta
	arg_tys'      = map (substTy tenv) arg_tys

	n_normal_tvs  = length tvs
	ex_tvs'       = drop n_normal_tvs all_tvs'
	result_ty     = mkTyConApp tycon (take n_normal_tvs ty_args')
    in
    newDicts orig stupid_theta'	`thenM` \ stupid_dicts ->
    newDicts orig ex_theta'	`thenM` \ ex_dicts ->

	-- Note that we return the stupid theta *only* in the LIE;
	-- we don't otherwise use it at all
    extendLIEs stupid_dicts	`thenM_`

    returnM (ty_args', ex_dicts, arg_tys', result_ty, ex_tvs')

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

tcInstClassOp :: InstLoc -> Id -> [TcType] -> TcM Inst
tcInstClassOp inst_loc sel_id tys
  = let
	(tyvars,rho) = tcSplitForAllTys (idType sel_id)
	rho_ty	     = ASSERT( length tyvars == length tys )
		       substTyWith tyvars tys rho
	(preds,tau)  = tcSplitPhiTy rho_ty
    in
    newMethod inst_loc sel_id tys preds tau

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

In newOverloadedLit we convert directly to an Int or Integer if we
know that's what we want.  This may save some time, by not
temporarily generating overloaded literals, but it won't catch all
cases (the rest are caught in lookupInst).

\begin{code}
newOverloadedLit :: InstOrigin
		 -> HsOverLit
		 -> TcType
		 -> TcM (LHsExpr TcId)
newOverloadedLit orig lit@(HsIntegral i fi) expected_ty
  | fi /= fromIntegerName	-- Do not generate a LitInst for rebindable syntax.  
				-- Reason: tcSyntaxName does unification
				-- which is very inconvenient in tcSimplify
				-- ToDo: noLoc sadness
  = tcSyntaxName orig expected_ty (fromIntegerName, noLoc (HsVar fi))	`thenM` \ (_,expr) ->
    mkIntegerLit i							`thenM` \ integer_lit ->
    returnM (mkHsApp expr integer_lit)

  | Just expr <- shortCutIntLit i expected_ty 
  = returnM expr

  | otherwise
  = newLitInst orig lit expected_ty

newOverloadedLit orig lit@(HsFractional r fr) expected_ty
  | fr /= fromRationalName	-- c.f. HsIntegral case
  = tcSyntaxName orig expected_ty (fromRationalName, noLoc (HsVar fr))	`thenM` \ (_,expr) ->
    mkRatLit r								`thenM` \ rat_lit ->
    returnM (mkHsApp expr rat_lit)

  | Just expr <- shortCutFracLit r expected_ty 
  = returnM expr

  | otherwise
  = newLitInst orig lit expected_ty

newLitInst :: InstOrigin -> HsOverLit -> TcType -> TcM (LHsExpr TcId)
newLitInst orig lit expected_ty
  = getInstLoc orig		`thenM` \ loc ->
    newUnique			`thenM` \ new_uniq ->
    let
	lit_inst = LitInst lit_id lit expected_ty loc
	lit_id   = mkSysLocal FSLIT("lit") new_uniq expected_ty
    in
    extendLIE lit_inst		`thenM_`
    returnM (L (instLocSrcSpan loc) (HsVar (instToId lit_inst)))

shortCutIntLit :: Integer -> TcType -> Maybe (LHsExpr TcId)	-- Returns noLoc'd result :-)
shortCutIntLit i ty
  | isIntTy ty && inIntRange i 		-- Short cut for Int
  = Just (noLoc (HsLit (HsInt i)))
  | isIntegerTy ty 			-- Short cut for Integer
  = Just (noLoc (HsLit (HsInteger i ty)))
  | otherwise = Nothing

shortCutFracLit :: Rational -> TcType -> Maybe (LHsExpr TcId)	-- Returns noLoc'd result :-)
shortCutFracLit f ty
  | isFloatTy ty 
  = Just (mkHsConApp floatDataCon [] [HsLit (HsFloatPrim f)])
  | isDoubleTy ty
  = Just (mkHsConApp doubleDataCon [] [HsLit (HsDoublePrim f)])
  | otherwise = Nothing

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
zonkInst :: Inst -> TcM Inst
zonkInst (Dict id pred loc)
  = zonkTcPredType pred			`thenM` \ new_pred ->
    returnM (Dict id new_pred loc)

zonkInst (Method m id tys theta tau loc) 
  = zonkId id			`thenM` \ new_id ->
	-- Essential to zonk the id in case it's a local variable
	-- Can't use zonkIdOcc because the id might itself be
	-- an InstId, in which case it won't be in scope

    zonkTcTypes tys		`thenM` \ new_tys ->
    zonkTcThetaType theta	`thenM` \ new_theta ->
    zonkTcType tau		`thenM` \ new_tau ->
    returnM (Method m new_id new_tys new_theta new_tau loc)

zonkInst (LitInst id lit ty loc)
  = zonkTcType ty			`thenM` \ new_ty ->
    returnM (LitInst id lit new_ty loc)

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

pprInsts :: [Inst] -> SDoc
pprInsts insts  = parens (sep (punctuate comma (map pprInst insts)))

pprInstsInFull insts
  = vcat (map go insts)
  where
    go inst = sep [quotes (ppr inst), nest 2 (pprInstLoc (instLoc inst))]

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


pprDFuns :: [DFunId] -> SDoc
-- Prints the dfun as an instance declaration
pprDFuns dfuns = vcat [ hang (ppr (getSrcLoc dfun) <> colon)
			2 (ptext SLIT("instance") <+> sep [pprThetaArrow theta,
							   pprClassPred clas tys])
		      | dfun <- dfuns
		      , let (_, theta, clas, tys) = tcSplitDFunTy (idType dfun) ]
	-- Print without the for-all, which the programmer doesn't write

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

showLIE :: SDoc -> TcM ()	-- Debugging
showLIE str
  = do { lie_var <- getLIEVar ;
	 lie <- readMutVar lie_var ;
	 traceTc (str <+> pprInstsInFull (lieToList lie)) }
\end{code}


%************************************************************************
%*									*
	Extending the instance environment
%*									*
%************************************************************************

\begin{code}
tcExtendLocalInstEnv :: [DFunId] -> TcM a -> TcM a
  -- Add new locally-defined instances
tcExtendLocalInstEnv dfuns thing_inside
 = do { traceDFuns dfuns
      ; env <- getGblEnv
      ; dflags  <- getDOpts
      ; inst_env' <- foldlM (addInst dflags) (tcg_inst_env env) dfuns
      ; let env' = env { tcg_insts = dfuns ++ tcg_insts env,
			 tcg_inst_env = inst_env' }
      ; setGblEnv env' thing_inside }

addInst :: DynFlags -> InstEnv -> DFunId -> TcM InstEnv
-- Check that the proposed new instance is OK, 
-- and then add it to the home inst env
addInst dflags home_ie dfun
  = do	{ 	-- Load imported instances, so that we report
		-- duplicates correctly
	  pkg_ie  <- loadImportedInsts cls tys

		-- Check functional dependencies
	; case checkFunDeps (pkg_ie, home_ie) dfun of
		Just dfuns -> funDepErr dfun dfuns
		Nothing    -> return ()

		-- Check for duplicate instance decls
	; let { (matches, _) = lookupInstEnv dflags (pkg_ie, home_ie) cls tys
	      ;	dup_dfuns = [dup_dfun | (_, (_, dup_tys, dup_dfun)) <- matches,
			  		isJust (matchTys (mkVarSet tvs) tys dup_tys)] }
		-- Find memebers of the match list which 
		-- dfun itself matches. If the match is 2-way, it's a duplicate
	; case dup_dfuns of
	    dup_dfun : _ -> dupInstErr dfun dup_dfun
	    []           -> return ()

		-- OK, now extend the envt
	; return (extendInstEnv home_ie dfun) }
  where
    (tvs, _, cls, tys) = tcSplitDFunTy (idType dfun)

traceDFuns dfuns
  = traceTc (text "Adding instances:" <+> vcat (map pp dfuns))
  where
    pp dfun = ppr dfun <+> dcolon <+> ppr (idType dfun)

funDepErr dfun dfuns
  = addDictLoc dfun $
    addErr (hang (ptext SLIT("Functional dependencies conflict between instance declarations:"))
	       2 (pprDFuns (dfun:dfuns)))
dupInstErr dfun dup_dfun
  = addDictLoc dfun $
    addErr (hang (ptext SLIT("Duplicate instance declarations:"))
	       2 (pprDFuns [dfun, dup_dfun]))

addDictLoc dfun thing_inside
  = addSrcSpan (mkSrcSpan loc loc) thing_inside
  where
   loc = getSrcLoc dfun
\end{code}

%************************************************************************
%*									*
\subsection{Looking up Insts}
%*									*
%************************************************************************

\begin{code}
data LookupInstResult s
  = NoInstance
  | SimpleInst (LHsExpr TcId)		-- Just a variable, type application, or literal
  | GenInst    [Inst] (LHsExpr TcId)	-- The expression and its needed insts

lookupInst :: Inst -> TcM (LookupInstResult s)
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
-- This is essential  (see nofib/spectral/nucleic).
-- [Same shortcut as in newOverloadedLit, but we
--  may have done some unification by now] 		


lookupInst inst@(LitInst u (HsIntegral i from_integer_name) ty loc)
  | Just expr <- shortCutIntLit i ty
  = returnM (GenInst [] expr)	-- GenInst, not SimpleInst, because 
					-- expr may be a constructor application
  | otherwise
  = ASSERT( from_integer_name == fromIntegerName )	-- A LitInst invariant
    tcLookupId fromIntegerName			`thenM` \ from_integer ->
    tcInstClassOp loc from_integer [ty]		`thenM` \ method_inst ->
    mkIntegerLit i				`thenM` \ integer_lit ->
    returnM (GenInst [method_inst]
		     (mkHsApp (L (instLocSrcSpan loc)
			   	 (HsVar (instToId method_inst))) integer_lit))

lookupInst inst@(LitInst u (HsFractional f from_rat_name) ty loc)
  | Just expr <- shortCutFracLit f ty
  = returnM (GenInst [] expr)

  | otherwise
  = ASSERT( from_rat_name == fromRationalName )	-- A LitInst invariant
    tcLookupId fromRationalName			`thenM` \ from_rational ->
    tcInstClassOp loc from_rational [ty]	`thenM` \ method_inst ->
    mkRatLit f					`thenM` \ rat_lit ->
    returnM (GenInst [method_inst] (mkHsApp (L (instLocSrcSpan loc) 
					       (HsVar (instToId method_inst))) rat_lit))

-- Dictionaries
lookupInst dict@(Dict _ pred@(ClassP clas tys) loc)
  = do	{ dflags  <- getDOpts
	; if all tcIsTyVarTy tys && 
	     not (dopt Opt_AllowUndecidableInstances dflags)
		-- Common special case; no lookup
		-- NB: tcIsTyVarTy... don't look through newtypes!
		-- Don't take this short cut if we allow undecidable instances
		-- because we might have "instance T a where ...".
		-- [That means we need -fallow-undecidable-instances in the 
		--  client module, as well as the module with the instance decl.]
	  then return NoInstance

	  else do
	{ pkg_ie  <- loadImportedInsts clas tys
		-- Suck in any instance decls that may be relevant
	; tcg_env <- getGblEnv
	; case lookupInstEnv dflags (pkg_ie, tcg_inst_env tcg_env) clas tys of {
	    ([(tenv, (_,_,dfun_id))], []) -> instantiate_dfun tenv dfun_id pred loc ;
	    (matches, unifs)		  -> do
	{ traceTc (text "lookupInst" <+> vcat [text "matches" <+> ppr matches,
					       text "unifs" <+> ppr unifs])
	; return NoInstance } } } }
		-- In the case of overlap (multiple matches) we report
		-- NoInstance here.  That has the effect of making the 
		-- context-simplifier return the dict as an irreducible one.
		-- Then it'll be given to addNoInstanceErrs, which will do another
		-- lookupInstEnv to get the detailed info about what went wrong.

lookupInst (Dict _ _ _) = returnM NoInstance

-----------------
instantiate_dfun tenv dfun_id pred loc
  = 	-- Record that this dfun is needed
    record_dfun_usage dfun_id		`thenM_`

 	-- It's possible that not all the tyvars are in
	-- the substitution, tenv. For example:
	--	instance C X a => D X where ...
	-- (presumably there's a functional dependency in class C)
	-- Hence the mk_ty_arg to instantiate any un-substituted tyvars.	
    getStage						`thenM` \ use_stage ->
    checkWellStaged (ptext SLIT("instance for") <+> quotes (ppr pred))
    		    (topIdLvl dfun_id) use_stage		`thenM_`
    let
    	(tyvars, rho) = tcSplitForAllTys (idType dfun_id)
    	mk_ty_arg tv  = case lookupSubstEnv tenv tv of
    			   Just (DoneTy ty) -> returnM ty
    			   Nothing 	    -> tcInstTyVar VanillaTv tv `thenM` \ tc_tv ->
    					       returnM (mkTyVarTy tc_tv)
    in
    mappM mk_ty_arg tyvars	`thenM` \ ty_args ->
    let
    	dfun_rho   = substTy (mkTyVarSubst tyvars ty_args) rho
    	(theta, _) = tcSplitPhiTy dfun_rho
    	ty_app     = mkHsTyApp (L (instLocSrcSpan loc) (HsVar dfun_id)) ty_args
    in
    if null theta then
    	returnM (SimpleInst ty_app)
    else
    newDictsAtLoc loc theta	`thenM` \ dicts ->
    let 
    	rhs = mkHsDictApp ty_app (map instToId dicts)
    in
    returnM (GenInst dicts rhs)

record_dfun_usage dfun_id
  | isInternalName dfun_name = return ()		-- From this module
  | not (isHomePackageName dfun_name) = return ()	-- From another package package
  | otherwise = getGblEnv 	`thenM` \ tcg_env ->
		updMutVar (tcg_inst_uses tcg_env)
			  (`addOneToNameSet` idName dfun_id)
  where
    dfun_name = idName dfun_id

tcGetInstEnvs :: TcM (InstEnv, InstEnv)
-- Gets both the home-pkg inst env (includes module being compiled)
-- and the external-package inst-env
tcGetInstEnvs = do { eps <- getEps; env <- getGblEnv;
		     return (tcg_inst_env env, eps_inst_env eps) }
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
	     -> (Name, LHsExpr Name)	-- (Standard name, user name)
	     -> TcM (Name, LHsExpr TcId)	-- (Standard name, suitable expression)

-- NB: tcSyntaxName calls tcExpr, and hence can do unification.
-- So we do not call it from lookupInst, which is called from tcSimplify

tcSyntaxName orig ty (std_nm, L span (HsVar user_nm))
  | std_nm == user_nm
  = addSrcSpan span (tcStdSyntaxName orig ty std_nm)

tcSyntaxName orig ty (std_nm, user_nm_expr)
  = tcLookupId std_nm		`thenM` \ std_id ->
    let	
	-- C.f. newMethodAtLoc
	([tv], _, tau)  = tcSplitSigmaTy (idType std_id)
 	tau1		= substTyWith [tv] [ty] tau
	-- Actually, the "tau-type" might be a sigma-type in the
	-- case of locally-polymorphic methods.
    in
    addErrCtxtM (syntaxNameCtxt user_nm_expr orig tau1)	$

	-- Check that the user-supplied thing has the
	-- same type as the standard one
    tcCheckSigma user_nm_expr tau1		`thenM` \ expr ->
    returnM (std_nm, expr)

tcStdSyntaxName :: InstOrigin
	        -> TcType			-- Type to instantiate it at
	        -> Name				-- Standard name
	        -> TcM (Name, LHsExpr TcId)	-- (Standard name, suitable expression)

tcStdSyntaxName orig ty std_nm
  = newMethodFromName orig ty std_nm	`thenM` \ id ->
    getSrcSpanM				`thenM` \ span -> 
    returnM (std_nm, L span (HsVar id))

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
