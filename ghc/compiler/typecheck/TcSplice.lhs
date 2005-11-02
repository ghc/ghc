%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcSplice]{Template Haskell splices}

\begin{code}
module TcSplice( tcSpliceExpr, tcSpliceDecls, tcBracket ) where

#include "HsVersions.h"

import HscMain		( compileExpr )
import TcRnDriver	( tcTopSrcDecls )
	-- These imports are the reason that TcSplice 
	-- is very high up the module hierarchy

import qualified Language.Haskell.TH as TH
-- THSyntax gives access to internal functions and data types
import qualified Language.Haskell.TH.Syntax as TH

import HsSyn		( HsBracket(..), HsExpr(..), HsSplice(..), LHsExpr, LHsDecl, 
		   	  HsType, LHsType )
import Convert		( convertToHsExpr, convertToHsDecls, convertToHsType, thRdrName )
import RnExpr		( rnLExpr )
import RnEnv		( lookupFixityRn, lookupSrcOcc_maybe, lookupImportedName )
import RdrName		( RdrName, lookupLocalRdrEnv, isSrcRdrName )
import RnTypes		( rnLHsType )
import TcExpr		( tcCheckRho, tcMonoExpr )
import TcHsSyn		( mkHsDictLet, zonkTopLExpr )
import TcSimplify	( tcSimplifyTop, tcSimplifyBracket )
import TcUnify		( Expected, zapExpectedTo, zapExpectedType )
import TcType		( TcType, TcKind, liftedTypeKind, mkAppTy, tcSplitSigmaTy )
import TcEnv		( spliceOK, tcMetaTy, bracketOK )
import TcMType		( newTyFlexiVarTy, newKindVar, UserTypeCtxt(ExprSigCtxt), zonkTcType )
import TcHsType		( tcHsSigType, kcHsType )
import TcIface		( tcImportDecl )
import TypeRep		( Type(..), PredType(..), TyThing(..) )	-- For reification
import PrelNames	( thFAKE )
import Name		( Name, NamedThing(..), nameOccName, nameModule, isExternalName, 
			  nameIsLocalOrFrom )
import NameEnv		( lookupNameEnv )
import HscTypes		( lookupType, ExternalPackageState(..), emptyModDetails )
import OccName
import Var		( Id, TyVar, idType )
import Module		( moduleUserString )
import TcRnMonad
import IfaceEnv		( lookupOrig )
import Class		( Class, classExtraBigSig )
import TyCon		( TyCon, tyConTyVars, getSynTyConDefn, 
			  isSynTyCon, isNewTyCon, tyConDataCons, isPrimTyCon, isFunTyCon,
			  tyConArity, tyConStupidTheta, isUnLiftedTyCon )
import DataCon		( DataCon, dataConTyCon, dataConOrigArgTys, dataConStrictMarks, 
			  dataConName, dataConFieldLabels, dataConWrapId, dataConIsInfix, 
			  isVanillaDataCon )
import Id		( idName, globalIdDetails )
import IdInfo		( GlobalIdDetails(..) )
import TysWiredIn	( mkListTy )
import DsMeta		( expQTyConName, typeQTyConName, decTyConName, qTyConName, nameTyConName )
import ErrUtils		( Message )
import SrcLoc		( noLoc, unLoc, getLoc )
import Outputable
import Unique		( Unique, Uniquable(..), getKey, mkUniqueGrimily )

import BasicTypes	( StrictnessMark(..), Fixity(..), FixityDirection(..) )
import Panic		( showException )
import FastString	( LitString )

import GHC.Base		( unsafeCoerce#, Int#, Int(..) )	-- Should have a better home in the module hierarchy
import Monad 		( liftM )

#ifdef GHCI
import FastString	( mkFastString )
#endif
\end{code}


%************************************************************************
%*									*
\subsection{Main interface + stubs for the non-GHCI case
%*									*
%************************************************************************

\begin{code}
tcSpliceDecls :: LHsExpr Name -> TcM [LHsDecl RdrName]
tcSpliceExpr  :: HsSplice Name -> Expected TcType -> TcM (HsExpr TcId)
kcSpliceType  :: HsSplice Name -> TcM (HsType Name, TcKind)

#ifndef GHCI
tcSpliceExpr n e ty = pprPanic "Cant do tcSpliceExpr without GHCi" (ppr e)
tcSpliceDecls e     = pprPanic "Cant do tcSpliceDecls without GHCi" (ppr e)
#else
\end{code}

%************************************************************************
%*									*
\subsection{Quoting an expression}
%*									*
%************************************************************************

\begin{code}
tcBracket :: HsBracket Name -> Expected TcType -> TcM (LHsExpr Id)
tcBracket brack res_ty
  = getStage 				`thenM` \ level ->
    case bracketOK level of {
	Nothing         -> failWithTc (illegalBracket level) ;
	Just next_level ->

   	-- Typecheck expr to make sure it is valid,
	-- but throw away the results.  We'll type check
	-- it again when we actually use it.
    recordThUse				`thenM_`
    newMutVar []	 		`thenM` \ pending_splices ->
    getLIEVar				`thenM` \ lie_var ->

    setStage (Brack next_level pending_splices lie_var) (
	getLIE (tc_bracket brack)
    )					`thenM` \ (meta_ty, lie) ->
    tcSimplifyBracket lie 		`thenM_`  

	-- Make the expected type have the right shape
    zapExpectedTo res_ty meta_ty	`thenM_`

	-- Return the original expression, not the type-decorated one
    readMutVar pending_splices		`thenM` \ pendings ->
    returnM (noLoc (HsBracketOut brack pendings))
    }

tc_bracket :: HsBracket Name -> TcM TcType
tc_bracket (VarBr v) 
  = tcMetaTy nameTyConName 	-- Result type is Var (not Q-monadic)

tc_bracket (ExpBr expr) 
  = newTyFlexiVarTy liftedTypeKind	`thenM` \ any_ty ->
    tcCheckRho expr any_ty		`thenM_`
    tcMetaTy expQTyConName
	-- Result type is Expr (= Q Exp)

tc_bracket (TypBr typ) 
  = tcHsSigType ExprSigCtxt typ		`thenM_`
    tcMetaTy typeQTyConName
	-- Result type is Type (= Q Typ)

tc_bracket (DecBr decls)
  = do	{  tcTopSrcDecls emptyModDetails decls
	-- Typecheck the declarations, dicarding the result
	-- We'll get all that stuff later, when we splice it in

	; decl_ty <- tcMetaTy decTyConName
	; q_ty    <- tcMetaTy qTyConName
	; return (mkAppTy q_ty (mkListTy decl_ty))
	-- Result type is Q [Dec]
    }
\end{code}


%************************************************************************
%*									*
\subsection{Splicing an expression}
%*									*
%************************************************************************

\begin{code}
tcSpliceExpr (HsSplice name expr) res_ty
  = setSrcSpan (getLoc expr) 	$
    getStage		`thenM` \ level ->
    case spliceOK level of {
	Nothing 	-> failWithTc (illegalSplice level) ;
	Just next_level -> 

    case level of {
	Comp 		       -> do { e <- tcTopSplice expr res_ty
				     ; returnM (unLoc e) } ;
	Brack _ ps_var lie_var ->  

	-- A splice inside brackets
  	-- NB: ignore res_ty, apart from zapping it to a mono-type
	-- e.g.   [| reverse $(h 4) |]
	-- Here (h 4) :: Q Exp
	-- but $(h 4) :: forall a.a 	i.e. anything!

    zapExpectedType res_ty liftedTypeKind	`thenM_`
    tcMetaTy expQTyConName			`thenM` \ meta_exp_ty ->
    setStage (Splice next_level) (
	setLIEVar lie_var	   $
	tcCheckRho expr meta_exp_ty
    )						`thenM` \ expr' ->

	-- Write the pending splice into the bucket
    readMutVar ps_var				`thenM` \ ps ->
    writeMutVar ps_var ((name,expr') : ps) 	`thenM_`

    returnM (panic "tcSpliceExpr")	-- The returned expression is ignored
    }} 

-- tcTopSplice used to have this:
-- Note that we do not decrement the level (to -1) before 
-- typechecking the expression.  For example:
--	f x = $( ...$(g 3) ... )
-- The recursive call to tcMonoExpr will simply expand the 
-- inner escape before dealing with the outer one

tcTopSplice :: LHsExpr Name -> Expected TcType -> TcM (LHsExpr Id)
tcTopSplice expr res_ty
  = tcMetaTy expQTyConName		`thenM` \ meta_exp_ty ->

	-- Typecheck the expression
    tcTopSpliceExpr expr meta_exp_ty	`thenM` \ zonked_q_expr ->

	-- Run the expression
    traceTc (text "About to run" <+> ppr zonked_q_expr) 	`thenM_`
    runMetaE zonked_q_expr		`thenM` \ simple_expr ->
  
    let 
	-- simple_expr :: TH.Exp

	expr2 :: LHsExpr RdrName
	expr2 = convertToHsExpr (getLoc expr) simple_expr 
    in
    traceTc (text "Got result" <+> ppr expr2) 	`thenM_`

    showSplice "expression" 
	       zonked_q_expr (ppr expr2)	`thenM_`

	-- Rename it, but bale out if there are errors
	-- otherwise the type checker just gives more spurious errors
    checkNoErrs (rnLExpr expr2)			`thenM` \ (exp3, fvs) ->

    tcMonoExpr exp3 res_ty


tcTopSpliceExpr :: LHsExpr Name -> TcType -> TcM (LHsExpr Id)
-- Type check an expression that is the body of a top-level splice
--   (the caller will compile and run it)
tcTopSpliceExpr expr meta_ty
  = checkNoErrs $	-- checkNoErrs: must not try to run the thing
			--	        if the type checker fails!

    setStage topSpliceStage $ do

	
    do	{ recordThUse	-- Record that TH is used (for pkg depdendency)

	-- Typecheck the expression
	; (expr', lie) <- getLIE (tcCheckRho expr meta_ty)
	
	-- Solve the constraints
	; const_binds <- tcSimplifyTop lie
	
	-- And zonk it
	; zonkTopLExpr (mkHsDictLet const_binds expr') }
\end{code}


%************************************************************************
%*									*
		Splicing a type
%*									*
%************************************************************************

Very like splicing an expression, but we don't yet share code.

\begin{code}
kcSpliceType (HsSplice name hs_expr)
  = setSrcSpan (getLoc hs_expr) $ do 	
	{ level <- getStage
	; case spliceOK level of {
		Nothing 	-> failWithTc (illegalSplice level) ;
		Just next_level -> do 

	{ case level of {
		Comp 		       -> do { (t,k) <- kcTopSpliceType hs_expr 
					     ; return (unLoc t, k) } ;
		Brack _ ps_var lie_var -> do

	{ 	-- A splice inside brackets
	; meta_ty <- tcMetaTy typeQTyConName
	; expr' <- setStage (Splice next_level) $
		   setLIEVar lie_var	   	$
		   tcCheckRho hs_expr meta_ty

		-- Write the pending splice into the bucket
	; ps <- readMutVar ps_var
	; writeMutVar ps_var ((name,expr') : ps)

	-- e.g.   [| Int -> $(h 4) |]
	-- Here (h 4) :: Q Type
	-- but $(h 4) :: forall a.a 	i.e. any kind
	; kind <- newKindVar
	; returnM (panic "kcSpliceType", kind)	-- The returned type is ignored
    }}}}}

kcTopSpliceType :: LHsExpr Name -> TcM (LHsType Name, TcKind)
kcTopSpliceType expr
  = do	{ meta_ty <- tcMetaTy typeQTyConName

	-- Typecheck the expression
	; zonked_q_expr <- tcTopSpliceExpr expr meta_ty

	-- Run the expression
	; traceTc (text "About to run" <+> ppr zonked_q_expr)
	; simple_ty <- runMetaT zonked_q_expr
  
	; let 	-- simple_ty :: TH.Type
		hs_ty2 :: LHsType RdrName
		hs_ty2 = convertToHsType (getLoc expr) simple_ty
	 
	; traceTc (text "Got result" <+> ppr hs_ty2)

	; showSplice "type" zonked_q_expr (ppr hs_ty2)

	-- Rename it, but bale out if there are errors
	-- otherwise the type checker just gives more spurious errors
	; let doc = ptext SLIT("In the spliced type") <+> ppr hs_ty2
	; hs_ty3 <- checkNoErrs (rnLHsType doc hs_ty2)

	; kcHsType hs_ty3 }
\end{code}

%************************************************************************
%*									*
\subsection{Splicing an expression}
%*									*
%************************************************************************

\begin{code}
-- Always at top level
-- Type sig at top of file:
-- 	tcSpliceDecls :: LHsExpr Name -> TcM [LHsDecl RdrName]
tcSpliceDecls expr
  = do	{ meta_dec_ty <- tcMetaTy decTyConName
	; meta_q_ty <- tcMetaTy qTyConName
	; let list_q = mkAppTy meta_q_ty (mkListTy meta_dec_ty)
	; zonked_q_expr <- tcTopSpliceExpr expr list_q

		-- Run the expression
	; traceTc (text "About to run" <+> ppr zonked_q_expr)
	; simple_expr <- runMetaD zonked_q_expr

	    -- simple_expr :: [TH.Dec]
	    -- decls :: [RdrNameHsDecl]
	; decls <- handleErrors (convertToHsDecls (getLoc expr) simple_expr)
	; traceTc (text "Got result" <+> vcat (map ppr decls))
	; showSplice "declarations"
	  	     zonked_q_expr 
		     (ppr (getLoc expr) $$ (vcat (map ppr decls)))
	; returnM decls }

  where handleErrors :: [Either a Message] -> TcM [a]
        handleErrors [] = return []
        handleErrors (Left x:xs) = liftM (x:) (handleErrors xs)
        handleErrors (Right m:xs) = do addErrTc m
                                       handleErrors xs
\end{code}


%************************************************************************
%*									*
\subsection{Running an expression}
%*									*
%************************************************************************

\begin{code}
runMetaE :: LHsExpr Id 	-- Of type (Q Exp)
	 -> TcM TH.Exp	-- Of type Exp
runMetaE e = runMeta e

runMetaT :: LHsExpr Id 		-- Of type (Q Type)
	 -> TcM TH.Type		-- Of type Type
runMetaT e = runMeta e

runMetaD :: LHsExpr Id 		-- Of type Q [Dec]
	 -> TcM [TH.Dec]	-- Of type [Dec]
runMetaD e = runMeta e

runMeta :: LHsExpr Id 		-- Of type X
	-> TcM t		-- Of type t
runMeta expr
  = do	{ hsc_env <- getTopEnv
	; tcg_env <- getGblEnv
	; this_mod <- getModule
	; let type_env = tcg_type_env tcg_env
	      rdr_env  = tcg_rdr_env tcg_env

	-- Compile and link it; might fail if linking fails
	; either_hval <- tryM $ ioToTcRn $
			 HscMain.compileExpr 
				      hsc_env this_mod 
			              rdr_env type_env expr
	; case either_hval of {
	    Left exn   -> failWithTc (mk_msg "compile and link" exn) ;
	    Right hval -> do

	{ 	-- Coerce it to Q t, and run it
		-- Running might fail if it throws an exception of any kind (hence tryAllM)
		-- including, say, a pattern-match exception in the code we are running
	  either_tval <- tryAllM (TH.runQ (unsafeCoerce# hval))

	; case either_tval of
	      Left exn -> failWithTc (mk_msg "run" exn)
	      Right v  -> returnM v
	}}}
  where
    mk_msg s exn = vcat [text "Exception when trying to" <+> text s <+> text "compile-time code:",
			 nest 2 (text (Panic.showException exn)),
			 nest 2 (text "Code:" <+> ppr expr)]
\end{code}

To call runQ in the Tc monad, we need to make TcM an instance of Quasi:

\begin{code}
instance TH.Quasi (IOEnv (Env TcGblEnv TcLclEnv)) where
  qNewName s = do { u <- newUnique 
		  ; let i = getKey u
		  ; return (TH.mkNameU s i) }

  qReport True msg  = addErr (text msg)
  qReport False msg = addReport (text msg)

  qCurrentModule = do { m <- getModule; return (moduleUserString m) }
  qReify v = reify v
  qRecover = recoverM

  qRunIO io = ioToTcRn io
\end{code}


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

\begin{code}
showSplice :: String -> LHsExpr Id -> SDoc -> TcM ()
showSplice what before after
  = getSrcSpanM		`thenM` \ loc ->
    traceSplice (vcat [ppr loc <> colon <+> text "Splicing" <+> text what, 
		       nest 2 (sep [nest 2 (ppr before),
				    text "======>",
				    nest 2 after])])

illegalBracket level
  = ptext SLIT("Illegal bracket at level") <+> ppr level

illegalSplice level
  = ptext SLIT("Illegal splice at level") <+> ppr level

#endif 	/* GHCI */
\end{code}


%************************************************************************
%*									*
			Reification
%*									*
%************************************************************************


\begin{code}
reify :: TH.Name -> TcM TH.Info
reify th_name
  = do	{ name <- lookupThName th_name
	; thing <- tcLookupTh name
		-- ToDo: this tcLookup could fail, which would give a
		-- 	 rather unhelpful error message
	; traceIf (text "reify" <+> text (show th_name) <+> brackets (ppr_ns th_name) <+> ppr name)
	; reifyThing thing
    }
  where
    ppr_ns (TH.Name _ (TH.NameG TH.DataName mod)) = text "data"
    ppr_ns (TH.Name _ (TH.NameG TH.TcClsName mod)) = text "tc"
    ppr_ns (TH.Name _ (TH.NameG TH.VarName mod)) = text "var"

lookupThName :: TH.Name -> TcM Name
lookupThName th_name
  =  do { let rdr_name = thRdrName guessed_ns th_name

	-- Repeat much of lookupOccRn, becase we want
	-- to report errors in a TH-relevant way
	; rdr_env <- getLocalRdrEnv
	; case lookupLocalRdrEnv rdr_env rdr_name of
	    Just name -> return name
	    Nothing | not (isSrcRdrName rdr_name)	-- Exact, Orig
		    -> lookupImportedName rdr_name
		    | otherwise				-- Unqual, Qual
		    -> do { 
				  mb_name <- lookupSrcOcc_maybe rdr_name
			  ; case mb_name of
			      Just name -> return name
			      Nothing   -> failWithTc (notInScope th_name) }
	}
  where
	-- guessed_ns is the name space guessed from looking at the TH name
    guessed_ns | isLexCon occ_fs = OccName.dataName
	       | otherwise	 = OccName.varName
    occ_fs = mkFastString (TH.nameBase th_name)

tcLookupTh :: Name -> TcM TcTyThing
-- This is a specialised version of TcEnv.tcLookup; specialised mainly in that
-- it gives a reify-related error message on failure, whereas in the normal
-- tcLookup, failure is a bug.
tcLookupTh name
  = do	{ (gbl_env, lcl_env) <- getEnvs
	; case lookupNameEnv (tcl_env lcl_env) name of {
		Just thing -> returnM thing;
		Nothing    -> do
	{ if nameIsLocalOrFrom (tcg_mod gbl_env) name
	  then	-- It's defined in this module
	      case lookupNameEnv (tcg_type_env gbl_env) name of
		Just thing -> return (AGlobal thing)
		Nothing	   -> failWithTc (notInEnv name)
	 
	  else do 		-- It's imported
	{ (eps,hpt) <- getEpsAndHpt
	; case lookupType hpt (eps_PTE eps) name of 
	    Just thing -> return (AGlobal thing)
	    Nothing    -> do { thing <- tcImportDecl name
			     ; return (AGlobal thing) }
		-- Imported names should always be findable; 
		-- if not, we fail hard in tcImportDecl
    }}}}

notInScope :: TH.Name -> SDoc
notInScope th_name = quotes (text (TH.pprint th_name)) <+> 
		     ptext SLIT("is not in scope at a reify")
	-- Ugh! Rather an indirect way to display the name

notInEnv :: Name -> SDoc
notInEnv name = quotes (ppr name) <+> 
		     ptext SLIT("is not in the type environment at a reify")

------------------------------
reifyThing :: TcTyThing -> TcM TH.Info
-- The only reason this is monadic is for error reporting,
-- which in turn is mainly for the case when TH can't express
-- some random GHC extension

reifyThing (AGlobal (AnId id))
  = do	{ ty <- reifyType (idType id)
	; fix <- reifyFixity (idName id)
	; let v = reifyName id
	; case globalIdDetails id of
	    ClassOpId cls    -> return (TH.ClassOpI v ty (reifyName cls) fix)
	    other	     -> return (TH.VarI     v ty Nothing fix)
    }

reifyThing (AGlobal (ATyCon tc))  = reifyTyCon tc
reifyThing (AGlobal (AClass cls)) = reifyClass cls
reifyThing (AGlobal (ADataCon dc))
  = do	{ let name = dataConName dc
	; ty <- reifyType (idType (dataConWrapId dc))
	; fix <- reifyFixity name
	; return (TH.DataConI (reifyName name) ty (reifyName (dataConTyCon dc)) fix) }

reifyThing (ATcId id _) 
  = do	{ ty1 <- zonkTcType (idType id)	-- Make use of all the info we have, even
					-- though it may be incomplete
	; ty2 <- reifyType ty1
	; fix <- reifyFixity (idName id)
	; return (TH.VarI (reifyName id) ty2 Nothing fix) }

reifyThing (ATyVar tv ty) 
  = do	{ ty1 <- zonkTcType ty
	; ty2 <- reifyType ty1
	; return (TH.TyVarI (reifyName tv) ty2) }

------------------------------
reifyTyCon :: TyCon -> TcM TH.Info
reifyTyCon tc
  | isFunTyCon tc  = return (TH.PrimTyConI (reifyName tc) 2 		  False)
  | isPrimTyCon tc = return (TH.PrimTyConI (reifyName tc) (tyConArity tc) (isUnLiftedTyCon tc))
  | isSynTyCon tc
  = do	{ let (tvs, rhs) = getSynTyConDefn tc
	; rhs' <- reifyType rhs
	; return (TH.TyConI $ TH.TySynD (reifyName tc) (reifyTyVars tvs) rhs') }

reifyTyCon tc
  = do 	{ cxt <- reifyCxt (tyConStupidTheta tc)
	; cons <- mapM reifyDataCon (tyConDataCons tc)
	; let name = reifyName tc
	      tvs  = reifyTyVars (tyConTyVars tc)
	      deriv = []	-- Don't know about deriving
	      decl | isNewTyCon tc = TH.NewtypeD cxt name tvs (head cons) deriv
		   | otherwise	   = TH.DataD    cxt name tvs cons 	  deriv
	; return (TH.TyConI decl) }

reifyDataCon :: DataCon -> TcM TH.Con
reifyDataCon dc
  | isVanillaDataCon dc
  = do 	{ arg_tys <- reifyTypes (dataConOrigArgTys dc)
	; let stricts = map reifyStrict (dataConStrictMarks dc)
	      fields  = dataConFieldLabels dc
	      name    = reifyName dc
	      [a1,a2] = arg_tys
	      [s1,s2] = stricts
	; ASSERT( length arg_tys == length stricts )
          if not (null fields) then
	     return (TH.RecC name (zip3 (map reifyName fields) stricts arg_tys))
	  else
	  if dataConIsInfix dc then
	     ASSERT( length arg_tys == 2 )
	     return (TH.InfixC (s1,a1) name (s2,a2))
	  else
	     return (TH.NormalC name (stricts `zip` arg_tys)) }
  | otherwise
  = failWithTc (ptext SLIT("Can't reify a non-Haskell-98 data constructor:") 
		<+> quotes (ppr dc))

------------------------------
reifyClass :: Class -> TcM TH.Info
reifyClass cls 
  = do	{ cxt <- reifyCxt theta
	; ops <- mapM reify_op op_stuff
	; return (TH.ClassI $ TH.ClassD cxt (reifyName cls) (reifyTyVars tvs) fds' ops) }
  where
    (tvs, fds, theta, _, op_stuff) = classExtraBigSig cls
    fds' = map reifyFunDep fds
    reify_op (op, _) = do { ty <- reifyType (idType op)
			  ; return (TH.SigD (reifyName op) ty) }

------------------------------
reifyType :: TypeRep.Type -> TcM TH.Type
reifyType (TyVarTy tv)	    = return (TH.VarT (reifyName tv))
reifyType (TyConApp tc tys) = reify_tc_app (reifyName tc) tys
reifyType (NoteTy _ ty)     = reifyType ty
reifyType (AppTy t1 t2)     = do { [r1,r2] <- reifyTypes [t1,t2] ; return (r1 `TH.AppT` r2) }
reifyType (FunTy t1 t2)     = do { [r1,r2] <- reifyTypes [t1,t2] ; return (TH.ArrowT `TH.AppT` r1 `TH.AppT` r2) }
reifyType ty@(ForAllTy _ _) = do { cxt' <- reifyCxt cxt; 
				 ; tau' <- reifyType tau 
				 ; return (TH.ForallT (reifyTyVars tvs) cxt' tau') }
			    where
				(tvs, cxt, tau) = tcSplitSigmaTy ty
reifyTypes = mapM reifyType
reifyCxt   = mapM reifyPred

reifyFunDep :: ([TyVar], [TyVar]) -> TH.FunDep
reifyFunDep (xs, ys) = TH.FunDep (map reifyName xs) (map reifyName ys)

reifyTyVars :: [TyVar] -> [TH.Name]
reifyTyVars = map reifyName

reify_tc_app :: TH.Name -> [TypeRep.Type] -> TcM TH.Type
reify_tc_app tc tys = do { tys' <- reifyTypes tys 
			 ; return (foldl TH.AppT (TH.ConT tc) tys') }

reifyPred :: TypeRep.PredType -> TcM TH.Type
reifyPred (ClassP cls tys) = reify_tc_app (reifyName cls) tys
reifyPred p@(IParam _ _)   = noTH SLIT("implicit parameters") (ppr p)


------------------------------
reifyName :: NamedThing n => n -> TH.Name
reifyName thing
  | isExternalName name = mk_varg mod occ_str
  | otherwise	        = TH.mkNameU occ_str (getKey (getUnique name))
	-- Many of the things we reify have local bindings, and 
	-- NameL's aren't supposed to appear in binding positions, so
	-- we use NameU.  When/if we start to reify nested things, that
	-- have free variables, we may need to generate NameL's for them.
  where
    name    = getName thing
    mod     = moduleUserString (nameModule name)
    occ_str = occNameUserString occ
    occ     = nameOccName name
    mk_varg | OccName.isDataOcc occ = TH.mkNameG_d
	    | OccName.isVarOcc  occ = TH.mkNameG_v
	    | OccName.isTcOcc   occ = TH.mkNameG_tc
	    | otherwise		    = pprPanic "reifyName" (ppr name)

------------------------------
reifyFixity :: Name -> TcM TH.Fixity
reifyFixity name
  = do	{ fix <- lookupFixityRn name
	; return (conv_fix fix) }
    where
      conv_fix (BasicTypes.Fixity i d) = TH.Fixity i (conv_dir d)
      conv_dir BasicTypes.InfixR = TH.InfixR
      conv_dir BasicTypes.InfixL = TH.InfixL
      conv_dir BasicTypes.InfixN = TH.InfixN

reifyStrict :: BasicTypes.StrictnessMark -> TH.Strict
reifyStrict MarkedStrict    = TH.IsStrict
reifyStrict MarkedUnboxed   = TH.IsStrict
reifyStrict NotMarkedStrict = TH.NotStrict

------------------------------
noTH :: LitString -> SDoc -> TcM a
noTH s d = failWithTc (hsep [ptext SLIT("Can't represent") <+> ptext s <+> 
				ptext SLIT("in Template Haskell:"),
		 	     nest 2 d])
\end{code}
