%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcModule]{Typechecking a whole module}

\begin{code}
module TcRnDriver (
#ifdef GHCI
	mkGlobalContext, getModuleContents, tcRnStmt, tcRnThing, tcRnExpr,
#endif
	tcRnModule, checkOldIface, 
	importSupportingDecls, tcTopSrcDecls,
	tcRnIface, tcRnExtCore
    ) where

#include "HsVersions.h"

#ifdef GHCI
import {-# SOURCE #-} TcSplice ( tcSpliceDecls )
import		      DsMeta   ( templateHaskellNames )
#endif

import CmdLineOpts	( DynFlag(..), opt_PprStyle_Debug, dopt )
import DriverState	( v_MainModIs, v_MainFunIs )
import HsSyn		( HsModule(..), HsBinds(..), MonoBinds(..), HsExpr(..),
			  Stmt(..), Pat(VarPat), HsStmtContext(..), RuleDecl(..),
			  HsGroup(..), SpliceDecl(..),
			  mkSimpleMatch, placeHolderType, toHsType, andMonoBinds,
			  isSrcRule, collectStmtsBinders
			)
import RdrHsSyn		( RdrNameHsModule, RdrNameHsDecl, RdrNameStmt, RdrNameHsExpr,
			  emptyGroup, mkGroup, findSplice, addImpDecls, main_RDR_Unqual )

import PrelNames	( iNTERACTIVE, ioTyConName, printName, monadNames,
			  returnIOName, runIOName, 
			  rootMainName, itName, mAIN_Name
			)
import RdrName		( RdrName, getRdrName, mkRdrUnqual, 
			  lookupRdrEnv, elemRdrEnv )

import RnHsSyn		( RenamedStmt, RenamedTyClDecl, 
			  ruleDeclFVs, instDeclFVs, tyClDeclFVs )
import TcHsSyn		( TypecheckedHsExpr, TypecheckedRuleDecl,
			  zonkTopDecls, mkHsLet,
			  zonkTopExpr, zonkTopBndrs
			)

import TcExpr 		( tcInferRho, tcCheckRho )
import TcRnMonad
import TcType		( Type, 
			  tyVarsOfType, tcFunResultTy, tidyTopType,
			  mkForAllTys, mkFunTys, mkTyConApp, tcSplitForAllTys
			)
import Inst		( showLIE, tcStdSyntaxName )
import MkId		( unsafeCoerceId )
import TcBinds		( tcTopBinds )
import TcClassDcl	( tcClassDecls2 )
import TcDefaults	( tcDefaults )
import TcEnv		( tcExtendGlobalValEnv, 
			  tcExtendInstEnv, tcExtendRules,
			  tcLookupTyCon, tcLookupGlobal,
			  tcLookupId 
			)
import TcRules		( tcRules )
import TcForeign	( tcForeignImports, tcForeignExports )
import TcIfaceSig	( tcInterfaceSigs, tcCoreBinds )
import TcInstDcls	( tcInstDecls1, tcIfaceInstDecls, tcInstDecls2 )
import TcSimplify	( tcSimplifyTop, tcSimplifyInteractive, tcSimplifyInfer )
import TcTyClsDecls	( tcTyAndClassDecls )

import RnNames		( importsFromLocalDecls, rnImports, exportsFromAvail, 
			  reportUnusedNames )
import RnIfaces		( slurpImpDecls, checkVersions, RecompileRequired, outOfDate )
import RnHiFiles	( readIface, loadOldIface )
import RnEnv		( lookupSrcName, lookupOccRn, plusGlobalRdrEnv,
			  ubiquitousNames, implicitModuleFVs, implicitStmtFVs, dataTcOccs )
import RnSource		( rnSrcDecls, checkModDeprec, rnStats )

import CoreUnfold	( unfoldingTemplate )
import CoreSyn		( IdCoreRule, Bind(..) )
import PprCore		( pprIdRules, pprCoreBindings )
import ErrUtils		( mkDumpDoc, showPass, pprBagOfErrors )
import Id		( Id, mkLocalId, isLocalId, idName, idType, idUnfolding, setIdLocalExported )
import Var		( Var, setGlobalIdDetails )
import Module           ( Module, mkHomeModule, mkModuleName, moduleName, moduleUserString, moduleEnvElts )
import OccName		( mkVarOcc )
import Name		( Name, isExternalName, getSrcLoc, nameOccName )
import NameSet
import TyCon		( tyConGenInfo )
import BasicTypes       ( EP(..), RecFlag(..) )
import Outputable
import HscTypes		( PersistentCompilerState(..), InteractiveContext(..),
			  ModIface, ModDetails(..), ModGuts(..),
			  HscEnv(..), 
			  ModIface(..), ModDetails(..), IfaceDecls(..),
			  GhciMode(..), noDependencies,
			  Deprecations(..), plusDeprecs,
			  emptyGlobalRdrEnv,
			  GenAvailInfo(Avail), availsToNameSet, 
			  ForeignStubs(..),
			  TypeEnv, TyThing, typeEnvTyCons, 
			  extendTypeEnvWithIds, typeEnvIds, typeEnvTyCons,
			  extendLocalRdrEnv, emptyFixityEnv
			)
#ifdef GHCI
import TcMType		( zonkTcType )
import TcMatches	( tcStmtsAndThen, TcStmtCtxt(..) )
import RdrName		( rdrEnvElts )
import RnExpr		( rnStmts, rnExpr )
import RnHiFiles	( loadInterface )
import RnEnv		( mkGlobalRdrEnv )
import TysWiredIn	( mkListTy, unitTy )
import IdInfo		( GlobalIdDetails(..) )
import SrcLoc		( noSrcLoc )
import NameEnv		( delListFromNameEnv )
import HscTypes		( GlobalRdrElt(..), GlobalRdrEnv, ImportReason(..), Provenance(..), 
			  isLocalGRE )
#endif

import FastString	( mkFastString )
import Panic		( showException )
import List		( partition )
import Util		( sortLt )
\end{code}



%************************************************************************
%*									*
	Typecheck and rename a module
%*									*
%************************************************************************


\begin{code}
tcRnModule :: HscEnv -> PersistentCompilerState
	   -> RdrNameHsModule 
	   -> IO (PersistentCompilerState, Maybe TcGblEnv)

tcRnModule hsc_env pcs
	   (HsModule maybe_mod exports import_decls local_decls mod_deprec loc)
 = do { showPass (hsc_dflags hsc_env) "Renamer/typechecker" ;

   let { this_mod = case maybe_mod of
			Nothing  -> mkHomeModule mAIN_Name	-- 'module M where' is omitted
			Just mod -> mod } ;			-- The normal case
		
   initTc hsc_env pcs this_mod $ addSrcLoc loc $
   do { 	-- Deal with imports; sets tcg_rdr_env, tcg_imports
	(rdr_env, imports) <- rnImports import_decls ;
	updGblEnv ( \ gbl -> gbl { tcg_rdr_env = rdr_env,
				   tcg_imports = tcg_imports gbl `plusImportAvails` imports }) 
		     $ do {
	traceRn (text "rn1" <+> ppr (imp_dep_mods imports)) ;
		-- Fail if there are any errors so far
		-- The error printing (if needed) takes advantage 
		-- of the tcg_env we have now set
	failIfErrsM ;

	traceRn (text "rn1a") ;
		-- Rename and type check the declarations
	(tcg_env, src_dus) <- tcRnSrcDecls local_decls ;
	setGblEnv tcg_env		$ do {

	traceRn (text "rn3") ;
		-- Check whether the entire module is deprecated
		-- This happens only once per module
		-- Returns the full new deprecations; a module deprecation 
		-- 	over-rides the earlier ones
	let { mod_deprecs = checkModDeprec mod_deprec } ;
	updGblEnv (\gbl -> gbl { tcg_deprecs = tcg_deprecs gbl `plusDeprecs` mod_deprecs })
		  $ do {

		-- Process the export list
	export_avails <- exportsFromAvail maybe_mod exports ;
	updGblEnv (\gbl -> gbl { tcg_exports = export_avails })
		  $  do {

		-- Get any supporting decls for the exports that have not already
		-- been sucked in for the declarations in the body of the module.
		-- (This can happen if something is imported only to be re-exported.)
		--
		-- Importing these supporting declarations is required 
		--	*only* to gether usage information
		--	(see comments with MkIface.mkImportInfo for why)
		-- For OneShot compilation we could just throw away the decls
		-- but for Batch or Interactive we must put them in the type
		-- envt because they've been removed from the holding pen
	let { export_fvs = availsToNameSet export_avails } ;
	tcg_env <- importSupportingDecls export_fvs ;
	setGblEnv tcg_env $ do {

		-- Report unused names
	let { all_dus = src_dus `plusDU` usesOnly export_fvs } ;
 	reportUnusedNames tcg_env all_dus ;

		-- Dump output and return
	tcDump tcg_env ;
	return tcg_env
    }}}}}}}
\end{code}


%*********************************************************
%*						 	 *
\subsection{Closing up the interface decls}
%*							 *
%*********************************************************

Suppose we discover we don't need to recompile.   Then we start from the
IfaceDecls in the ModIface, and fluff them up by sucking in all the decls they need.

\begin{code}
tcRnIface :: HscEnv
	  -> PersistentCompilerState
	  -> ModIface 	-- Get the decls from here
	  -> IO (PersistentCompilerState, Maybe ModDetails)
				-- Nothing <=> errors happened
tcRnIface hsc_env pcs
	    (ModIface {mi_module = mod, mi_decls = iface_decls})
  = initTc hsc_env pcs mod $ do {

	-- Get the supporting decls, and typecheck them all together
	-- so that any mutually recursive types are done right
    extra_decls <- slurpImpDecls needed ;
    env <- typecheckIfaceDecls (group `addImpDecls` extra_decls) ;

    returnM (ModDetails { md_types = tcg_type_env env,
			  md_insts = tcg_insts env,
			  md_rules = hsCoreRules (tcg_rules env)
		  -- All the rules from an interface are of the IfaceRuleOut form
		 }) }
  where
	rule_decls = dcl_rules iface_decls
	inst_decls = dcl_insts iface_decls
	tycl_decls = dcl_tycl  iface_decls
	group = emptyGroup { hs_ruleds = rule_decls,
			     hs_instds = inst_decls,
			     hs_tyclds = tycl_decls }
	needed = unionManyNameSets (map ruleDeclFVs rule_decls) `unionNameSets`
		 unionManyNameSets (map instDeclFVs inst_decls) `unionNameSets`
		 unionManyNameSets (map tyClDeclFVs tycl_decls) `unionNameSets`
		 ubiquitousNames
			-- Data type decls with record selectors,
			-- which may appear in the decls, need unpackCString
			-- and friends. It's easier to just grab them right now.

hsCoreRules :: [TypecheckedRuleDecl] -> [IdCoreRule]
-- All post-typechecking Iface rules have the form IfaceRuleOut
hsCoreRules rules = [(id,rule) | IfaceRuleOut id rule <- rules]
\end{code}


%************************************************************************
%*									*
		The interactive interface 
%*									*
%************************************************************************

\begin{code}
#ifdef GHCI
tcRnStmt :: HscEnv -> PersistentCompilerState
	 -> InteractiveContext
	 -> RdrNameStmt
	 -> IO (PersistentCompilerState, 
		Maybe (InteractiveContext, [Name], TypecheckedHsExpr))
		-- The returned [Name] is the same as the input except for
		-- ExprStmt, in which case the returned [Name] is [itName]
		--
		-- The returned TypecheckedHsExpr is of type IO [ () ],
		-- a list of the bound values, coerced to ().

tcRnStmt hsc_env pcs ictxt rdr_stmt
  = initTc hsc_env pcs iNTERACTIVE $ 
    setInteractiveContext ictxt $ do {

    -- Rename; use CmdLineMode because tcRnStmt is only used interactively
    ([rn_stmt], fvs) <- initRnInteractive ictxt 
					(rnStmts DoExpr [rdr_stmt]) ;
    traceRn (text "tcRnStmt" <+> vcat [ppr rdr_stmt, ppr rn_stmt, ppr fvs]) ;
    failIfErrsM ;
    
    -- Suck in the supporting declarations and typecheck them
    tcg_env <- importSupportingDecls (fvs `plusFV` implicitStmtFVs fvs) ;
	-- NB: an earlier version deleted (rdrEnvElts local_env) from
	--     the fvs.  But (a) that isn't necessary, because previously
	--     bound things in the local_env will be in the TypeEnv, and 
	--     the renamer doesn't re-slurp such things, and 
	-- (b) it's WRONG to delete them. Consider in GHCi:
	--	  Mod> let x = e :: T
	--	  Mod> let y = x + 3
	--     We need to pass 'x' among the fvs to slurpImpDecls, so that
	--     the latter can see that T is a gate, and hence import the Num T 
	--     instance decl.  (See the InTypEnv case in RnIfaces.slurpSourceRefs.)
    setGblEnv tcg_env $ do {
    
    -- The real work is done here
    (bound_ids, tc_expr) <- tcUserStmt rn_stmt ;
    
    traceTc (text "tcs 1") ;
    let {	-- Make all the bound ids "global" ids, now that
    		-- they're notionally top-level bindings.  This is
	    	-- important: otherwise when we come to compile an expression
	    	-- using these ids later, the byte code generator will consider
	    	-- the occurrences to be free rather than global.
	global_ids     = map globaliseId bound_ids ;
	globaliseId id = setGlobalIdDetails id VanillaGlobal ;
    
		-- Update the interactive context
	rn_env   = ic_rn_local_env ictxt ;
	type_env = ic_type_env ictxt ;

	bound_names = map idName global_ids ;
	new_rn_env  = extendLocalRdrEnv rn_env bound_names ;

		-- Remove any shadowed bindings from the type_env;
		-- they are inaccessible but might, I suppose, cause 
		-- a space leak if we leave them there
	shadowed = [ n | name <- bound_names,
			 let rdr_name = mkRdrUnqual (nameOccName name),
			 Just n <- [lookupRdrEnv rn_env rdr_name] ] ;

	filtered_type_env = delListFromNameEnv type_env shadowed ;
	new_type_env = extendTypeEnvWithIds filtered_type_env global_ids ;

	new_ic = ictxt { ic_rn_local_env = new_rn_env, 
		  	 ic_type_env     = new_type_env }
    } ;

    dumpOptTcRn Opt_D_dump_tc 
    	(vcat [text "Bound Ids" <+> pprWithCommas ppr global_ids,
    	       text "Typechecked expr" <+> ppr tc_expr]) ;

    returnM (new_ic, bound_names, tc_expr)
    }}
\end{code}		


Here is the grand plan, implemented in tcUserStmt

	What you type			The IO [HValue] that hscStmt returns
	-------------			------------------------------------
	let pat = expr		==> 	let pat = expr in return [coerce HVal x, coerce HVal y, ...]
					bindings: [x,y,...]

	pat <- expr		==> 	expr >>= \ pat -> return [coerce HVal x, coerce HVal y, ...]
					bindings: [x,y,...]

	expr (of IO type)	==>	expr >>= \ v -> return [coerce HVal v]
	  [NB: result not printed]	bindings: [it]
	  
	expr (of non-IO type,	==>	let v = expr in print v >> return [coerce HVal v]
	  result showable)		bindings: [it]

	expr (of non-IO type, 
	  result not showable)	==>	error


\begin{code}
---------------------------
tcUserStmt :: RenamedStmt -> TcM ([Id], TypecheckedHsExpr)
tcUserStmt (ExprStmt expr _ loc)
  = newUnique 		`thenM` \ uniq ->
    let 
	fresh_it = itName uniq
        the_bind = FunMonoBind fresh_it False 
			[ mkSimpleMatch [] expr placeHolderType loc ] loc
    in
    tryTcLIE_ (do { 	-- Try this if the other fails
		traceTc (text "tcs 1b") ;
		tc_stmts [
		    LetStmt (MonoBind the_bind [] NonRecursive),
		    ExprStmt (HsApp (HsVar printName) (HsVar fresh_it)) 
			     placeHolderType loc] })
	  (do { 	-- Try this first 
		traceTc (text "tcs 1a") ;
		tc_stmts [BindStmt (VarPat fresh_it) expr loc] })

tcUserStmt stmt = tc_stmts [stmt]

---------------------------
tc_stmts stmts
 = do { ioTyCon <- tcLookupTyCon ioTyConName ;
	let {
	    ret_ty    = mkListTy unitTy ;
	    io_ret_ty = mkTyConApp ioTyCon [ret_ty] ;

	    names = collectStmtsBinders stmts ;

	    stmt_ctxt = SC { sc_what = DoExpr, 
			     sc_rhs  = check_rhs,
			     sc_body = check_body,
			     sc_ty   = ret_ty } ;

	    check_rhs rhs rhs_ty = tcCheckRho rhs  (mkTyConApp ioTyCon [rhs_ty]) ;
	    check_body body      = tcCheckRho body io_ret_ty ;

		-- mk_return builds the expression
		--	returnIO @ [()] [coerce () x, ..,  coerce () z]
		--
		-- Despite the inconvenience of building the type applications etc,
		-- this *has* to be done in type-annotated post-typecheck form
		-- because we are going to return a list of *polymorphic* values
		-- coerced to type (). If we built a *source* stmt
		--	return [coerce x, ..., coerce z]
		-- then the type checker would instantiate x..z, and we wouldn't
		-- get their *polymorphic* values.  (And we'd get ambiguity errs
		-- if they were overloaded, since they aren't applied to anything.)
	    mk_return ret_id ids = HsApp (TyApp (HsVar ret_id) [ret_ty]) 
			      		 (ExplicitList unitTy (map mk_item ids)) ;
	    mk_item id = HsApp (TyApp (HsVar unsafeCoerceId) [idType id, unitTy])
		    	       (HsVar id) ;

	    io_ty = mkTyConApp ioTyCon []
	 } ;

	-- OK, we're ready to typecheck the stmts
	traceTc (text "tcs 2") ;
	((ids, tc_expr), lie) <- getLIE $ do {
	    (ids, tc_stmts) <- tcStmtsAndThen combine stmt_ctxt stmts	$ 
			do {
			    -- Look up the names right in the middle,
			    -- where they will all be in scope
			    ids <- mappM tcLookupId names ;
			    ret_id <- tcLookupId returnIOName ;		-- return @ IO
			    return (ids, [ResultStmt (mk_return ret_id ids) noSrcLoc]) } ;

	    io_ids <- mappM (tcStdSyntaxName DoOrigin io_ty) monadNames ;
	    return (ids, HsDo DoExpr tc_stmts io_ids io_ret_ty noSrcLoc) 
	} ;

	-- Simplify the context right here, so that we fail
	-- if there aren't enough instances.  Notably, when we see
	--		e
	-- we use recoverTc_ to try	it <- e
	-- and then			let it = e
	-- It's the simplify step that rejects the first.
	traceTc (text "tcs 3") ;
	const_binds <- tcSimplifyInteractive lie ;

	-- Build result expression and zonk it
	let { expr = mkHsLet const_binds tc_expr } ;
	zonked_expr <- zonkTopExpr expr ;
	zonked_ids  <- zonkTopBndrs ids ;

	return (zonked_ids, zonked_expr)
	}
  where
    combine stmt (ids, stmts) = (ids, stmt:stmts)
\end{code}


tcRnExpr just finds the type of an expression

\begin{code}
tcRnExpr :: HscEnv -> PersistentCompilerState
	 -> InteractiveContext
	 -> RdrNameHsExpr
	 -> IO (PersistentCompilerState, Maybe Type)
tcRnExpr hsc_env pcs ictxt rdr_expr
  = initTc hsc_env pcs iNTERACTIVE $ 
    setInteractiveContext ictxt $ do {

    (rn_expr, fvs) <- initRnInteractive ictxt (rnExpr rdr_expr) ;
    failIfErrsM ;

	-- Suck in the supporting declarations and typecheck them
    tcg_env <- importSupportingDecls (fvs `plusFV` ubiquitousNames) ;
    setGblEnv tcg_env $ do {
    
	-- Now typecheck the expression; 
	-- it might have a rank-2 type (e.g. :t runST)
    ((tc_expr, res_ty), lie)	   <- getLIE (tcInferRho rn_expr) ;
    ((qtvs, _, dict_ids), lie_top) <- getLIE (tcSimplifyInfer smpl_doc (tyVarsOfType res_ty) lie)  ;
    tcSimplifyInteractive lie_top ;

    let { all_expr_ty = mkForAllTys qtvs 		$
    		        mkFunTys (map idType dict_ids)	$
    		        res_ty } ;
    zonkTcType all_expr_ty
    }}
  where
    smpl_doc = ptext SLIT("main expression")
\end{code}


\begin{code}
tcRnThing :: HscEnv -> PersistentCompilerState
	  -> InteractiveContext
	  -> RdrName
	  -> IO (PersistentCompilerState, Maybe [TyThing])
-- Look up a RdrName and return all the TyThings it might be
-- A capitalised RdrName is given to us in the DataName namespace,
-- but we want to treat it as *both* a data constructor 
-- *and* as a type or class constructor; 
-- hence the call to dataTcOccs, and we return up to two results
tcRnThing hsc_env pcs ictxt rdr_name
  = initTc hsc_env pcs iNTERACTIVE $ 
    setInteractiveContext ictxt $ do {

	-- If the identifier is a constructor (begins with an
	-- upper-case letter), then we need to consider both
	-- constructor and type class identifiers.
    let { rdr_names = dataTcOccs rdr_name } ;

	-- results :: [(Messages, Maybe Name)]
    results <- initRnInteractive ictxt
			    (mapM (tryTc . lookupOccRn) rdr_names) ;

	-- The successful lookups will be (Just name)
    let { (warns_s, good_names) = unzip [ (msgs, name) 
					| (msgs, Just name) <- results] ;
	  errs_s = [msgs | (msgs, Nothing) <- results] } ;

	-- Fail if nothing good happened, else add warnings
    if null good_names then
		-- No lookup succeeded, so
		-- pick the first error message and report it
		-- ToDo: If one of the errors is "could be Foo.X or Baz.X",
		--	 while the other is "X is not in scope", 
		--	 we definitely want the former; but we might pick the latter
	do { addMessages (head errs_s) ; failM }
      else 			-- Add deprecation warnings
	mapM_ addMessages warns_s ;
	
	-- Slurp in the supporting declarations
    tcg_env <- importSupportingDecls (mkFVs good_names) ;
    setGblEnv tcg_env $ do {

	-- And lookup up the entities
    mapM tcLookupGlobal good_names
    }}
\end{code}


\begin{code}
setInteractiveContext :: InteractiveContext -> TcRn m a -> TcRn m a
setInteractiveContext icxt thing_inside 
  = traceTc (text "setIC" <+> ppr (ic_type_env icxt))	`thenM_`
    updGblEnv (\ env -> env { tcg_rdr_env  = ic_rn_gbl_env icxt,
			      tcg_type_env = ic_type_env   icxt })
	      thing_inside

initRnInteractive :: InteractiveContext -> RnM a -> TcM a
-- Set the local RdrEnv from the interactive context
initRnInteractive ictxt rn_thing
  = initRn CmdLineMode $
    setLocalRdrEnv (ic_rn_local_env ictxt) $
    rn_thing
#endif /* GHCI */
\end{code}

%************************************************************************
%*									*
	Type-checking external-core modules
%*									*
%************************************************************************

\begin{code}
tcRnExtCore :: HscEnv -> PersistentCompilerState 
	    -> RdrNameHsModule 
	    -> IO (PersistentCompilerState, Maybe ModGuts)
	-- Nothing => some error occurred 

tcRnExtCore hsc_env pcs (HsModule (Just this_mod) _ _ decls _ loc)
	-- For external core, the module name is syntactically reqd
	-- Rename the (Core) module.  It's a bit like an interface
	-- file: all names are original names
 = do { showPass (hsc_dflags hsc_env) "Renamer/typechecker" ;

   initTc hsc_env pcs this_mod $ addSrcLoc loc $ do {

	-- Rename the source, only in interface mode.
	-- rnSrcDecls handles fixity decls etc too, which won't occur
	-- but that doesn't matter
   let { local_group = mkGroup decls } ;
   (_, rn_src_decls, dus) <- initRn (InterfaceMode this_mod) 
				    (rnSrcDecls local_group) ;
   failIfErrsM ;

 	-- Get the supporting decls
   rn_imp_decls <- slurpImpDecls (duUses dus) ;
   let { rn_decls = rn_src_decls `addImpDecls` rn_imp_decls } ;

	-- Dump trace of renaming part
   rnDump (ppr rn_decls) ;
   rnStats rn_imp_decls ;

	-- Typecheck them all together so that
	-- any mutually recursive types are done right
   tcg_env <- typecheckIfaceDecls rn_decls ;
   setGblEnv tcg_env $ do {
   
	-- Now the core bindings
   core_prs <- tcCoreBinds (hs_coreds rn_decls) ;
   tcExtendGlobalValEnv (map fst core_prs) $ do {
   
	-- Wrap up
   let {
	bndrs 	   = map fst core_prs ;
	my_exports = map (Avail . idName) bndrs ;
		-- ToDo: export the data types also?

	final_type_env = extendTypeEnvWithIds (tcg_type_env tcg_env) bndrs ;

	mod_guts = ModGuts {	mg_module   = this_mod,
				mg_usages   = [],		-- ToDo: compute usage
				mg_dir_imps = [],		-- ??
				mg_deps     = noDependencies,	-- ??
				mg_exports  = my_exports,
				mg_types    = final_type_env,
				mg_insts    = tcg_insts tcg_env,
				mg_rules    = hsCoreRules (tcg_rules tcg_env),
				mg_binds    = [Rec core_prs],

				-- Stubs
				mg_rdr_env  = emptyGlobalRdrEnv,
				mg_fix_env  = emptyFixityEnv,
				mg_deprecs  = NoDeprecs,
				mg_foreign  = NoStubs
		    } } ;

   tcCoreDump mod_guts ;

   return mod_guts
   }}}}
\end{code}


%************************************************************************
%*									*
	Type-checking the top level of a module
%*									*
%************************************************************************

\begin{code}
tcRnSrcDecls :: [RdrNameHsDecl] -> TcM (TcGblEnv, DefUses)
	-- Returns the variables free in the decls
	-- Reason: solely to report unused imports and bindings
tcRnSrcDecls decls
 = do { 	-- Do all the declarations
	((tc_envs, dus), lie) <- getLIE (tc_rn_src_decls decls) ;

	     -- tcSimplifyTop deals with constant or ambiguous InstIds.  
	     -- How could there be ambiguous ones?  They can only arise if a
	     -- top-level decl falls under the monomorphism
	     -- restriction, and no subsequent decl instantiates its
	     -- type.  (Usually, ambiguous type variables are resolved
	     -- during the generalisation step.)
        traceTc (text "Tc8") ;
	setEnvs tc_envs		$ do {
		-- Setting the global env exposes the instances to tcSimplifyTop
		-- Setting the local env exposes the local Ids, so that
		-- we get better error messages (monomorphism restriction)
	inst_binds <- tcSimplifyTop lie ;

	    -- Backsubstitution.  This must be done last.
	    -- Even tcSimplifyTop may do some unification.
        traceTc (text "Tc9") ;
	let { (tcg_env, _) = tc_envs ;
	      TcGblEnv { tcg_type_env = type_env, tcg_binds = binds, 
		         tcg_rules = rules, tcg_fords = fords } = tcg_env } ;

	(bind_ids, binds', fords', rules') <- zonkTopDecls (binds `andMonoBinds` inst_binds)
							   rules fords ;

	return (tcg_env { tcg_type_env = extendTypeEnvWithIds type_env bind_ids,
			  tcg_binds = binds', tcg_rules = rules', tcg_fords = fords' }, 
		dus)
    }}

tc_rn_src_decls :: [RdrNameHsDecl] -> TcM ((TcGblEnv, TcLclEnv), DefUses)

tc_rn_src_decls ds
 = do { let { (first_group, group_tail) = findSplice ds } ;
		-- If ds is [] we get ([], Nothing)

	-- Type check the decls up to, but not including, the first splice
	(tc_envs@(_,tcl_env), src_dus1) <- tcRnGroup first_group ;

	-- Bale out if errors; for example, error recovery when checking
	-- the RHS of 'main' can mean that 'main' is not in the envt for 
	-- the subsequent checkMain test
	failIfErrsM ;

	setEnvs tc_envs $

	-- If there is no splice, we're nearly done
	case group_tail of {
	   Nothing -> do { 	-- Last thing: check for `main'
			   (tcg_env, main_fvs) <- checkMain ;
			   return ((tcg_env, tcl_env), 
				    src_dus1 `plusDU` usesOnly main_fvs)
		      } ;

	-- If there's a splice, we must carry on
	   Just (SpliceDecl splice_expr splice_loc, rest_ds) -> do {
#ifndef GHCI
	failWithTc (text "Can't do a top-level splice; need a bootstrapped compiler")
#else

	-- Rename the splice expression, and get its supporting decls
	(rn_splice_expr, splice_fvs) <- initRn SourceMode $
				 	addSrcLoc splice_loc $
				 	rnExpr splice_expr ;
	tcg_env <- importSupportingDecls (splice_fvs `plusFV` templateHaskellNames) ;
	setGblEnv tcg_env $ do {

	-- Execute the splice
	spliced_decls <- tcSpliceDecls rn_splice_expr ;

	-- Glue them on the front of the remaining decls and loop
	(tc_envs, src_dus2) <- tc_rn_src_decls (spliced_decls ++ rest_ds) ;

	return (tc_envs, src_dus1 `plusDU` usesOnly splice_fvs `plusDU` src_dus2)
    }
#endif /* GHCI */
    }}}
\end{code}


%************************************************************************
%*									*
	Type-checking the top level of a module
%*									*
%************************************************************************

tcRnGroup takes a bunch of top-level source-code declarations, and
 * renames them
 * gets supporting declarations from interface files
 * typechecks them
 * zonks them
 * and augments the TcGblEnv with the results

In Template Haskell it may be called repeatedly for each group of
declarations.  It expects there to be an incoming TcGblEnv in the
monad; it augments it and returns the new TcGblEnv.

\begin{code}
tcRnGroup :: HsGroup RdrName -> TcM ((TcGblEnv, TcLclEnv), DefUses)
	-- Returns the variables free in the decls, for unused-binding reporting
tcRnGroup decls
 = do {		-- Rename the declarations
	(tcg_env, rn_decls, src_dus) <- rnTopSrcDecls decls ;
	setGblEnv tcg_env $ do {

		-- Typecheck the declarations
	tc_envs <- tcTopSrcDecls rn_decls ;

	return (tc_envs, src_dus)
  }}

------------------------------------------------
rnTopSrcDecls :: HsGroup RdrName -> TcM (TcGblEnv, HsGroup Name, DefUses)
rnTopSrcDecls group
 = do { 	-- Bring top level binders into scope
	(rdr_env, imports) <- importsFromLocalDecls group ;
	updGblEnv (\gbl -> gbl { tcg_rdr_env = rdr_env `plusGlobalRdrEnv`
						  tcg_rdr_env gbl,
				 tcg_imports = imports `plusImportAvails` 
						  tcg_imports gbl }) 
		     $ do {

	failIfErrsM ;	-- No point in continuing if (say) we have duplicate declarations

		-- Rename the source decls
	(tcg_env, rn_src_decls, src_dus) <- initRn SourceMode (rnSrcDecls group) ;
	setGblEnv tcg_env $ do {

	failIfErrsM ;

		-- Import consquential imports
	let { src_fvs = duUses src_dus } ;
	rn_imp_decls <- slurpImpDecls (src_fvs `plusFV` implicitModuleFVs src_fvs) ;
	let { rn_decls = rn_src_decls `addImpDecls` rn_imp_decls } ;

		-- Dump trace of renaming part
	rnDump (ppr rn_decls) ;
	rnStats rn_imp_decls ;

	return (tcg_env, rn_decls, src_dus)
  }}}

------------------------------------------------
tcTopSrcDecls :: HsGroup Name -> TcM (TcGblEnv, TcLclEnv)
tcTopSrcDecls
	(HsGroup { hs_tyclds = tycl_decls, 
		   hs_instds = inst_decls,
		   hs_fords  = foreign_decls,
		   hs_defds  = default_decls,
		   hs_ruleds = rule_decls,
		   hs_valds  = val_binds })
 = do {		-- Type-check the type and class decls, and all imported decls
		-- The latter come in via tycl_decls
        traceTc (text "Tc2") ;
	tcg_env <- tcTyClDecls tycl_decls ;
	setGblEnv tcg_env	$ do {

		-- Source-language instances, including derivings,
		-- and import the supporting declarations
        traceTc (text "Tc3") ;
	(tcg_env, inst_infos, deriv_binds, fvs) <- tcInstDecls1 tycl_decls inst_decls ;
	setGblEnv tcg_env	$ do {
	tcg_env <- importSupportingDecls fvs ;
	setGblEnv tcg_env	$ do {

	        -- Foreign import declarations next.  No zonking necessary
		-- here; we can tuck them straight into the global environment.
        traceTc (text "Tc4") ;
	(fi_ids, fi_decls) <- tcForeignImports foreign_decls ;
	tcExtendGlobalValEnv fi_ids		     $
	updGblEnv (\gbl -> gbl { tcg_fords = tcg_fords gbl ++ fi_decls }) 
		  $ do {

		-- Default declarations
        traceTc (text "Tc4a") ;
	default_tys <- tcDefaults default_decls ;
	updGblEnv (\gbl -> gbl { tcg_default = default_tys }) $ do {
	
		-- Value declarations next
		-- We also typecheck any extra binds that came out 
		-- of the "deriving" process
        traceTc (text "Tc5") ;
	(tc_val_binds, lcl_env) <- tcTopBinds (val_binds `ThenBinds` deriv_binds) ;
	setLclTypeEnv lcl_env 	$ do {

	     	-- Second pass over class and instance declarations, 
		-- plus rules and foreign exports, to generate bindings
        traceTc (text "Tc6") ;
	(cls_dm_binds, dm_ids) <- tcClassDecls2 tycl_decls ;
	tcExtendGlobalValEnv dm_ids	$ do {
	inst_binds <- tcInstDecls2 inst_infos ;
	showLIE (text "after instDecls2") ;

		-- Foreign exports
		-- They need to be zonked, so we return them
        traceTc (text "Tc7") ;
	(foe_binds, foe_decls) <- tcForeignExports foreign_decls ;

		-- Rules
		-- Need to partition them because the source rules
		-- must be zonked before adding them to tcg_rules
		-- NB: built-in rules come in as IfaceRuleOut's, and
		--     get added to tcg_rules right here by tcExtendRules
	rules <- tcRules rule_decls ;
	let { (src_rules, iface_rules) = partition isSrcRule rules } ;
	tcExtendRules iface_rules $ do {

		-- Wrap up
	tcg_env <- getGblEnv ;
	let { all_binds = tc_val_binds	 `AndMonoBinds`
			  inst_binds	 `AndMonoBinds`
			  cls_dm_binds	 `AndMonoBinds`
			  foe_binds  ;

		-- Extend the GblEnv with the (as yet un-zonked) 
		-- bindings, rules, foreign decls
	      tcg_env' = tcg_env {  tcg_binds = tcg_binds tcg_env `andMonoBinds` all_binds,
				    tcg_rules = tcg_rules tcg_env ++ src_rules,
				    tcg_fords = tcg_fords tcg_env ++ foe_decls } } ;
	
  	return (tcg_env', lcl_env)
     }}}}}}}}}
\end{code}

\begin{code}
tcTyClDecls :: [RenamedTyClDecl]
	    -> TcM TcGblEnv

-- tcTyClDecls deals with 
--	type and class decls (some source, some imported)
--	interface signatures (checked lazily)
--
-- It returns the TcGblEnv for this module, and side-effects the
-- persistent compiler state to reflect the things imported from
-- other modules

tcTyClDecls tycl_decls
  = checkNoErrs $
	-- tcTyAndClassDecls recovers internally, but if anything gave rise to
	-- an error we'd better stop now, to avoid a cascade
	
    traceTc (text "TyCl1")		`thenM_`
    tcTyAndClassDecls tycl_decls	`thenM` \ tcg_env ->
	-- Returns the extended environment
    setGblEnv tcg_env 			$

    traceTc (text "TyCl2")		`thenM_`
    tcInterfaceSigs tycl_decls		`thenM` \ tcg_env ->
	-- Returns the extended environment

    returnM tcg_env
\end{code}    



%************************************************************************
%*									*
	Load the old interface file for this module (unless
	we have it aleady), and check whether it is up to date
	
%*									*
%************************************************************************

\begin{code}
checkOldIface :: HscEnv
	      -> PersistentCompilerState
	      -> Module
	      -> FilePath		-- Where the interface file is
	      -> Bool 			-- Source unchanged
	      -> Maybe ModIface 	-- Old interface from compilation manager, if any
	      -> IO (PersistentCompilerState, Maybe (RecompileRequired, Maybe ModIface))
				-- Nothing <=> errors happened

checkOldIface hsc_env pcs mod iface_path source_unchanged maybe_iface
  = do { showPass (hsc_dflags hsc_env) 
	          ("Checking old interface for " ++ moduleUserString mod) ;

	 initTc hsc_env pcs mod
		(check_old_iface iface_path source_unchanged maybe_iface)
     }

check_old_iface iface_path source_unchanged maybe_iface
 = 	-- CHECK WHETHER THE SOURCE HAS CHANGED
    ifM (not source_unchanged)
	(traceHiDiffs (nest 4 (text "Source file changed or recompilation check turned off")))
					      	`thenM_`

     -- If the source has changed and we're in interactive mode, avoid reading
     -- an interface; just return the one we might have been supplied with.
    getGhciMode					`thenM` \ ghci_mode ->
    if (ghci_mode == Interactive) && not source_unchanged then
         returnM (outOfDate, maybe_iface)
    else

    case maybe_iface of {
       Just old_iface -> -- Use the one we already have
                         checkVersions source_unchanged old_iface	`thenM` \ recomp ->
			 returnM (recomp, Just old_iface)

    ;  Nothing ->

	-- Try and read the old interface for the current module
	-- from the .hi file left from the last time we compiled it
    getModule					`thenM` \ this_mod ->
    readIface this_mod iface_path False	`thenM` \ read_result ->
    case read_result of {
       Left err ->	-- Old interface file not found, or garbled; give up
		   traceHiDiffs (text "FYI: cannot read old interface file:"
			   	 $$ nest 4 (text (showException err))) 	`thenM_`
	           returnM (outOfDate, Nothing)

    ;  Right parsed_iface ->	

	-- We found the file and parsed it; now load it
    tryTc (initRn (InterfaceMode this_mod)
		  (loadOldIface parsed_iface))	`thenM` \ ((_,errs), mb_iface) ->
    case mb_iface of {
	Nothing ->	-- Something went wrong in loading.  The main likely thing
			-- is that the usages mentioned B.f, where B.hi and B.hs no
			-- longer exist.  Then newGlobalName2 fails with an error message
			-- This isn't an error; we just don't have an old iface file to
			-- look at.  Spit out a traceHiDiffs for info though.
		   traceHiDiffs (text "FYI: loading old interface file failed"
				   $$ nest 4 (docToSDoc (pprBagOfErrors errs)))	`thenM_`
		   return (outOfDate, Nothing)

    ;	Just iface -> 

	-- At last, we have got the old iface; check its versions
    checkVersions source_unchanged iface	`thenM` \ recomp ->
    returnM (recomp, Just iface)
    }}}
\end{code}


%************************************************************************
%*									*
	Type-check and rename supporting declarations
	This is used to deal with the free vars of a splice,
	or derived code: slurp in the necessary declarations,
	typecheck them, and add them to the EPS
%*									*
%************************************************************************

\begin{code}
importSupportingDecls :: FreeVars -> TcM TcGblEnv
-- Completely deal with the supporting imports needed
-- by the specified free-var set
importSupportingDecls fvs
 = do { traceRn (text "Import supporting decls for" <+> ppr (nameSetToList fvs)) ;
	decls <- slurpImpDecls fvs ;
	traceRn (text "...namely:" <+> vcat (map ppr decls)) ;
	typecheckIfaceDecls (mkGroup decls) }

typecheckIfaceDecls :: HsGroup Name -> TcM TcGblEnv
  -- The decls are all interface-file declarations
  -- Usually they are all from other modules, but when we are reading
  -- this module's interface from a file, it's possible that some of
  -- them are for the module being compiled.
  -- That is why the tcExtendX functions need to do partitioning.
  --
  -- If all the decls are from other modules, the returned TcGblEnv
  -- will have an empty tc_genv, but its tc_inst_env
  -- cache may have been augmented.
typecheckIfaceDecls (HsGroup { hs_tyclds = tycl_decls,
			       hs_instds = inst_decls,
			       hs_ruleds = rule_decls })
 = do {		-- Typecheck the type, class, and interface-sig decls
	tcg_env <- tcTyClDecls tycl_decls ;
	setGblEnv tcg_env		$ do {
	
    	-- Typecheck the instance decls, and rules
	-- Note that imported dictionary functions are already
	-- in scope from the preceding tcTyClDecls
	tcIfaceInstDecls inst_decls	`thenM` \ dfuns ->
	tcExtendInstEnv dfuns		$
	tcRules rule_decls		`thenM` \ rules ->
	tcExtendRules rules		$
    
	getGblEnv		-- Return the environment
   }}
\end{code}



%*********************************************************
%*						 	 *
	mkGlobalContext: make up an interactive context

	Used for initialising the lexical environment
	of the interactive read-eval-print loop
%*							 *
%*********************************************************

\begin{code}
#ifdef GHCI
mkGlobalContext
	:: HscEnv -> PersistentCompilerState
	-> [Module] 	-- Expose these modules' top-level scope
	-> [Module]	-- Expose these modules' exports only
        -> IO (PersistentCompilerState, Maybe GlobalRdrEnv)

mkGlobalContext hsc_env pcs toplevs exports
  = initTc hsc_env pcs iNTERACTIVE $ do {

    toplev_envs <- mappM getTopLevScope   toplevs ;
    export_envs <- mappM getModuleExports exports ;
    returnM (foldr plusGlobalRdrEnv emptyGlobalRdrEnv
		   (toplev_envs ++ export_envs))
    }

getTopLevScope :: Module -> TcRn m GlobalRdrEnv
getTopLevScope mod
  = do { iface <- loadInterface contextDoc (moduleName mod) (ImportByUser False) ;
 	 case mi_globals iface of
		Nothing  -> panic "getTopLevScope"
		Just env -> returnM env }

getModuleExports :: Module -> TcRn m GlobalRdrEnv
getModuleExports mod 
  = do { iface <- loadInterface contextDoc (moduleName mod) (ImportByUser False) ;
         returnM (foldl add emptyGlobalRdrEnv (mi_exports iface)) }
  where
    prov_fn n = NonLocalDef ImplicitImport
    add env (mod,avails)
	= plusGlobalRdrEnv env (mkGlobalRdrEnv mod True prov_fn avails NoDeprecs)

contextDoc = text "context for compiling statements"
\end{code}

\begin{code}
getModuleContents
  :: HscEnv
  -> PersistentCompilerState    -- IN: persistent compiler state
  -> Module			-- module to inspect
  -> Bool			-- grab just the exports, or the whole toplev
  -> IO (PersistentCompilerState, Maybe [TyThing])

getModuleContents hsc_env pcs mod exports_only
 = initTc hsc_env pcs iNTERACTIVE $ do {   

	-- Load the interface if necessary (a home module will certainly
	-- alraedy be loaded, but a package module might not be)
	iface <- loadInterface contextDoc (moduleName mod) (ImportByUser False) ;

        let { export_names = availsToNameSet export_avails ;
	      export_avails = [ avail | (mn, avails) <- mi_exports iface, 
		  	                avail <- avails ] } ;

	all_names <- if exports_only then 
			return export_names
		     else case mi_globals iface of {
			   Just rdr_env -> 
				return (get_locals rdr_env) ;

			   Nothing -> do { addErr (noRdrEnvErr mod) ;
					   return export_names } } ;
	   			-- Invariant; we only have (not exports_only) 
				-- for a home module so it must already be in the HIT
				-- So the Nothing case is a bug

	env <- importSupportingDecls all_names ;
	setGblEnv env (mappM tcLookupGlobal (nameSetToList all_names))
    }
  where
	-- Grab all the things from the global env that are locally def'd
    get_locals rdr_env = mkNameSet [ gre_name gre
				   | elts <- rdrEnvElts rdr_env, 
				     gre <- elts, 
				     isLocalGRE gre ]
	-- Make a set because a name is often in the envt in
	-- both qualified and unqualified forms

noRdrEnvErr mod = ptext SLIT("No top-level environment available for module") 
		  <+> quotes (ppr mod)
#endif
\end{code}

%************************************************************************
%*									*
	Checking for 'main'
%*									*
%************************************************************************

\begin{code}
checkMain 
  = do { ghci_mode <- getGhciMode ;
	 tcg_env   <- getGblEnv ;

	 mb_main_mod <- readMutVar v_MainModIs ;
	 mb_main_fn  <- readMutVar v_MainFunIs ;
	 let { main_mod = case mb_main_mod of {
				Just mod -> mkModuleName mod ;
				Nothing  -> mAIN_Name } ;
	        main_fn  = case mb_main_fn of {
				Just fn -> mkRdrUnqual (mkVarOcc (mkFastString fn)) ;
				Nothing -> main_RDR_Unqual } } ;
	
	 check_main ghci_mode tcg_env main_mod main_fn
    }


check_main ghci_mode tcg_env main_mod main_fn
     -- If we are in module Main, check that 'main' is defined.
     -- It may be imported from another module, in which case 
     -- we have to drag in its.
     -- 
     -- Also form the definition
     --		$main = runIO main
     -- so we need to slurp in runIO too.
     --
     -- ToDo: We have to return the main_name separately, because it's a
     -- bona fide 'use', and should be recorded as such, but the others
     -- aren't 
     -- 
     -- Blimey: a whole page of code to do this...

 | mod_name /= main_mod
 = return (tcg_env, emptyFVs)

	-- Check that 'main' is in scope
	-- It might be imported from another module!
	-- 
	-- We use a guard for this (rather than letting lookupSrcName fail)
	-- because it's not an error in ghci)
 | not (main_fn `elemRdrEnv` rdr_env)
 = do { complain_no_main; return (tcg_env, emptyFVs) }

 | otherwise	-- OK, so the appropriate 'main' is in scope
		-- 
 = do { main_name <- lookupSrcName main_fn ;

	tcg_env <- importSupportingDecls (unitFV runIOName) ;

	addSrcLoc (getSrcLoc main_name)	$
	addErrCtxt mainCtxt		$
	setGblEnv tcg_env 		$ do {
	
	-- :Main.main :: IO () = runIO main
	let { rhs = HsApp (HsVar runIOName) (HsVar main_name) } ;
	(main_expr, ty) <- tcInferRho rhs ;

	let { root_main_id = setIdLocalExported (mkLocalId rootMainName ty) ;
	      main_bind      = VarMonoBind root_main_id main_expr ;
	      tcg_env' 	     = tcg_env { tcg_binds = tcg_binds tcg_env 
						     `andMonoBinds` main_bind } } ;

	return (tcg_env', unitFV main_name)
    }}
  where
    mod_name = moduleName (tcg_mod tcg_env) 
    rdr_env  = tcg_rdr_env tcg_env
 
    complain_no_main | ghci_mode == Interactive = return ()
		     | otherwise 		= failWithTc noMainMsg
	-- In interactive mode, don't worry about the absence of 'main'
	-- In other modes, fail altogether, so that we don't go on
	-- and complain a second time when processing the export list.

    mainCtxt  = ptext SLIT("When checking the type of the main function") <+> quotes (ppr main_fn)
    noMainMsg = ptext SLIT("The main function") <+> quotes (ppr main_fn) 
		<+> ptext SLIT("is not defined in module") <+> quotes (ppr main_mod)
\end{code}


%************************************************************************
%*									*
		Degugging output
%*									*
%************************************************************************

\begin{code}
rnDump :: SDoc -> TcRn m ()
-- Dump, with a banner, if -ddump-rn
rnDump doc = dumpOptTcRn Opt_D_dump_rn (mkDumpDoc "Renamer" doc)

tcDump :: TcGblEnv -> TcRn m ()
tcDump env
 = do { dflags <- getDOpts ;

	-- Dump short output if -ddump-types or -ddump-tc
	ifM (dopt Opt_D_dump_types dflags || dopt Opt_D_dump_tc dflags)
	    (dumpTcRn short_dump) ;

	-- Dump bindings if -ddump-tc
	dumpOptTcRn Opt_D_dump_tc (mkDumpDoc "Typechecker" full_dump)
   }
  where
    short_dump = pprTcGblEnv env
    full_dump  = ppr (tcg_binds env)
	-- NB: foreign x-d's have undefined's in their types; 
	--     hence can't show the tc_fords

tcCoreDump mod_guts
 = do { dflags <- getDOpts ;
	ifM (dopt Opt_D_dump_types dflags || dopt Opt_D_dump_tc dflags)
 	    (dumpTcRn (pprModGuts mod_guts)) ;

	-- Dump bindings if -ddump-tc
	dumpOptTcRn Opt_D_dump_tc (mkDumpDoc "Typechecker" full_dump) }
  where
    full_dump = pprCoreBindings (mg_binds mod_guts)

-- It's unpleasant having both pprModGuts and pprModDetails here
pprTcGblEnv :: TcGblEnv -> SDoc
pprTcGblEnv (TcGblEnv { tcg_type_env = type_env, 
		        tcg_insts    = dfun_ids, 
		        tcg_rules    = rules,
			tcg_imports  = imports })
  = vcat [ ppr_types dfun_ids type_env
	 , ppr_insts dfun_ids
	 , vcat (map ppr rules)
	 , ppr_gen_tycons (typeEnvTyCons type_env)
	 , ptext SLIT("Dependent modules:") <+> ppr (moduleEnvElts (imp_dep_mods imports))
	 , ptext SLIT("Dependent packages:") <+> ppr (imp_dep_pkgs imports)]

pprModGuts :: ModGuts -> SDoc
pprModGuts (ModGuts { mg_types = type_env,
		      mg_rules = rules })
  = vcat [ ppr_types [] type_env,
	   ppr_rules rules ]


ppr_types :: [Var] -> TypeEnv -> SDoc
ppr_types dfun_ids type_env
  = text "TYPE SIGNATURES" $$ nest 4 (ppr_sigs ids)
  where
    ids = [id | id <- typeEnvIds type_env, want_sig id]
    want_sig id | opt_PprStyle_Debug = True
	        | otherwise	     = isLocalId id && 
				       isExternalName (idName id) && 
				       not (id `elem` dfun_ids)
	-- isLocalId ignores data constructors, records selectors etc.
	-- The isExternalName ignores local dictionary and method bindings
	-- that the type checker has invented.  Top-level user-defined things 
	-- have External names.

ppr_insts :: [Var] -> SDoc
ppr_insts []       = empty
ppr_insts dfun_ids = text "INSTANCES" $$ nest 4 (ppr_sigs dfun_ids)

ppr_sigs :: [Var] -> SDoc
ppr_sigs ids
	-- Print type signatures
   	-- Convert to HsType so that we get source-language style printing
	-- And sort by RdrName
  = vcat $ map ppr_sig $ sortLt lt_sig $
    [ (getRdrName id, toHsType (tidyTopType (idType id)))
    | id <- ids ]
  where
    lt_sig (n1,_) (n2,_) = n1 < n2
    ppr_sig (n,t)        = ppr n <+> dcolon <+> ppr t


ppr_rules :: [IdCoreRule] -> SDoc
ppr_rules [] = empty
ppr_rules rs = vcat [ptext SLIT("{-# RULES"),
		      nest 4 (pprIdRules rs),
		      ptext SLIT("#-}")]

ppr_gen_tycons []  = empty
ppr_gen_tycons tcs = vcat [ptext SLIT("Generic type constructor details:"),
			   nest 2 (vcat (map ppr_gen_tycon tcs))
		     ]

-- x&y are now Id's, not CoreExpr's 
ppr_gen_tycon tycon 
  | Just ep <- tyConGenInfo tycon
  = (ppr tycon <> colon) $$ nest 4 (ppr_ep ep)

  | otherwise = ppr tycon <> colon <+> ptext SLIT("Not derivable")

ppr_ep (EP from to)
  = vcat [ ptext SLIT("Rep type:") <+> ppr (tcFunResultTy from_tau),
	   ptext SLIT("From:") <+> ppr (unfoldingTemplate (idUnfolding from)),
	   ptext SLIT("To:")   <+> ppr (unfoldingTemplate (idUnfolding to))
    ]
  where
    (_,from_tau) = tcSplitForAllTys (idType from)
\end{code}
