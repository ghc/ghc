%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcModule]{Typechecking a whole module}

\begin{code}
module TcModule (
	typecheckModule, typecheckIface, typecheckStmt, typecheckExpr,
	typecheckExtraDecls,
	TcResults(..)
    ) where

#include "HsVersions.h"

import CmdLineOpts	( DynFlag(..), DynFlags, dopt )
import HsSyn		( HsBinds(..), MonoBinds(..), HsDecl(..), HsExpr(..),
			  Stmt(..), InPat(..), HsMatchContext(..), HsDoContext(..), RuleDecl(..),
			  isIfaceRuleDecl, nullBinds, andMonoBindList, mkSimpleMatch, placeHolderType
			)
import PrelNames	( mAIN_Name, mainName, ioTyConName, printName,
			  returnIOName, bindIOName, failIOName, 
			  itName
			)
import MkId		( unsafeCoerceId )
import RnHsSyn		( RenamedHsBinds, RenamedHsDecl, RenamedStmt,
			  RenamedHsExpr )
import TcHsSyn		( TypecheckedMonoBinds, TypecheckedHsExpr,
			  TypecheckedForeignDecl, TypecheckedRuleDecl,
			  zonkTopBinds, zonkForeignExports, zonkRules, mkHsLet,
			  zonkExpr, zonkIdBndr
			)

import MkIface		( pprModDetails )
import TcExpr 		( tcMonoExpr )
import TcMonad
import TcMType		( unifyTauTy, newTyVarTy, zonkTcType, tcInstType )
import TcType		( Type, liftedTypeKind, openTypeKind,
			  tyVarsOfType, tidyType, tcFunResultTy,
			  mkForAllTys, mkFunTys, mkTyConApp, tcSplitForAllTys
			)
import TcMatches	( tcStmtsAndThen )
import Inst		( emptyLIE, plusLIE )
import TcBinds		( tcTopBinds )
import TcClassDcl	( tcClassDecls2 )
import TcDefaults	( tcDefaults, defaultDefaultTys )
import TcEnv		( TcEnv, RecTcEnv, InstInfo(iDFunId), tcExtendGlobalValEnv, tcLookup_maybe,
			  isLocalThing, tcSetEnv, tcSetInstEnv, initTcEnv, getTcGEnv,
			  tcExtendGlobalTypeEnv, tcLookupGlobalId, tcLookupTyCon,
			  TcTyThing(..), tcLookupId 
			)
import TcRules		( tcIfaceRules, tcSourceRules )
import TcForeign	( tcForeignImports, tcForeignExports )
import TcIfaceSig	( tcInterfaceSigs )
import TcInstDcls	( tcInstDecls1, tcInstDecls2 )
import TcSimplify	( tcSimplifyTop, tcSimplifyInfer )
import TcTyClsDecls	( tcTyAndClassDecls )
import CoreUnfold	( unfoldingTemplate, hasUnfolding )
import TysWiredIn	( mkListTy, unitTy )
import ErrUtils		( printErrorsAndWarnings, errorsFound, 
			  dumpIfSet_dyn, dumpIfSet_dyn_or, showPass )
import Id		( Id, idType, idUnfolding )
import Module           ( Module, moduleName )
import Name		( Name )
import NameEnv		( lookupNameEnv )
import TyCon		( tyConGenInfo )
import BasicTypes       ( EP(..), Fixity, RecFlag(..) )
import SrcLoc		( noSrcLoc )
import Outputable
import IO		( stdout )
import HscTypes		( PersistentCompilerState(..), HomeSymbolTable, 
			  PackageTypeEnv, ModIface(..),
			  ModDetails(..), DFunId,
			  TypeEnv, extendTypeEnvList, typeEnvTyCons, typeEnvElts,
		          TyThing(..), 
			  mkTypeEnv
			)
\end{code}


%************************************************************************
%*									*
\subsection{The stmt interface}
%*									*
%************************************************************************

\begin{code}
typecheckStmt
   :: DynFlags
   -> PersistentCompilerState
   -> HomeSymbolTable
   -> TypeEnv		   -- The interactive context's type envt 
   -> PrintUnqualified	   -- For error printing
   -> Module		   -- Is this really needed
   -> [Name]		   -- Names bound by the Stmt (empty for expressions)
   -> (RenamedStmt, 	   -- The stmt itself
       [RenamedHsDecl])	   -- Plus extra decls it sucked in from interface files
   -> IO (Maybe (PersistentCompilerState, 
		 TypecheckedHsExpr, 
		 [Id],
		 Type))
		-- The returned [Id] is the same as the input except for
		-- ExprStmt, in which case the returned [Name] is [itName]

typecheckStmt dflags pcs hst ic_type_env unqual this_mod names (stmt, iface_decls)
  = typecheck dflags pcs hst unqual $

 	 -- use the default default settings, i.e. [Integer, Double]
    tcSetDefaultTys defaultDefaultTys $

	-- Typecheck the extra declarations
    fixTc (\ ~(unf_env, _, _, _, _) ->
	tcImports unf_env pcs hst get_fixity this_mod iface_decls
    )			`thenTc` \ (env, new_pcs, local_inst_info, deriv_binds, local_rules) ->
    ASSERT( null local_inst_info && nullBinds deriv_binds && null local_rules )

    tcSetEnv env				$
    tcExtendGlobalTypeEnv ic_type_env		$

	-- The real work is done here
    tcUserStmt names stmt 		`thenTc` \ (expr, bound_ids) ->

    traceTc (text "tcs 1") `thenNF_Tc_`
    zonkExpr expr			`thenNF_Tc` \ zonked_expr ->
    mapNF_Tc zonkIdBndr bound_ids	`thenNF_Tc` \ zonked_ids ->

    ioToTc (dumpIfSet_dyn dflags Opt_D_dump_tc "Bound Ids" (vcat (map ppr zonked_ids)))	`thenNF_Tc_`
    ioToTc (dumpIfSet_dyn dflags Opt_D_dump_tc "Typechecked" (ppr zonked_expr))		`thenNF_Tc_`

    returnTc (new_pcs, zonked_expr, zonked_ids, error "typecheckStmt: no type")

  where
    get_fixity :: Name -> Maybe Fixity
    get_fixity n = pprPanic "typecheckStmt" (ppr n)
\end{code}

Here is the grand plan, implemented in tcUserStmt

	What you type			The IO [HValue] that hscStmt returns
	-------------			------------------------------------
	let pat = expr		==> 	let pat = expr in return [coerce HVal x, coerce HVal y, ...]
					bindings: [x,y,...]

	pat <- expr		==> 	expr >>= \ pat -> return [coerce HVal x, coerce HVal y, ...]
					bindings: [x,y,...]

	expr (of IO type)	==>	expr >>= \ v -> return [v]
	  [NB: result not printed]	bindings: [it]
	  

	expr (of non-IO type, 
	  result showable)	==>	let v = expr in print v >> return [v]
	  				bindings: [it]

	expr (of non-IO type, 
	  result not showable)	==>	error


\begin{code}
tcUserStmt :: [Name] -> RenamedStmt -> TcM (TypecheckedHsExpr, [Id])

tcUserStmt names (ExprStmt expr _ loc)
  = ASSERT( null names )
    tcGetUnique 		`thenNF_Tc` \ uniq ->
    let 
	fresh_it = itName uniq
        the_bind = FunMonoBind fresh_it False 
			[ mkSimpleMatch [] expr placeHolderType loc ] loc
    in
    tryTc_ (traceTc (text "tcs 1b") `thenNF_Tc_`
		tc_stmts [fresh_it] [
		    LetStmt (MonoBind the_bind [] NonRecursive),
		    ExprStmt (HsApp (HsVar printName) (HsVar fresh_it)) placeHolderType loc])
	   (    traceTc (text "tcs 1a") `thenNF_Tc_`
		tc_stmts [fresh_it] [BindStmt (VarPatIn fresh_it) expr loc])

tcUserStmt names stmt
  = tc_stmts names [stmt]
    

tc_stmts names stmts
  = tcLookupGlobalId returnIOName	`thenNF_Tc` \ return_id ->
    tcLookupGlobalId bindIOName		`thenNF_Tc` \ bind_id ->
    tcLookupGlobalId failIOName		`thenNF_Tc` \ fail_id ->
    tcLookupTyCon ioTyConName		`thenNF_Tc` \ ioTyCon ->
    newTyVarTy liftedTypeKind		`thenNF_Tc` \ res_ty ->
    let
	io_ty = (\ ty -> mkTyConApp ioTyCon [ty], res_ty)

		-- mk_return builds the expression
		--	returnIO @ [()] [coerce () x, ..,  coerce () z]
	mk_return ids = HsApp (TyApp (HsVar return_id) [mkListTy unitTy]) 
			      (ExplicitList unitTy (map mk_item ids))

	mk_item id = HsApp (TyApp (HsVar unsafeCoerceId) [idType id, unitTy])
		  	   (HsVar id)
    in

    traceTc (text "tcs 2") `thenNF_Tc_`
    tcStmtsAndThen combine (DoCtxt DoExpr) io_ty stmts	(
	-- Look up the names right in the middle,
	-- where they will all be in scope
	mapNF_Tc tcLookupId names			`thenNF_Tc` \ ids ->
	returnTc ((ids, [ResultStmt (mk_return ids) noSrcLoc]), emptyLIE)
    )							`thenTc` \ ((ids, tc_stmts), lie) ->

	-- Simplify the context right here, so that we fail
	-- if there aren't enough instances.  Notably, when we see
	--		e
	-- we use tryTc_ to try		it <- e
	-- and then			let it = e
	-- It's the simplify step that rejects the first.

    traceTc (text "tcs 3") `thenNF_Tc_`
    tcSimplifyTop lie			`thenTc` \ const_binds ->
    traceTc (text "tcs 4") `thenNF_Tc_`

    returnTc (mkHsLet const_binds $
	      HsDoOut DoExpr tc_stmts return_id bind_id fail_id 
		      (mkTyConApp ioTyCon [mkListTy unitTy]) noSrcLoc,
	      ids)
  where
    combine stmt (ids, stmts) = (ids, stmt:stmts)
\end{code}

%************************************************************************
%*									*
\subsection{Typechecking an expression}
%*									*
%************************************************************************

\begin{code}
typecheckExpr :: DynFlags
	      -> PersistentCompilerState
	      -> HomeSymbolTable
	      -> TypeEnv	   -- The interactive context's type envt 
	      -> PrintUnqualified	-- For error printing
	      -> Module
	      -> (RenamedHsExpr, 	-- The expression itself
	          [RenamedHsDecl])	-- Plus extra decls it sucked in from interface files
	      -> IO (Maybe (PersistentCompilerState, 
			    TypecheckedHsExpr, 
			    [Id],	-- always empty (matches typecheckStmt)
			    Type))

typecheckExpr dflags pcs hst ic_type_env unqual this_mod (expr, decls)
  = typecheck dflags pcs hst unqual $

 	 -- use the default default settings, i.e. [Integer, Double]
    tcSetDefaultTys defaultDefaultTys $

	-- Typecheck the extra declarations
    fixTc (\ ~(unf_env, _, _, _, _) ->
	tcImports unf_env pcs hst get_fixity this_mod decls
    )			`thenTc` \ (env, new_pcs, local_inst_info, deriv_binds, local_rules) ->
    ASSERT( null local_inst_info && nullBinds deriv_binds && null local_rules )

	-- Now typecheck the expression
    tcSetEnv env			$
    tcExtendGlobalTypeEnv ic_type_env	$

    newTyVarTy openTypeKind		`thenTc` \ ty ->
    tcMonoExpr expr ty 			`thenTc` \ (e', lie) ->
    tcSimplifyInfer smpl_doc (tyVarsOfType ty) lie 
		    		    	`thenTc` \ (qtvs, lie_free, dict_binds, dict_ids) ->
    tcSimplifyTop lie_free		`thenTc` \ const_binds ->

    let all_expr = mkHsLet const_binds	$
    		   TyLam qtvs  		$
    		   DictLam dict_ids	$
    		   mkHsLet dict_binds	$	
    		   e'

    	all_expr_ty = mkForAllTys qtvs 	$
    		      mkFunTys (map idType dict_ids) $
    		      ty
    in

    zonkExpr all_expr				`thenNF_Tc` \ zonked_expr ->
    zonkTcType all_expr_ty			`thenNF_Tc` \ zonked_ty ->
    ioToTc (dumpIfSet_dyn dflags 
		Opt_D_dump_tc "Typechecked" (ppr zonked_expr)) `thenNF_Tc_`
    returnTc (new_pcs, zonked_expr, [], zonked_ty) 

  where
    get_fixity :: Name -> Maybe Fixity
    get_fixity n = pprPanic "typecheckExpr" (ppr n)

    smpl_doc = ptext SLIT("main expression")
\end{code}

%************************************************************************
%*									*
\subsection{Typechecking extra declarations}
%*									*
%************************************************************************

\begin{code}
typecheckExtraDecls 
   :: DynFlags
   -> PersistentCompilerState
   -> HomeSymbolTable
   -> PrintUnqualified	   -- For error printing
   -> Module		   -- Is this really needed
   -> [RenamedHsDecl]	   -- extra decls sucked in from interface files
   -> IO (Maybe PersistentCompilerState)

typecheckExtraDecls  dflags pcs hst unqual this_mod decls
 = typecheck dflags pcs hst unqual $
     fixTc (\ ~(unf_env, _, _, _, _) ->
     	  tcImports unf_env pcs hst get_fixity this_mod decls
     )	`thenTc` \ (env, new_pcs, local_inst_info, deriv_binds, local_rules) ->
     ASSERT( null local_inst_info && nullBinds deriv_binds && null local_rules )
     returnTc new_pcs
 where
    get_fixity n = pprPanic "typecheckExpr" (ppr n)
\end{code}

%************************************************************************
%*									*
\subsection{Typechecking a module}
%*									*
%************************************************************************

\begin{code}
typecheckModule
	:: DynFlags
	-> PersistentCompilerState
	-> HomeSymbolTable
	-> ModIface		-- Iface for this module
	-> PrintUnqualified	-- For error printing
	-> [RenamedHsDecl]
	-> IO (Maybe (PersistentCompilerState, TcResults))
			-- The new PCS is Augmented with imported information,
						-- (but not stuff from this module)

data TcResults
  = TcResults {
	-- All these fields have info *just for this module*
	tc_env	   :: TypeEnv,			-- The top level TypeEnv
	tc_insts   :: [DFunId],			-- Instances 
	tc_rules   :: [TypecheckedRuleDecl],	-- Transformation rules
	tc_binds   :: TypecheckedMonoBinds,	-- Bindings
	tc_fords   :: [TypecheckedForeignDecl]	-- Foreign import & exports.
    }


typecheckModule dflags pcs hst mod_iface unqual decls
  = do	{ maybe_tc_result <- typecheck dflags pcs hst unqual $
			     tcModule pcs hst get_fixity this_mod decls
	; printTcDump dflags unqual maybe_tc_result
	; return maybe_tc_result }
  where
    this_mod   = mi_module   mod_iface
    fixity_env = mi_fixities mod_iface

    get_fixity :: Name -> Maybe Fixity
    get_fixity nm = lookupNameEnv fixity_env nm


tcModule :: PersistentCompilerState
	 -> HomeSymbolTable
	 -> (Name -> Maybe Fixity)
	 -> Module
	 -> [RenamedHsDecl]
	 -> TcM (PersistentCompilerState, TcResults)

tcModule pcs hst get_fixity this_mod decls
  = fixTc (\ ~(unf_env, _, _) ->
		-- Loop back the final environment, including the fully zonked
		-- versions of bindings from this module.  In the presence of mutual
		-- recursion, interface type signatures may mention variables defined
		-- in this module, which is why the knot is so big

		-- Type-check the type and class decls, and all imported decls
	tcImports unf_env pcs hst get_fixity this_mod decls	
				`thenTc` \ (env, new_pcs, local_insts, deriv_binds, local_rules) ->

    	tcSetEnv env				$

        -- Foreign import declarations next
        traceTc (text "Tc4")			`thenNF_Tc_`
	tcForeignImports decls			`thenTc`    \ (fo_ids, foi_decls) ->
	tcExtendGlobalValEnv fo_ids		$
    
	-- Default declarations
	tcDefaults decls			`thenTc` \ defaulting_tys ->
	tcSetDefaultTys defaulting_tys 		$
	
	-- Value declarations next.
	-- We also typecheck any extra binds that came out of the "deriving" process
	traceTc (text "Default types" <+> ppr defaulting_tys)	`thenNF_Tc_`
        traceTc (text "Tc5")				`thenNF_Tc_`
	tcTopBinds (val_binds `ThenBinds` deriv_binds)	`thenTc` \ ((val_binds, env), lie_valdecls) ->
	
     	-- Second pass over class and instance declarations, 
	-- plus rules and foreign exports, to generate bindings
	tcSetEnv env				$
	tcInstDecls2  local_insts		`thenNF_Tc` \ (lie_instdecls, inst_binds) ->
	tcClassDecls2 this_mod tycl_decls	`thenNF_Tc` \ (lie_clasdecls, cls_dm_binds) ->
	tcForeignExports decls			`thenTc`    \ (lie_fodecls,   foe_binds, foe_decls) ->
	tcSourceRules source_rules		`thenNF_Tc` \ (lie_rules,     more_local_rules) ->
	
		-- CHECK THAT main IS DEFINED WITH RIGHT TYPE, IF REQUIRED
        traceTc (text "Tc6")			`thenNF_Tc_`
	tcCheckMain this_mod			`thenTc_`

	     -- Deal with constant or ambiguous InstIds.  How could
	     -- there be ambiguous ones?  They can only arise if a
	     -- top-level decl falls under the monomorphism
	     -- restriction, and no subsequent decl instantiates its
	     -- type.  (Usually, ambiguous type variables are resolved
	     -- during the generalisation step.)
	     --
	     -- Note that we must do this *after* tcCheckMain, because of the
	     -- following bizarre case: 
	     --		main = return ()
	     -- Here, we infer main :: forall a. m a, where m is a free
	     -- type variable.  tcCheckMain will unify it with IO, and that
	     -- must happen before tcSimplifyTop, since the latter will report
	     -- m as ambiguous
	let
	    lie_alldecls = lie_valdecls	 `plusLIE`
			   lie_instdecls `plusLIE`
			   lie_clasdecls `plusLIE`
			   lie_fodecls	 `plusLIE`
			   lie_rules
	in
	tcSimplifyTop lie_alldecls	`thenTc` \ const_inst_binds ->
        traceTc (text "endsimpltop") `thenTc_`
	
	    -- Backsubstitution.    This must be done last.
	    -- Even tcSimplifyTop may do some unification.
	let
	    all_binds = val_binds		`AndMonoBinds`
			    inst_binds		`AndMonoBinds`
			    cls_dm_binds	`AndMonoBinds`
			    const_inst_binds	`AndMonoBinds`
			    foe_binds
	in
 	traceTc (text "Tc7")		`thenNF_Tc_`
	zonkTopBinds all_binds		`thenNF_Tc` \ (all_binds', final_env)  ->
	tcSetEnv final_env		$
		-- zonkTopBinds puts all the top-level Ids into the tcGEnv
 	traceTc (text "Tc8")		`thenNF_Tc_`
	zonkForeignExports foe_decls	`thenNF_Tc` \ foe_decls' ->
 	traceTc (text "Tc9")		`thenNF_Tc_`
	zonkRules more_local_rules	`thenNF_Tc` \ more_local_rules' ->
	
	
	let	local_things = filter (isLocalThing this_mod) (typeEnvElts (getTcGEnv final_env))
	
		local_type_env :: TypeEnv
		local_type_env = mkTypeEnv local_things
		    
		all_local_rules = local_rules ++ more_local_rules'
	in  
	traceTc (text "Tc10")		`thenNF_Tc_`
	returnTc (final_env,
		  new_pcs,
		  TcResults { tc_env     = local_type_env,
			      tc_insts   = map iDFunId local_insts,
			      tc_binds   = all_binds', 
			      tc_fords   = foi_decls ++ foe_decls',
			      tc_rules   = all_local_rules
			    }
	)
    )			`thenTc` \ (_, pcs, tc_result) ->
    returnTc (pcs, tc_result)
  where
    tycl_decls   = [d | TyClD d <- decls]
    val_binds    = foldr ThenBinds EmptyBinds [binds | ValD binds <- decls]
    source_rules = [d | RuleD d <- decls, not (isIfaceRuleDecl d)]
\end{code}


%************************************************************************
%*									*
\subsection{Typechecking interface decls}
%*									*
%************************************************************************

\begin{code}
typecheckIface
	:: DynFlags
	-> PersistentCompilerState
	-> HomeSymbolTable
	-> ModIface		-- Iface for this module (just module & fixities)
	-> [RenamedHsDecl]
	-> IO (Maybe (PersistentCompilerState, ModDetails))
			-- The new PCS is Augmented with imported information,
			-- (but not stuff from this module).

typecheckIface dflags pcs hst mod_iface decls
  = do	{ maybe_tc_stuff <- typecheck dflags pcs hst alwaysQualify $
			    tcIfaceImports pcs hst get_fixity this_mod decls
	; printIfaceDump dflags maybe_tc_stuff
	; return maybe_tc_stuff }
  where
    this_mod   = mi_module   mod_iface
    fixity_env = mi_fixities mod_iface

    get_fixity :: Name -> Maybe Fixity
    get_fixity nm = lookupNameEnv fixity_env nm

    tcIfaceImports pcs hst get_fixity this_mod decls
	= fixTc (\ ~(unf_env, _, _, _, _) ->
	      tcImports unf_env pcs hst get_fixity this_mod decls
          )	`thenTc` \ (env, new_pcs, local_inst_info, 
			    deriv_binds, local_rules) ->
	  ASSERT(nullBinds deriv_binds)
	  let 
	      local_things = filter (isLocalThing this_mod) (typeEnvElts (getTcGEnv env))

	      mod_details = ModDetails { md_types = mkTypeEnv local_things,
					 md_insts = map iDFunId local_inst_info,
					 md_rules = [(id,rule) | IfaceRuleOut id rule <- local_rules],
					 md_binds = [] }
			-- All the rules from an interface are of the IfaceRuleOut form
	  in
          returnTc (new_pcs, mod_details)

tcImports :: RecTcEnv
	  -> PersistentCompilerState
	  -> HomeSymbolTable
	  -> (Name -> Maybe Fixity)
	  -> Module
	  -> [RenamedHsDecl]
	  -> TcM (TcEnv, PersistentCompilerState, [InstInfo], 
			 RenamedHsBinds, [TypecheckedRuleDecl])

-- tcImports is a slight mis-nomer.  
-- It deals with everything that could be an import:
--	type and class decls
--	interface signatures (checked lazily)
--	instance decls
--	rule decls
-- These can occur in source code too, of course

tcImports unf_env pcs hst get_fixity this_mod decls
   	  -- (unf_env :: RecTcEnv) is used for type-checking interface pragmas
	  -- which is done lazily [ie failure just drops the pragma
	  -- without having any global-failure effect].
	  -- 
	  -- unf_env is also used to get the pragama info
	  -- for imported dfuns and default methods

  = checkNoErrsTc $
	-- tcImports recovers internally, but if anything gave rise to
	-- an error we'd better stop now, to avoid a cascade
	
    traceTc (text "Tc1")				`thenNF_Tc_`
    tcTyAndClassDecls unf_env this_mod tycl_decls	`thenTc` \ env ->
    tcSetEnv env 					$
    
    	-- Typecheck the instance decls, includes deriving
    traceTc (text "Tc2")	`thenNF_Tc_`
    tcInstDecls1 (pcs_insts pcs) (pcs_PRS pcs) 
    	     hst unf_env get_fixity this_mod 
    	     decls			`thenTc` \ (new_pcs_insts, inst_env, local_insts, deriv_binds) ->
    tcSetInstEnv inst_env			$
    
    -- Interface type signatures
    -- We tie a knot so that the Ids read out of interfaces are in scope
    --   when we read their pragmas.
    -- What we rely on is that pragmas are typechecked lazily; if
    --   any type errors are found (ie there's an inconsistency)
    --   we silently discard the pragma
    traceTc (text "Tc3")			`thenNF_Tc_`
    tcInterfaceSigs unf_env this_mod tycl_decls	`thenTc` \ sig_ids ->
    tcExtendGlobalValEnv sig_ids		$
    
    
    tcIfaceRules unf_env (pcs_rules pcs) this_mod iface_rules	`thenNF_Tc` \ (new_pcs_rules, local_rules) ->
    	-- When relinking this module from its interface-file decls
    	-- we'll have IfaceRules that are in fact local to this module
    	-- That's the reason we we get any local_rules out here
    
    tcGetEnv						`thenTc` \ unf_env ->
    let
        all_things = typeEnvElts (getTcGEnv unf_env)
    
         -- sometimes we're compiling in the context of a package module
         -- (on the GHCi command line, for example).  In this case, we
         -- want to treat everything we pulled in as an imported thing.
        imported_things
       	  = filter (not . isLocalThing this_mod) all_things
        
        new_pte :: PackageTypeEnv
        new_pte = extendTypeEnvList (pcs_PTE pcs) imported_things
        
        new_pcs :: PersistentCompilerState
        new_pcs = pcs { pcs_PTE   = new_pte,
    		        pcs_insts = new_pcs_insts,
    		        pcs_rules = new_pcs_rules
      	          }
    in
    returnTc (unf_env, new_pcs, local_insts, deriv_binds, local_rules)
  where
    tycl_decls  = [d | TyClD d <- decls]
    iface_rules = [d | RuleD d <- decls, isIfaceRuleDecl d]
\end{code}    


%************************************************************************
%*									*
\subsection{Checking the type of main}
%*									*
%************************************************************************

We must check that in module Main,
	a) main is defined
	b) main :: forall a1...an. IO t,  for some type t

If we have
	main = error "Urk"
then the type of main will be 
	main :: forall a. a
and that should pass the test too.  

So we just instantiate the type and unify with IO t, and declare 
victory if doing so succeeds.

\begin{code}
tcCheckMain :: Module -> TcM ()
tcCheckMain this_mod
  | not (moduleName this_mod == mAIN_Name )
  = returnTc ()

  | otherwise
  =	-- First unify the main_id with IO t, for any old t
    tcLookup_maybe mainName		`thenNF_Tc` \ maybe_thing ->
    case maybe_thing of
	Just (ATcId main_id) -> check_main_ty (idType main_id)
	other		     -> addErrTc noMainErr	
  where
    check_main_ty main_ty
      = tcInstType main_ty		`thenNF_Tc` \ (tvs, theta, main_tau) ->
	newTyVarTy liftedTypeKind	`thenNF_Tc` \ arg_ty ->
	tcLookupTyCon ioTyConName	`thenNF_Tc` \ ioTyCon ->
	tcAddErrCtxtM (mainTypeCtxt main_ty)	$
	if not (null theta) then 
		failWithTc empty	-- Context has the error message
	else
	unifyTauTy main_tau (mkTyConApp ioTyCon [arg_ty])

mainTypeCtxt main_ty tidy_env 
  = zonkTcType main_ty		`thenNF_Tc` \ main_ty' ->
    returnNF_Tc (tidy_env, ptext SLIT("`main' has the illegal type") <+> 
	 		 	 quotes (ppr (tidyType tidy_env main_ty')))

noMainErr = hsep [ptext SLIT("Module") <+> quotes (ppr mAIN_Name), 
	  	  ptext SLIT("must include a definition for") <+> quotes (ptext SLIT("main"))]
\end{code}


%************************************************************************
%*									*
\subsection{Interfacing the Tc monad to the IO monad}
%*									*
%************************************************************************

\begin{code}
typecheck :: DynFlags
	  -> PersistentCompilerState
	  -> HomeSymbolTable
	  -> PrintUnqualified	-- For error printing
	  -> TcM r
	  -> IO (Maybe r)

typecheck dflags pcs hst unqual thing_inside 
 = do	{ showPass dflags "Typechecker";
	; env <- initTcEnv hst (pcs_PTE pcs)

	; (maybe_tc_result, errs) <- initTc dflags env thing_inside

	; printErrorsAndWarnings unqual errs

	; if errorsFound errs then 
             return Nothing 
           else 
             return maybe_tc_result
	}
\end{code}


%************************************************************************
%*									*
\subsection{Dumping output}
%*									*
%************************************************************************

\begin{code}
printTcDump dflags unqual Nothing = return ()
printTcDump dflags unqual (Just (_, results))
  = do if dopt Opt_D_dump_types dflags || dopt Opt_D_dump_tc dflags then
	  printForUser stdout unqual (dump_tc_iface dflags results)
          else return ()

       dumpIfSet_dyn dflags Opt_D_dump_tc    
                     "Typechecked" (ppr (tc_binds results))

	  
printIfaceDump dflags Nothing = return ()
printIfaceDump dflags (Just (_, details))
  = dumpIfSet_dyn_or dflags [Opt_D_dump_types, Opt_D_dump_tc]
                     "Interface" (pprModDetails details)

dump_tc_iface dflags results
  = vcat [pprModDetails (ModDetails {md_types = tc_env results, 
				     md_insts = tc_insts results,
				     md_rules = [], md_binds = []}) ,
	  ppr_rules (tc_rules results),

	  if dopt Opt_Generics dflags then
		ppr_gen_tycons (typeEnvTyCons (tc_env results))
	  else 
		empty
    ]

ppr_rules [] = empty
ppr_rules rs = vcat [ptext SLIT("{-# RULES"),
		      nest 4 (vcat (map ppr rs)),
		      ptext SLIT("#-}")]

ppr_gen_tycons tcs = vcat [ptext SLIT("{-# Generic type constructor details"),
			   vcat (map ppr_gen_tycon tcs),
		   	   ptext SLIT("#-}")
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
