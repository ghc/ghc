%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcModule]{Typechecking a whole module}

\begin{code}
module TcModule (
	typecheckModule,
	TcResults(..)
    ) where

#include "HsVersions.h"

import CmdLineOpts	( DynFlag(..), DynFlags, opt_PprStyle_Debug )
import HsSyn		( HsModule(..), HsBinds(..), MonoBinds(..), HsDecl(..) )
import HsTypes		( toHsType )
import RnHsSyn		( RenamedHsModule, RenamedHsDecl )
import TcHsSyn		( TypecheckedMonoBinds, 
			  TypecheckedForeignDecl, TypecheckedRuleDecl,
			  zonkTopBinds, zonkForeignExports, zonkRules
			)

import TcMonad
import Inst		( emptyLIE, plusLIE )
import TcBinds		( tcTopBinds )
import TcClassDcl	( tcClassDecls2, mkImplicitClassBinds )
import TcDefaults	( tcDefaults )
import TcEnv		( TcEnv, tcExtendGlobalValEnv, tcLookupGlobal_maybe,
			  tcEnvTyCons, tcEnvClasses, 
			  tcSetEnv, tcSetInstEnv, initTcEnv, getTcGEnv
			)
import TcRules		( tcRules )
import TcForeign	( tcForeignImports, tcForeignExports )
import TcIfaceSig	( tcInterfaceSigs )
import TcInstDcls	( tcInstDecls1, tcInstDecls2 )
import TcInstUtil	( InstInfo(..) )
import TcSimplify	( tcSimplifyTop )
import TcTyClsDecls	( tcTyAndClassDecls )
import TcTyDecls	( mkImplicitDataBinds )

import CoreUnfold	( unfoldingTemplate )
import Type		( funResultTy, splitForAllTys )
import Bag		( isEmptyBag )
import ErrUtils		( printErrorsAndWarnings, dumpIfSet_dyn )
import Id		( idType, idName, idUnfolding )
import Module           ( Module, moduleName, {-mkThisModule,-} plusModuleEnv )
import Name		( nameOccName, isLocallyDefined, isGlobalName,
			  toRdrName, nameEnvElts, emptyNameEnv
			)
import TyCon		( TyCon, isDataTyCon, tyConName, tyConGenInfo )
import OccName		( isSysOcc )
import TyCon		( TyCon, isClassTyCon )
import Class		( Class )
import PrelNames	( mAIN_Name, mainName )
import UniqSupply       ( UniqSupply )
import Maybes		( maybeToBool )
import Util
import BasicTypes       ( EP(..) )
import Bag		( Bag, isEmptyBag )
import Outputable
import HscTypes		( PersistentCompilerState(..), HomeSymbolTable, 
			  PackageSymbolTable, DFunId, 
			  TypeEnv, extendTypeEnv,
		          TyThing(..), groupTyThings )
import FiniteMap	( FiniteMap, delFromFM, lookupWithDefaultFM )
\end{code}

Outside-world interface:
\begin{code}

-- Convenient type synonyms first:
data TcResults
  = TcResults {
	tc_pcs	   :: PersistentCompilerState,	-- Augmented with imported information,
						-- (but not stuff from this module)
	tc_env	   :: TypeEnv,			-- The TypeEnv just for the stuff from this module
	tc_insts   :: [DFunId],			-- Instances, just for this module
	tc_binds   :: TypecheckedMonoBinds,
	tc_fords   :: [TypecheckedForeignDecl], -- Foreign import & exports.
	tc_rules   :: [TypecheckedRuleDecl]	-- Transformation rules
    }

---------------
typecheckModule
	:: DynFlags
	-> PersistentCompilerState
	-> HomeSymbolTable
	-> RenamedHsModule
	-> IO (Maybe (TcEnv, TcResults))

typecheckModule dflags pcs hst (HsModule mod_name _ _ _ decls _ src_loc)
  = do env <- initTcEnv global_symbol_table
       (maybe_result, (errs,warns)) <- initTc dflags env src_loc tc_module
       printErrorsAndWarnings (errs,warns)
       printTcDump dflags maybe_result
       if isEmptyBag errs then 
          return Nothing 
         else 
          return maybe_result
  where
    this_mod	        = panic "mkThisModule: unimp"  -- WAS: mkThisModule
    global_symbol_table = pcs_PST pcs `plusModuleEnv` hst

    tc_module = fixTc (\ ~(unf_env ,_) -> tcModule pcs hst this_mod decls unf_env)
\end{code}

The internal monster:
\begin{code}
tcModule :: PersistentCompilerState
	 -> HomeSymbolTable
	 -> Module
	 -> [RenamedHsDecl]
	 -> TcEnv		-- The knot-tied environment
	 -> TcM (TcEnv, TcResults)

  -- (unf_env :: TcEnv) is used for type-checking interface pragmas
  -- which is done lazily [ie failure just drops the pragma
  -- without having any global-failure effect].
  -- 
  -- unf_env is also used to get the pragama info
  -- for imported dfuns and default methods

tcModule pcs hst this_mod decls unf_env
  = 		 -- Type-check the type and class decls
    tcTyAndClassDecls unf_env decls		`thenTc` \ env ->
    tcSetEnv env 				$
    let
        classes       = tcEnvClasses env
        tycons        = tcEnvTyCons env	-- INCLUDES tycons derived from classes
        local_classes = filter isLocallyDefined classes
        local_tycons  = [ tc | tc <- tycons,
    			       isLocallyDefined tc,
    			       not (isClassTyCon tc)
  	  	        ]
    			-- For local_tycons, filter out the ones derived from classes
    			-- Otherwise the latter show up in interface files
    in
    
    	-- Typecheck the instance decls, includes deriving
    tcInstDecls1 pcs hst unf_env this_mod 
		 local_tycons decls		`thenTc` \ (pcs_with_insts, inst_env, inst_info, deriv_binds) ->
    tcSetInstEnv inst_env			$
    
        -- Default declarations
    tcDefaults decls			`thenTc` \ defaulting_tys ->
    tcSetDefaultTys defaulting_tys 	$
    
    -- Interface type signatures
    -- We tie a knot so that the Ids read out of interfaces are in scope
    --   when we read their pragmas.
    -- What we rely on is that pragmas are typechecked lazily; if
    --   any type errors are found (ie there's an inconsistency)
    --   we silently discard the pragma
    -- We must do this before mkImplicitDataBinds (which comes next), since
    -- the latter looks up unpackCStringId, for example, which is usually 
    -- imported
    tcInterfaceSigs unf_env decls		`thenTc` \ sig_ids ->
    tcExtendGlobalValEnv sig_ids		$
    
    -- Create any necessary record selector Ids and their bindings
    -- "Necessary" includes data and newtype declarations
    -- We don't create bindings for dictionary constructors;
    -- they are always fully applied, and the bindings are just there
    -- to support partial applications
    mkImplicitDataBinds tycons			`thenTc`    \ (data_ids, imp_data_binds) ->
    mkImplicitClassBinds classes		`thenNF_Tc` \ (cls_ids,  imp_cls_binds) ->
    
    -- Extend the global value environment with 
    --	(a) constructors
    --	(b) record selectors
    --	(c) class op selectors
    -- 	(d) default-method ids... where? I can't see where these are
    --	    put into the envt, and I'm worried that the zonking phase
    --	    will find they aren't there and complain.
    tcExtendGlobalValEnv data_ids		$
    tcExtendGlobalValEnv cls_ids		$
    
        -- Foreign import declarations next
    tcForeignImports decls			`thenTc`    \ (fo_ids, foi_decls) ->
    tcExtendGlobalValEnv fo_ids			$
    
    -- Value declarations next.
    -- We also typecheck any extra binds that came out of the "deriving" process
    tcTopBinds (get_binds decls `ThenBinds` deriv_binds)	`thenTc` \ ((val_binds, env), lie_valdecls) ->
    tcSetEnv env $
    
        -- Foreign export declarations next
    tcForeignExports decls		`thenTc`    \ (lie_fodecls, foe_binds, foe_decls) ->
    
    	-- Second pass over class and instance declarations,
    	-- to compile the bindings themselves.
    tcInstDecls2  inst_info		`thenNF_Tc` \ (lie_instdecls, inst_binds) ->
    tcClassDecls2 decls			`thenNF_Tc` \ (lie_clasdecls, cls_dm_binds) ->
    tcRules decls			`thenNF_Tc` \ (lie_rules,     rules) ->
    
         -- Deal with constant or ambiguous InstIds.  How could
         -- there be ambiguous ones?  They can only arise if a
         -- top-level decl falls under the monomorphism
         -- restriction, and no subsequent decl instantiates its
         -- type.  (Usually, ambiguous type variables are resolved
         -- during the generalisation step.)
    let
        lie_alldecls = lie_valdecls	`plusLIE`
    		   lie_instdecls	`plusLIE`
    		   lie_clasdecls	`plusLIE`
    		   lie_fodecls		`plusLIE`
    		   lie_rules
    in
    tcSimplifyTop lie_alldecls			`thenTc` \ const_inst_binds ->
    
    	-- Check that Main defines main
    checkMain this_mod				`thenTc_`
    
        -- Backsubstitution.    This must be done last.
        -- Even tcSimplifyTop may do some unification.
    let
        all_binds = imp_data_binds 	`AndMonoBinds` 
    		    imp_cls_binds	`AndMonoBinds` 
    		    val_binds		`AndMonoBinds`
    	            inst_binds		`AndMonoBinds`
    	            cls_dm_binds	`AndMonoBinds`
    	            const_inst_binds	`AndMonoBinds`
    		    foe_binds
    in
    zonkTopBinds all_binds		`thenNF_Tc` \ (all_binds', final_env)  ->
    tcSetEnv final_env			$
    	-- zonkTopBinds puts all the top-level Ids into the tcGEnv
    zonkForeignExports foe_decls	`thenNF_Tc` \ foe_decls' ->
    zonkRules rules			`thenNF_Tc` \ rules' ->
    
    
    let	groups :: FiniteMap Module TypeEnv
    	groups = groupTyThings (nameEnvElts (getTcGEnv final_env))
    
    	local_type_env :: TypeEnv
    	local_type_env = lookupWithDefaultFM groups emptyNameEnv this_mod 
    
    	new_pst :: PackageSymbolTable
    	new_pst = extendTypeEnv (pcs_PST pcs) (delFromFM groups this_mod)

	final_pcs :: PersistentCompilerState
	final_pcs = pcs_with_insts {pcs_PST = new_pst}
    in  
    returnTc (final_env, -- WAS: really_final_env, 
	      TcResults { tc_pcs     = final_pcs,
			  tc_env     = local_type_env,
			  tc_binds   = all_binds', 
			  tc_insts   = map iDFunId inst_info,
			  tc_fords   = foi_decls ++ foe_decls',
			  tc_rules   = rules'
                        })

get_binds decls = foldr ThenBinds EmptyBinds [binds | ValD binds <- decls]
\end{code}


\begin{code}
checkMain :: Module -> TcM ()
checkMain this_mod 
  | moduleName this_mod == mAIN_Name 
  = tcLookupGlobal_maybe mainName		`thenNF_Tc` \ maybe_main ->
    case maybe_main of
	Just (AnId _) -> returnTc ()
	other	      -> addErrTc noMainErr

  | otherwise = returnTc ()

noMainErr
  = hsep [ptext SLIT("Module"), quotes (ppr mAIN_Name), 
	  ptext SLIT("must include a definition for"), quotes (ptext SLIT("main"))]
\end{code}


%************************************************************************
%*									*
\subsection{Dumping output}
%*									*
%************************************************************************

\begin{code}
printTcDump dflags Nothing = return ()
printTcDump dflags (Just (_,results))
  = do dumpIfSet_dyn dflags Opt_D_dump_types 
                     "Type signatures" (dump_sigs results)
       dumpIfSet_dyn dflags Opt_D_dump_tc    
                     "Typechecked" (dump_tc results) 

dump_tc results
  = vcat [ppr (tc_binds results),
	  pp_rules (tc_rules results) --,
--	  ppr_gen_tycons (tc_tycons results)
    ]

dump_sigs results	-- Print type signatures
  = 	-- Convert to HsType so that we get source-language style printing
	-- And sort by RdrName
    vcat $ map ppr_sig $ sortLt lt_sig $
    [(toRdrName id, toHsType (idType id))
        | AnId id <- nameEnvElts (tc_env results), 
          want_sig id
    ]
  where
    lt_sig (n1,_) (n2,_) = n1 < n2
    ppr_sig (n,t)        = ppr n <+> dcolon <+> ppr t

    want_sig id | opt_PprStyle_Debug = True
	        | otherwise	     = isLocallyDefined n && 
				       isGlobalName n && 
				       not (isSysOcc (nameOccName n))
				     where
				       n = idName id

ppr_gen_tycons tcs = vcat [ptext SLIT("{-# Generic type constructor details"),
			   vcat (map ppr_gen_tycon (filter isLocallyDefined tcs)),
		   	   ptext SLIT("#-}")
		     ]

-- x&y are now Id's, not CoreExpr's 
ppr_gen_tycon tycon 
  | Just ep <- tyConGenInfo tycon
  = (ppr tycon <> colon) $$ nest 4 (ppr_ep ep)

  | otherwise = ppr tycon <> colon <+> ptext SLIT("Not derivable")

ppr_ep (EP from to)
  = vcat [ ptext SLIT("Rep type:") <+> ppr (funResultTy from_tau),
	   ptext SLIT("From:") <+> ppr (unfoldingTemplate (idUnfolding from)),
	   ptext SLIT("To:")   <+> ppr (unfoldingTemplate (idUnfolding to))
    ]
  where
    (_,from_tau) = splitForAllTys (idType from)

pp_rules [] = empty
pp_rules rs = vcat [ptext SLIT("{-# RULES"),
		    nest 4 (vcat (map ppr rs)),
		    ptext SLIT("#-}")]
\end{code}
