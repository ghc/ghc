%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcModule]{Typechecking a whole module}

\begin{code}
module TcModule (
	typecheckModule, typecheckExpr, TcResults(..)
    ) where

#include "HsVersions.h"

import CmdLineOpts	( DynFlag(..), DynFlags, opt_PprStyle_Debug )
import HsSyn		( HsBinds(..), MonoBinds(..), HsDecl(..) )
import HsTypes		( toHsType )
import RnHsSyn		( RenamedHsDecl, RenamedHsExpr )
import TcHsSyn		( TypecheckedMonoBinds, TypecheckedHsExpr,
			  TypecheckedForeignDecl, TypecheckedRuleDecl,
			  zonkTopBinds, zonkForeignExports, zonkRules, mkHsLet
			)


import TcMonad
import TcType		( newTyVarTy )
import Inst		( plusLIE )
import TcBinds		( tcTopBinds )
import TcClassDcl	( tcClassDecls2, mkImplicitClassBinds )
import TcDefaults	( tcDefaults )
import TcExpr		( tcMonoExpr )
import TcEnv		( TcEnv, InstInfo(iDFunId), tcExtendGlobalValEnv, 
			  tcEnvTyCons, tcEnvClasses,  isLocalThing,
			  tcSetEnv, tcSetInstEnv, initTcEnv, getTcGEnv
			)
import TcRules		( tcRules )
import TcForeign	( tcForeignImports, tcForeignExports )
import TcIfaceSig	( tcInterfaceSigs )
import TcInstDcls	( tcInstDecls1, tcInstDecls2 )
import TcSimplify	( tcSimplifyTop )
import TcTyClsDecls	( tcTyAndClassDecls )
import TcTyDecls	( mkImplicitDataBinds )

import CoreUnfold	( unfoldingTemplate )
import Type		( funResultTy, splitForAllTys, openTypeKind )
import Bag		( isEmptyBag )
import ErrUtils		( printErrorsAndWarnings, dumpIfSet_dyn, showPass )
import Id		( idType, idUnfolding )
import Module           ( Module )
import Name		( Name, toRdrName )
import Name		( nameEnvElts, lookupNameEnv )
import TyCon		( tyConGenInfo )
import Util
import BasicTypes       ( EP(..), Fixity )
import Bag		( isEmptyBag )
import Outputable
import HscTypes		( PersistentCompilerState(..), HomeSymbolTable, 
			  PackageTypeEnv, DFunId, ModIface(..),
			  TypeEnv, extendTypeEnvList, 
		          TyThing(..), mkTypeEnv )
import List		( partition )
\end{code}

Outside-world interface:
\begin{code}

-- Convenient type synonyms first:
data TcResults
  = TcResults {
	tc_pcs	   :: PersistentCompilerState,	-- Augmented with imported information,
						-- (but not stuff from this module)

	-- All these fields have info *just for this module*
	tc_env	   :: TypeEnv,			-- The top level TypeEnv
	tc_insts   :: [DFunId],			-- Instances
	tc_binds   :: TypecheckedMonoBinds,	-- Bindings
	tc_fords   :: [TypecheckedForeignDecl], -- Foreign import & exports.
	tc_rules   :: [TypecheckedRuleDecl]	-- Transformation rules
    }

---------------
typecheckModule
	:: DynFlags
	-> Module
	-> PersistentCompilerState
	-> HomeSymbolTable
	-> ModIface		-- Iface for this module
	-> PrintUnqualified	-- For error printing
	-> [RenamedHsDecl]
	-> IO (Maybe TcResults)

typecheckModule dflags this_mod pcs hst mod_iface unqual decls
  = do	{ maybe_tc_result <- typecheck dflags pcs hst unqual $
			    tcModule pcs hst get_fixity this_mod decls
	; printTcDump dflags maybe_tc_result
	; return maybe_tc_result }
  where
    fixity_env = mi_fixities mod_iface

    get_fixity :: Name -> Maybe Fixity
    get_fixity nm = lookupNameEnv fixity_env nm

---------------
typecheckExpr :: DynFlags
	      -> PersistentCompilerState
	      -> HomeSymbolTable
	      -> PrintUnqualified	-- For error printing
	      -> RenamedHsExpr
	      -> IO (Maybe TypecheckedHsExpr)

typecheckExpr dflags pcs hst unqual expr
  = typecheck dflags pcs hst unqual $
    newTyVarTy openTypeKind	`thenTc` \ ty ->
    tcMonoExpr expr ty		`thenTc` \ (expr', lie) ->
    tcSimplifyTop lie		`thenTc` \ binds ->
    returnTc (mkHsLet binds expr') 

---------------
typecheck :: DynFlags
	  -> PersistentCompilerState
	  -> HomeSymbolTable
	  -> PrintUnqualified	-- For error printing
	  -> TcM r
	  -> IO (Maybe r)

typecheck dflags pcs hst unqual thing_inside
  = do	{ showPass dflags "Typechecker";
	; env <- initTcEnv hst (pcs_PTE pcs)

	; (maybe_tc_result, (warns,errs)) <- initTc dflags env thing_inside

	; printErrorsAndWarnings unqual (errs,warns)

	; if isEmptyBag errs then 
             return maybe_tc_result
           else 
             return Nothing 
	}
\end{code}

The internal monster:
\begin{code}
tcModule :: PersistentCompilerState
	 -> HomeSymbolTable
	 -> (Name -> Maybe Fixity)
	 -> Module
	 -> [RenamedHsDecl]
	 -> TcM TcResults

tcModule pcs hst get_fixity this_mod decls
  = 		 -- Type-check the type and class decls
    fixTc (\ ~(unf_env, _, _, _, _) -> 
	  -- (unf_env :: RecTcEnv) is used for type-checking interface pragmas
	  -- which is done lazily [ie failure just drops the pragma
	  -- without having any global-failure effect].
	  -- 
	  -- unf_env is also used to get the pragama info
	  -- for imported dfuns and default methods
		
--	traceTc (text "Tc1")			`thenNF_Tc_`
	tcTyAndClassDecls unf_env decls		`thenTc` \ env ->
	tcSetEnv env 				$
	let
	    classes = tcEnvClasses env
	    tycons  = tcEnvTyCons env	-- INCLUDES tycons derived from classes
	in
	
		-- Typecheck the instance decls, includes deriving
--	traceTc (text "Tc2")	`thenNF_Tc_`
	tcInstDecls1 (pcs_insts pcs) (pcs_PRS pcs) 
			 hst unf_env get_fixity this_mod 
			 tycons decls		`thenTc` \ (new_pcs_insts, inst_env, local_inst_info, deriv_binds) ->
	tcSetInstEnv inst_env			$
	
	-- Interface type signatures
	-- We tie a knot so that the Ids read out of interfaces are in scope
	--   when we read their pragmas.
	-- What we rely on is that pragmas are typechecked lazily; if
	--   any type errors are found (ie there's an inconsistency)
	--   we silently discard the pragma
	-- We must do this before mkImplicitDataBinds (which comes next), since
	-- the latter looks up unpackCStringId, for example, which is usually 
	-- imported
--	traceTc (text "Tc3")			`thenNF_Tc_`
	tcInterfaceSigs unf_env decls		`thenTc` \ sig_ids ->
	tcExtendGlobalValEnv sig_ids		$
	
	-- Create any necessary record selector Ids and their bindings
	-- "Necessary" includes data and newtype declarations
	-- We don't create bindings for dictionary constructors;
	-- they are always fully applied, and the bindings are just there
	-- to support partial applications
	mkImplicitDataBinds  this_mod tycons	`thenTc`    \ (data_ids, imp_data_binds) ->
	mkImplicitClassBinds this_mod classes	`thenNF_Tc` \ (cls_ids,  imp_cls_binds) ->
	
	-- Extend the global value environment with 
	--	(a) constructors
	--	(b) record selectors
	--	(c) class op selectors
	-- 	(d) default-method ids... where? I can't see where these are
	--	    put into the envt, and I'm worried that the zonking phase
	--	    will find they aren't there and complain.
	tcExtendGlobalValEnv data_ids		$
	tcExtendGlobalValEnv cls_ids		$
	tcGetEnv					`thenTc` \ unf_env ->
	returnTc (unf_env, new_pcs_insts, local_inst_info, deriv_binds,
			   imp_data_binds `AndMonoBinds` imp_cls_binds)
    )		`thenTc` \ (env, new_pcs_insts, local_inst_info, deriv_binds, data_cls_binds) ->
    
    tcSetEnv env				$

        -- Foreign import declarations next
--  traceTc (text "Tc4")			`thenNF_Tc_`
    tcForeignImports decls			`thenTc`    \ (fo_ids, foi_decls) ->
    tcExtendGlobalValEnv fo_ids			$
    
	-- Default declarations
    tcDefaults decls				`thenTc` \ defaulting_tys ->
    tcSetDefaultTys defaulting_tys 		$
	
	-- Value declarations next.
	-- We also typecheck any extra binds that came out of the "deriving" process
--  traceTc (text "Tc5")					`thenNF_Tc_`
    tcTopBinds (get_binds decls `ThenBinds` deriv_binds)	`thenTc` \ ((val_binds, env), lie_valdecls) ->
    tcSetEnv env $
    
	-- Foreign export declarations next
--  traceTc (text "Tc6")		`thenNF_Tc_`
    tcForeignExports decls		`thenTc`    \ (lie_fodecls, foe_binds, foe_decls) ->
    
    	-- Second pass over class and instance declarations,
    	-- to compile the bindings themselves.
--  traceTc (text "Tc7")			`thenNF_Tc_`
    tcInstDecls2  local_inst_info		`thenNF_Tc` \ (lie_instdecls, inst_binds) ->
--  traceTc (text "Tc8")			`thenNF_Tc_`
    tcClassDecls2 this_mod decls		`thenNF_Tc` \ (lie_clasdecls, cls_dm_binds) ->
    tcRules (pcs_rules pcs) this_mod decls	`thenNF_Tc` \ (new_pcs_rules, lie_rules, local_rules) ->
    
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
    
        -- Backsubstitution.    This must be done last.
        -- Even tcSimplifyTop may do some unification.
    let
        all_binds = data_cls_binds 	`AndMonoBinds` 
    		    val_binds		`AndMonoBinds`
    	            inst_binds		`AndMonoBinds`
    	            cls_dm_binds	`AndMonoBinds`
    	            const_inst_binds	`AndMonoBinds`
    		    foe_binds
    in
--  traceTc (text "Tc9")		`thenNF_Tc_`
    zonkTopBinds all_binds		`thenNF_Tc` \ (all_binds', final_env)  ->
    tcSetEnv final_env			$
    	-- zonkTopBinds puts all the top-level Ids into the tcGEnv
    zonkForeignExports foe_decls	`thenNF_Tc` \ foe_decls' ->
    zonkRules local_rules		`thenNF_Tc` \ local_rules' ->
    
    
    let	(local_things, imported_things) = partition (isLocalThing this_mod) 
						    (nameEnvElts (getTcGEnv final_env))

    	local_type_env :: TypeEnv
    	local_type_env = mkTypeEnv local_things
    
    	new_pte :: PackageTypeEnv
    	new_pte = extendTypeEnvList (pcs_PTE pcs) imported_things

	final_pcs :: PersistentCompilerState
	final_pcs = pcs { pcs_PTE   = new_pte,
			  pcs_insts = new_pcs_insts,
			  pcs_rules = new_pcs_rules
		    }
    in  
--  traceTc (text "Tc10")		`thenNF_Tc_`
    returnTc (TcResults { tc_pcs     = final_pcs,
			  tc_env     = local_type_env,
			  tc_binds   = all_binds', 
			  tc_insts   = map iDFunId local_inst_info,
			  tc_fords   = foi_decls ++ foe_decls',
			  tc_rules   = local_rules'
                        }
    )

get_binds decls = foldr ThenBinds EmptyBinds [binds | ValD binds <- decls]
\end{code}



%************************************************************************
%*									*
\subsection{Dumping output}
%*									*
%************************************************************************

\begin{code}
printTcDump dflags Nothing = return ()
printTcDump dflags (Just results)
  = do dumpIfSet_dyn dflags Opt_D_dump_types 
                     "Type signatures" (dump_sigs results)
       dumpIfSet_dyn dflags Opt_D_dump_tc    
                     "Typechecked" (dump_tc results) 

dump_tc results
  = vcat [ppr (tc_binds results),
	  pp_rules (tc_rules results),
	  ppr_gen_tycons [tc | ATyCon tc <- nameEnvElts (tc_env results)]
    ]

dump_sigs results	-- Print type signatures
  = 	-- Convert to HsType so that we get source-language style printing
	-- And sort by RdrName
    vcat $ map ppr_sig $ sortLt lt_sig $
    [ (toRdrName id, toHsType (idType id))
    | AnId id <- nameEnvElts (tc_env results),
      want_sig id
    ]
  where
    lt_sig (n1,_) (n2,_) = n1 < n2
    ppr_sig (n,t)        = ppr n <+> dcolon <+> ppr t

    want_sig id | opt_PprStyle_Debug = True
	        | otherwise	     = True	-- For now

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
