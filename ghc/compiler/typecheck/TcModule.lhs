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

import CmdLineOpts	( opt_D_dump_tc )
import HsSyn		( HsModule(..), HsBinds(..), MonoBinds(..), HsDecl(..) )
import RnHsSyn		( RenamedHsModule )
import TcHsSyn		( TcMonoBinds, TypecheckedMonoBinds, 
			  TypecheckedForeignDecl, TypecheckedRuleDecl,
			  zonkTopBinds, zonkForeignExports, zonkRules
			)

import TcMonad
import Inst		( Inst, emptyLIE, plusLIE )
import TcBinds		( tcTopBindsAndThen )
import TcClassDcl	( tcClassDecls2 )
import TcDefaults	( tcDefaults )
import TcEnv		( tcExtendGlobalValEnv, tcExtendTypeEnv,
			  getEnvTyCons, getEnvClasses, tcLookupValueMaybe,
			  explicitLookupValueByKey, tcSetValueEnv,
			  tcLookupTyCon, initEnv, 
			  ValueEnv, TcTyThing(..)
			)
import TcExpr		( tcId )
import TcRules		( tcRules )
import TcForeign	( tcForeignImports, tcForeignExports )
import TcIfaceSig	( tcInterfaceSigs )
import TcInstDcls	( tcInstDecls1, tcInstDecls2 )
import TcInstUtil	( buildInstanceEnvs, classDataCon, InstInfo )
import TcSimplify	( tcSimplifyTop )
import TcTyClsDecls	( tcTyAndClassDecls )
import TcTyDecls	( mkDataBinds )
import TcType		( TcType, typeToTcType,
			  TcKind, kindToTcKind,
			  newTyVarTy
			)

import RnMonad		( RnNameSupply, getIfaceFixities, Fixities, InterfaceDetails )
import Bag		( isEmptyBag )
import ErrUtils		( Message, printErrorsAndWarnings, dumpIfSet )
import Id		( Id, idType )
import Module           ( pprModuleName )
import Name		( Name, nameUnique, isLocallyDefined, NamedThing(..) )
import TyCon		( TyCon, tyConKind )
import DataCon		( dataConId )
import Class		( Class, classSelIds, classTyCon )
import Type		( mkTyConApp, mkForAllTy,
			  boxedTypeKind, getTyVar, Type )
import TysWiredIn	( unitTy )
import PrelMods		( mAIN_Name )
import PrelInfo		( main_NAME, thinAirIdNames, setThinAirIds )
import TcUnify		( unifyTauTy )
import Unique		( Unique  )
import UniqSupply       ( UniqSupply )
import Maybes		( maybeToBool )
import Util
import Bag		( Bag, isEmptyBag )
import Outputable

import IOExts
\end{code}

Outside-world interface:
\begin{code}

-- Convenient type synonyms first:
data TcResults
  = TcResults {
	tc_binds   :: TypecheckedMonoBinds,
	tc_tycons  :: [TyCon],
	tc_classes :: [Class],
	tc_insts   :: Bag InstInfo,		-- Instance declaration information
	tc_fords   :: [TypecheckedForeignDecl], -- Foreign import & exports.
	tc_rules   :: [TypecheckedRuleDecl],	-- Transformation rules
	tc_env	   :: ValueEnv,
	tc_thinair :: [Id]			-- The thin-air Ids
    }

---------------
typecheckModule
	:: UniqSupply
	-> RnNameSupply
	-> InterfaceDetails
	-> RenamedHsModule
	-> IO (Maybe TcResults)

typecheckModule us rn_name_supply iface_det mod
  = initTc us initEnv (tcModule rn_name_supply (getIfaceFixities iface_det) mod)
		    	>>= \ (maybe_result, warns, errs) ->
		
    printErrorsAndWarnings errs warns		>>

    -- write the thin-air Id map
    (case maybe_result of
	Just results -> setThinAirIds (tc_thinair results)
	Nothing      -> return ()
    ) 									>>

    dumpIfSet opt_D_dump_tc "Typechecked"
	(case maybe_result of
	    Just results -> ppr (tc_binds results) 
			    $$ 
			    pp_rules (tc_rules results)
	    Nothing 	 -> text "Typecheck failed") 	>>

    return (if isEmptyBag errs then 
		maybe_result 
	    else 
		Nothing)

pp_rules [] = empty
pp_rules rs = vcat [ptext SLIT("{-# RULES"),
		    nest 4 (vcat (map ppr rs)),
		    ptext SLIT("#-}")]
\end{code}

The internal monster:
\begin{code}
tcModule :: RnNameSupply	-- for renaming derivings
	 -> Fixities		-- needed for Show/Read derivings.
	 -> RenamedHsModule	-- input
	 -> TcM s TcResults	-- output

tcModule rn_name_supply fixities
	(HsModule mod_name verion exports imports decls src_loc)
  = tcAddSrcLoc src_loc $	-- record where we're starting

    fixTc (\ ~(unf_env ,_) ->
	-- unf_env is used for type-checking interface pragmas
	-- which is done lazily [ie failure just drops the pragma
	-- without having any global-failure effect].
	-- 
	-- unf_env is also used to get the pragam info
	-- for imported dfuns and default methods

    	    -- The knot for instance information.  This isn't used at all
	    -- till we type-check value declarations
    	fixTc ( \ ~(rec_inst_mapper, _, _, _) ->
    
		 -- Type-check the type and class decls
		tcTyAndClassDecls unf_env rec_inst_mapper decls	`thenTc` \ env ->
    
		    -- Typecheck the instance decls, includes deriving
		tcSetEnv env (
		tcInstDecls1 unf_env decls mod_name fixities rn_name_supply
		)				`thenTc` \ (inst_info, deriv_binds) ->
    
    		buildInstanceEnvs inst_info	`thenNF_Tc` \ inst_mapper ->
    
		returnTc (inst_mapper, env, inst_info, deriv_binds)
    
    	-- End of inner fix loop
    	) `thenTc` \ (_, env, inst_info, deriv_binds) ->
    
    	tcSetEnv env 		(
    	
    	    -- Default declarations
    	tcDefaults decls		`thenTc` \ defaulting_tys ->
    	tcSetDefaultTys defaulting_tys 	$
    	
    	-- Create any necessary record selector Ids and their bindings
    	-- "Necessary" includes data and newtype declarations
	-- We don't create bindings for dictionary constructors;
	-- they are always fully applied, and the bindings are just there
	-- to support partial applications
    	let
    	    tycons       = getEnvTyCons env
    	    classes      = getEnvClasses env
	    local_tycons  = filter isLocallyDefined tycons
	    local_classes = filter isLocallyDefined classes
    	in
    	mkDataBinds tycons		`thenTc` \ (data_ids, data_binds) ->
    	
    	-- Extend the global value environment with 
    	--	(a) constructors
    	--	(b) record selectors
    	--	(c) class op selectors
    	-- 	(d) default-method ids... where? I can't see where these are
	--	    put into the envt, and I'm worried that the zonking phase
	--	    will find they aren't there and complain.
    	tcExtendGlobalValEnv data_ids				$
    	tcExtendGlobalValEnv (concat (map classSelIds classes))	$

	-- Extend the TyCon envt with the tycons corresponding to
	-- the classes, and the global value environment with the
	-- corresponding data cons.
	--  They are mentioned in types in interface files.
    	tcExtendGlobalValEnv (map (dataConId . classDataCon) classes)		$
        tcExtendTypeEnv [ (getName tycon, (kindToTcKind (tyConKind tycon), Nothing, ATyCon tycon))
		        | clas <- classes,
			  let tycon = classTyCon clas
		        ]				$

	    -- Interface type signatures
	    -- We tie a knot so that the Ids read out of interfaces are in scope
	    --   when we read their pragmas.
	    -- What we rely on is that pragmas are typechecked lazily; if
	    --   any type errors are found (ie there's an inconsistency)
	    --   we silently discard the pragma
	tcInterfaceSigs unf_env decls		`thenTc` \ sig_ids ->
	tcExtendGlobalValEnv sig_ids		$

	    -- foreign import declarations next.
	tcForeignImports decls		`thenTc`    \ (fo_ids, foi_decls) ->
	tcExtendGlobalValEnv fo_ids		$

	-- Value declarations next.
	-- We also typecheck any extra binds that came out of the "deriving" process
    	tcTopBindsAndThen
	    (\ is_rec binds1 (binds2, thing) -> (binds1 `AndMonoBinds` binds2, thing))
	    (get_val_decls decls `ThenBinds` deriv_binds)
	    (	tcGetEnv				`thenNF_Tc` \ env ->
		tcGetUnique				`thenNF_Tc` \ uniq ->
		returnTc ((EmptyMonoBinds, env), emptyLIE)
	    )				`thenTc` \ ((val_binds, final_env), lie_valdecls) ->
	tcSetEnv final_env $

	    -- foreign export declarations next.
	tcForeignExports decls		`thenTc`    \ (lie_fodecls, foe_binds, foe_decls) ->

		-- Second pass over class and instance declarations,
		-- to compile the bindings themselves.
	tcInstDecls2  inst_info		`thenNF_Tc` \ (lie_instdecls, inst_binds) ->
	tcClassDecls2 decls		`thenNF_Tc` \ (lie_clasdecls, cls_binds) ->
	tcRules decls			`thenNF_Tc` \ (lie_rules,     rules) ->


	     -- Deal with constant or ambiguous InstIds.  How could
	     -- there be ambiguous ones?  They can only arise if a
	     -- top-level decl falls under the monomorphism
	     -- restriction, and no subsequent decl instantiates its
	     -- type.  (Usually, ambiguous type variables are resolved
	     -- during the generalisation step.)
	let
	    lie_alldecls = lie_valdecls  `plusLIE`
			   lie_instdecls `plusLIE`
			   lie_clasdecls `plusLIE`
			   lie_fodecls	 `plusLIE`
			   lie_rules
	in
	tcSimplifyTop lie_alldecls			`thenTc` \ const_inst_binds ->

		-- Check that Main defines main
	(if mod_name == mAIN_Name then
		tcLookupValueMaybe main_NAME	`thenNF_Tc` \ maybe_main ->
		checkTc (maybeToBool maybe_main) noMainErr
	 else
		returnTc ()
	)					`thenTc_`

	    -- Backsubstitution.    This must be done last.
	    -- Even tcSimplifyTop may do some unification.
	let
	    all_binds = data_binds 		`AndMonoBinds` 
			val_binds		`AndMonoBinds`
		        inst_binds		`AndMonoBinds`
		        cls_binds		`AndMonoBinds`
		        const_inst_binds	`AndMonoBinds`
			foe_binds
	in
	zonkTopBinds all_binds		`thenNF_Tc` \ (all_binds', really_final_env)  ->
	tcSetValueEnv really_final_env	$
	zonkForeignExports foe_decls    `thenNF_Tc` \ foe_decls' ->
	zonkRules rules			`thenNF_Tc` \ rules' ->

	let
	   thin_air_ids = map (explicitLookupValueByKey really_final_env . nameUnique) thinAirIdNames
		-- When looking up the thin-air names we must use
		-- a global env that includes the zonked locally-defined Ids too
		-- Hence using really_final_env
	in
	returnTc (really_final_env, 
		  (TcResults {	tc_binds   = all_binds', 
				tc_tycons  = local_tycons,
				tc_classes = local_classes,
				tc_insts   = inst_info,
				tc_fords   = foi_decls ++ foe_decls',
				tc_rules   = rules',
				tc_env     = really_final_env,
				tc_thinair = thin_air_ids
		 }))
	)

    -- End of outer fix loop
    ) `thenTc` \ (final_env, stuff) ->
    returnTc stuff

get_val_decls decls = foldr ThenBinds EmptyBinds [binds | ValD binds <- decls]
\end{code}


\begin{code}
noMainErr
  = hsep [ptext SLIT("Module"), quotes (pprModuleName mAIN_Name), 
	  ptext SLIT("must include a definition for"), quotes (ppr main_NAME)]
\end{code}

