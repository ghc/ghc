%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcModule]{Typechecking a whole module}

\begin{code}
module TcModule (
	typecheckModule,
	TcResults,
	TcDDumpDeriv
    ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_D_dump_tc, opt_D_dump_deriv )
import HsSyn		( HsModule(..), HsBinds(..), MonoBinds(..), HsDecl(..) )
import RnHsSyn		( RenamedHsModule, RenamedFixityDecl(..) )
import TcHsSyn		( TypecheckedHsBinds, TypecheckedHsExpr,
			  TypecheckedDictBinds, TcMonoBinds,
			  TypecheckedMonoBinds,
			  zonkTopBinds )

import TcMonad
import Inst		( Inst, emptyLIE, plusLIE )
import TcBinds		( tcTopBindsAndThen )
import TcClassDcl	( tcClassDecls2 )
import TcDefaults	( tcDefaults )
import TcEnv		( TcIdOcc(..), tcExtendGlobalValEnv, tcExtendTyConEnv, getEnv_LocalIds,
			  getEnv_TyCons, getEnv_Classes, tcLookupLocalValue,
			  tcLookupLocalValueByKey, tcLookupTyCon,
			  tcLookupGlobalValueByKeyMaybe )
import TcExpr		( tcId )
import TcIfaceSig	( tcInterfaceSigs )
import TcInstDcls	( tcInstDecls1, tcInstDecls2 )
import TcInstUtil	( buildInstanceEnvs, classDataCon, InstInfo )
import TcSimplify	( tcSimplifyTop )
import TcTyClsDecls	( tcTyAndClassDecls1 )
import TcTyDecls	( mkDataBinds )
import TcType		( TcType, tcInstType )
import TcKind		( TcKind, kindToTcKind )

import RnMonad		( RnNameSupply(..) )
import Bag		( isEmptyBag )
import ErrUtils		( WarnMsg, ErrMsg, 
			  pprBagOfErrors, dumpIfSet, ghcExit
			)
import Id		( idType, GenId, IdEnv, nullIdEnv )
import Maybes		( catMaybes, MaybeErr(..) )
import Name		( Name, isLocallyDefined, pprModule, NamedThing(..) )
import TyCon		( TyCon, isSynTyCon, tyConKind )
import Class		( Class, classSelIds, classTyCon )
import Type		( mkTyConApp, mkSynTy, Type )
import TyVar		( emptyTyVarEnv )
import TysWiredIn	( unitTy )
import PrelMods		( gHC_MAIN, mAIN )
import PrelInfo		( main_NAME, ioTyCon_NAME )
import Unify		( unifyTauTy )
import UniqFM		( lookupUFM_Directly, lookupWithDefaultUFM_Directly,
		          filterUFM, eltsUFM )
import Unique		( Unique  )
import UniqSupply       ( UniqSupply )
import Util
import Bag		( Bag, isEmptyBag )
import FiniteMap	( emptyFM, FiniteMap )
import Outputable
\end{code}

Outside-world interface:
\begin{code}

-- Convenient type synonyms first:
type TcResults
  = (TypecheckedMonoBinds,
     [TyCon], [Class],
     Bag InstInfo,		-- Instance declaration information
     TcDDumpDeriv)

type TcDDumpDeriv = SDoc

---------------
typecheckModule
	:: UniqSupply
	-> RnNameSupply
	-> RenamedHsModule
	-> IO (Maybe TcResults)

typecheckModule us rn_name_supply mod
  = let
      (maybe_result, warns, errs) = initTc us (tcModule rn_name_supply mod)
    in
    print_errs warns	>>
    print_errs errs	>>

    dumpIfSet opt_D_dump_tc "Typechecked"
	(case maybe_result of
	    Just (binds, _, _, _, _) -> ppr binds
	    Nothing 		     -> text "Typecheck failed")  	>>

    dumpIfSet opt_D_dump_deriv "Derived instances"
	(case maybe_result of
	    Just (_, _, _, _, dump_deriv) -> dump_deriv
	    Nothing 		          -> empty)  	>>

    return (if isEmptyBag errs then 
		maybe_result 
	    else 
		Nothing)

print_errs errs
  | isEmptyBag errs = return ()
  | otherwise       = printErrs (pprBagOfErrors errs)
\end{code}

The internal monster:
\begin{code}
tcModule :: RnNameSupply	-- for renaming derivings
	 -> RenamedHsModule	-- input
	 -> TcM s TcResults	-- output

tcModule rn_name_supply
	(HsModule mod_name verion exports imports fixities decls src_loc)
  = tcAddSrcLoc src_loc $	-- record where we're starting

    fixTc (\ ~(unf_env ,_) ->
	-- unf_env is used for type-checking interface pragmas
	-- which is done lazily [ie failure just drops the pragma
	-- without having any global-failure effect].
	-- 
	-- unf_env is also used to get the pragam info for dfuns.

    	    -- The knot for instance information.  This isn't used at all
	    -- till we type-check value declarations
    	fixTc ( \ ~(rec_inst_mapper, _, _, _, _) ->
    
		 -- Type-check the type and class decls
		-- trace "tcTyAndClassDecls:"	$
		tcTyAndClassDecls1 unf_env rec_inst_mapper decls	`thenTc` \ env ->
    
		-- trace "tc3" $
		    -- Typecheck the instance decls, includes deriving
		tcSetEnv env (
		-- trace "tcInstDecls:"	$
		tcInstDecls1 unf_env decls mod_name rn_name_supply
		)				`thenTc` \ (inst_info, deriv_binds, ddump_deriv) ->
    
		-- trace "tc4" $
    		buildInstanceEnvs inst_info	`thenNF_Tc` \ inst_mapper ->
    
		returnTc (inst_mapper, env, inst_info, deriv_binds, ddump_deriv)
    
    	-- End of inner fix loop
    	) `thenTc` \ (_, env, inst_info, deriv_binds, ddump_deriv) ->
    
    	-- trace "tc5" $
    	tcSetEnv env $
    	
    	    -- Default declarations
    	tcDefaults decls		`thenTc` \ defaulting_tys ->
    	tcSetDefaultTys defaulting_tys 	$
    	
    	-- Create any necessary record selector Ids and their bindings
    	-- "Necessary" includes data and newtype declarations
    	let
    	    tycons       = getEnv_TyCons env
    	    classes      = getEnv_Classes env
	    local_tycons  = filter isLocallyDefined tycons
	    local_classes = filter isLocallyDefined classes
    	in
    	mkDataBinds tycons		`thenTc` \ (data_ids, data_binds) ->
    	
    	-- Extend the global value environment with 
    	--	(a) constructors
    	--	(b) record selectors
    	--	(c) class op selectors
    	-- 	(d) default-method ids
    	tcExtendGlobalValEnv data_ids				$
    	tcExtendGlobalValEnv (concat (map classSelIds classes))	$

	-- Extend the TyCon envt with the tycons corresponding to
	-- the classes, and the global value environment with the
	-- corresponding data cons.
	--  They are mentioned in types in interface files.
    	tcExtendGlobalValEnv (map classDataCon classes)		$
        tcExtendTyConEnv [ (getName tycon, (kindToTcKind (tyConKind tycon), Nothing, tycon))
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


	-- Value declarations next.
	-- We also typecheck any extra binds that came out of the "deriving" process
        -- trace "tcBinds:"			$
    	tcTopBindsAndThen
	    (\ is_rec binds1 (binds2, thing) -> (binds1 `AndMonoBinds` binds2, thing))
	    (get_val_decls decls `ThenBinds` deriv_binds)
	    (	tcGetEnv		`thenNF_Tc` \ env ->
		returnTc ((EmptyMonoBinds, env), emptyLIE)
	    )				`thenTc` \ ((val_binds, final_env), lie_valdecls) ->
	tcSetEnv final_env $


		-- Second pass over class and instance declarations,
		-- to compile the bindings themselves.
	-- trace "tc8" $
	tcInstDecls2  inst_info		`thenNF_Tc` \ (lie_instdecls, inst_binds) ->
	tcClassDecls2 decls		`thenNF_Tc` \ (lie_clasdecls, cls_binds) ->



	-- Check that "main" has the right signature
	tcCheckMainSig mod_name		`thenTc_` 

	     -- Deal with constant or ambiguous InstIds.  How could
	     -- there be ambiguous ones?  They can only arise if a
	     -- top-level decl falls under the monomorphism
	     -- restriction, and no subsequent decl instantiates its
	     -- type.  (Usually, ambiguous type variables are resolved
	     -- during the generalisation step.)
	-- trace "tc9" $
	let
	    lie_alldecls = lie_valdecls `plusLIE` lie_instdecls `plusLIE` lie_clasdecls
	in
	tcSimplifyTop lie_alldecls			`thenTc` \ const_inst_binds ->


	    -- Backsubstitution.    This must be done last.
	    -- Even tcCheckMainSig and tcSimplifyTop may do some unification.
	let
	    all_binds = data_binds 		`AndMonoBinds` 
			val_binds		`AndMonoBinds`
		        inst_binds		`AndMonoBinds`
		        cls_binds		`AndMonoBinds`
		        const_inst_binds
	in
	zonkTopBinds all_binds	`thenNF_Tc` \ (all_binds', really_final_env)  ->

	returnTc (really_final_env, 
		  (all_binds', local_tycons, local_classes, inst_info, ddump_deriv))

    -- End of outer fix loop
    ) `thenTc` \ (final_env, stuff) ->
    returnTc stuff

get_val_decls decls = foldr ThenBinds EmptyBinds [binds | ValD binds <- decls]
\end{code}


\begin{code}
tcCheckMainSig mod_name
  | mod_name /= mAIN
  = returnTc ()		-- A non-main module

  | otherwise
  = 	-- Check that main is defined
    tcLookupTyCon ioTyCon_NAME		`thenTc`    \ (_,_,ioTyCon) ->
    tcLookupLocalValue main_NAME	`thenNF_Tc` \ maybe_main_id ->
    case maybe_main_id of {
	Nothing	 -> failWithTc noMainErr ;
	Just main_id   ->

	-- Check that it has the right type (or a more general one)
    let 
	expected_ty = mkTyConApp ioTyCon [unitTy]
    in
    tcInstType emptyTyVarEnv expected_ty	`thenNF_Tc` \ expected_tau ->
    tcId main_NAME				`thenNF_Tc` \ (_, lie, main_tau) ->
    tcSetErrCtxt mainTyCheckCtxt $
    unifyTauTy expected_tau
	       main_tau			`thenTc_`
    checkTc (isEmptyBag lie) (mainTyMisMatch expected_ty (idType main_id))
    }


mainTyCheckCtxt
  = hsep [ptext SLIT("When checking that"), ppr main_NAME, ptext SLIT("has the required type")]

noMainErr
  = hsep [ptext SLIT("Module"), quotes (pprModule mAIN), 
	  ptext SLIT("must include a definition for"), quotes (ppr main_NAME)]

mainTyMisMatch :: Type -> TcType s -> ErrMsg
mainTyMisMatch expected actual
  = hang (hsep [ppr main_NAME, ptext SLIT("has the wrong type")])
	 4 (vcat [
			hsep [ptext SLIT("Expected:"), ppr expected],
			hsep [ptext SLIT("Inferred:"), ppr actual]
		     ])
\end{code}
