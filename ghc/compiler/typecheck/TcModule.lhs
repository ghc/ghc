%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcModule]{Typechecking a whole module}

\begin{code}
#include "HsVersions.h"

module TcModule (
	typecheckModule,
	SYN_IE(TcResults),
	SYN_IE(TcSpecialiseRequests),
	SYN_IE(TcDDumpDeriv)
    ) where

IMP_Ubiq(){-uitous-}

import HsSyn		( HsDecl(..), HsModule(..), HsBinds(..), HsExpr, MonoBinds(..),
			  TyDecl, SpecDataSig, ClassDecl, InstDecl, IfaceSig,
			  SpecInstSig, DefaultDecl, Sig, Fake, InPat,
			  SYN_IE(RecFlag), nonRecursive,  GRHSsAndBinds, Match,
 			  FixityDecl, IE, ImportDecl
			)
import RnHsSyn		( SYN_IE(RenamedHsModule), RenamedFixityDecl(..) )
import TcHsSyn		( SYN_IE(TypecheckedHsBinds), SYN_IE(TypecheckedHsExpr),
			  SYN_IE(TypecheckedDictBinds), SYN_IE(TcMonoBinds),
			  SYN_IE(TypecheckedMonoBinds),
			  TcIdOcc(..), zonkTopBinds )

import TcMonad
import Inst		( Inst, emptyLIE, plusLIE )
import TcBinds		( tcBindsAndThen )
import TcClassDcl	( tcClassDecls2 )
import TcDefaults	( tcDefaults )
import TcEnv		( tcExtendGlobalValEnv, getEnv_LocalIds,
			  getEnv_TyCons, getEnv_Classes, tcLookupLocalValue,
			  tcLookupLocalValueByKey, tcLookupTyCon,
			  tcLookupGlobalValueByKeyMaybe )
import SpecEnv		( SpecEnv )
import TcExpr		( tcId )
import TcIfaceSig	( tcInterfaceSigs )
import TcInstDcls	( tcInstDecls1, tcInstDecls2 )
import TcInstUtil	( buildInstanceEnvs, InstInfo )
import TcSimplify	( tcSimplifyTop )
import TcTyClsDecls	( tcTyAndClassDecls1 )
import TcTyDecls	( mkDataBinds )
import TcType		( SYN_IE(TcType), tcInstType )
import TcKind		( TcKind )

import RnMonad		( RnNameSupply(..) )
import Bag		( listToBag )
import ErrUtils		( SYN_IE(Warning), SYN_IE(Error) )
import Id		( idType, GenId, SYN_IE(IdEnv), nullIdEnv )
import Maybes		( catMaybes, MaybeErr )
import Name		( Name, isLocallyDefined, pprModule )
import Pretty
import TyCon		( TyCon, isSynTyCon )
import Class		( GenClass, SYN_IE(Class), classSelIds )
import Type		( applyTyCon, mkSynTy, SYN_IE(Type) )
import PprType		( GenType, GenTyVar )
import TysWiredIn	( unitTy )
import PrelMods		( gHC_MAIN, mAIN )
import PrelInfo		( main_NAME, mainPrimIO_NAME, ioTyCon_NAME, primIoTyCon_NAME )
import TyVar		( GenTyVar, SYN_IE(TyVarEnv), nullTyVarEnv )
import Unify		( unifyTauTy )
import UniqFM		( lookupUFM_Directly, lookupWithDefaultUFM_Directly,
		          filterUFM, eltsUFM )
import Unique		( Unique  )
import UniqSupply       ( UniqSupply )
import Util
import Bag		( Bag, isEmptyBag )

import FiniteMap	( emptyFM, FiniteMap )

import Outputable	( Outputable(..), PprStyle )

tycon_specs = emptyFM
\end{code}

Outside-world interface:
\begin{code}
--ToDo: put this in HsVersions
#if __GLASGOW_HASKELL__ >= 200
# define REAL_WORLD RealWorld
#else
# define REAL_WORLD _RealWorld
#endif


-- Convenient type synonyms first:
type TcResults
  = (TypecheckedMonoBinds,
     [TyCon], [Class],
     Bag InstInfo,		-- Instance declaration information
     TcSpecialiseRequests,
     TcDDumpDeriv)

type TcSpecialiseRequests
  = FiniteMap TyCon [(Bool, [Maybe Type])]
    -- source tycon specialisation requests

type TcDDumpDeriv
  = PprStyle -> Doc

---------------
typecheckModule
	:: UniqSupply
	-> RnNameSupply
	-> RenamedHsModule
	-> MaybeErr
	    (TcResults, 		-- if all goes well...
	     Bag Warning)	      	-- (we can still get warnings)
	    (Bag Error,		      	-- if we had errors...
	     Bag Warning)

typecheckModule us rn_name_supply mod
  = initTc us (tcModule rn_name_supply mod)
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
		)					`thenTc` \ (inst_info, deriv_binds, ddump_deriv) ->
    
		-- trace "tc4" $
		buildInstanceEnvs inst_info	`thenTc` \ inst_mapper ->
    
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
    	    tycons   = getEnv_TyCons env
    	    classes  = getEnv_Classes env
    	in
    	mkDataBinds tycons		`thenTc` \ (data_ids, data_binds) ->
    	
    	-- Extend the global value environment with 
    	--	(a) constructors
    	--	(b) record selectors
    	--	(c) class op selectors
    	-- 	(d) default-method ids
    	tcExtendGlobalValEnv data_ids				$
    	tcExtendGlobalValEnv (concat (map classSelIds classes))	$


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
    	tcBindsAndThen
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

	returnTc (really_final_env, (all_binds', inst_info, ddump_deriv))

    -- End of outer fix loop
    ) `thenTc` \ (final_env, (all_binds', inst_info, ddump_deriv)) ->


    let
	tycons   = getEnv_TyCons   final_env
	classes  = getEnv_Classes  final_env

	local_tycons  = filter isLocallyDefined tycons
	local_classes = filter isLocallyDefined classes
    in
	-- FINISHED AT LAST
    returnTc (
	all_binds',

	local_tycons, local_classes, inst_info, tycon_specs,

	ddump_deriv
    )

get_val_decls decls = foldr ThenBinds EmptyBinds [binds | ValD binds <- decls]
\end{code}


\begin{code}
tcCheckMainSig mod_name
  | not is_main && not is_ghc_main
  = returnTc ()		-- A non-main module

  | otherwise
  = 	-- Check that main is defined
    tcLookupTyCon tycon_name			`thenTc` \ (_,_,tycon) ->
    tcLookupLocalValue main_name		`thenNF_Tc` \ maybe_main_id ->
    case maybe_main_id of {
	Nothing	 -> failTc (noMainErr mod_name main_name);
	Just main_id   ->

	-- Check that it has the right type (or a more general one)
    let
	expected_ty | isSynTyCon tycon = mkSynTy tycon [unitTy]
		    | otherwise	       = applyTyCon tycon [unitTy]
		-- This is bizarre.  There ought to be a suitable function in Type.lhs!
    in
    tcInstType [] expected_ty			`thenNF_Tc` \ expected_tau ->
    tcId main_name				`thenNF_Tc` \ (_, lie, main_tau) ->
    tcSetErrCtxt (mainTyCheckCtxt main_name) $
    unifyTauTy expected_tau
	       main_tau				`thenTc_`
    checkTc (isEmptyBag lie) (mainTyMisMatch main_name expected_ty (idType main_id))
    }
  where
    is_main     = mod_name == mAIN
    is_ghc_main = mod_name == gHC_MAIN

    main_name | is_main   = main_NAME
	      | otherwise = mainPrimIO_NAME

    tycon_name | is_main   = ioTyCon_NAME
	       | otherwise = primIoTyCon_NAME

mainTyCheckCtxt main_name sty
  = hsep [ptext SLIT("When checking that"), ppr sty main_name, ptext SLIT("has the required type")]

noMainErr mod_name main_name sty
  = hsep [ptext SLIT("Module"), pprModule sty mod_name, 
	   ptext SLIT("must include a definition for"), ppr sty main_name]

mainTyMisMatch :: Name -> Type -> TcType s -> Error
mainTyMisMatch main_name expected actual sty
  = hang (hsep [ppr sty main_name, ptext SLIT("has the wrong type")])
	 4 (vcat [
			hsep [ptext SLIT("Expected:"), ppr sty expected],
			hsep [ptext SLIT("Inferred:"), ppr sty actual]
		     ])
\end{code}
