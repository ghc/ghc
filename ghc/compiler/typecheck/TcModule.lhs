%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcModule]{Typechecking a whole module}

\begin{code}
#include "HsVersions.h"

module TcModule (
	typecheckModule,
	SYN_IE(TcResults),
	SYN_IE(TcResultBinds),
	SYN_IE(TcIfaceInfo),
	SYN_IE(TcSpecialiseRequests),
	SYN_IE(TcDDumpDeriv)
    ) where

IMP_Ubiq(){-uitous-}

import HsSyn		( HsModule(..), HsBinds(..), Bind, HsExpr,
			  TyDecl, SpecDataSig, ClassDecl, InstDecl,
			  SpecInstSig, DefaultDecl, Sig, Fake, InPat,
 			  FixityDecl, IE, ImportDecl
			)
import RnHsSyn		( SYN_IE(RenamedHsModule), RenamedFixityDecl(..) )
import TcHsSyn		( SYN_IE(TypecheckedHsBinds), SYN_IE(TypecheckedHsExpr),
			  TcIdOcc(..), zonkBinds, zonkDictBinds )

import TcMonad		hiding ( rnMtoTcM )
import Inst		( Inst, plusLIE )
import TcBinds		( tcBindsAndThen )
import TcClassDcl	( tcClassDecls2 )
import TcDefaults	( tcDefaults )
import TcEnv		( tcExtendGlobalValEnv, getEnv_LocalIds,
			  getEnv_TyCons, getEnv_Classes,
			  tcLookupLocalValueByKey, tcLookupTyConByKey )
import SpecEnv		( SpecEnv )
import TcIfaceSig	( tcInterfaceSigs )
import TcInstDcls	( tcInstDecls1, tcInstDecls2 )
import TcInstUtil	( buildInstanceEnvs, InstInfo )
import TcSimplify	( tcSimplifyTop )
import TcTyClsDecls	( tcTyAndClassDecls1 )
import TcTyDecls	( mkDataBinds )

import Bag		( listToBag )
import Class		( GenClass, classSelIds )
import ErrUtils		( SYN_IE(Warning), SYN_IE(Error) )
import Id		( idType, isMethodSelId, isTopLevId, GenId, SYN_IE(IdEnv), nullIdEnv )
import Maybes		( catMaybes )
import Name		( isLocallyDefined )
import Pretty
import RnUtils		( SYN_IE(RnEnv) )
import TyCon		( TyCon )
import Type		( applyTyCon )
import TysWiredIn	( unitTy, mkPrimIoTy )
import TyVar		( SYN_IE(TyVarEnv), nullTyVarEnv )
import Unify		( unifyTauTy )
import UniqFM		( lookupUFM_Directly, lookupWithDefaultUFM_Directly,
		          filterUFM, eltsUFM )
import Unique		( iOTyConKey )
import Util

import FiniteMap	( emptyFM, FiniteMap )
tycon_specs = emptyFM
\end{code}

Outside-world interface:
\begin{code}
-- Convenient type synonyms first:
type TcResults
  = (TcResultBinds,
     TcIfaceInfo,
     TcSpecialiseRequests,
     TcDDumpDeriv)

type TcResultBinds
  = (TypecheckedHsBinds,	-- record selector binds
     TypecheckedHsBinds,	-- binds from class decls; does NOT
				-- include default-methods bindings
     TypecheckedHsBinds,	-- binds from instance decls; INCLUDES
				-- class default-methods binds
     TypecheckedHsBinds,	-- binds from value decls

     [(Id, TypecheckedHsExpr)]) -- constant instance binds

type TcIfaceInfo -- things for the interface generator
  = ([Id], [TyCon], [Class], Bag InstInfo)

type TcSpecialiseRequests
  = FiniteMap TyCon [(Bool, [Maybe Type])]
    -- source tycon specialisation requests

type TcDDumpDeriv
  = PprStyle -> Pretty

---------------
typecheckModule
	:: UniqSupply
	-> RnEnv		-- for renaming derivings
	-> RenamedHsModule
	-> MaybeErr
	    (TcResults,		-- if all goes well...
	     Bag Warning)	-- (we can still get warnings)
	    (Bag Error,		-- if we had errors...
	     Bag Warning)

typecheckModule us rn_env mod
  = initTc us (tcModule rn_env mod)
\end{code}

The internal monster:
\begin{code}
tcModule :: RnEnv		-- for renaming derivings
	 -> RenamedHsModule	-- input
	 -> TcM s TcResults	-- output

tcModule rn_env
	(HsModule mod_name verion exports imports fixities
		  ty_decls specdata_sigs cls_decls inst_decls specinst_sigs
		  default_decls val_decls sigs src_loc)

  = ASSERT(null imports)

    tcAddSrcLoc src_loc $	-- record where we're starting

	-- Tie the knot for inteface-file value declaration signatures
	-- This info is only used inside the knot for type-checking the
	-- pragmas, which is done lazily [ie failure just drops the pragma
	-- without having any global-failure effect].

    fixTc (\ ~(_, _, _, _, _, _, sig_ids) ->
	tcExtendGlobalValEnv sig_ids (

	-- The knot for instance information.  This isn't used at all
	-- till we type-check value declarations
	fixTc ( \ ~(rec_inst_mapper, _, _, _, _) ->

	     -- Type-check the type and class decls
	    --trace "tcTyAndClassDecls:"	$
	    tcTyAndClassDecls1 rec_inst_mapper ty_decls_bag cls_decls_bag
					`thenTc` \ env ->

		-- Typecheck the instance decls, includes deriving
	    tcSetEnv env (
	    --trace "tcInstDecls:"	$
	    tcInstDecls1 inst_decls_bag specinst_sigs
			 mod_name rn_env fixities 
	    )				`thenTc` \ (inst_info, deriv_binds, ddump_deriv) ->

	    buildInstanceEnvs inst_info	`thenTc` \ inst_mapper ->

	    returnTc (inst_mapper, env, inst_info, deriv_binds, ddump_deriv)

	) `thenTc` \ (_, env, inst_info, deriv_binds, ddump_deriv) ->
	tcSetEnv env (

	    -- Default declarations
	tcDefaults default_decls	`thenTc` \ defaulting_tys ->
	tcSetDefaultTys defaulting_tys 	( -- for the iface sigs...

	-- Create any necessary record selector Ids and their bindings
	-- "Necessary" includes data and newtype declarations
	let
		tycons   = getEnv_TyCons env
		classes  = getEnv_Classes env
	in
	mkDataBinds tycons		`thenTc` \ (data_ids, data_binds) ->

	-- Extend the global value environment with 
	--	a) constructors
	--	b) record selectors
	--	c) class op selectors
	tcExtendGlobalValEnv data_ids				$
	tcExtendGlobalValEnv (concat (map classSelIds classes))	$

	    -- Interface type signatures
	    -- We tie a knot so that the Ids read out of interfaces are in scope
	    --   when we read their pragmas.
	    -- What we rely on is that pragmas are typechecked lazily; if
	    --   any type errors are found (ie there's an inconsistency)
	    --   we silently discard the pragma
	tcInterfaceSigs sigs		`thenTc` \ sig_ids ->
	tcGetEnv			`thenNF_Tc` \ env ->

	returnTc (env, inst_info, data_binds, deriv_binds, ddump_deriv, defaulting_tys, sig_ids)

    )))) `thenTc` \ (env, inst_info, data_binds, deriv_binds, ddump_deriv, defaulting_tys, _) ->

    tcSetEnv env (				-- to the end...
    tcSetDefaultTys defaulting_tys (		-- ditto

	-- Value declarations next.
	-- We also typecheck any extra binds that came out of the "deriving" process
    --trace "tcBinds:"			$
    tcBindsAndThen
	(\ binds1 (binds2, thing) -> (binds1 `ThenBinds` binds2, thing))
	(val_decls `ThenBinds` deriv_binds)
	(	-- Second pass over instance declarations,
		-- to compile the bindings themselves.
	    tcInstDecls2  inst_info	`thenNF_Tc` \ (lie_instdecls, inst_binds) ->
	    tcClassDecls2 cls_decls_bag	`thenNF_Tc` \ (lie_clasdecls, cls_binds) ->
	    tcGetEnv			`thenNF_Tc` \ env ->
	    returnTc ( (EmptyBinds, (inst_binds, cls_binds, env)),
		       lie_instdecls `plusLIE` lie_clasdecls,
		       () ))

	`thenTc` \ ((val_binds, (inst_binds, cls_binds, final_env)), lie_alldecls, _) ->

	-- Deal with constant or ambiguous InstIds.  How could
	-- there be ambiguous ones?  They can only arise if a
	-- top-level decl falls under the monomorphism
	-- restriction, and no subsequent decl instantiates its
	-- type.  (Usually, ambiguous type variables are resolved
	-- during the generalisation step.)
    tcSimplifyTop lie_alldecls			`thenTc` \ const_insts ->

	-- Backsubstitution.  Monomorphic top-level decls may have
	-- been instantiated by subsequent decls, and the final
	-- simplification step may have instantiated some
	-- ambiguous types.  So, sadly, we need to back-substitute
	-- over the whole bunch of bindings.
	-- 
	-- More horrible still, we have to do it in a careful order, so that
	-- all the TcIds are in scope when we come across them.
	-- 
	-- These bindings ought really to be bundled together in a huge
	-- recursive group, but HsSyn doesn't have recursion among Binds, only
	-- among MonoBinds.  Sigh again.
    zonkDictBinds nullTyVarEnv nullIdEnv const_insts 	`thenNF_Tc` \ (const_insts', ve1) ->
    zonkBinds nullTyVarEnv ve1 val_binds 		`thenNF_Tc` \ (val_binds', ve2) ->

    zonkBinds nullTyVarEnv ve2 data_binds 	`thenNF_Tc` \ (data_binds', _) ->
    zonkBinds nullTyVarEnv ve2 inst_binds	`thenNF_Tc` \ (inst_binds', _) ->
    zonkBinds nullTyVarEnv ve2 cls_binds	`thenNF_Tc` \ (cls_binds', _) ->

    let
        localids = getEnv_LocalIds final_env
	tycons   = getEnv_TyCons   final_env
	classes  = getEnv_Classes  final_env

	local_tycons  = filter isLocallyDefined tycons
	local_classes = filter isLocallyDefined classes
	local_vals    = [ v | v <- eltsUFM ve2, isLocallyDefined v && isTopLevId v ]
			-- the isTopLevId is doubtful...
    in
	-- FINISHED AT LAST
    returnTc (
	(data_binds', cls_binds', inst_binds', val_binds', const_insts'),

	     -- the next collection is just for mkInterface
	(local_vals, local_tycons, local_classes, inst_info),

	tycon_specs,

	ddump_deriv
    )))
  where
    ty_decls_bag   = listToBag ty_decls
    cls_decls_bag  = listToBag cls_decls
    inst_decls_bag = listToBag inst_decls
\end{code}
