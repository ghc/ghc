%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcModule]{Typechecking a whole module}

\begin{code}
#include "HsVersions.h"

module TcModule (
	tcModule,

	-- to make the interface self-sufficient...
	Module, Bag, CE(..), E, Binds, FixityDecl, Expr, InPat,
	RenamedPat(..), TypecheckedPat, Id, Inst, Maybe, TcResult,
	Name, ProtoName, SrcLoc, Subst, TCE(..), UniqFM,
	Error(..), Pretty(..), PprStyle, PrettyRep, InstInfo
    ) where

import TcMonad		-- typechecking monad machinery
import AbsSyn		-- the stuff being typechecked

-- OLD:
--import AbsPrel	( stringTy,
--			  eqStringId, neStringId, ltStringId,
--			  leStringId, geStringId, gtStringId,
--			  maxStringId, minStringId, tagCmpStringId,
--			  dfunEqStringId, dfunOrdStringId,
--			  pRELUDE_CORE
--			  IF_ATTACK_PRAGMAS(COMMA mkListTy COMMA charTy)
--			)
--#if USE_ATTACK_PRAGMAS
--import PrelVals	( string_cmp_id ) -- shouldn't even be visible, really
--#endif
import BackSubst	( applyTcSubstToBinds )
import Bag		( unionBags, bagToList, emptyBag, listToBag )
import CE		( nullCE, checkClassCycles, lookupCE, CE(..) )
import CmdLineOpts	( GlobalSwitch(..) )
import E
import HsCore		-- ****** NEED TO SEE CONSTRUCTORS ******
import HsPragmas	-- ****** NEED TO SEE CONSTRUCTORS ******
import InstEnv
import LIE		( unMkLIE, plusLIE, LIE )
import Name		( Name(..) )
import RenameAuxFuns	( GlobalNameFuns(..), GlobalNameFun(..), ProtoName, Maybe )
import SrcLoc		( mkBuiltinSrcLoc, SrcLoc )
import TCE		( checkTypeCycles, TCE(..), UniqFM )
import TcBinds		( tcTopBindsAndThen )
import TcClassDcl	( tcClassDecls1, tcClassDecls2, ClassInfo )
import TcDefaults	( tcDefaults )
import TcDeriv		( tcDeriving )
import TcIfaceSig	( tcInterfaceSigs )
import TcInstDcls	( tcInstDecls1, tcInstDecls2, tcSpecInstSigs, buildInstanceEnvs, InstInfo(..) )
import TcSimplify	( tcSimplifyTop )
import TcTyDecls	( tcTyDecls )
import Unique		-- some ClassKey stuff
import UniqFM		( emptyUFM ) -- profiling, pragmas only
import Util

import Pretty		-- Debugging
\end{code}

\begin{code}
tcModule :: E				-- initial typechecker environment
	 -> GlobalNameFuns		-- final renamer info (to do derivings)
	 -> RenamedModule		-- input
	 -> TcM ((TypecheckedBinds,	-- binds from class decls; does NOT
					-- include default-methods bindings
		  TypecheckedBinds,	-- binds from instance decls; INCLUDES
					-- class default-methods binds
		  TypecheckedBinds,	-- binds from value decls
		  [(Inst, TypecheckedExpr)]),

		 ([RenamedFixityDecl],	-- things for the interface generator
		  [Id],			-- to look at...
		  CE,
		  TCE,
		  Bag InstInfo),

		 FiniteMap TyCon [(Bool, [Maybe UniType])],
					-- source tycon specialisation requests

--UNUSED:	 E,			-- environment of total accumulated info
		 E,			-- environment of info due to this module only
		 PprStyle -> Pretty)	-- -ddump-deriving info (passed upwards)

tcModule e1 renamer_name_funs
	(Module mod_name exports imports_should_be_empty fixities
	     tydecls ty_sigs classdecls instdecls specinst_sigs
	     default_decls valdecls sigs src_loc)

  = addSrcLocTc src_loc (	-- record where we're starting

	-- Tie the knot for inteface-file value declaration signatures
	-- This info is only used inside the knot for type-checking the
	-- pragmas, which is done lazily [ie failure just drops the pragma
	-- without having any global-failure effect].

    fixTc (\ ~(rec_gve_sigs, _, _, _, _, _, _, _, _, _) ->
    let
	e2 = plusE_GVE e1 rec_gve_sigs
    in

	-- The knot for instance information.  This isn't used at all
	-- till we type-check value declarations.
	fixTc ( \ ~(rec_inst_mapper, _, _, _, _, _, _, _, _) ->

	     -- The knot for TyCons and Classes
	    fixTc ( \ ~(_, rec_tce, rec_ce, rec_datacons_gve, rec_ops_gve, _, _) ->
		let
		    e3 = e2
			 `plusE_GVE` rec_datacons_gve
			 `plusE_GVE` rec_ops_gve
			 `plusE_TCE` rec_tce
			 `plusE_CE`  rec_ce
		in
		    -- DO THE TYPE DECLS
		    -- Including the pragmas: {-# ABSTRACT TypeSyn #-}
		    --			      {-# SPECIALIZE data DataType ... #-}
		let
		    (absty_sigs, specdata_sigs) = partition is_absty_sig ty_sigs
		    is_absty_sig (AbstractTypeSig _ _) = True
		    is_absty_sig (SpecDataSig _ _ _)   = False

		    is_abs_syn :: Name -> Bool  -- a lookup fn for abs synonyms
		    is_abs_syn n
		      = n `is_elem` [ tc | (AbstractTypeSig tc _) <- absty_sigs ]
		      where
		        is_elem = isIn "tcModule"

		    get_spec_sigs :: Name -> [RenamedDataTypeSig]
		    get_spec_sigs n
		      = [ sig | sig@(SpecDataSig tc _ _) <- specdata_sigs, n == tc]
		in
		babyTcMtoTcM (tcTyDecls e3 is_abs_syn get_spec_sigs tydecls)
			`thenTc` \ (tce, datacons_gve, tycon_specs) ->

		    -- DO THE CLASS DECLS
		tcClassDecls1 e3 rec_inst_mapper classdecls
			`thenTc` \ (class_info, ce, ops_gve) ->

		    -- End of TyCon/Class knot
		    -- Augment whatever TCE/GVE/CE stuff was in orig_e
		returnTc (e3, tce, ce, datacons_gve, ops_gve, class_info, tycon_specs)

		   -- End of inner fixTc
	    )   `thenTc` ( \ (e3, tce_here, ce_here, _, _, class_info, tycon_specs) ->
			     -- The "here" things are the extra decls defined in this
			     -- module or its imports; but not including whatever was
			     -- in the incoming e.

		    -- Grab completed tce/ce and check for type/class cycles
		    -- The tce/ce are now stable and lookable-at, with the
		    -- exception of the instance information inside classes
	    let
		ce3  = getE_CE e3
		tce3 = getE_TCE e3
	    in
	    checkMaybeErrTc (checkTypeCycles tce3) id    `thenTc_`
	    checkMaybeErrTc (checkClassCycles ce3) id    `thenTc_`

		    -- Now instance declarations
	    tcInstDecls1 e3 ce3 tce3 instdecls		`thenNF_Tc` \ decl_inst_info ->

		    -- Handle "derived" instances; note that we only do derivings
		    -- for things in this module; we ignore deriving decls from
		    -- interfaces! We pass fixities, because they may be used in
		    -- doing Text.

	    tcDeriving mod_name renamer_name_funs decl_inst_info tce3 fixities
		    `thenTc` \ (deriv_inst_info, extra_deriv_binds, ddump_deriv) ->

	    let
		inst_info = deriv_inst_info `unionBags` decl_inst_info 
	    in
		    -- Handle specialise instance pragmas
	    getSwitchCheckerTc			`thenNF_Tc` \ sw_chkr ->
	    (if sw_chkr SpecialiseOverloaded then
	         tcSpecInstSigs e3 ce3 tce3 inst_info specinst_sigs
	     else
		 returnTc emptyBag)
		    				`thenTc` \ spec_inst_info ->
	    let
		full_inst_info = inst_info `unionBags` spec_inst_info 
	    in
		    -- OK, now do the inst-mapper stuff
	    buildInstanceEnvs full_inst_info	`thenTc` \ all_insts_mapper ->

	    returnTc (all_insts_mapper, e3, ce_here, tce_here, class_info, tycon_specs,
		      full_inst_info, extra_deriv_binds, ddump_deriv)

		    -- End of outer fixTc
	)) `thenTc` ( \ (_, e3, ce_here, tce_here, class_info, tycon_specs,
			full_inst_info, extra_deriv_binds, ddump_deriv) ->

    -- Default declarations
    tcDefaults e3 default_decls	`thenTc` \ defaulting_tys ->
    setDefaultingTys defaulting_tys ( -- for the iface sigs...

    -- Interface type signatures

    -- We tie a knot so that the Ids read out of interfaces are in scope
    --   when we read their pragmas.
    -- What we rely on is that pragmas are typechecked lazily; if
    --   any type errors are found (ie there's an inconsistency) 
    --   we silently discard the pragma

    babyTcMtoTcM (tcInterfaceSigs e3 sigs)	`thenTc` \ gve_sigs ->

    returnTc (gve_sigs, e3, ce_here, tce_here, class_info, tycon_specs, defaulting_tys,
  	      full_inst_info, extra_deriv_binds, ddump_deriv)

    -- End of extremely outer fixTc
    )))	`thenTc` \ (_, e3, ce_here, tce_here, class_info, tycon_specs, defaulting_tys,
		    full_inst_info, extra_deriv_binds, ddump_deriv) ->

    setDefaultingTys defaulting_tys ( -- to the end...

	-- Value declarations next.
	-- We also typecheck any extra binds that came out of the "deriving" process
	-- Nota bene
    tcTopBindsAndThen
	e3
	(\ binds1 (binds2, thing) -> (binds1 `ThenBinds` binds2, thing))
	(valdecls `ThenBinds` extra_deriv_binds)
	(\ e4 ->
		-- Second pass over instance declarations,
		-- to compile the bindings themselves.
	    tcInstDecls2  e4 full_inst_info `thenNF_Tc` \ (lie_instdecls, inst_binds) ->
	    tcClassDecls2 e4 class_info	    `thenNF_Tc` \ (lie_clasdecls, class_binds) ->
	    returnTc ( (EmptyBinds, (inst_binds, class_binds, e4)),
		       lie_instdecls `plusLIE` lie_clasdecls,
		       () )
	)

	`thenTc` \ ((val_binds, (inst_binds, class_binds, e4)), lie_alldecls, _) ->

	-- Deal with constant or ambiguous InstIds.  How could
	-- there be ambiguous ones?  They can only arise if a
	-- top-level decl falls under the monomorphism
	-- restriction, and no subsequent decl instantiates its
	-- type.  (Usually, ambiguous type variables are resolved
	-- during the generalisation step.)

    tcSimplifyTop (unMkLIE lie_alldecls)    `thenTc` \ const_inst_binds ->

	-- Backsubstitution.  Monomorphic top-level decls may have
	-- been instantiated by subsequent decls, and the final
	-- simplification step may have instantiated some
	-- ambiguous types.  So, sadly, we need to back-substitute
	-- over the whole bunch of bindings.

    applyTcSubstToBinds val_binds	    `thenNF_Tc` \ val_binds' ->
    applyTcSubstToBinds inst_binds	    `thenNF_Tc` \ inst_binds' ->
    applyTcSubstToBinds class_binds	    `thenNF_Tc` \ class_binds' ->

	-- ToDo: probably need to back-substitute over all
	-- stuff in 'e4'; we do so here over the Ids,
	-- which is probably enough.  WDP 95/06
    mapNF_Tc applyTcSubstToId (getE_GlobalVals e4)
					    `thenNF_Tc` \ if_global_ids ->

	-- FINISHED AT LAST
    returnTc (
	(class_binds', inst_binds', val_binds', const_inst_binds),

	     -- the next collection is just for mkInterface
	(fixities, if_global_ids, ce_here, tce_here, full_inst_info),

	tycon_specs,

--UNUSED: e4,

	  -- and... TCE needed for code generation; rest needed for interpreter.
	  -- ToDo: still wrong: needs isLocallyDeclared run over everything
	mkE tce_here {-gve_here lve-} ce_here,
	     -- NB: interpreter would probably need the gve_here stuff
	ddump_deriv
    )))
\end{code}
