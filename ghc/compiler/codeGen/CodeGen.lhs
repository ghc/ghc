%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[CodeGen]{@CodeGen@: main module of the code generator}

This module says how things get going at the top level.

@codeGen@ is the interface to the outside world.  The \tr{cgTop*}
functions drive the mangling of top-level bindings.

%************************************************************************
%*									*
\subsection[codeGen-outside-interface]{The code generator's offering to the world}
%*									*
%************************************************************************

\begin{code}
#include "HsVersions.h"

module CodeGen ( codeGen ) where

import Ubiq{-uitous-}

import StgSyn
import CgMonad
import AbsCSyn

import AbsCUtils	( mkAbstractCs, mkAbsCStmts )
import Bag		( foldBag )
import CgClosure	( cgTopRhsClosure )
import CgCon		( cgTopRhsCon )
import CgConTbls	( genStaticConBits )
import ClosureInfo	( mkClosureLFInfo )
import CmdLineOpts	( opt_SccProfilingOn, opt_CompilingPrelude,
			  opt_EnsureSplittableC, opt_SccGroup
			)
import CStrings		( modnameToC )
import Maybes		( maybeToBool )
import PrimRep		( getPrimRepSize, PrimRep(..) )
import Util		( panic, assertPanic )
\end{code}

\begin{code}
codeGen :: FAST_STRING		-- module name
	-> ([CostCentre],	-- local cost-centres needing declaring/registering
	    [CostCentre])	-- "extern" cost-centres needing declaring
	-> Bag FAST_STRING	-- import names
	-> [TyCon]		-- tycons with data constructors to convert
	-> FiniteMap TyCon [(Bool, [Maybe Type])]
				-- tycon specialisation info
	-> [StgBinding]	-- bindings to convert
	-> AbstractC		-- output

codeGen mod_name (local_CCs, extern_CCs) import_names gen_tycons tycon_specs stg_pgm
  = let
	doing_profiling   = opt_SccProfilingOn
	compiling_prelude = opt_CompilingPrelude
	maybe_split       = if maybeToBool (opt_EnsureSplittableC)
			    then CSplitMarker
			    else AbsCNop

	cinfo = MkCompInfo mod_name
    in
    if not doing_profiling then
	mkAbstractCs [
	    genStaticConBits cinfo gen_tycons tycon_specs,
	    initC cinfo (cgTopBindings maybe_split stg_pgm) ]

    else -- yes, cost-centre profiling:
	 -- Besides the usual stuff, we must produce:
	 --
    	 -- * Declarations for the cost-centres defined in this module;
	 -- * Code to participate in "registering" all the cost-centres
	 --   in the program (done at startup time when the pgm is run).
	 --
	 -- (The local cost-centres involved in this are passed
	 -- into the code-generator, as are the imported-modules' names.)
	 --
	 -- Note: we don't register/etc if compiling Prelude bits.

	mkAbstractCs [
		if compiling_prelude
		then AbsCNop
		else mkAbstractCs [mkAbstractCs (map (CCostCentreDecl True)  local_CCs),
				   mkAbstractCs (map (CCostCentreDecl False) extern_CCs),
				   mkCcRegister local_CCs import_names],

		genStaticConBits cinfo gen_tycons tycon_specs,
		initC cinfo (cgTopBindings maybe_split stg_pgm) ]
  where
    -----------------
    grp_name  = case opt_SccGroup of
		  Just xx -> xx
		  Nothing -> mod_name	-- default: module name

    -----------------
    mkCcRegister ccs import_names
      = let
	    register_ccs     = mkAbstractCs (map mk_register ccs)
	    register_imports
	      = foldBag mkAbsCStmts mk_import_register AbsCNop import_names
	in
	mkAbstractCs [
	    CCallProfCCMacro SLIT("START_REGISTER_CCS") [CLitLit (modnameToC (SLIT("_reg") _APPEND_ mod_name)) AddrRep],
	    register_ccs,
	    register_imports,
	    CCallProfCCMacro SLIT("END_REGISTER_CCS") []
	]
      where
	mk_register cc
	  = CCallProfCCMacro SLIT("REGISTER_CC") [mkCCostCentre cc]

	mk_import_register import_name
	  = CCallProfCCMacro SLIT("REGISTER_IMPORT") [CLitLit (modnameToC (SLIT("_reg") _APPEND_ import_name)) AddrRep]
\end{code}

%************************************************************************
%*									*
\subsection[codegen-top-bindings]{Converting top-level STG bindings}
%*									*
%************************************************************************

@cgTopBindings@ is only used for top-level bindings, since they need
to be allocated statically (not in the heap) and need to be labelled.
No unboxed bindings can happen at top level.

In the code below, the static bindings are accumulated in the
@MkCgState@, and transferred into the ``statics'' slot by @forkStatics@.
This is so that we can write the top level processing in a compositional
style, with the increasing static environment being plumbed as a state
variable.

\begin{code}
cgTopBindings :: AbstractC -> [StgBinding] -> Code

cgTopBindings split bindings = mapCs (cgTopBinding split) bindings

cgTopBinding :: AbstractC -> StgBinding -> Code

cgTopBinding split (StgNonRec name rhs)
  = absC split		`thenC`
    cgTopRhs name rhs	`thenFC` \ (name, info) ->
    addBindC name info

cgTopBinding split (StgRec pairs)
  = absC split		`thenC`
    fixC (\ new_binds -> addBindsC new_binds	`thenC`
			 mapFCs ( \ (b,e) -> cgTopRhs b e ) pairs
    )			`thenFC` \ new_binds ->
    addBindsC new_binds

-- Urgh!  I tried moving the forkStatics call from the rhss of cgTopRhs
-- to enclose the listFCs in cgTopBinding, but that tickled the
-- statics "error" call in initC.  I DON'T UNDERSTAND WHY!

cgTopRhs :: Id -> StgRhs -> FCode (Id, CgIdInfo)
	-- the Id is passed along for setting up a binding...

cgTopRhs name (StgRhsCon cc con args)
  = forkStatics (cgTopRhsCon name con args (all zero_size args))
  where
    zero_size atom = getPrimRepSize (getArgPrimRep atom) == 0

cgTopRhs name (StgRhsClosure cc bi fvs upd_flag args body)
  = ASSERT(null fvs) -- There should be no free variables
    forkStatics (cgTopRhsClosure name cc bi args body lf_info)
  where
    lf_info = mkClosureLFInfo True{-top level-} [{-no fvs-}] upd_flag args body
\end{code}
