%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
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

module CodeGen (
	codeGen,

	-- and to make the interface self-sufficient...
	UniqFM, AbstractC, StgBinding, Id, FiniteMap
    ) where


import StgSyn
import CgMonad
import AbsCSyn

import CLabelInfo	( modnameToC )
import CgClosure	( cgTopRhsClosure )
import CgCon		( cgTopRhsCon )
import CgConTbls	( genStaticConBits, TCE(..), UniqFM )
import ClosureInfo	( LambdaFormInfo, mkClosureLFInfo )
import CmdLineOpts	( GlobalSwitch(..), switchIsOn, stringSwitchSet, SwitchResult )
import FiniteMap	( FiniteMap )
import Maybes		( Maybe(..) )
import PrimKind		( getKindSize )
import Util
\end{code}

\begin{code}
codeGen :: FAST_STRING		-- module name
	-> ([CostCentre],	-- local cost-centres needing declaring/registering
	    [CostCentre])	-- "extern" cost-centres needing declaring
	-> [FAST_STRING]	-- import names
	-> (GlobalSwitch -> SwitchResult)
				-- global switch lookup function
	-> [TyCon]		-- tycons with data constructors to convert
	-> FiniteMap TyCon [[Maybe UniType]]
				-- tycon specialisation info
	-> PlainStgProgram	-- bindings to convert
	-> AbstractC		-- output

codeGen mod_name (local_CCs, extern_CCs) import_names sw_lookup_fn gen_tycons tycon_specs stg_pgm
  = let
	switch_is_on	  = switchIsOn sw_lookup_fn
	doing_profiling   = switch_is_on SccProfilingOn
	compiling_prelude = switch_is_on CompilingPrelude
	splitting         = switch_is_on (EnsureSplittableC (panic "codeGen:esc"))
    in
    if not doing_profiling then
	let
	    cinfo = MkCompInfo switch_is_on mod_name
	in
	mkAbstractCs [
	    genStaticConBits cinfo gen_tycons tycon_specs,
	    initC cinfo (cgTopBindings splitting stg_pgm) ]

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
	let
	    cinfo = MkCompInfo switch_is_on mod_name
	in
	mkAbstractCs [
		if compiling_prelude
		then AbsCNop
		else mkAbstractCs [mkAbstractCs (map (CCostCentreDecl True)  local_CCs),
				   mkAbstractCs (map (CCostCentreDecl False) extern_CCs),
				   mkCcRegister local_CCs import_names],

		genStaticConBits cinfo gen_tycons tycon_specs,
		initC cinfo (cgTopBindings splitting stg_pgm) ]
  where
    -----------------
    grp_name  = case (stringSwitchSet sw_lookup_fn SccGroup) of
		  Just xx -> _PK_ xx
		  Nothing -> mod_name	-- default: module name

    -----------------
    mkCcRegister ccs import_names
      = let 
	    register_ccs     = mkAbstractCs (map mk_register ccs)
	    register_imports = mkAbstractCs (map mk_import_register import_names)
	in
	mkAbstractCs [
	    CCallProfCCMacro SLIT("START_REGISTER_CCS") [CLitLit (modnameToC (SLIT("_reg") _APPEND_ mod_name)) AddrKind],
	    register_ccs,
	    register_imports,
	    CCallProfCCMacro SLIT("END_REGISTER_CCS") []
	]
      where
	mk_register cc
	  = CCallProfCCMacro SLIT("REGISTER_CC") [mkCCostCentre cc]

	mk_import_register import_name
	  = CCallProfCCMacro SLIT("REGISTER_IMPORT") [CLitLit (modnameToC (SLIT("_reg") _APPEND_ import_name)) AddrKind]
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
cgTopBindings :: Bool -> PlainStgProgram -> Code

cgTopBindings splitting bindings = mapCs (cgTopBinding splitting) bindings
  
cgTopBinding :: Bool -> PlainStgBinding -> Code

cgTopBinding splitting (StgNonRec name rhs) 
  = absC maybe_split	`thenC`
    cgTopRhs name rhs	`thenFC` \ (name, info) ->
    addBindC name info
  where
    maybe_split = if splitting then CSplitMarker else AbsCNop

cgTopBinding splitting (StgRec pairs) 
  = absC maybe_split	`thenC`
    fixC (\ new_binds -> addBindsC new_binds	`thenC`
			 mapFCs ( \ (b,e) -> cgTopRhs b e ) pairs
    )			`thenFC` \ new_binds ->
    addBindsC new_binds
  where
    maybe_split = if splitting then CSplitMarker else AbsCNop

-- Urgh!  I tried moving the forkStatics call from the rhss of cgTopRhs
-- to enclose the listFCs in cgTopBinding, but that tickled the
-- statics "error" call in initC.  I DON'T UNDERSTAND WHY!

cgTopRhs :: Id -> PlainStgRhs -> FCode (Id, CgIdInfo)
	-- the Id is passed along for setting up a binding...

cgTopRhs name (StgRhsCon cc con args)
  = forkStatics (cgTopRhsCon name con args (all zero_size args))
  where
    zero_size atom = getKindSize (getAtomKind atom) == 0

cgTopRhs name (StgRhsClosure cc bi fvs upd_flag args body)
  = ASSERT(null fvs) -- There should be no free variables
    forkStatics (cgTopRhsClosure name cc bi args body lf_info)
  where
    lf_info = mkClosureLFInfo True{-top level-} [{-no fvs-}] upd_flag args body
\end{code}
