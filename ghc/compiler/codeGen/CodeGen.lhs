%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
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
module CodeGen ( codeGen ) where

#include "HsVersions.h"

import StgSyn
import CgMonad
import AbsCSyn
import CLabel		( CLabel, mkSRTLabel, mkClosureLabel )

import AbsCUtils	( mkAbstractCs, mkAbsCStmts )
import CgBindery	( CgIdInfo )
import CgClosure	( cgTopRhsClosure )
import CgCon		( cgTopRhsCon )
import CgConTbls	( genStaticConBits )
import ClosureInfo	( mkClosureLFInfo )
import CmdLineOpts	( opt_SccProfilingOn, opt_EnsureSplittableC, 
					      opt_SccGroup
			)
import CostCentre       ( CostCentre, CostCentreStack )
import FiniteMap	( FiniteMap )
import Id               ( Id, idName )
import Module           ( Module, moduleString )
import PrimRep		( getPrimRepSize, PrimRep(..) )
import Type             ( Type )
import TyCon            ( TyCon )
import BasicTypes	( TopLevelFlag(..) )
import Util
import Panic		( assertPanic )
\end{code}

\begin{code}
codeGen :: Module		-- module name
	-> ([CostCentre],	-- local cost-centres needing declaring/registering
	    [CostCentre],	-- "extern" cost-centres needing declaring
	    [CostCentreStack])  -- pre-defined "singleton" cost centre stacks
	-> [Module]		-- import names
	-> [TyCon]		-- tycons with data constructors to convert
	-> FiniteMap TyCon [(Bool, [Maybe Type])]
				-- tycon specialisation info
	-> [(StgBinding,[Id])]	-- bindings to convert, with SRTs
	-> AbstractC		-- output

codeGen mod_name (local_CCs, extern_CCs, singleton_CCSs) 
	import_names gen_tycons tycon_specs stg_pgm
  = let
	maybe_split       = if opt_EnsureSplittableC 
				then CSplitMarker 
				else AbsCNop
	cinfo             = MkCompInfo mod_name
    in
    let 
	module_code = mkAbstractCs [
	    genStaticConBits cinfo gen_tycons tycon_specs,
	    initC cinfo (cgTopBindings maybe_split stg_pgm) ]

	 -- Cost-centre profiling:
	 -- Besides the usual stuff, we must produce:
	 --
    	 -- * Declarations for the cost-centres defined in this module;
	 -- * Code to participate in "registering" all the cost-centres
	 --   in the program (done at startup time when the pgm is run).
	 --
	 -- (The local cost-centres involved in this are passed
	 -- into the code-generator, as are the imported-modules' names.)
	 --
	 --
	cost_centre_stuff 
		| not opt_SccProfilingOn = AbsCNop
		| otherwise = mkAbstractCs (
		    map (CCostCentreDecl True)   local_CCs ++
		    map (CCostCentreDecl False)  extern_CCs ++
		    map CCostCentreStackDecl     singleton_CCSs ++
		    mkCcRegister local_CCs singleton_CCSs import_names
		   )
   in
   mkAbstractCs [ cost_centre_stuff, module_code ]

  where
    mkCcRegister ccs cc_stacks import_names
      = let
	    register_ccs     = mkAbstractCs (map mk_register ccs)
	    register_imports
	      = foldr (mkAbsCStmts . mk_import_register) AbsCNop import_names
	    register_cc_stacks = mkAbstractCs (map mk_register_ccs cc_stacks)
	in
	[
	    CCallProfCCMacro SLIT("START_REGISTER_CCS") 
	       [ CLitLit (_PK_ ("_reg" ++ moduleString mod_name)) AddrRep],
	    register_ccs,
	    register_cc_stacks,
	    register_imports,
	    CCallProfCCMacro SLIT("END_REGISTER_CCS") []
	]
      where
	mk_register cc
	  = CCallProfCCMacro SLIT("REGISTER_CC") [mkCCostCentre cc]

	mk_register_ccs ccs
	  = CCallProfCCMacro SLIT("REGISTER_CCS") [mkCCostCentreStack ccs]

	mk_import_register import_name
	  = CCallProfCCMacro SLIT("REGISTER_IMPORT") 
	      [CLitLit (_PK_ ("_reg" ++ moduleString import_name)) AddrRep]
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
cgTopBindings :: AbstractC -> [(StgBinding,[Id])] -> Code

cgTopBindings split bindings = mapCs (cgTopBinding split) bindings

cgTopBinding :: AbstractC -> (StgBinding,[Id]) -> Code

cgTopBinding split ((StgNonRec name rhs), srt)
  = absC split		  	`thenC`
    absC (mkSRT srt_label srt) 	`thenC`
    setSRTLabel srt_label (
    cgTopRhs name rhs	  	`thenFC` \ (name, info) ->
    addBindC name info
    )
  where
    srt_label = mkSRTLabel (idName name)

cgTopBinding split ((StgRec pairs@((name,rhs):_)), srt)
  = absC split		  	`thenC`
    absC (mkSRT srt_label srt) 	`thenC`
    setSRTLabel srt_label (
    fixC (\ new_binds -> addBindsC new_binds	`thenC`
			 mapFCs ( \ (b,e) -> cgTopRhs b e ) pairs
    )			  `thenFC` \ new_binds ->
    addBindsC new_binds
    )
  where
    srt_label = mkSRTLabel (idName name)

mkSRT :: CLabel -> [Id] -> AbstractC
mkSRT lbl []  = AbsCNop
mkSRT lbl ids = CSRT lbl (map (mkClosureLabel . idName) ids)

-- Urgh!  I tried moving the forkStatics call from the rhss of cgTopRhs
-- to enclose the listFCs in cgTopBinding, but that tickled the
-- statics "error" call in initC.  I DON'T UNDERSTAND WHY!

cgTopRhs :: Id -> StgRhs -> FCode (Id, CgIdInfo)
	-- the Id is passed along for setting up a binding...

cgTopRhs bndr (StgRhsCon cc con args)
  = forkStatics (cgTopRhsCon bndr con args (all zero_size args))
  where
    zero_size atom = getPrimRepSize (getArgPrimRep atom) == 0

cgTopRhs bndr (StgRhsClosure cc bi srt fvs upd_flag args body)
  = ASSERT(null fvs) -- There should be no free variables
    forkStatics (cgTopRhsClosure bndr cc bi srt args body lf_info)
  where
    lf_info = mkClosureLFInfo bndr TopLevel [{-no fvs-}] upd_flag args
\end{code}
