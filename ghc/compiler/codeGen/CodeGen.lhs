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
import CLabel		( CLabel, mkSRTLabel, mkClosureLabel, mkModuleInitLabel )

import PprAbsC		( dumpRealC )
import AbsCUtils	( mkAbstractCs, mkAbsCStmts, flattenAbsC )
import CgBindery	( CgIdInfo, addBindC, addBindsC )
import CgClosure	( cgTopRhsClosure )
import CgCon		( cgTopRhsCon )
import CgConTbls	( genStaticConBits )
import ClosureInfo	( mkClosureLFInfo )
import CmdLineOpts	( opt_SccProfilingOn, opt_EnsureSplittableC, 
			  opt_D_dump_absC
			)
import CostCentre       ( CostCentre, CostCentreStack )
import FiniteMap	( FiniteMap )
import Id               ( Id, idName )
import Module           ( Module, moduleString, moduleName, 
			  ModuleName, moduleNameString )
import PrimRep		( getPrimRepSize, PrimRep(..) )
import Type             ( Type )
import TyCon            ( TyCon, isDataTyCon )
import Class		( Class, classTyCon )
import BasicTypes	( TopLevelFlag(..) )
import UniqSupply	( mkSplitUniqSupply )
import ErrUtils		( dumpIfSet )
import Util
import Panic		( assertPanic )
\end{code}

\begin{code}


codeGen :: Module		-- Module name
	-> [ModuleName]		-- Import names
	-> ([CostCentre],	-- Local cost-centres needing declaring/registering
	    [CostCentre],	-- "extern" cost-centres needing declaring
	    [CostCentreStack])  -- Pre-defined "singleton" cost centre stacks
	-> [Id]			-- foreign-exported binders
	-> [TyCon] -> [Class]	-- Local tycons and classes
	-> [(StgBinding,[Id])]	-- Bindings to convert, with SRTs
	-> IO AbstractC		-- Output

codeGen mod_name imported_modules cost_centre_info fe_binders
	tycons classes stg_binds
  = mkSplitUniqSupply 'f'	>>= \ fl_uniqs  -> -- absC flattener
    let
	datatype_stuff 	  = genStaticConBits cinfo data_tycons
	code_stuff     	  = initC cinfo (cgTopBindings maybe_split stg_binds)
	init_stuff        = mkModuleInit fe_binders mod_name imported_modules 
					 cost_centre_info

	abstractC = mkAbstractCs [ init_stuff, 
				   code_stuff,
				   datatype_stuff]
		-- Put datatype_stuff after code_stuff, because the
		-- datatype closure table (for enumeration types)
		-- to (say) PrelBase_True_closure, which is defined in code_stuff

	flat_abstractC = flattenAbsC fl_uniqs abstractC
    in
    dumpIfSet opt_D_dump_absC "Abstract C" (dumpRealC abstractC)	>>
    return flat_abstractC

  where
    data_tycons = filter isDataTyCon (tycons ++ map classTyCon classes)
			-- Generate info tables  for the data constrs arising
			-- from class decls as well

    maybe_split = if opt_EnsureSplittableC 
		  then CSplitMarker 
		  else AbsCNop
    cinfo       = MkCompInfo mod_name
\end{code}

%************************************************************************
%*									*
\subsection[codegen-init]{Module initialisation code}
%*									*
%************************************************************************

\begin{code}
mkModuleInit 
	:: [Id]			-- foreign exported functions
	-> Module		-- module name
	-> [ModuleName]		-- import names
	-> ([CostCentre],	-- cost centre info
	    [CostCentre],	
	    [CostCentreStack])
	-> AbstractC
mkModuleInit fe_binders mod imps cost_centre_info
  = let
	register_fes = 
	   map (\f -> CMacroStmt REGISTER_FOREIGN_EXPORT [f]) fe_labels

    	fe_labels = 
	   map (\f -> CLbl (mkClosureLabel (idName f)) PtrRep) fe_binders

	(cc_decls, cc_regs) = mkCostCentreStuff cost_centre_info

	mk_import_register import_name
	  = CMacroStmt REGISTER_IMPORT [
		CLbl (mkModuleInitLabel import_name) AddrRep
	    ]

	register_imports = map mk_import_register imps
    in
    mkAbstractCs [
	cc_decls,
        CModuleInitBlock (mkModuleInitLabel (Module.moduleName mod))
		         (mkAbstractCs (register_fes ++
				        cc_regs :
				        register_imports))
    ]
\end{code}

Cost-centre profiling: Besides the usual stuff, we must produce
declarations for the cost-centres defined in this module;

(The local cost-centres involved in this are passed into the
code-generator.)

\begin{code}
mkCostCentreStuff (local_CCs, extern_CCs, singleton_CCSs)
  | not opt_SccProfilingOn = (AbsCNop, AbsCNop)
  | otherwise = 
	( mkAbstractCs (
		map (CCostCentreDecl True)   local_CCs ++
		map (CCostCentreDecl False)  extern_CCs ++
		map CCostCentreStackDecl     singleton_CCSs),
  	  mkAbstractCs (mkCcRegister local_CCs singleton_CCSs)
	)
  where
    mkCcRegister ccs cc_stacks
      = let
	    register_ccs       = mkAbstractCs (map mk_register ccs)
	    register_cc_stacks = mkAbstractCs (map mk_register_ccs cc_stacks)
	in
	[ register_ccs, register_cc_stacks ]
      where
	mk_register cc
	  = CCallProfCCMacro SLIT("REGISTER_CC") [mkCCostCentre cc]

	mk_register_ccs ccs
	  = CCallProfCCMacro SLIT("REGISTER_CCS") [mkCCostCentreStack ccs]
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
  = forkStatics (cgTopRhsCon bndr con args)

cgTopRhs bndr (StgRhsClosure cc bi srt fvs upd_flag args body)
  = ASSERT(null fvs) -- There should be no free variables
    getSRTLabel `thenFC` \srt_label ->
    let lf_info = 
	  mkClosureLFInfo bndr TopLevel [{-no fvs-}] upd_flag args srt_label srt
    in
    forkStatics (cgTopRhsClosure bndr cc bi args body lf_info)
\end{code}
