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

-- Kludge (??) so that CgExpr is reached via at least one non-SOURCE
-- import.  Before, that wasn't the case, and CM therefore didn't 
-- bother to compile it.
import CgExpr           ( {-NOTHING!-} )	-- DO NOT DELETE THIS IMPORT

import StgSyn
import CgMonad
import AbsCSyn
import CLabel		( CLabel, mkSRTLabel, mkClosureLabel, mkModuleInitLabel )

import PprAbsC		( dumpRealC )
import AbsCUtils	( mkAbstractCs, flattenAbsC )
import CgBindery	( CgIdInfo, addBindC, addBindsC, getCAddrModeAndInfo )
import CgClosure	( cgTopRhsClosure )
import CgCon		( cgTopRhsCon )
import CgConTbls	( genStaticConBits )
import ClosureInfo	( mkClosureLFInfo )
import CmdLineOpts	( DynFlags, DynFlag(..),
			  opt_SccProfilingOn, opt_EnsureSplittableC )
import CostCentre       ( CostCentre, CostCentreStack )
import Id               ( Id, idName, setIdName )
import Name		( globaliseName )
import Module           ( Module )
import PrimRep		( PrimRep(..) )
import TyCon            ( TyCon, isDataTyCon )
import BasicTypes	( TopLevelFlag(..) )
import UniqSupply	( mkSplitUniqSupply )
import ErrUtils		( dumpIfSet_dyn, showPass )
import Panic		( assertPanic )

#ifdef DEBUG
import Outputable
#endif
\end{code}

\begin{code}
codeGen :: DynFlags
	-> Module		-- Module name
	-> [Module]		-- Import names
	-> ([CostCentre],	-- Local cost-centres needing declaring/registering
	    [CostCentre],	-- "extern" cost-centres needing declaring
	    [CostCentreStack])  -- Pre-defined "singleton" cost centre stacks
	-> [Id]			-- foreign-exported binders
	-> [TyCon] 		-- Local tycons, including ones from classes
	-> [(StgBinding,[Id])]	-- Bindings to convert, with SRTs
	-> IO AbstractC		-- Output

codeGen dflags mod_name imported_modules cost_centre_info fe_binders
	tycons stg_binds
  = do	{ showPass dflags "CodeGen"

	; fl_uniqs <- mkSplitUniqSupply 'f'
	; let
	    datatype_stuff = genStaticConBits cinfo data_tycons
	    code_stuff     = initC cinfo (mapCs cgTopBinding stg_binds)
	    init_stuff     = mkModuleInit fe_binders mod_name imported_modules 
					  cost_centre_info

	    abstractC = mkAbstractCs [ maybeSplitCode,
				       init_stuff, 
				       code_stuff,
				       datatype_stuff]
		-- Put datatype_stuff after code_stuff, because the
		-- datatype closure table (for enumeration types)
		-- to (say) PrelBase_True_closure, which is defined in code_stuff

	    flat_abstractC = flattenAbsC fl_uniqs abstractC

	; dumpIfSet_dyn dflags Opt_D_dump_absC "Abstract C" (dumpRealC abstractC)
	; return flat_abstractC
	}
  where
    data_tycons = filter isDataTyCon tycons
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
	-> [Module]		-- import names
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

	mk_import_register imp =
	    CMacroStmt REGISTER_IMPORT [
		CLbl (mkModuleInitLabel imp) AddrRep
	    ]

	register_imports = map mk_import_register imps
    in
    mkAbstractCs [
	cc_decls,
        CModuleInitBlock (mkModuleInitLabel mod)
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

@cgTopBinding@ is only used for top-level bindings, since they need
to be allocated statically (not in the heap) and need to be labelled.
No unboxed bindings can happen at top level.

In the code below, the static bindings are accumulated in the
@MkCgState@, and transferred into the ``statics'' slot by @forkStatics@.
This is so that we can write the top level processing in a compositional
style, with the increasing static environment being plumbed as a state
variable.

\begin{code}
cgTopBinding :: (StgBinding,[Id]) -> Code
cgTopBinding (StgNonRec srt_info id rhs, srt)
  = absC maybeSplitCode	  	`thenC`
    maybeGlobaliseId id		`thenFC` \ id' ->
    let
	srt_label = mkSRTLabel (idName id')
    in
    mkSRT srt_label srt [] 	`thenC`
    setSRTLabel srt_label (
    cgTopRhs id' rhs srt_info  	`thenFC` \ (id, info) ->
    addBindC id info
    )

cgTopBinding (StgRec srt_info pairs, srt)
  = absC maybeSplitCode		  	`thenC`
    let
        (bndrs, rhss) = unzip pairs
    in
    mapFCs maybeGlobaliseId bndrs	`thenFC` \ bndrs'@(id:_) ->
    let
	srt_label = mkSRTLabel (idName id)
	pairs'    = zip bndrs' rhss
    in
    mkSRT srt_label srt bndrs'		`thenC`
    setSRTLabel srt_label (
       fixC (\ new_binds -> 
		addBindsC new_binds		`thenC`
		mapFCs ( \ (b,e) -> cgTopRhs b e srt_info ) pairs'
       )  `thenFC` \ new_binds -> nopC
    )

mkSRT :: CLabel -> [Id] -> [Id] -> Code
mkSRT lbl []  these = nopC
mkSRT lbl ids these
  = mapFCs remap ids `thenFC` \ ids ->
    absC (CSRT lbl (map (mkClosureLabel . idName) ids))
  where
	-- sigh, better map all the ids against the environment in case they've
	-- been globalised (see maybeGlobaliseId below).
    remap id = case filter (==id) these of
		[] ->  getCAddrModeAndInfo id 
				`thenFC` \ (id, _, _) -> returnFC id
		(id':_) -> returnFC id'

-- If we're splitting the object, we need to globalise all the top-level names
-- (and then make sure we only use the globalised one in any C label we use
-- which refers to this name).
maybeGlobaliseId :: Id -> FCode Id
maybeGlobaliseId id
  = moduleName `thenFC` \ mod ->
    let
    	name = idName id

	-- globalise the name for -split-objs, if necessary
	real_name | opt_EnsureSplittableC = globaliseName name mod
	          | otherwise             = name

	id' = setIdName id real_name
    in 
    returnFC id'

maybeSplitCode
  | opt_EnsureSplittableC = CSplitMarker 
  | otherwise             = AbsCNop

-- Urgh!  I tried moving the forkStatics call from the rhss of cgTopRhs
-- to enclose the listFCs in cgTopBinding, but that tickled the
-- statics "error" call in initC.  I DON'T UNDERSTAND WHY!

cgTopRhs :: Id -> StgRhs -> SRT -> FCode (Id, CgIdInfo)
	-- the Id is passed along for setting up a binding...

cgTopRhs bndr (StgRhsCon cc con args) srt
  = maybeGlobaliseId bndr `thenFC` \ bndr' ->
    forkStatics (cgTopRhsCon bndr con args)

cgTopRhs bndr (StgRhsClosure cc bi fvs upd_flag args body) srt
  =     -- There should be no free variables
    ASSERT(null fvs)
    let 
	lf_info = mkClosureLFInfo bndr TopLevel [{-no fvs-}] upd_flag args
    in
    maybeGlobaliseId bndr			`thenFC` \ bndr' ->
    forkStatics (cgTopRhsClosure bndr' cc bi srt args body lf_info)
\end{code}
