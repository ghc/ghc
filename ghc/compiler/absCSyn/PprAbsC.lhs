%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
%************************************************************************
%*									*
\section[PprAbsC]{Pretty-printing Abstract~C}
%*									*
%************************************************************************

\begin{code}
#include "HsVersions.h"

module PprAbsC (
	writeRealC,
	dumpRealC,
#if defined(DEBUG)
	pprAmode, -- otherwise, not exported
#endif

	-- and for interface self-sufficiency...
	AbstractC, CAddrMode, MagicId,
	PprStyle, CSeq
    ) where

IMPORT_Trace		-- ToDo: rm (debugging only)

import AbsCSyn

import PrelInfo		( pprPrimOp, primOpNeedsWrapper, PrimOp(..)
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			)
import Literal		( literalPrimRep, showLiteral )
import CLabel	-- lots of things
import CgCompInfo	( spARelToInt, spBRelToInt, mIN_UPD_SIZE )
import CgRetConv 	( noLiveRegsMask )
import ClosureInfo	-- quite a few things
import Costs		-- for GrAnSim; cost counting function -- HWL
import CostCentre
import FiniteMap
import Maybes		( catMaybes, maybeToBool, Maybe(..) )
import Outputable
import Pretty	    	( codeStyle, prettyToUn )
import PrimRep		( showPrimRep, isFloatingRep, PrimRep(..) )
import StgSyn
import UniqFM
import Unpretty		-- ********** NOTE **********
import Util

infixr 9 `thenTE`
\end{code}

For spitting out the costs of an abstract~C expression, @writeRealC@
now not only prints the C~code of the @absC@ arg but also adds a macro
call to a cost evaluation function @GRAN_EXEC@. For that,
@pprAbsC@ has a new ``costs'' argument.  %% HWL

\begin{code}
writeRealC :: _FILE -> AbstractC -> PrimIO ()

writeRealC sw_chker file absC
  = uppAppendFile file 80 (
      uppAbove (pprAbsC (PprForC sw_chker) absC (costs absC)) (uppChar '\n')
    )

dumpRealC :: AbstractC -> String

dumpRealC sw_chker absC
  = uppShow 80 (
      uppAbove (pprAbsC (PprForC sw_chker) absC (costs absC)) (uppChar '\n')
    )
\end{code}

This emits the macro,  which is used in GrAnSim  to compute the total costs
from a cost 5 tuple. %%  HWL

\begin{code}
emitMacro :: CostRes -> Unpretty

#ifndef GRAN
emitMacro _ = uppNil
#else
emitMacro (Cost (i,b,l,s,f))
  = uppBesides [ uppStr "GRAN_EXEC(",
	uppInt i, uppComma, uppInt b, uppComma, uppInt l, uppComma,
	uppInt s, uppComma, uppInt f, pp_paren_semi ]
#endif {-GRAN-}
\end{code}

\begin{code}
pp_paren_semi = uppStr ");"

-- ---------------------------------------------------------------------------
-- New type: Now pprAbsC also takes the costs for evaluating the Abstract C
-- code as an argument (that's needed when spitting out the GRAN_EXEC macro
-- which must be done before the return i.e. inside absC code)   HWL
-- ---------------------------------------------------------------------------

pprAbsC :: PprStyle -> AbstractC -> CostRes -> Unpretty

pprAbsC sty AbsCNop _ = uppNil
pprAbsC sty (AbsCStmts s1 s2) c = uppAbove (pprAbsC sty s1 c) (pprAbsC sty s2 c)

pprAbsC sty (CClosureUpdInfo info) c
  = pprAbsC sty info c

pprAbsC sty (CAssign dest src) _ = pprAssign sty (getAmodeRep dest) dest src

pprAbsC sty (CJump target) c
  = uppAbove (uppBesides [emitMacro c {-WDP:, uppStr "/* <--++  CJump */"-} ])
	     (uppBesides [ uppStr "JMP_(", pprAmode sty target, pp_paren_semi ])

pprAbsC sty (CFallThrough target) c
  = uppAbove (uppBesides [emitMacro c {-WDP:, uppStr "/* <--++  CFallThrough */"-} ])
	     (uppBesides [ uppStr "JMP_(", pprAmode sty target, pp_paren_semi ])

-- --------------------------------------------------------------------------
-- Spit out GRAN_EXEC macro immediately before the return                 HWL

pprAbsC sty (CReturn am return_info)  c
  = uppAbove (uppBesides [emitMacro c {-WDP:, uppStr "/* <----  CReturn */"-} ])
	     (uppBesides [uppStr "JMP_(", target, pp_paren_semi ])
  where
   target = case return_info of
    	DirectReturn -> uppBesides [uppStr "DIRECT(", pprAmode sty am, uppRparen]
	DynamicVectoredReturn am' -> mk_vector (pprAmode sty am')
	StaticVectoredReturn n -> mk_vector (uppInt n)	-- Always positive
   mk_vector x = uppBesides [uppLparen, pprAmode sty am, uppStr ")[RVREL(", x, uppStr ")]"]

pprAbsC sty (CSplitMarker) _ = uppPStr SLIT("/* SPLIT */")

-- we optimise various degenerate cases of CSwitches.

-- --------------------------------------------------------------------------
-- Assume: CSwitch is also end of basic block
--         costs function yields nullCosts for whole switch
--         ==> inherited costs c are those of basic block up to switch
--         ==> inherit c + costs for the corresponding branch
--                                                                       HWL
-- --------------------------------------------------------------------------

pprAbsC sty (CSwitch discrim [] deflt) c
  = pprAbsC sty deflt (c + costs deflt)
    -- Empty alternative list => no costs for discrim as nothing cond. here HWL

pprAbsC sty (CSwitch discrim [(tag,alt_code)] deflt) c -- only one alt
  = case (nonemptyAbsC deflt) of
      Nothing ->		-- one alt and no default
		 pprAbsC sty alt_code (c + costs alt_code)
		 -- Nothing conditional in here either  HWL

      Just dc ->		-- make it an "if"
		 do_if_stmt sty discrim tag alt_code dc c

pprAbsC sty (CSwitch discrim [(tag1@(MachInt i1 _), alt_code1),
			      (tag2@(MachInt i2 _), alt_code2)] deflt) c
  | empty_deflt && ((i1 == 0 && i2 == 1) || (i1 == 1 && i2 == 0))
  = if (i1 == 0) then
	do_if_stmt sty discrim tag1 alt_code1 alt_code2 c
    else
	do_if_stmt sty discrim tag2 alt_code2 alt_code1 c
  where
    empty_deflt = not (maybeToBool (nonemptyAbsC deflt))

pprAbsC sty (CSwitch discrim alts deflt) c -- general case
  | isFloatingRep (getAmodeRep discrim)
    = pprAbsC sty (foldr ( \ a -> CSwitch discrim [a]) deflt alts) c
  | otherwise
    = uppAboves [
	uppBesides [uppStr "switch (", pp_discrim, uppStr ") {"],
	uppNest 2 (uppAboves (map (ppr_alt sty) alts)),
	(case (nonemptyAbsC deflt) of
	   Nothing -> uppNil
	   Just dc ->
	    uppNest 2 (uppAboves [uppPStr SLIT("default:"),
				  pprAbsC sty dc (c + switch_head_cost
						    + costs dc),
				  uppPStr SLIT("break;")])),
	uppChar '}' ]
  where
    pp_discrim
      = pprAmode sty discrim

    ppr_alt sty (lit, absC)
      = uppAboves [ uppBesides [uppPStr SLIT("case "), pprBasicLit sty lit, uppChar ':'],
		   uppNest 2 (uppAbove (pprAbsC sty absC (c + switch_head_cost + costs absC))
				       (uppPStr SLIT("break;"))) ]

    -- Costs for addressing header of switch and cond. branching        -- HWL
    switch_head_cost = addrModeCosts discrim Rhs + (Cost (0, 1, 0, 0, 0))

pprAbsC sty stmt@(COpStmt results op@(CCallOp _ _ _ _ _) args liveness_mask vol_regs) _
  = pprCCall sty op args results liveness_mask vol_regs

pprAbsC sty stmt@(COpStmt results op args liveness_mask vol_regs) _
  = let
	non_void_args = grab_non_void_amodes args
	non_void_results = grab_non_void_amodes results
	-- if just one result, we print in the obvious "assignment" style;
	-- if 0 or many results, we emit a macro call, w/ the results
	-- followed by the arguments.  The macro presumably knows which
	-- are which :-)

    	the_op = ppr_op_call non_void_results non_void_args
		-- liveness mask is *in* the non_void_args
    in
    BIND (ppr_vol_regs sty vol_regs) _TO_ (pp_saves, pp_restores) ->
    if primOpNeedsWrapper op then
    	uppAboves [  pp_saves,
    	    	    the_op,
    	    	    pp_restores
    	    	 ]
    else
    	the_op
    BEND
  where
    ppr_op_call results args
      = uppBesides [ prettyToUn (pprPrimOp sty op), uppLparen,
	uppIntersperse uppComma (map ppr_op_result results),
	if null results || null args then uppNil else uppComma,
	uppIntersperse uppComma (map (pprAmode sty) args),
	pp_paren_semi ]

    ppr_op_result r = ppr_amode sty r
      -- primop macros do their own casting of result;
      -- hence we can toss the provided cast...

pprAbsC sty (CSimultaneous abs_c) c
  = uppBesides [uppStr "{{", pprAbsC sty abs_c c, uppStr "}}"]

pprAbsC sty stmt@(CMacroStmt macro as) _
  = uppBesides [uppStr (show macro), uppLparen,
	uppIntersperse uppComma (map (ppr_amode sty) as),pp_paren_semi] -- no casting
pprAbsC sty stmt@(CCallProfCtrMacro op as) _
  = uppBesides [uppPStr op, uppLparen,
	uppIntersperse uppComma (map (ppr_amode sty) as),pp_paren_semi]
pprAbsC sty stmt@(CCallProfCCMacro op as) _
  = uppBesides [uppPStr op, uppLparen,
	uppIntersperse uppComma (map (ppr_amode sty) as),pp_paren_semi]

pprAbsC sty (CCodeBlock label abs_C) _
  = ASSERT( maybeToBool(nonemptyAbsC abs_C) )
    BIND (pprTempAndExternDecls abs_C) _TO_ (pp_temps, pp_exts) ->
    uppAboves [
	uppBesides [uppStr (if (externallyVisibleCLabel label)
			  then "FN_("	-- abbreviations to save on output
			  else "IFN_("),
		   pprCLabel sty label, uppStr ") {"],
	case sty of
	  PprForC _ -> uppAbove pp_exts pp_temps
	  _ -> uppNil,
	uppNest 8 (uppPStr SLIT("FB_")),
	uppNest 8 (pprAbsC sty abs_C (costs abs_C)),
	uppNest 8 (uppPStr SLIT("FE_")),
	uppChar '}' ]
    BEND

pprAbsC sty (CInitHdr cl_info reg_rel cost_centre inplace_upd) _
  = uppBesides [ pp_init_hdr, uppStr "_HDR(",
		ppr_amode sty (CAddr reg_rel), uppComma,
		pprCLabel sty info_lbl, uppComma,
		if_profiling sty (pprAmode sty cost_centre), uppComma,
		pprHeapOffset sty size, uppComma, uppInt ptr_wds, pp_paren_semi ]
  where
    info_lbl	= infoTableLabelFromCI cl_info
    sm_rep	= closureSMRep	   cl_info
    size	= closureSizeWithoutFixedHdr cl_info
    ptr_wds	= closurePtrsSize  cl_info

    pp_init_hdr = uppStr (if inplace_upd then
			    getSMUpdInplaceHdrStr sm_rep
		  	else
			    getSMInitHdrStr sm_rep)

pprAbsC sty stmt@(CStaticClosure closure_lbl cl_info cost_centre amodes) _
  = BIND (pprTempAndExternDecls stmt) _TO_ (_, pp_exts) ->
    uppAboves [
	case sty of
	  PprForC _ -> pp_exts
	  _ -> uppNil,
	uppBesides [
		uppStr "SET_STATIC_HDR(",
		pprCLabel sty closure_lbl,			uppComma,
		pprCLabel sty info_lbl,				uppComma,
		if_profiling sty (pprAmode sty cost_centre),	uppComma,
		ppLocalness closure_lbl,			uppComma,
		ppLocalnessMacro False{-for data-} info_lbl,
		uppChar ')'
		],
	uppNest 2 (uppBesides (map (ppr_item sty) amodes)),
	uppNest 2 (uppBesides (map (ppr_item sty) padding_wds)),
	uppStr "};" ]
    BEND
  where
    info_lbl = infoTableLabelFromCI cl_info

    ppr_item sty item
      = if getAmodeRep item == VoidRep
	then uppStr ", (W_) 0" -- might not even need this...
	else uppBeside (uppStr ", (W_)") (ppr_amode sty item)

    padding_wds =
	if not (closureUpdReqd cl_info) then
	    []
    	else
	    BIND (max 0 (mIN_UPD_SIZE - length amodes)) _TO_ still_needed ->
	    nOfThem still_needed (mkIntCLit 0) -- a bunch of 0s
	    BEND

{-
   STATIC_INIT_HDR(c,i,localness) blows into:
	localness W_ c_closure [] = { i_info, extra_fixed_wd<1..n>

   then *NO VarHdr STUFF FOR STATIC*...

   then the amodes are dropped in...
	,a1 ,a2 ... ,aN
   then a close brace:
	};
-}

pprAbsC sty stmt@(CClosureInfoAndCode cl_info slow maybe_fast upd cl_descr liveness) _
  = uppAboves [
	uppBesides [
	    pp_info_rep,
	    uppStr "_ITBL(",
	    pprCLabel sty info_lbl,			uppComma,

		-- CONST_ITBL needs an extra label for
		-- the static version of the object.
	    if isConstantRep sm_rep
	    then uppBeside (pprCLabel sty (closureLabelFromCI cl_info)) uppComma
	    else uppNil,

	    pprCLabel sty slow_lbl,	uppComma,
    	    pprAmode sty upd,		uppComma,
	    uppInt liveness,		uppComma,

	    pp_tag,			uppComma,
	    pp_size, 			uppComma,
	    pp_ptr_wds,			uppComma,

	    ppLocalness info_lbl,				uppComma,
	    ppLocalnessMacro True{-function-} slow_lbl,		uppComma,

	    if is_selector
	    then uppBeside (uppInt select_word_i) uppComma
	    else uppNil,

	    if_profiling sty pp_kind, uppComma,
	    if_profiling sty pp_descr, uppComma,
	    if_profiling sty pp_type,
	    uppStr ");"
	],
	pp_slow,
	case maybe_fast of
	    Nothing -> uppNil
	    Just fast -> let stuff = CCodeBlock fast_lbl fast in
			 pprAbsC sty stuff (costs stuff)
    ]
  where
    info_lbl	= infoTableLabelFromCI cl_info
    fast_lbl    = fastLabelFromCI cl_info
    sm_rep	= closureSMRep	  cl_info

    (slow_lbl, pp_slow)
      = case (nonemptyAbsC slow) of
	  Nothing -> (mkErrorStdEntryLabel, uppNil)
	  Just xx -> (entryLabelFromCI cl_info,
		       let stuff = CCodeBlock slow_lbl xx in
		       pprAbsC sty stuff (costs stuff))

    maybe_selector = maybeSelectorInfo cl_info
    is_selector = maybeToBool maybe_selector
    (Just (_, select_word_i)) = maybe_selector

    pp_info_rep	    -- special stuff if it's a selector; otherwise, just the SMrep
      = uppStr (if is_selector then "SELECT" else (getSMInfoStr sm_rep))

    pp_tag = uppInt (closureSemiTag cl_info)

    is_phantom = isPhantomRep sm_rep

    pp_size = if isSpecRep sm_rep then	-- exploiting: SPEC_VHS == 0 (always)
		 uppInt (closureNonHdrSize cl_info)

	      else if is_phantom then	-- do not have sizes for these
		 uppNil
	      else
		 pprHeapOffset sty (closureSizeWithoutFixedHdr cl_info)

    pp_ptr_wds	= if is_phantom then
		     uppNil
		  else
		     uppInt (closurePtrsSize cl_info)

    pp_kind  = uppStr (closureKind cl_info)
    pp_descr = uppBesides [uppChar '"', uppStr (stringToC cl_descr), uppChar '"']
    pp_type  = uppBesides [uppChar '"', uppStr (stringToC (closureTypeDescr cl_info)), uppChar '"']

pprAbsC sty (CRetVector lbl maybes deflt) c
  = uppAboves [ uppStr "{ // CRetVector (lbl????)",
	       uppNest 8 (uppSep (map (ppr_maybe_amode sty) maybes)),
	       uppStr "} /*default=*/ {", pprAbsC sty deflt c,
	       uppStr "}"]
  where
    ppr_maybe_amode sty Nothing  = uppPStr SLIT("/*default*/")
    ppr_maybe_amode sty (Just a) = pprAmode sty a

pprAbsC sty stmt@(CRetUnVector label amode) _
  = uppBesides [uppStr "UNVECTBL(", pp_static, uppComma, pprCLabel sty label, uppComma,
	    pprAmode sty amode, uppRparen]
  where
    pp_static = if externallyVisibleCLabel label then uppNil else uppPStr SLIT("static")

pprAbsC sty stmt@(CFlatRetVector label amodes) _
  =	BIND (pprTempAndExternDecls stmt) _TO_ (_, pp_exts) ->
	uppAboves [
	    case sty of
	      PprForC _ -> pp_exts
	      _ -> uppNil,
	    uppBesides [ppLocalness label, uppPStr SLIT(" W_ "),
    	    	       pprCLabel sty label, uppStr "[] = {"],
	    uppNest 2 (uppInterleave uppComma (map (ppr_item sty) amodes)),
	    uppStr "};" ]
	BEND
  where
    ppr_item sty item = uppBeside (uppStr "(W_) ") (ppr_amode sty item)

pprAbsC sty (CCostCentreDecl is_local cc) _ = uppCostCentreDecl sty is_local cc
\end{code}

\begin{code}
ppLocalness label
  = uppBeside static const
  where
    static = if (externallyVisibleCLabel label) then uppNil else uppPStr SLIT("static ")
    const  = if not (isReadOnly label)	        then uppNil else uppPStr SLIT("const")

ppLocalnessMacro for_fun{-vs data-} clabel
  = BIND (if externallyVisibleCLabel clabel then "E" else "I") _TO_ prefix ->
    BIND (if isReadOnly clabel then "RO_" else "")	      _TO_ suffix ->
    if for_fun
       then uppStr (prefix ++ "F_")
       else uppStr (prefix ++ "D_" ++ suffix)
    BEND BEND
\end{code}

\begin{code}
grab_non_void_amodes amodes
  = filter non_void amodes

non_void amode
  = case (getAmodeRep amode) of
      VoidRep -> False
      k	-> True
\end{code}

\begin{code}
ppr_vol_regs :: PprStyle -> [MagicId] -> (Unpretty, Unpretty)

ppr_vol_regs sty [] = (uppNil, uppNil)
ppr_vol_regs sty (VoidReg:rs) = ppr_vol_regs sty rs
ppr_vol_regs sty (r:rs)
  = let pp_reg = case r of
    	    	    VanillaReg pk n -> pprVanillaReg n
    	    	    _ -> pprMagicId sty r
	(more_saves, more_restores) = ppr_vol_regs sty rs
    in
    (uppAbove (uppBeside (uppPStr SLIT("CALLER_SAVE_"))    pp_reg) more_saves,
     uppAbove (uppBeside (uppPStr SLIT("CALLER_RESTORE_")) pp_reg) more_restores)

-- pp_basic_{saves,restores}: The BaseReg, SpA, SuA, SpB, SuB, Hp and
-- HpLim (see StgRegs.lh) may need to be saved/restored around CCalls,
-- depending on the platform.  (The "volatile regs" stuff handles all
-- other registers.)  Just be *sure* BaseReg is OK before trying to do
-- anything else.
pp_basic_saves
  = uppAboves [
	uppPStr SLIT("CALLER_SAVE_Base"),
	uppPStr SLIT("CALLER_SAVE_SpA"),
	uppPStr SLIT("CALLER_SAVE_SuA"),
	uppPStr SLIT("CALLER_SAVE_SpB"),
	uppPStr SLIT("CALLER_SAVE_SuB"),
	uppPStr SLIT("CALLER_SAVE_Ret"),
--	uppPStr SLIT("CALLER_SAVE_Activity"),
	uppPStr SLIT("CALLER_SAVE_Hp"),
	uppPStr SLIT("CALLER_SAVE_HpLim") ]

pp_basic_restores
  = uppAboves [
	uppPStr SLIT("CALLER_RESTORE_Base"), -- must be first!
	uppPStr SLIT("CALLER_RESTORE_SpA"),
	uppPStr SLIT("CALLER_RESTORE_SuA"),
	uppPStr SLIT("CALLER_RESTORE_SpB"),
	uppPStr SLIT("CALLER_RESTORE_SuB"),
	uppPStr SLIT("CALLER_RESTORE_Ret"),
--	uppPStr SLIT("CALLER_RESTORE_Activity"),
	uppPStr SLIT("CALLER_RESTORE_Hp"),
	uppPStr SLIT("CALLER_RESTORE_HpLim"),
	uppPStr SLIT("CALLER_RESTORE_StdUpdRetVec"),
	uppPStr SLIT("CALLER_RESTORE_StkStub") ]
\end{code}

\begin{code}
if_profiling sty pretty
  = case sty of
      PprForC sw_chker -> if  sw_chker SccProfilingOn
			  then pretty
			  else uppChar '0' -- leave it out!

      _ -> {-print it anyway-} pretty

-- ---------------------------------------------------------------------------
-- Changes for GrAnSim:
--  draw costs for computation in head of if into both branches;
--  as no abstractC data structure is given for the head, one is constructed
--  guessing unknown values and fed into the costs function
-- ---------------------------------------------------------------------------

do_if_stmt sty discrim tag alt_code deflt c
  = case tag of
      -- This special case happens when testing the result of a comparison.
      -- We can just avoid some redundant clutter in the output.
      MachInt n _ | n==0 -> ppr_if_stmt sty (pprAmode sty discrim)
				      deflt alt_code
				      (addrModeCosts discrim Rhs) c
      other              -> let
			       cond = uppBesides [ pprAmode sty discrim,
					  uppPStr SLIT(" == "),
					  pprAmode sty (CLit tag) ]
			    in
			    ppr_if_stmt sty cond
					 alt_code deflt
					 (addrModeCosts discrim Rhs) c

ppr_if_stmt sty pp_pred then_part else_part discrim_costs c
  = uppAboves [
      uppBesides [uppStr "if (", pp_pred, uppStr ") {"],
      uppNest 8 (pprAbsC sty then_part 	(c + discrim_costs +
				       	(Cost (0, 2, 0, 0, 0)) +
					costs then_part)),
      (case nonemptyAbsC else_part of Nothing -> uppNil; Just _ -> uppStr "} else {"),
      uppNest 8 (pprAbsC sty else_part  (c + discrim_costs +
					(Cost (0, 1, 0, 0, 0)) +
					costs else_part)),
      uppChar '}' ]
    {- Total costs = inherited costs (before if) + costs for accessing discrim
		     + costs for cond branch ( = (0, 1, 0, 0, 0) )
		     + costs for that alternative
    -}
\end{code}

Historical note: this used to be two separate cases -- one for `ccall'
and one for `casm'.  To get round a potential limitation to only 10
arguments, the numbering of arguments in @process_casm@ was beefed up a
bit. ADR

Some rough notes on generating code for @CCallOp@:

1) Evaluate all arguments and stuff them into registers. (done elsewhere)
2) Save any essential registers (heap, stack, etc).

   ToDo: If stable pointers are in use, these must be saved in a place
   where the runtime system can get at them so that the Stg world can
   be restarted during the call.

3) Save any temporary registers that are currently in use.
4) Do the call putting result into a local variable
5) Restore essential registers
6) Restore temporaries

   (This happens after restoration of essential registers because we
   might need the @Base@ register to access all the others correctly.)

7) If returning Malloc Pointer, build a closure containing the
   appropriate value.

   Otherwise, copy local variable into result register.

8) If ccall (not casm), declare the function being called as extern so
   that C knows if it returns anything other than an int.

\begin{pseudocode}
{ ResultType _ccall_result;
  basic_saves;
  saves;
  _ccall_result = f( args );
  basic_restores;
  restores;

  #if MallocPtr
	constructMallocPtr(liveness, return_reg, _ccall_result);
  #else
	return_reg = _ccall_result;
  #end
}
\end{pseudocode}

Amendment to the above: if we can GC, we have to:

* make sure we save all our registers away where the garbage collector
  can get at them.
* be sure that there are no live registers or we're in trouble.
  (This can cause problems if you try something foolish like passing
   an array or mallocptr to a _ccall_GC_ thing.)
* increment/decrement the @inCCallGC@ counter before/after the call so
  that the runtime check that PerformGC is being used sensibly will work.

\begin{code}
pprCCall sty op@(CCallOp op_str is_asm may_gc _ _) args results liveness_mask vol_regs
  = if (may_gc && liveness_mask /= noLiveRegsMask)
    then panic ("Live register in _casm_GC_ \"" ++ casm_str ++ "\" " ++ (uppShow 80 (uppCat pp_non_void_args)) ++ "\n")
    else
--    trace ("casm \"" ++ casm_str ++ "\" " ++ (uppShow 80 (uppCat localVars)) ++ (uppShow 80 (uppCat pp_non_void_args)))
    uppAboves [
      uppChar '{',
      declare_local_vars,   -- local var for *result*
      uppAboves local_arg_decls,
      -- if is_asm then uppNil else declareExtern,
      pp_save_context,
      process_casm local_vars pp_non_void_args casm_str,
      pp_restore_context,
      assign_results,
      uppChar '}'
    ]
  where
    (pp_saves, pp_restores) = ppr_vol_regs sty vol_regs
    (pp_save_context, pp_restore_context) =
	if may_gc
	then (	uppStr "extern StgInt inCCallGC; SaveAllStgRegs(); inCCallGC++;",
		uppStr "inCCallGC--; RestoreAllStgRegs();")
	else (	pp_basic_saves `uppAbove` pp_saves,
		pp_basic_restores `uppAbove` pp_restores)

    non_void_args =
	let nvas = tail args
	in ASSERT (all non_void nvas) nvas
    -- the first argument will be the "I/O world" token (a VoidRep)
    -- all others should be non-void

    non_void_results =
	let nvrs = grab_non_void_amodes results
	in ASSERT (length nvrs <= 1) nvrs
    -- there will usually be two results: a (void) state which we
    -- should ignore and a (possibly void) result.

    (local_arg_decls, pp_non_void_args)
      = unzip [ ppr_casm_arg sty a i | (a,i) <- non_void_args `zip` [1..] ]

    pp_liveness = pprAmode sty (mkIntCLit liveness_mask)

    (declare_local_vars, local_vars, assign_results)
      = ppr_casm_results sty non_void_results pp_liveness

    casm_str = if is_asm then _UNPK_ op_str else ccall_str

    -- Remainder only used for ccall

    ccall_str = uppShow 80
	(uppBesides [
		if null non_void_results
		  then uppNil
		  else uppPStr SLIT("%r = "),
		uppLparen, uppPStr op_str, uppLparen,
		  uppIntersperse uppComma ccall_args,
		uppStr "));"
	])
    num_args = length non_void_args
    ccall_args = take num_args [ uppBeside (uppChar '%') (uppInt i) | i <- [0..] ]
\end{code}

If the argument is a heap object, we need to reach inside and pull out
the bit the C world wants to see.  The only heap objects which can be
passed are @Array@s, @ByteArray@s and @MallocPtr@s.

\begin{code}
ppr_casm_arg :: PprStyle -> CAddrMode -> Int -> (Unpretty, Unpretty)
    -- (a) decl and assignment, (b) local var to be used later

ppr_casm_arg sty amode a_num
  = let
	a_kind	 = getAmodeRep amode
	pp_amode = pprAmode sty amode
	pp_kind  = pprPrimKind sty a_kind

	local_var  = uppBeside (uppPStr SLIT("_ccall_arg")) (uppInt a_num)

	(arg_type, pp_amode2)
	  = case a_kind of

	      -- for array arguments, pass a pointer to the body of the array
	      -- (PTRS_ARR_CTS skips over all the header nonsense)
	      ArrayRep	    -> (pp_kind,
				uppBesides [uppStr "PTRS_ARR_CTS(", pp_amode, uppRparen])
	      ByteArrayRep -> (pp_kind,
				uppBesides [uppStr "BYTE_ARR_CTS(", pp_amode, uppRparen])

	      -- for Malloc Pointers, use MALLOC_PTR_DATA to fish out the contents.
	      MallocPtrRep -> (uppPStr SLIT("StgMallocPtr"),
				uppBesides [uppStr "MallocPtr_CLOSURE_DATA(", pp_amode, uppStr")"])
	      other	    -> (pp_kind, pp_amode)

	declare_local_var
	  = uppBesides [ arg_type, uppSP, local_var, uppEquals, pp_amode2, uppSemi ]
    in
    (declare_local_var, local_var)
\end{code}

For l-values, the critical questions are:

1) Are there any results at all?

   We only allow zero or one results.

2) Is the result is a mallocptr?

   The mallocptr must be encapsulated immediately in a heap object.

\begin{code}
ppr_casm_results ::
	PprStyle	-- style
	-> [CAddrMode]	-- list of results (length <= 1)
	-> Unpretty	-- liveness mask
	->
	( Unpretty,	-- declaration of any local vars
	  [Unpretty],	-- list of result vars (same length as results)
	  Unpretty )	-- assignment (if any) of results in local var to registers

ppr_casm_results sty [] liveness
  = (uppNil, [], uppNil) 	-- no results

ppr_casm_results sty [r] liveness
  = let
	result_reg = ppr_amode sty r
	r_kind	   = getAmodeRep r

	local_var  = uppPStr SLIT("_ccall_result")

	(result_type, assign_result)
	  = case r_kind of
	      MallocPtrRep ->
		(uppPStr SLIT("StgMallocPtr"),
		 uppBesides [ uppStr "constructMallocPtr(",
				liveness, uppComma,
				result_reg, uppComma,
				local_var,
			     pp_paren_semi ])
	      _ ->
		(pprPrimKind sty r_kind,
		 uppBesides [ result_reg, uppEquals, local_var, uppSemi ])

	declare_local_var = uppBesides [ result_type, uppSP, local_var, uppSemi ]
    in
    (declare_local_var, [local_var], assign_result)

ppr_casm_results sty rs liveness
  = panic "ppr_casm_results: ccall/casm with many results"
\end{code}


Note the sneaky way _the_ result is represented by a list so that we
can complain if it's used twice.

ToDo: Any chance of giving line numbers when process-casm fails?
      Or maybe we should do a check _much earlier_ in compiler. ADR

\begin{code}
process_casm ::
	[Unpretty]		-- results (length <= 1)
	-> [Unpretty]		-- arguments
	-> String		-- format string (with embedded %'s)
	->
	Unpretty			-- code being generated

process_casm results args string = process results args string
 where
  process []    _ "" = uppNil
  process (_:_) _ "" = error ("process_casm: non-void result not assigned while processing _casm_ \"" ++ string ++ "\"\n(Try changing result type to PrimIO ()\n")

  process ress args ('%':cs)
    = case cs of
	[] ->
	    error ("process_casm: lonely % while processing _casm_ \"" ++ string ++ "\".\n")

	('%':css) ->
	    uppBeside (uppChar '%') (process ress args css)

	('r':css)  ->
	  case ress of
	    []  -> error ("process_casm: no result to match %r while processing _casm_ \"" ++ string ++ "\".\nTry deleting %r or changing result type from PrimIO ()\n")
	    [r] -> uppBeside r (process [] args css)
	    _   -> panic ("process_casm: casm with many results while processing _casm_ \"" ++ string ++ "\".\n")

	other ->
	  case readDec other of
	    [(num,css)] ->
		  if 0 <= num && num < length args
		  then uppBesides [uppLparen, args !! num, uppRparen,
				    process ress args css]
		    else error ("process_casm: no such arg #:"++(show num)++" while processing \"" ++ string ++ "\".\n")
	    _ -> error ("process_casm: not %<num> while processing _casm_ \"" ++ string ++ "\".\n")

  process ress args (other_c:cs)
    = uppBeside (uppChar other_c) (process ress args cs)
\end{code}

%************************************************************************
%*									*
\subsection[a2r-assignments]{Assignments}
%*									*
%************************************************************************

Printing assignments is a little tricky because of type coercion.

First of all, the kind of the thing being assigned can be gotten from
the destination addressing mode.  (It should be the same as the kind
of the source addressing mode.)  If the kind of the assignment is of
@VoidRep@, then don't generate any code at all.

\begin{code}
pprAssign :: PprStyle -> PrimRep -> CAddrMode -> CAddrMode -> Unpretty

pprAssign sty VoidRep dest src = uppNil

#if 0
pprAssign sty kind dest src
 | (kind /= getAmodeRep dest) || (kind /= getAmodeRep src)
 = uppCat [uppStr "Bad kind:", pprPrimKind sty kind,
	pprPrimKind sty (getAmodeRep dest), pprAmode sty dest,
	pprPrimKind sty (getAmodeRep src),  pprAmode sty src]
#endif
\end{code}

Special treatment for floats and doubles, to avoid unwanted conversions.

\begin{code}
pprAssign sty FloatRep dest@(CVal reg_rel _) src
  = uppBesides [ uppStr "ASSIGN_FLT(", ppr_amode sty (CAddr reg_rel), uppComma, pprAmode sty src, pp_paren_semi ]

pprAssign sty DoubleRep dest@(CVal reg_rel _) src
  = uppBesides [ uppStr "ASSIGN_DBL(", ppr_amode sty (CAddr reg_rel), uppComma, pprAmode sty src, pp_paren_semi ]
\end{code}

Lastly, the question is: will the C compiler think the types of the
two sides of the assignment match?

	We assume that the types will match
	if neither side is a @CVal@ addressing mode for any register
	which can point into the heap or B stack.

Why?  Because the heap and B stack are used to store miscellaneous things,
whereas the A stack, temporaries, registers, etc., are only used for things
of fixed type.

\begin{code}
pprAssign sty kind (CReg (VanillaReg _ dest)) (CReg (VanillaReg _ src))
  = uppBesides [ pprVanillaReg dest, uppEquals,
		pprVanillaReg src, uppSemi ]

pprAssign sty kind dest src
  | mixedTypeLocn dest
    -- Add in a cast to StgWord (a.k.a. W_) iff the destination is mixed
  = uppBesides [ ppr_amode sty dest, uppEquals,
		uppStr "(W_)(",	-- Here is the cast
		ppr_amode sty src, pp_paren_semi ]

pprAssign sty kind dest src
  | mixedPtrLocn dest && getAmodeRep src /= PtrRep
    -- Add in a cast to StgPtr (a.k.a. P_) iff the destination is mixed
  = uppBesides [ ppr_amode sty dest, uppEquals,
		uppStr "(P_)(",	-- Here is the cast
		ppr_amode sty src, pp_paren_semi ]

pprAssign sty ByteArrayRep dest src
  | mixedPtrLocn src
    -- Add in a cast to StgPtr (a.k.a. B_) iff the source is mixed
  = uppBesides [ ppr_amode sty dest, uppEquals,
		uppStr "(B_)(",	-- Here is the cast
		ppr_amode sty src, pp_paren_semi ]

pprAssign sty kind other_dest src
  = uppBesides [ ppr_amode sty other_dest, uppEquals,
		pprAmode  sty src, uppSemi ]
\end{code}


%************************************************************************
%*									*
\subsection[a2r-CAddrModes]{Addressing modes}
%*									*
%************************************************************************

@pprAmode@ is used to print r-values (which may need casts), whereas
@ppr_amode@ is used for l-values {\em and} as a help function for
@pprAmode@.

\begin{code}
pprAmode, ppr_amode :: PprStyle -> CAddrMode -> Unpretty
\end{code}

For reasons discussed above under assignments, @CVal@ modes need
to be treated carefully.  First come special cases for floats and doubles,
similar to those in @pprAssign@:

(NB: @PK_FLT@ and @PK_DBL@ require the {\em address} of the value in
question.)

\begin{code}
pprAmode sty (CVal reg_rel FloatRep)
  = uppBesides [ uppStr "PK_FLT(", ppr_amode sty (CAddr reg_rel), uppRparen ]
pprAmode sty (CVal reg_rel DoubleRep)
  = uppBesides [ uppStr "PK_DBL(", ppr_amode sty (CAddr reg_rel), uppRparen ]
\end{code}

Next comes the case where there is some other cast need, and the
no-cast case:

\begin{code}
pprAmode sty amode
  | mixedTypeLocn amode
  = uppBesides [ uppLparen, pprPrimKind sty (getAmodeRep amode), uppStr ")(",
		ppr_amode sty amode, uppRparen]
  | otherwise	-- No cast needed
  = ppr_amode sty amode
\end{code}

Now the rest of the cases for ``workhorse'' @ppr_amode@:

\begin{code}
ppr_amode sty (CVal reg_rel _)
  = case (pprRegRelative sty False{-no sign wanted-} reg_rel) of
	(pp_reg, Nothing)     -> uppBeside  (uppChar '*') pp_reg
	(pp_reg, Just offset) -> uppBesides [ pp_reg, uppLbrack, offset, uppRbrack ]

ppr_amode sty (CAddr reg_rel)
  = case (pprRegRelative sty True{-sign wanted-} reg_rel) of
	(pp_reg, Nothing)     -> pp_reg
	(pp_reg, Just offset) -> uppBeside pp_reg offset

ppr_amode sty (CReg magic_id) = pprMagicId sty magic_id

ppr_amode sty (CTemp uniq kind) = prettyToUn (pprUnique uniq)

ppr_amode sty (CLbl label kind) = pprCLabel sty label

ppr_amode sty (CUnVecLbl direct vectored)
  = uppBesides [uppStr "(StgRetAddr) UNVEC(", pprCLabel sty direct, uppComma,
	       pprCLabel sty vectored, uppRparen]

ppr_amode sty (CCharLike char)
  = uppBesides [uppStr "CHARLIKE_CLOSURE(", pprAmode sty char, uppRparen ]
ppr_amode sty (CIntLike int)
  = uppBesides [uppStr "INTLIKE_CLOSURE(", pprAmode sty int, uppRparen ]

ppr_amode sty (CString str) = uppBesides [uppChar '"', uppStr (stringToC (_UNPK_ str)), uppChar '"']
  -- ToDo: are these *used* for anything?

ppr_amode sty (CLit lit) = pprBasicLit sty lit

ppr_amode sty (CLitLit str _) = uppPStr str

ppr_amode sty (COffset off) = pprHeapOffset sty off

ppr_amode sty (CCode abs_C)
  = uppAboves [ uppStr "{ -- CCode", uppNest 8 (pprAbsC sty abs_C (costs abs_C)), uppChar '}' ]

ppr_amode sty (CLabelledCode label abs_C)
  = uppAboves [ uppBesides [pprCLabel sty label, uppStr " = { -- CLabelledCode"],
	       uppNest 8 (pprAbsC sty abs_C (costs abs_C)), uppChar '}' ]

ppr_amode sty (CJoinPoint _ _)
  = panic "ppr_amode: CJoinPoint"

ppr_amode sty (CTableEntry base index kind)
  = uppBesides [uppStr "((", pprPrimKind sty kind, uppStr " *)(",
	       ppr_amode sty base, uppStr "))[(I_)(", ppr_amode sty index,
    	       uppStr ")]"]

ppr_amode sty (CMacroExpr pk macro as)
  = uppBesides [uppLparen, pprPrimKind sty pk, uppStr ")(", uppStr (show macro), uppLparen,
	       uppIntersperse uppComma (map (pprAmode sty) as), uppStr "))"]

ppr_amode sty (CCostCentre cc print_as_string)
  = uppCostCentre sty print_as_string cc
\end{code}

%************************************************************************
%*									*
\subsection[a2r-MagicIds]{Magic ids}
%*									*
%************************************************************************

@pprRegRelative@ returns a pair of the @Unpretty@ for the register
(some casting may be required), and a @Maybe Unpretty@ for the offset
(zero offset gives a @Nothing@).

\begin{code}
addPlusSign :: Bool -> Unpretty -> Unpretty
addPlusSign False p = p
addPlusSign True  p = uppBeside (uppChar '+') p

pprSignedInt :: Bool -> Int -> Maybe Unpretty	-- Nothing => 0
pprSignedInt sign_wanted n
 = if n == 0 then Nothing else
   if n > 0  then Just (addPlusSign sign_wanted (uppInt n))
   else 	  Just (uppInt n)

pprRegRelative :: PprStyle
	       -> Bool		-- True <=> Print leading plus sign (if +ve)
	       -> RegRelative
	       -> (Unpretty, Maybe Unpretty)

pprRegRelative sty sign_wanted (SpARel spA off)
  = (pprMagicId sty SpA, pprSignedInt sign_wanted (spARelToInt spA off))

pprRegRelative sty sign_wanted (SpBRel spB off)
  = (pprMagicId sty SpB, pprSignedInt sign_wanted (spBRelToInt spB off))

pprRegRelative sty sign_wanted r@(HpRel hp off)
  = let to_print = hp `subOff` off
	pp_Hp	 = pprMagicId sty Hp
    in
    if isZeroOff to_print then
	(pp_Hp, Nothing)
    else
	(pp_Hp, Just (uppBeside (uppChar '-') (pprHeapOffset sty to_print)))
				-- No parens needed because pprHeapOffset
				-- does them when necessary

pprRegRelative sty sign_wanted (NodeRel off)
  = let pp_Node = pprMagicId sty node
    in
    if isZeroOff off then
	(pp_Node, Nothing)
    else
	(pp_Node, Just (addPlusSign sign_wanted (pprHeapOffset sty off)))

\end{code}

@pprMagicId@ just prints the register name.  @VanillaReg@ registers are
represented by a discriminated union (@StgUnion@), so we use the @PrimRep@
to select the union tag.

\begin{code}
pprMagicId :: PprStyle -> MagicId -> Unpretty

pprMagicId sty BaseReg	    	    = uppPStr SLIT("BaseReg")
pprMagicId sty StkOReg		    = uppPStr SLIT("StkOReg")
pprMagicId sty (VanillaReg pk n)
				    = uppBesides [ pprVanillaReg n, uppChar '.',
						  pprUnionTag pk ]
pprMagicId sty (FloatReg  n)        = uppBeside (uppPStr SLIT("FltReg")) (uppInt IBOX(n))
pprMagicId sty (DoubleReg n)	    = uppBeside (uppPStr SLIT("DblReg")) (uppInt IBOX(n))
pprMagicId sty TagReg		    = uppPStr SLIT("TagReg")
pprMagicId sty RetReg		    = uppPStr SLIT("RetReg")
pprMagicId sty SpA		    = uppPStr SLIT("SpA")
pprMagicId sty SuA		    = uppPStr SLIT("SuA")
pprMagicId sty SpB		    = uppPStr SLIT("SpB")
pprMagicId sty SuB		    = uppPStr SLIT("SuB")
pprMagicId sty Hp		    = uppPStr SLIT("Hp")
pprMagicId sty HpLim		    = uppPStr SLIT("HpLim")
pprMagicId sty LivenessReg	    = uppPStr SLIT("LivenessReg")
pprMagicId sty StdUpdRetVecReg      = uppPStr SLIT("StdUpdRetVecReg")
pprMagicId sty StkStubReg	    = uppPStr SLIT("StkStubReg")
pprMagicId sty CurCostCentre	    = uppPStr SLIT("CCC")
pprMagicId sty VoidReg		    = panic "pprMagicId:VoidReg!"

pprVanillaReg :: FAST_INT -> Unpretty

pprVanillaReg n = uppBeside (uppChar 'R') (uppInt IBOX(n))

pprUnionTag :: PrimRep -> Unpretty

pprUnionTag PtrRep		= uppChar 'p'
pprUnionTag CodePtrRep	    	= uppPStr SLIT("fp")
pprUnionTag DataPtrRep	    	= uppChar 'd'
pprUnionTag RetRep 	    	= uppChar 'r'
pprUnionTag CostCentreRep	= panic "pprUnionTag:CostCentre?"

pprUnionTag CharRep		= uppChar 'c'
pprUnionTag IntRep		= uppChar 'i'
pprUnionTag WordRep		= uppChar 'w'
pprUnionTag AddrRep		= uppChar 'v'
pprUnionTag FloatRep		= uppChar 'f'
pprUnionTag DoubleRep		= panic "pprUnionTag:Double?"

pprUnionTag StablePtrRep	= uppChar 'i'
pprUnionTag MallocPtrRep	= uppChar 'p'

pprUnionTag ArrayRep		= uppChar 'p'
pprUnionTag ByteArrayRep	= uppChar 'b'

pprUnionTag _                   = panic "pprUnionTag:Odd kind"
\end{code}


Find and print local and external declarations for a list of
Abstract~C statements.
\begin{code}
pprTempAndExternDecls :: AbstractC -> (Unpretty{-temps-}, Unpretty{-externs-})
pprTempAndExternDecls AbsCNop = (uppNil, uppNil)

pprTempAndExternDecls (AbsCStmts stmt1 stmt2)
  = initTE (ppr_decls_AbsC stmt1	`thenTE` \ (t_p1, e_p1) ->
	    ppr_decls_AbsC stmt2	`thenTE` \ (t_p2, e_p2) ->
	    BIND (catMaybes [t_p1, t_p2])	 _TO_ real_temps ->
	    BIND (catMaybes [e_p1, e_p2])	 _TO_ real_exts ->
	    returnTE (uppAboves real_temps, uppAboves real_exts)
	    BEND BEND
	   )

pprTempAndExternDecls other_stmt
  = initTE (ppr_decls_AbsC other_stmt `thenTE` \ (maybe_t, maybe_e) ->
	    returnTE (
		case maybe_t of
		  Nothing -> uppNil
		  Just pp -> pp,

		case maybe_e of
		  Nothing -> uppNil
		  Just pp -> pp )
	   )

pprBasicLit :: PprStyle -> Literal -> Unpretty
pprPrimKind :: PprStyle -> PrimRep -> Unpretty

pprBasicLit  sty lit = uppStr (showLiteral  sty lit)
pprPrimKind  sty k   = uppStr (showPrimRep k)
\end{code}


%************************************************************************
%*									*
\subsection[a2r-monad]{Monadery}
%*									*
%************************************************************************

We need some monadery to keep track of temps and externs we have already
printed.  This info must be threaded right through the Abstract~C, so
it's most convenient to hide it in this monad.

WDP 95/02: Switched from \tr{([Unique], [CLabel])} to
\tr{(UniqSet, CLabelSet)}.  Allegedly for efficiency.

\begin{code}
type CLabelSet = FiniteMap CLabel (){-any type will do-}
emptyCLabelSet = emptyFM
x `elementOfCLabelSet` labs
  = case (lookupFM labs x) of { Just _ -> True; Nothing -> False }
addToCLabelSet set x = addToFM set x ()

type UniqueSet = UniqFM ()
emptyUniqueSet = emptyUFM
x `elementOfUniqueSet` us
  = case (lookupDirectlyUFM us x) of { Just _ -> True; Nothing -> False }
addToUniqueSet set x = set `plusUFM` singletonDirectlyUFM x ()

type TEenv = (UniqueSet, CLabelSet)

type TeM result =  TEenv -> (TEenv, result)

initTE :: TeM a -> a
initTE sa
  = case sa (emptyUniqueSet, emptyCLabelSet) of { (_, result) ->
    result }

{-# INLINE thenTE #-}
{-# INLINE returnTE #-}

thenTE :: TeM a -> (a -> TeM b) -> TeM b
thenTE a b u
  = case a u	    of { (u_1, result_of_a) ->
    b result_of_a u_1 }

mapTE :: (a -> TeM b) -> [a] -> TeM [b]
mapTE f []     = returnTE []
mapTE f (x:xs)
  = f x		`thenTE` \ r  ->
    mapTE f xs	`thenTE` \ rs ->
    returnTE (r : rs)

returnTE :: a -> TeM a
returnTE result env = (env, result)

-- these next two check whether the thing is already
-- recorded, and THEN THEY RECORD IT
-- (subsequent calls will return False for the same uniq/label)

tempSeenTE :: Unique -> TeM Bool
tempSeenTE uniq env@(seen_uniqs, seen_labels)
  = if (uniq `elementOfUniqueSet` seen_uniqs)
    then (env, True)
    else ((addToUniqueSet seen_uniqs uniq,
	  seen_labels),
	  False)

labelSeenTE :: CLabel -> TeM Bool
labelSeenTE label env@(seen_uniqs, seen_labels)
  = if (label `elementOfCLabelSet` seen_labels)
    then (env, True)
    else ((seen_uniqs,
	  addToCLabelSet seen_labels label),
	  False)
\end{code}

\begin{code}
pprTempDecl :: Unique -> PrimRep -> Unpretty
pprTempDecl uniq kind
  = uppBesides [ pprPrimKind PprDebug kind, uppSP, prettyToUn (pprUnique uniq), uppSemi ]

ppr_for_C = PprForC ( \ x -> False ) -- pretend no special cmd-line flags

pprExternDecl :: CLabel -> PrimRep -> Unpretty

pprExternDecl clabel kind
  = if not (needsCDecl clabel) then
	uppNil -- do not print anything for "known external" things (e.g., < PreludeCore)
    else
	BIND (
	    case kind of
	      CodePtrRep -> ppLocalnessMacro True{-function-} clabel
	      _		  -> ppLocalnessMacro False{-data-}    clabel
	) _TO_ pp_macro_str ->

	uppBesides [ pp_macro_str, uppLparen, pprCLabel ppr_for_C clabel, pp_paren_semi ]
	BEND
\end{code}

\begin{code}
ppr_decls_AbsC :: AbstractC -> TeM (Maybe Unpretty{-temps-}, Maybe Unpretty{-externs-})

ppr_decls_AbsC AbsCNop		= returnTE (Nothing, Nothing)

ppr_decls_AbsC (AbsCStmts stmts_1 stmts_2)
  = ppr_decls_AbsC stmts_1  `thenTE` \ p1 ->
    ppr_decls_AbsC stmts_2  `thenTE` \ p2 ->
    returnTE (maybe_uppAboves [p1, p2])

ppr_decls_AbsC (CClosureUpdInfo info)
  = ppr_decls_AbsC info

ppr_decls_AbsC (CSplitMarker) = returnTE (Nothing, Nothing)

ppr_decls_AbsC (CAssign dest source)
  = ppr_decls_Amode dest    `thenTE` \ p1 ->
    ppr_decls_Amode source  `thenTE` \ p2 ->
    returnTE (maybe_uppAboves [p1, p2])

ppr_decls_AbsC (CJump target) = ppr_decls_Amode target

ppr_decls_AbsC (CFallThrough target) = ppr_decls_Amode target

ppr_decls_AbsC (CReturn target _) = ppr_decls_Amode target

ppr_decls_AbsC (CSwitch discrim alts deflt)
  = ppr_decls_Amode discrim	`thenTE` \ pdisc ->
    mapTE ppr_alt_stuff alts	`thenTE` \ palts  ->
    ppr_decls_AbsC deflt	`thenTE` \ pdeflt ->
    returnTE (maybe_uppAboves (pdisc:pdeflt:palts))
  where
    ppr_alt_stuff (_, absC) = ppr_decls_AbsC absC

ppr_decls_AbsC (CCodeBlock label absC)
  = ppr_decls_AbsC absC

ppr_decls_AbsC (CInitHdr cl_info reg_rel cost_centre inplace_upd)
	-- ToDo: strictly speaking, should chk "cost_centre" amode
  = labelSeenTE info_lbl     `thenTE` \  label_seen ->
    returnTE (Nothing,
	      if label_seen then
		  Nothing
	      else
		  Just (pprExternDecl info_lbl PtrRep))
  where
    info_lbl = infoTableLabelFromCI cl_info

ppr_decls_AbsC (COpStmt	results	_ args _ _) = ppr_decls_Amodes (results ++ args)
ppr_decls_AbsC (CSimultaneous abc)  	    = ppr_decls_AbsC abc

ppr_decls_AbsC (CMacroStmt	    _ amodes)	= ppr_decls_Amodes amodes

ppr_decls_AbsC (CCallProfCtrMacro   _ amodes)	= ppr_decls_Amodes [] -- *****!!!
  -- you get some nasty re-decls of stdio.h if you compile
  -- the prelude while looking inside those amodes;
  -- no real reason to, anyway.
ppr_decls_AbsC (CCallProfCCMacro    _ amodes)	= ppr_decls_Amodes amodes

ppr_decls_AbsC (CStaticClosure closure_lbl closure_info cost_centre amodes)
	-- ToDo: strictly speaking, should chk "cost_centre" amode
  = ppr_decls_Amodes amodes

ppr_decls_AbsC (CClosureInfoAndCode cl_info slow maybe_fast upd_lbl _ _)
  = ppr_decls_Amodes [entry_lbl, upd_lbl]	`thenTE` \ p1 ->
    ppr_decls_AbsC slow				`thenTE` \ p2 ->
    (case maybe_fast of
	Nothing   -> returnTE (Nothing, Nothing)
	Just fast -> ppr_decls_AbsC fast)	`thenTE` \ p3 ->
    returnTE (maybe_uppAboves [p1, p2, p3])
  where
    entry_lbl = CLbl slow_lbl CodePtrRep
    slow_lbl    = case (nonemptyAbsC slow) of
		    Nothing -> mkErrorStdEntryLabel
		    Just _  -> entryLabelFromCI cl_info

ppr_decls_AbsC (CRetVector label maybe_amodes absC)
  = ppr_decls_Amodes (catMaybes maybe_amodes)	`thenTE` \ p1 ->
    ppr_decls_AbsC   absC			`thenTE` \ p2 ->
    returnTE (maybe_uppAboves [p1, p2])

ppr_decls_AbsC (CRetUnVector   _ amode)  = ppr_decls_Amode amode
ppr_decls_AbsC (CFlatRetVector _ amodes) = ppr_decls_Amodes amodes
\end{code}

\begin{code}
ppr_decls_Amode :: CAddrMode -> TeM (Maybe Unpretty, Maybe Unpretty)
ppr_decls_Amode (CVal _ _)	= returnTE (Nothing, Nothing)
ppr_decls_Amode (CAddr _)	= returnTE (Nothing, Nothing)
ppr_decls_Amode (CReg _)	= returnTE (Nothing, Nothing)
ppr_decls_Amode (CString _)	= returnTE (Nothing, Nothing)
ppr_decls_Amode (CLit _)	= returnTE (Nothing, Nothing)
ppr_decls_Amode (CLitLit _ _) 	= returnTE (Nothing, Nothing)
ppr_decls_Amode (COffset _)	= returnTE (Nothing, Nothing)

-- CIntLike must be a literal -- no decls
ppr_decls_Amode (CIntLike int)	= returnTE (Nothing, Nothing)

-- CCharLike may have be arbitrary value -- may have decls
ppr_decls_Amode (CCharLike char)
  = ppr_decls_Amode char

-- now, the only place where we actually print temps/externs...
ppr_decls_Amode (CTemp uniq kind)
  = case kind of
      VoidRep -> returnTE (Nothing, Nothing)
      other ->
	tempSeenTE uniq `thenTE` \ temp_seen ->
	returnTE
	  (if temp_seen then Nothing else Just (pprTempDecl uniq kind), Nothing)

ppr_decls_Amode (CLbl label VoidRep)
  = returnTE (Nothing, Nothing)

ppr_decls_Amode (CLbl label kind)
  = labelSeenTE label `thenTE` \ label_seen ->
    returnTE (Nothing,
	      if label_seen then Nothing else Just (pprExternDecl label kind))

{- WRONG:
ppr_decls_Amode (CUnVecLbl direct vectored)
  = labelSeenTE direct   `thenTE` \ dlbl_seen ->
    labelSeenTE vectored `thenTE` \ vlbl_seen ->
    let
	ddcl = if dlbl_seen then uppNil else pprExternDecl direct CodePtrRep
	vdcl = if vlbl_seen then uppNil else pprExternDecl vectored DataPtrRep
    in
    returnTE (Nothing,
    	    	if (dlbl_seen || not (needsCDecl direct)) &&
    	    	   (vlbl_seen || not (needsCDecl vectored)) then Nothing
    	        else Just (uppBesides [uppStr "UNVEC(", ddcl, uppComma, vdcl, uppRparen]))
-}

ppr_decls_Amode (CUnVecLbl direct vectored)
  = -- We don't mark either label as "seen", because
    -- we don't know which one will be used and which one tossed
    -- by the C macro...
    --labelSeenTE direct   `thenTE` \ dlbl_seen ->
    --labelSeenTE vectored `thenTE` \ vlbl_seen ->
    let
	ddcl = {-if dlbl_seen then uppNil else-} pprExternDecl direct CodePtrRep
	vdcl = {-if vlbl_seen then uppNil else-} pprExternDecl vectored DataPtrRep
    in
    returnTE (Nothing,
    	    	if ({-dlbl_seen ||-} not (needsCDecl direct)) &&
    	    	   ({-vlbl_seen ||-} not (needsCDecl vectored)) then Nothing
    	        else Just (uppBesides [uppStr "UNVEC(", ddcl, uppComma, vdcl, uppRparen]))

ppr_decls_Amode (CTableEntry base index _)
  = ppr_decls_Amode base    `thenTE` \ p1 ->
    ppr_decls_Amode index   `thenTE` \ p2 ->
    returnTE (maybe_uppAboves [p1, p2])

ppr_decls_Amode (CMacroExpr _ _ amodes)
  = ppr_decls_Amodes amodes

ppr_decls_Amode other = returnTE (Nothing, Nothing)


maybe_uppAboves :: [(Maybe Unpretty, Maybe Unpretty)] -> (Maybe Unpretty, Maybe Unpretty)
maybe_uppAboves ps
  = BIND (unzip ps)	_TO_ (ts, es) ->
    BIND (catMaybes ts)	_TO_ real_ts ->
    BIND (catMaybes es)	_TO_ real_es ->
    (if (null real_ts) then Nothing else Just (uppAboves real_ts),
     if (null real_es) then Nothing else Just (uppAboves real_es))
    BEND BEND BEND
\end{code}

\begin{code}
ppr_decls_Amodes :: [CAddrMode] -> TeM (Maybe Unpretty, Maybe Unpretty)
ppr_decls_Amodes amodes
  = mapTE ppr_decls_Amode amodes `thenTE` \ ps ->
    returnTE ( maybe_uppAboves ps )
\end{code}
