%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
%************************************************************************
%*									*
\section[PprAbsC]{Pretty-printing Abstract~C}
%*									*
%************************************************************************

\begin{code}
module PprAbsC (
	writeRealC,
	dumpRealC,
	pprAmode,
	pprMagicId
    ) where

#include "HsVersions.h"

import IO	( Handle )

import PrimRep 
import AbsCSyn
import ClosureInfo
import AbsCUtils	( getAmodeRep, nonemptyAbsC,
			  mixedPtrLocn, mixedTypeLocn
			)

import ForeignCall	( CCallSpec(..), CCallTarget(..), playSafe,
			  playThreadSafe, ccallConvAttribute,
			  ForeignCall(..), DNCallSpec(..),
			  DNType(..), DNKind(..) )
import CLabel		( externallyVisibleCLabel,
			  needsCDecl, pprCLabel, mkClosureLabel,
			  mkReturnInfoLabel, mkReturnPtLabel, mkClosureTblLabel,
			  CLabel, CLabelType(..), labelType, labelDynamic
			)

import CmdLineOpts	( opt_SccProfilingOn, opt_GranMacros )
import CostCentre	( pprCostCentreDecl, pprCostCentreStackDecl )

import Costs		( costs, addrModeCosts, CostRes(..), Side(..) )
import CStrings		( pprCLabelString )
import FiniteMap	( addToFM, emptyFM, lookupFM, FiniteMap )
import Literal		( Literal(..) )
import TyCon		( tyConDataCons )
import Name		( NamedThing(..) )
import Maybes		( catMaybes )
import PrimOp		( primOpNeedsWrapper )
import MachOp		( MachOp(..) )
import PrimRep		( isFloatingRep, PrimRep(..), getPrimRepSize )
import Unique		( pprUnique, Unique{-instance NamedThing-} )
import UniqSet		( emptyUniqSet, elementOfUniqSet,
			  addOneToUniqSet, UniqSet
			)
import StgSyn		( StgOp(..) )
import Outputable
import FastString
import Util		( lengthExceeds )

#if __GLASGOW_HASKELL__ >= 504
import Data.Array.ST
#endif

#ifdef DEBUG
import Util		( listLengthCmp )
#endif

import Maybe		( isJust )
import GLAEXTS
import MONAD_ST

infixr 9 `thenTE`
\end{code}

For spitting out the costs of an abstract~C expression, @writeRealC@
now not only prints the C~code of the @absC@ arg but also adds a macro
call to a cost evaluation function @GRAN_EXEC@. For that,
@pprAbsC@ has a new ``costs'' argument.  %% HWL

\begin{code}
{-
writeRealC :: Handle -> AbstractC -> IO ()
writeRealC handle absC
     -- avoid holding on to the whole of absC in the !Gransim case.
     if opt_GranMacros
	then printForCFast fp (pprAbsC absC (costs absC))
	else printForCFast fp (pprAbsC absC (panic "costs"))
	     --printForC handle (pprAbsC absC (panic "costs"))
dumpRealC :: AbstractC -> SDoc
dumpRealC absC = pprAbsC absC (costs absC)
-}

writeRealC :: Handle -> AbstractC -> IO ()
--writeRealC handle absC = 
-- _scc_ "writeRealC" 
-- printDoc LeftMode handle (pprAbsC absC (costs absC))

writeRealC handle absC
 | opt_GranMacros = _scc_ "writeRealC" printForC handle $ 
				       pprCode CStyle (pprAbsC absC (costs absC))
 | otherwise	  = _scc_ "writeRealC" printForC handle $
				       pprCode CStyle (pprAbsC absC (panic "costs"))

dumpRealC :: AbstractC -> SDoc
dumpRealC absC
 | opt_GranMacros = pprCode CStyle (pprAbsC absC (costs absC))
 | otherwise	  = pprCode CStyle (pprAbsC absC (panic "costs"))

\end{code}

This emits the macro,  which is used in GrAnSim  to compute the total costs
from a cost 5 tuple. %%  HWL

\begin{code}
emitMacro :: CostRes -> SDoc

emitMacro _ | not opt_GranMacros = empty

emitMacro (Cost (i,b,l,s,f))
  = hcat [ ptext SLIT("GRAN_EXEC"), char '(',
                          int i, comma, int b, comma, int l, comma,
	                  int s, comma, int f, pp_paren_semi ]

pp_paren_semi = text ");"
\end{code}

New type: Now pprAbsC also takes the costs for evaluating the Abstract C
code as an argument (that's needed when spitting out the GRAN_EXEC macro
which must be done before the return i.e. inside absC code)   HWL

\begin{code}
pprAbsC :: AbstractC -> CostRes -> SDoc
pprAbsC AbsCNop _ = empty
pprAbsC (AbsCStmts s1 s2) c = ($$) (pprAbsC s1 c) (pprAbsC s2 c)

pprAbsC (CAssign dest src) _ = pprAssign (getAmodeRep dest) dest src

pprAbsC (CJump target) c
  = ($$) (hcat [emitMacro c {-WDP:, text "/* <--++  CJump */"-} ])
	     (hcat [ text jmp_lit, pprAmode target, pp_paren_semi ])

pprAbsC (CFallThrough target) c
  = ($$) (hcat [emitMacro c {-WDP:, text "/* <--++  CFallThrough */"-} ])
	     (hcat [ text jmp_lit, pprAmode target, pp_paren_semi ])

-- --------------------------------------------------------------------------
-- Spit out GRAN_EXEC macro immediately before the return                 HWL

pprAbsC (CReturn am return_info)  c
  = ($$) (hcat [emitMacro c {-WDP:, text "/* <----  CReturn */"-} ])
	     (hcat [text jmp_lit, target, pp_paren_semi ])
  where
   target = case return_info of
    	DirectReturn -> hcat [ptext SLIT("ENTRY_CODE"), lparen,
			      pprAmode am, rparen]
	DynamicVectoredReturn am' -> mk_vector (pprAmode am')
	StaticVectoredReturn n -> mk_vector (int n)	-- Always positive
   mk_vector x = hcat [ptext SLIT("RET_VEC"), char '(', pprAmode am, comma,
		       x, rparen ]

pprAbsC (CSplitMarker) _ = ptext SLIT("__STG_SPLIT_MARKER")

-- we optimise various degenerate cases of CSwitches.

-- --------------------------------------------------------------------------
-- Assume: CSwitch is also end of basic block
--         costs function yields nullCosts for whole switch
--         ==> inherited costs c are those of basic block up to switch
--         ==> inherit c + costs for the corresponding branch
--                                                                       HWL
-- --------------------------------------------------------------------------

pprAbsC (CSwitch discrim [] deflt) c
  = pprAbsC deflt (c + costs deflt)
    -- Empty alternative list => no costs for discrim as nothing cond. here HWL

pprAbsC (CSwitch discrim [(tag,alt_code)] deflt) c -- only one alt
  = case (nonemptyAbsC deflt) of
      Nothing ->		-- one alt and no default
		 pprAbsC alt_code (c + costs alt_code)
		 -- Nothing conditional in here either  HWL

      Just dc ->		-- make it an "if"
		 do_if_stmt discrim tag alt_code dc c

-- What problem is the re-ordering trying to solve ?
pprAbsC (CSwitch discrim [(tag1@(MachInt i1), alt_code1),
			  (tag2@(MachInt i2), alt_code2)] deflt) c
  | empty_deflt && ((i1 == 0 && i2 == 1) || (i1 == 1 && i2 == 0))
  = if (i1 == 0) then
	do_if_stmt discrim tag1 alt_code1 alt_code2 c
    else
	do_if_stmt discrim tag2 alt_code2 alt_code1 c
  where
    empty_deflt = not (isJust (nonemptyAbsC deflt))

pprAbsC (CSwitch discrim alts deflt) c -- general case
  | isFloatingRep (getAmodeRep discrim)
    = pprAbsC (foldr ( \ a -> CSwitch discrim [a]) deflt alts) c
  | otherwise
    = vcat [
	hcat [text "switch (", pp_discrim, text ") {"],
	nest 2 (vcat (map ppr_alt alts)),
	(case (nonemptyAbsC deflt) of
	   Nothing -> empty
	   Just dc ->
	    nest 2 (vcat [ptext SLIT("default:"),
				  pprAbsC dc (c + switch_head_cost
						    + costs dc),
				  ptext SLIT("break;")])),
	char '}' ]
  where
    pp_discrim
      = pprAmode discrim

    ppr_alt (lit, absC)
      = vcat [ hcat [ptext SLIT("case "), pprBasicLit lit, char ':'],
		   nest 2 (($$) (pprAbsC absC (c + switch_head_cost + costs absC))
				       (ptext SLIT("break;"))) ]

    -- Costs for addressing header of switch and cond. branching        -- HWL
    switch_head_cost = addrModeCosts discrim Rhs + (Cost (0, 1, 0, 0, 0))

pprAbsC stmt@(COpStmt results (StgFCallOp fcall uniq) args vol_regs) _
  = pprFCall fcall uniq args results vol_regs

pprAbsC stmt@(COpStmt results (StgPrimOp op) args vol_regs) _
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
    if primOpNeedsWrapper op then
    	case (ppr_vol_regs vol_regs) of { (pp_saves, pp_restores) ->
    	vcat [  pp_saves,
    	    	the_op,
    	    	pp_restores
    	     ]
	}
    else
    	the_op
  where
    ppr_op_call results args
      = hcat [ ppr op, lparen,
	hcat (punctuate comma (map ppr_op_result results)),
	if null results || null args then empty else comma,
	hcat (punctuate comma (map pprAmode args)),
	pp_paren_semi ]

    ppr_op_result r = ppr_amode r
      -- primop macros do their own casting of result;
      -- hence we can toss the provided cast...

-- NEW CASES FOR EXPANDED PRIMOPS

pprAbsC stmt@(CMachOpStmt res mop [arg1,arg2] maybe_vols) _
  = let prefix_fn = mop `elem` [MO_Dbl_Pwr, MO_Flt_Pwr, MO_NatS_MulMayOflo]
    in
    case ppr_maybe_vol_regs maybe_vols of {(saves,restores) ->
    saves $$
    hcat (
       [ppr_amode res, equals]
       ++ (if prefix_fn 
           then [pprMachOp_for_C mop, parens (pprAmode arg1 <> comma <> pprAmode arg2)]
           else [pprAmode arg1, pprMachOp_for_C mop, pprAmode arg2])
       ++ [semi]
    )
    $$ restores
    }

pprAbsC stmt@(CMachOpStmt res mop [arg1] maybe_vols) _
  = case ppr_maybe_vol_regs maybe_vols of {(saves,restores) ->
    saves $$
    hcat [ppr_amode res, equals, 
          pprMachOp_for_C mop, parens (pprAmode arg1),
          semi]
    $$ restores
    }

pprAbsC stmt@(CSequential stuff) c
  = vcat (map (flip pprAbsC c) stuff)

-- end of NEW CASES FOR EXPANDED PRIMOPS

pprAbsC stmt@(CSRT lbl closures) c
  = case (pprTempAndExternDecls stmt) of { (_, pp_exts) ->
         pp_exts
      $$ ptext SLIT("SRT") <> lparen <> pprCLabel lbl <> rparen
      $$ nest 2 (hcat (punctuate comma (map pp_closure_lbl closures)))
         <> ptext SLIT("};")
  }

pprAbsC stmt@(CBitmap liveness@(Liveness lbl size mask)) c
  = pprWordArray lbl (mkWordCLit (fromIntegral size) : bitmapAddrModes mask)

pprAbsC stmt@(CSRTDesc desc_lbl srt_lbl off len bitmap) c
  = pprWordArray desc_lbl (
	CAddr (CIndex (CLbl srt_lbl DataPtrRep) (mkIntCLit off) WordRep) :
	mkWordCLit (fromIntegral len) :
	bitmapAddrModes bitmap
     )

pprAbsC (CSimultaneous abs_c) c
  = hcat [ptext SLIT("{{"), pprAbsC abs_c c, ptext SLIT("}}")]

pprAbsC (CCheck macro as code) c
  = hcat [ptext (cCheckMacroText macro), lparen,
       hcat (punctuate comma (map ppr_amode as)), comma,
       pprAbsC code c, pp_paren_semi
    ]
pprAbsC (CMacroStmt macro as) _
  = hcat [ptext (cStmtMacroText macro), lparen,
	hcat (punctuate comma (map ppr_amode as)),pp_paren_semi] -- no casting
pprAbsC (CCallProfCtrMacro op as) _
  = hcat [ftext op, lparen,
	hcat (punctuate comma (map ppr_amode as)),pp_paren_semi]
pprAbsC (CCallProfCCMacro op as) _
  = hcat [ftext op, lparen,
	hcat (punctuate comma (map ppr_amode as)),pp_paren_semi]
pprAbsC stmt@(CCallTypedef is_tdef (CCallSpec op_str cconv _) uniq results args) _
  =  hsep [ ptext (if is_tdef then SLIT("typedef") else SLIT("extern"))
	  , ccall_res_ty
	  , fun_nm
	  , parens (hsep (punctuate comma ccall_decl_ty_args))
	  ] <> semi
    where
    {-
      In the non-casm case, to ensure that we're entering the given external
      entry point using the correct calling convention, we have to do the following:

	- When entering via a function pointer (the `dynamic' case) using the specified
	  calling convention, we emit a typedefn declaration attributed with the
	  calling convention to use together with the result and parameter types we're
	  assuming. Coerce the function pointer to this type and go.

        - to enter the function at a given code label, we emit an extern declaration
	  for the label here, stating the calling convention together with result and
          argument types we're assuming. 

          The C compiler will hopefully use this extern declaration to good effect,
          reporting any discrepancies between our extern decl and any other that
	  may be in scope.
    
	  Re: calling convention, notice that gcc (2.8.1 and egcs-1.0.2) will for
  	  the external function `foo' use the calling convention of the first `foo'
	  prototype it encounters (nor does it complain about conflicting attribute
	  declarations). The consequence of this is that you cannot override the
	  calling convention of `foo' using an extern declaration (you'd have to use
	  a typedef), but why you would want to do such a thing in the first place
	  is totally beyond me.
	  
	  ToDo: petition the gcc folks to add code to warn about conflicting attribute
	  declarations.

    -}

     fun_nm
       | is_tdef   = parens (text (ccallConvAttribute cconv) <+> char '*' <> ccall_fun_ty)
       | otherwise = text (ccallConvAttribute cconv) <+> ccall_fun_ty

     ccall_fun_ty = 
        case op_str of
	  DynamicTarget  -> ptext SLIT("_ccall_fun_ty") <> ppr uniq
	  StaticTarget x -> pprCLabelString x

     ccall_res_ty = 
       case non_void_results of
          []       -> ptext SLIT("void")
	  [amode]  -> ppr (getAmodeRep amode)
	  _	   -> panic "pprAbsC{CCallTypedef}: ccall_res_ty"

     ccall_decl_ty_args 
       | is_tdef   = tail ccall_arg_tys
       | otherwise = ccall_arg_tys

     ccall_arg_tys      = map (ppr . getAmodeRep) non_void_args

      -- the first argument will be the "I/O world" token (a VoidRep)
      -- all others should be non-void
     non_void_args =
	let nvas = init args
	in ASSERT (all non_void nvas) nvas

      -- there will usually be two results: a (void) state which we
      -- should ignore and a (possibly void) result.
     non_void_results =
	let nvrs = grab_non_void_amodes results
	in ASSERT (listLengthCmp nvrs 1 /= GT) nvrs

pprAbsC (CCodeBlock lbl abs_C) _
  = if not (isJust(nonemptyAbsC abs_C)) then
	pprTrace "pprAbsC: curious empty code block for" (pprCLabel lbl) empty
    else
    case (pprTempAndExternDecls abs_C) of { (pp_temps, pp_exts) ->
    vcat [
        empty,
	pp_exts, 
	hcat [text (if (externallyVisibleCLabel lbl)
			  then "FN_("	-- abbreviations to save on output
			  else "IF_("),
		   pprCLabel lbl, text ") {"],

	pp_temps,

	nest 8 (ptext SLIT("FB_")),
	nest 8 (pprAbsC abs_C (costs abs_C)),
	nest 8 (ptext SLIT("FE_")),
	char '}',
        char ' ' ]
    }


pprAbsC (CInitHdr cl_info amode cost_centre size) _
  = hcat [ ptext SLIT("SET_HDR_"), char '(',
		ppr_amode amode, comma,
		pprCLabelAddr info_lbl, comma,
		if_profiling (pprAmode cost_centre), comma,
		if_profiling (int size),
		pp_paren_semi ]
  where
    info_lbl	= infoTableLabelFromCI cl_info


pprAbsC stmt@(CStaticClosure closure_lbl cl_info cost_centre amodes) _
  = case (pprTempAndExternDecls stmt) of { (_, pp_exts) ->
    vcat [
	pp_exts,
	hcat [
		ptext SLIT("SET_STATIC_HDR"), char '(',
		pprCLabel closure_lbl,			        comma,
		pprCLabel info_lbl,				comma,
		if_profiling (pprAmode cost_centre), 		comma,
		ppLocalness closure_lbl,			comma,
		ppLocalnessMacro True{-include dyn-} info_lbl,
		char ')'
		],
	nest 2 (ppr_payload amodes),
	ptext SLIT("};") ]
    }
  where
    info_lbl    = infoTableLabelFromCI cl_info

    ppr_payload [] = empty
    ppr_payload ls = 
	comma <+> 
	  (braces $ hsep $ punctuate comma $
	   map (text "(L_)" <>) (foldr ppr_item [] ls))

    ppr_item item rest
      | rep == VoidRep   = rest
      | rep == FloatRep  = ppr_amode (floatToWord item) : rest
      | rep == DoubleRep = map ppr_amode (doubleToWords item) ++ rest
      | otherwise  	 = ppr_amode item : rest
      where 
	rep  = getAmodeRep item

pprAbsC stmt@(CClosureInfoAndCode cl_info entry) _
  =  pprWordArray info_lbl (mkInfoTable cl_info)
  $$ let stuff = CCodeBlock entry_lbl entry in
     pprAbsC stuff (costs stuff)
  where
	entry_lbl = entryLabelFromCI cl_info
	info_lbl  = infoTableLabelFromCI cl_info

pprAbsC stmt@(CClosureTbl tycon) _
  = vcat (
	ptext SLIT("CLOSURE_TBL") <> 
	   lparen <> pprCLabel (mkClosureTblLabel tycon) <> rparen :
	punctuate comma (
	   map (pp_closure_lbl . mkClosureLabel . getName) (tyConDataCons tycon)
	)
   ) $$ ptext SLIT("};")

pprAbsC stmt@(CRetDirect uniq code srt liveness) _
  =  pprWordArray info_lbl (mkRetInfoTable entry_lbl srt liveness)
  $$ let stuff = CCodeBlock entry_lbl code in
     pprAbsC stuff (costs stuff)
  where
     info_lbl  = mkReturnInfoLabel uniq
     entry_lbl = mkReturnPtLabel uniq

pprAbsC stmt@(CRetVector lbl amodes srt liveness) _
  = pprWordArray lbl (mkVecInfoTable amodes srt liveness)

pprAbsC stmt@(CModuleInitBlock plain_lbl lbl code) _
  = vcat [
	ptext SLIT("START_MOD_INIT") <> 
	    parens (pprCLabel plain_lbl <> comma <> pprCLabel lbl),
	case (pprTempAndExternDecls stmt) of { (_, pp_exts) -> pp_exts },
	pprAbsC code (costs code),
	hcat [ptext SLIT("END_MOD_INIT"), lparen, rparen]
    ]

pprAbsC (CCostCentreDecl is_local cc) _ = pprCostCentreDecl is_local cc
pprAbsC (CCostCentreStackDecl ccs)    _ = pprCostCentreStackDecl ccs
\end{code}

Info tables... just arrays of words (the translation is done in
ClosureInfo).

\begin{code}
pprWordArray lbl amodes
  = (case snd (initTE (ppr_decls_Amodes amodes)) of
	Just pp -> pp
	Nothing -> empty)
  $$ hcat [ ppLocalness lbl, ptext SLIT("StgWord "), 
	    pprCLabel lbl, ptext SLIT("[] = {") ]
  $$ hcat (punctuate comma (map (castToWord.pprAmode) amodes))
  $$ ptext SLIT("};")

castToWord s = text "(W_)(" <> s <> char ')'
\end{code}

\begin{code}
-- Print a CMachOp in a way suitable for emitting via C.
pprMachOp_for_C MO_Nat_Add       = char '+'
pprMachOp_for_C MO_Nat_Sub       = char '-'
pprMachOp_for_C MO_Nat_Eq        = text "==" 
pprMachOp_for_C MO_Nat_Ne        = text "!="

pprMachOp_for_C MO_NatS_Ge       = text ">="
pprMachOp_for_C MO_NatS_Le       = text "<="
pprMachOp_for_C MO_NatS_Gt       = text ">"
pprMachOp_for_C MO_NatS_Lt       = text "<"

pprMachOp_for_C MO_NatU_Ge       = text ">="
pprMachOp_for_C MO_NatU_Le       = text "<="
pprMachOp_for_C MO_NatU_Gt       = text ">"
pprMachOp_for_C MO_NatU_Lt       = text "<"

pprMachOp_for_C MO_NatS_Mul      = char '*'
pprMachOp_for_C MO_NatS_MulMayOflo = text "mulIntMayOflo"
pprMachOp_for_C MO_NatS_Quot     = char '/'
pprMachOp_for_C MO_NatS_Rem      = char '%'
pprMachOp_for_C MO_NatS_Neg      = char '-'

pprMachOp_for_C MO_NatU_Mul      = char '*'
pprMachOp_for_C MO_NatU_Quot     = char '/'
pprMachOp_for_C MO_NatU_Rem      = char '%'

pprMachOp_for_C MO_Nat_And       = text "&"
pprMachOp_for_C MO_Nat_Or        = text "|"
pprMachOp_for_C MO_Nat_Xor       = text "^"
pprMachOp_for_C MO_Nat_Not       = text "~"
pprMachOp_for_C MO_Nat_Shl       = text "<<"
pprMachOp_for_C MO_Nat_Shr       = text ">>"
pprMachOp_for_C MO_Nat_Sar       = text ">>"

pprMachOp_for_C MO_32U_Eq        = text "=="
pprMachOp_for_C MO_32U_Ne        = text "!="
pprMachOp_for_C MO_32U_Ge        = text ">="
pprMachOp_for_C MO_32U_Le        = text "<="
pprMachOp_for_C MO_32U_Gt        = text ">"
pprMachOp_for_C MO_32U_Lt        = text "<"

pprMachOp_for_C MO_Dbl_Eq        = text "=="
pprMachOp_for_C MO_Dbl_Ne        = text "!="
pprMachOp_for_C MO_Dbl_Ge        = text ">="
pprMachOp_for_C MO_Dbl_Le        = text "<="
pprMachOp_for_C MO_Dbl_Gt        = text ">"
pprMachOp_for_C MO_Dbl_Lt        = text "<"

pprMachOp_for_C MO_Dbl_Add       = text "+"
pprMachOp_for_C MO_Dbl_Sub       = text "-"
pprMachOp_for_C MO_Dbl_Mul       = text "*"
pprMachOp_for_C MO_Dbl_Div       = text "/"
pprMachOp_for_C MO_Dbl_Pwr       = text "pow"

pprMachOp_for_C MO_Dbl_Sin       = text "sin"
pprMachOp_for_C MO_Dbl_Cos       = text "cos"
pprMachOp_for_C MO_Dbl_Tan       = text "tan"
pprMachOp_for_C MO_Dbl_Sinh      = text "sinh"
pprMachOp_for_C MO_Dbl_Cosh      = text "cosh"
pprMachOp_for_C MO_Dbl_Tanh      = text "tanh"
pprMachOp_for_C MO_Dbl_Asin      = text "asin"
pprMachOp_for_C MO_Dbl_Acos      = text "acos"
pprMachOp_for_C MO_Dbl_Atan      = text "atan"
pprMachOp_for_C MO_Dbl_Log       = text "log"
pprMachOp_for_C MO_Dbl_Exp       = text "exp"
pprMachOp_for_C MO_Dbl_Sqrt      = text "sqrt"
pprMachOp_for_C MO_Dbl_Neg       = text "-"

pprMachOp_for_C MO_Flt_Add       = text "+"
pprMachOp_for_C MO_Flt_Sub       = text "-"
pprMachOp_for_C MO_Flt_Mul       = text "*"
pprMachOp_for_C MO_Flt_Div       = text "/"
pprMachOp_for_C MO_Flt_Pwr       = text "pow"

pprMachOp_for_C MO_Flt_Eq        = text "=="
pprMachOp_for_C MO_Flt_Ne        = text "!="
pprMachOp_for_C MO_Flt_Ge        = text ">="
pprMachOp_for_C MO_Flt_Le        = text "<="
pprMachOp_for_C MO_Flt_Gt        = text ">"
pprMachOp_for_C MO_Flt_Lt        = text "<"

pprMachOp_for_C MO_Flt_Sin       = text "sin"
pprMachOp_for_C MO_Flt_Cos       = text "cos"
pprMachOp_for_C MO_Flt_Tan       = text "tan"
pprMachOp_for_C MO_Flt_Sinh      = text "sinh"
pprMachOp_for_C MO_Flt_Cosh      = text "cosh"
pprMachOp_for_C MO_Flt_Tanh      = text "tanh"
pprMachOp_for_C MO_Flt_Asin      = text "asin"
pprMachOp_for_C MO_Flt_Acos      = text "acos"
pprMachOp_for_C MO_Flt_Atan      = text "atan"
pprMachOp_for_C MO_Flt_Log       = text "log"
pprMachOp_for_C MO_Flt_Exp       = text "exp"
pprMachOp_for_C MO_Flt_Sqrt      = text "sqrt"
pprMachOp_for_C MO_Flt_Neg       = text "-"

pprMachOp_for_C MO_32U_to_NatS   = text "(StgInt)"
pprMachOp_for_C MO_NatS_to_32U   = text "(StgWord32)"

pprMachOp_for_C MO_NatS_to_Dbl   = text "(StgDouble)"
pprMachOp_for_C MO_Dbl_to_NatS   = text "(StgInt)"

pprMachOp_for_C MO_NatS_to_Flt   = text "(StgFloat)"
pprMachOp_for_C MO_Flt_to_NatS   = text "(StgInt)"

pprMachOp_for_C MO_NatS_to_NatU  = text "(StgWord)"
pprMachOp_for_C MO_NatU_to_NatS  = text "(StgInt)"

pprMachOp_for_C MO_NatS_to_NatP  = text "(void*)"
pprMachOp_for_C MO_NatP_to_NatS  = text "(StgInt)"
pprMachOp_for_C MO_NatU_to_NatP  = text "(void*)"
pprMachOp_for_C MO_NatP_to_NatU  = text "(StgWord)"

pprMachOp_for_C MO_Dbl_to_Flt    = text "(StgFloat)"
pprMachOp_for_C MO_Flt_to_Dbl    = text "(StgDouble)"

pprMachOp_for_C MO_8S_to_NatS    = text "(StgInt8)(StgInt)"
pprMachOp_for_C MO_16S_to_NatS   = text "(StgInt16)(StgInt)"
pprMachOp_for_C MO_32S_to_NatS   = text "(StgInt32)(StgInt)"

pprMachOp_for_C MO_8U_to_NatU    = text "(StgWord8)(StgWord)"
pprMachOp_for_C MO_16U_to_NatU   = text "(StgWord16)(StgWord)"
pprMachOp_for_C MO_32U_to_NatU   = text "(StgWord32)(StgWord)"

pprMachOp_for_C MO_8U_to_32U     = text "(StgWord32)"
pprMachOp_for_C MO_32U_to_8U     = text "(StgWord8)"


ppLocalness lbl
  = if (externallyVisibleCLabel lbl) 
		then empty 
		else ptext SLIT("static ")

-- Horrible macros for declaring the types and locality of labels (see
-- StgMacros.h).

ppLocalnessMacro include_dyn_prefix clabel =
     hcat [
        visiblity_prefix,
	dyn_prefix,
        case label_type of
	  ClosureType        -> ptext SLIT("C_")
	  CodeType           -> ptext SLIT("F_")
	  InfoTblType        -> ptext SLIT("I_")
	  RetInfoTblType     -> ptext SLIT("RI_")
	  ClosureTblType     -> ptext SLIT("CP_")
	  DataType           -> ptext SLIT("D_")
     ]
  where
   is_visible = externallyVisibleCLabel clabel
   label_type = labelType clabel

   visiblity_prefix
     | is_visible = char 'E'
     | otherwise  = char 'I'

   dyn_prefix
     | include_dyn_prefix && labelDynamic clabel = char 'D'
     | otherwise	      			 = empty

\end{code}

\begin{code}
jmp_lit = "JMP_("

grab_non_void_amodes amodes
  = filter non_void amodes

non_void amode
  = case (getAmodeRep amode) of
      VoidRep -> False
      k	-> True
\end{code}

\begin{code}
ppr_maybe_vol_regs :: Maybe [MagicId] -> (SDoc, SDoc)
ppr_maybe_vol_regs Nothing
   = (empty, empty)
ppr_maybe_vol_regs (Just vrs)
   = case ppr_vol_regs vrs of
        (saves, restores) 
           -> (pp_basic_saves $$ saves,
               pp_basic_restores $$ restores)

ppr_vol_regs :: [MagicId] -> (SDoc, SDoc)

ppr_vol_regs [] = (empty, empty)
ppr_vol_regs (VoidReg:rs) = ppr_vol_regs rs
ppr_vol_regs (r:rs)
  = let pp_reg = case r of
    	    	    VanillaReg pk n -> pprVanillaReg n
    	    	    _ -> pprMagicId r
	(more_saves, more_restores) = ppr_vol_regs rs
    in
    (($$) ((<>) (ptext SLIT("CALLER_SAVE_"))    pp_reg) more_saves,
     ($$) ((<>) (ptext SLIT("CALLER_RESTORE_")) pp_reg) more_restores)

-- pp_basic_{saves,restores}: The BaseReg, Sp, Hp and
-- HpLim (see StgRegs.lh) may need to be saved/restored around CCalls,
-- depending on the platform.  (The "volatile regs" stuff handles all
-- other registers.)  Just be *sure* BaseReg is OK before trying to do
-- anything else. The correct sequence of saves&restores are
-- encoded by the CALLER_*_SYSTEM macros.
pp_basic_saves    = ptext SLIT("CALLER_SAVE_SYSTEM")
pp_basic_restores = ptext SLIT("CALLER_RESTORE_SYSTEM")
\end{code}

\begin{code}
pp_closure_lbl lbl
      | labelDynamic lbl = text "DLL_SRT_ENTRY" <> parens (pprCLabel lbl)
      | otherwise	 = char '&' <> pprCLabel lbl
\end{code}

\begin{code}
if_profiling pretty
  = if  opt_SccProfilingOn
    then pretty
    else char '0' -- leave it out!
-- ---------------------------------------------------------------------------
-- Changes for GrAnSim:
--  draw costs for computation in head of if into both branches;
--  as no abstractC data structure is given for the head, one is constructed
--  guessing unknown values and fed into the costs function
-- ---------------------------------------------------------------------------

do_if_stmt discrim tag alt_code deflt c
   = let
       cond = hcat [ pprAmode discrim
		   , ptext SLIT(" == ")
		   , tcast
		   , pprAmode (CLit tag)
		   ]
	-- to be absolutely sure that none of the 
	-- conversion rules hit, e.g.,
	--
	--     minInt is different to (int)minInt
        --
	-- in C (when minInt is a number not a constant
	--  expression which evaluates to it.)
	-- 
       tcast = case tag of
		   MachInt _  -> ptext SLIT("(I_)")
		   _ 	      -> empty
     in
     ppr_if_stmt cond
		 alt_code deflt
		 (addrModeCosts discrim Rhs) c

ppr_if_stmt pp_pred then_part else_part discrim_costs c
  = vcat [
      hcat [text "if (", pp_pred, text ") {"],
      nest 8 (pprAbsC then_part 	(c + discrim_costs +
				       	(Cost (0, 2, 0, 0, 0)) +
					costs then_part)),
      (case nonemptyAbsC else_part of Nothing -> empty; Just _ -> text "} else {"),
      nest 8 (pprAbsC else_part  (c + discrim_costs +
					(Cost (0, 1, 0, 0, 0)) +
					costs else_part)),
      char '}' ]
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
4) Do the call, putting result into a local variable
5) Restore essential registers
6) Restore temporaries

   (This happens after restoration of essential registers because we
   might need the @Base@ register to access all the others correctly.)

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

  return_reg = _ccall_result;
}
\end{pseudocode}

Amendment to the above: if we can GC, we have to:

* make sure we save all our registers away where the garbage collector
  can get at them.
* be sure that there are no live registers or we're in trouble.
  (This can cause problems if you try something foolish like passing
   an array or a foreign obj to a _ccall_GC_ thing.)
* increment/decrement the @inCCallGC@ counter before/after the call so
  that the runtime check that PerformGC is being used sensibly will work.

\begin{code}
pprFCall call uniq args results vol_regs
  = case call of
      CCall (CCallSpec target _cconv safety) ->
        vcat [ char '{',
	        declare_local_vars,   -- local var for *result*
	        vcat local_arg_decls,
		makeCall target safety 
			 (process_casm local_vars pp_non_void_args (call_str target)),
	        assign_results,
	      char '}'
	     ]
      DNCall (DNCallSpec isStatic kind assem nm argTys resTy) ->
         let
	  target    = StaticTarget (mkFastString nm)
	  resultVar = "_ccall_result"
	  
	  hasAssemArg = isStatic || kind == DNConstructor
	  invokeOp  = 
	    case kind of
	      DNMethod 
	        | isStatic  -> "DN_invokeStatic"
		| otherwise -> "DN_invokeMethod"
	      DNField
	        | isStatic ->
		   if resTy == DNUnit 
		    then "DN_setStatic"
		    else "DN_getStatic"
                | otherwise ->
		   if resTy == DNUnit 
		    then "DN_setField"
		    else "DN_getField"
	      DNConstructor -> "DN_createObject"

	  (methArrDecl, methArrInit, methArrName, methArrLen) 
	    | null argTys = (empty, empty, text "NULL", text "0")
	    | otherwise   = 
	      ( text "DotnetArg __meth_args[" <> int (length argTys) <> text "];"
	      , vcat (zipWith3 (\ idx arg argTy -> 
	      			 text "__meth_args[" <> int idx <> text "].arg." <> text (toDotnetArgField argTy) <> equals <> ppr_amode arg <> semi $$
				 text "__meth_args[" <> int idx <> text "].arg_type=" <> text (toDotnetTy argTy) <> semi)
			       [0..]
			       non_void_args
			       argTys)
	      , text "__meth_args"
	      , int (length non_void_args)
	      )
	 in
         vcat [ char '{',
	 	  declare_local_vars,
		  vcat local_arg_decls,
		  vcat [ methArrDecl
		       , methArrInit
		       , text "_ccall_result1 =" <+> text invokeOp <> parens (
		    	  hcat (punctuate comma $
				     (if hasAssemArg then
				     	((if null assem then 
					    text "NULL" 
			  	         else 
				            doubleQuotes (text assem)):)
				      else
				      	 id) $
				     [ doubleQuotes $ text nm
				     , methArrName
				     , methArrLen
				     , text (toDotnetTy resTy)
				     , text "(void*)&" <> text resultVar 
				     ])) <> semi
			],
	          assign_results,
	        char '}'
	       ]
  where
    (pp_saves, pp_restores) = ppr_vol_regs vol_regs
    
    makeCall target safety theCall = 
        vcat [ pp_save_context,	theCall, pp_restore_context ]
     where
      (pp_save_context, pp_restore_context)
	| playSafe safety = ( text "{ I_" <+> ppr_uniq_token <> 
				text "; SUSPEND_THREAD" <> parens thread_macro_args <> semi
			    , text "RESUME_THREAD" <> parens thread_macro_args <> text ";}"
			    )
	| otherwise = (	pp_basic_saves $$ pp_saves,
			pp_basic_restores $$ pp_restores)
	   where
	    thread_macro_args = ppr_uniq_token <> comma <+> 
    		        	text "rts" <> ppr (playThreadSafe safety)
	    ppr_uniq_token = text "tok_" <> ppr uniq


    non_void_args = 
	let nvas = init args
	in ASSERT2 ( all non_void nvas, ppr call <+> hsep (map pprAmode args) )
	nvas
    -- the last argument will be the "I/O world" token (a VoidRep)
    -- all others should be non-void

    non_void_results =
	let nvrs = grab_non_void_amodes results
	in ASSERT (forDotnet || listLengthCmp nvrs 1 /= GT) nvrs
    -- there will usually be two results: a (void) state which we
    -- should ignore and a (possibly void) result.

    (local_arg_decls, pp_non_void_args)
      = unzip [ ppr_casm_arg a i | (a,i) <- non_void_args `zip` [1..] ]

    (declare_local_vars, local_vars, assign_results)
      = ppr_casm_results non_void_results forDotnet

    forDotnet
      = case call of
          DNCall{} -> True
	  _ -> False

    call_str tgt 
      = case tgt of
  	  StaticTarget fn -> mk_ccall_str (pprCLabelString fn) ccall_args
	  DynamicTarget   -> mk_ccall_str dyn_fun	       (tail ccall_args)

    ccall_args = zipWith (\ _ i -> char '%' <> int i) non_void_args [0..]
    dyn_fun    = parens (parens (ptext SLIT("_ccall_fun_ty") <> ppr uniq) <> text "%0")
						 

    -- Remainder only used for ccall
    mk_ccall_str fun_name ccall_fun_args = showSDoc
	(hcat [
		if null non_void_results
		  then empty
		  else text "%r = ",
		lparen, fun_name, lparen,
		  hcat (punctuate comma ccall_fun_args),
		text "));"
	])

toDotnetTy :: DNType -> String
toDotnetTy x = 
  case x of 
    DNByte -> "Dotnet_Byte"
    DNBool -> "Dotnet_Bool"
    DNChar -> "Dotnet_Char"
    DNDouble -> "Dotnet_Double"
    DNFloat  -> "Dotnet_Float"
    DNInt    -> "Dotnet_Int"
    DNInt8   -> "Dotnet_Int8"
    DNInt16  -> "Dotnet_Int16"
    DNInt32  -> "Dotnet_Int32"
    DNInt64  -> "Dotnet_Int64"
    DNWord8  -> "Dotnet_Word8"
    DNWord16 -> "Dotnet_Word16"
    DNWord32 -> "Dotnet_Word32"
    DNWord64 -> "Dotnet_Word64"
    DNPtr    -> "Dotnet_Ptr"
    DNUnit   -> "Dotnet_Unit"
    DNObject -> "Dotnet_Object"
    DNString -> "Dotnet_String"

toDotnetArgField :: DNType -> String
toDotnetArgField x = 
  case x of 
    DNByte -> "arg_byte"
    DNBool -> "arg_bool"
    DNChar -> "arg_char"
    DNDouble -> "arg_double"
    DNFloat  -> "arg_float"
    DNInt    -> "arg_int"
    DNInt8   -> "arg_int8"
    DNInt16  -> "arg_int16"
    DNInt32  -> "arg_int32"
    DNInt64  -> "arg_int64"
    DNWord8  -> "arg_word8"
    DNWord16 -> "arg_word16"
    DNWord32 -> "arg_word32"
    DNWord64 -> "arg_word64"
    DNPtr    -> "arg_ptr"
    DNUnit   -> "arg_ptr" -- can't happen
    DNObject -> "arg_obj"
    DNString -> "arg_str"

ppr_casm_arg :: CAddrMode -> Int -> (SDoc, SDoc)
    -- (a) decl and assignment, (b) local var to be used later

ppr_casm_arg amode a_num
  = let
	a_kind	 = getAmodeRep amode
	pp_amode = pprAmode amode
	pp_kind  = pprPrimKind a_kind

	local_var  = (<>) (ptext SLIT("_ccall_arg")) (int a_num)

	declare_local_var
	  = hcat [ pp_kind, space, local_var, equals, pp_amode, semi ]
    in
    (declare_local_var, local_var)
\end{code}

For l-values, the critical questions are:

1) Are there any results at all?

   We only allow zero or one results.

\begin{code}
ppr_casm_results
	:: [CAddrMode]	-- list of results (length <= 1)
	-> Bool         -- True => multiple results OK.
	->
	( SDoc,		-- declaration of any local vars
	  [SDoc],	-- list of result vars (same length as results)
	  SDoc )	-- assignment (if any) of results in local var to registers

ppr_casm_results [] _
  = (empty, [], empty) 	-- no results

ppr_casm_results (r:rs) multiResultsOK
  | not multiResultsOK && not (null rs) = panic "ppr_casm_results: ccall/casm with many results"
  | otherwise
  = foldr (\ (a,b,c) (as,bs,cs) -> (a $$ as, b ++ bs, c $$ cs))
  	  (empty,[],empty)
	  (zipWith pprRes (r:rs) ("" : map show [(1::Int)..]))
    where
      pprRes r suf = (declare_local_var, [local_var], assign_result)
       where
	result_reg = ppr_amode r
	r_kind	   = getAmodeRep r

	local_var  = ptext SLIT("_ccall_result") <> text suf

	(result_type, assign_result)
	  = (pprPrimKind r_kind,
	     hcat [ result_reg, equals, local_var, semi ])

	declare_local_var = hcat [ result_type, space, local_var, semi ]

\end{code}


Note the sneaky way _the_ result is represented by a list so that we
can complain if it's used twice.

ToDo: Any chance of giving line numbers when process-casm fails?
      Or maybe we should do a check _much earlier_ in compiler. ADR

\begin{code}
process_casm :: [SDoc]		-- results (length <= 1)
	     -> [SDoc]		-- arguments
	     -> String		-- format string (with embedded %'s)
	     ->	SDoc		-- code being generated

process_casm results args string = process results args string
 where
  process []    _ "" = empty
  process (_:_) _ "" = error ("process_casm: non-void result not assigned while processing _casm_ \"" ++ 
			      string ++ 
			      "\"\n(Try changing result type to IO ()\n")

  process ress args ('%':cs)
    = case cs of
	[] ->
	    error ("process_casm: lonely % while processing _casm_ \"" ++ string ++ "\".\n")

	('%':css) ->
	    char '%' <> process ress args css

	('r':css)  ->
	  case ress of
	    []  -> error ("process_casm: no result to match %r while processing _casm_ \"" ++ string ++ "\".\nTry deleting %r or changing result type from PrimIO ()\n")
	    [r] -> r <> (process [] args css)
	    _   -> panic ("process_casm: casm with many results while processing _casm_ \"" ++ string ++ "\".\n")

	other ->
	  let
		read_int :: ReadS Int
		read_int = reads
	  in
	  case (read_int other) of
	    [(num,css)] ->
		  if num >= 0 && args `lengthExceeds` num
		  then parens (args !! num) <> process ress args css
		  else error ("process_casm: no such arg #:"++(show num)++" while processing \"" ++ string ++ "\".\n")
	    _ -> error ("process_casm: not %<num> while processing _casm_ \"" ++ string ++ "\".\n")

  process ress args (other_c:cs)
    = char other_c <> process ress args cs
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
pprAssign :: PrimRep -> CAddrMode -> CAddrMode -> SDoc

pprAssign VoidRep dest src = empty
\end{code}

Special treatment for floats and doubles, to avoid unwanted conversions.

\begin{code}
pprAssign FloatRep dest@(CVal reg_rel _) src
  = hcat [ ptext SLIT("ASSIGN_FLT((W_*)"), parens (ppr_amode (CAddr reg_rel)), comma, pprAmode src, pp_paren_semi ]

pprAssign DoubleRep dest@(CVal reg_rel _) src
  = hcat [ ptext SLIT("ASSIGN_DBL((W_*)"), parens (ppr_amode (CAddr reg_rel)), comma, pprAmode src, pp_paren_semi ]

pprAssign Int64Rep dest@(CVal reg_rel _) src
  = hcat [ ptext SLIT("ASSIGN_Int64((W_*)"), parens (ppr_amode (CAddr reg_rel)), comma, pprAmode src, pp_paren_semi ]
pprAssign Word64Rep dest@(CVal reg_rel _) src
  = hcat [ ptext SLIT("ASSIGN_Word64((W_*)"), parens (ppr_amode (CAddr reg_rel)), comma, pprAmode src, pp_paren_semi ]
\end{code}

Lastly, the question is: will the C compiler think the types of the
two sides of the assignment match?

	We assume that the types will match if neither side is a
	@CVal@ addressing mode for any register which can point into
	the heap or stack.

Why?  Because the heap and stack are used to store miscellaneous
things, whereas the temporaries, registers, etc., are only used for
things of fixed type.

\begin{code}
pprAssign kind (CReg (VanillaReg _ dest)) (CReg (VanillaReg _ src))
  = hcat [ pprVanillaReg dest, equals,
		pprVanillaReg src, semi ]

pprAssign kind dest src
  | mixedTypeLocn dest
    -- Add in a cast to StgWord (a.k.a. W_) iff the destination is mixed
  = hcat [ ppr_amode dest, equals,
		text "(W_)(",	-- Here is the cast
		ppr_amode src, pp_paren_semi ]

pprAssign kind dest src
  | mixedPtrLocn dest && getAmodeRep src /= PtrRep
    -- Add in a cast to StgPtr (a.k.a. P_) iff the destination is mixed
  = hcat [ ppr_amode dest, equals,
		text "(P_)(",	-- Here is the cast
		ppr_amode src, pp_paren_semi ]

pprAssign kind other_dest src
  = hcat [ ppr_amode other_dest, equals,
		pprAmode  src, semi ]
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
pprAmode, ppr_amode :: CAddrMode -> SDoc
\end{code}

For reasons discussed above under assignments, @CVal@ modes need
to be treated carefully.  First come special cases for floats and doubles,
similar to those in @pprAssign@:

(NB: @PK_FLT@ and @PK_DBL@ require the {\em address} of the value in
question.)

\begin{code}
pprAmode (CVal reg_rel FloatRep)
  = hcat [ text "PK_FLT((W_*)", parens (ppr_amode (CAddr reg_rel)), rparen ]
pprAmode (CVal reg_rel DoubleRep)
  = hcat [ text "PK_DBL((W_*)", parens (ppr_amode (CAddr reg_rel)), rparen ]
pprAmode (CVal reg_rel Int64Rep)
  = hcat [ text "PK_Int64((W_*)", parens (ppr_amode (CAddr reg_rel)), rparen ]
pprAmode (CVal reg_rel Word64Rep)
  = hcat [ text "PK_Word64((W_*)", parens (ppr_amode (CAddr reg_rel)), rparen ]
\end{code}

Next comes the case where there is some other cast need, and the
no-cast case:

\begin{code}
pprAmode amode
  | mixedTypeLocn amode
  = parens (hcat [ pprPrimKind (getAmodeRep amode), ptext SLIT(")("),
		ppr_amode amode ])
  | otherwise	-- No cast needed
  = ppr_amode amode
\end{code}

When we have an indirection through a CIndex, we have to be careful to
get the type casts right.  

this amode:

	CVal (CIndex kind1 base offset) kind2

means (in C speak): 
	
	*(kind2 *)((kind1 *)base + offset)

That is, the indexing is done in units of kind1, but the resulting
amode has kind2.

\begin{code}
ppr_amode (CVal reg_rel@(CIndex _ _ _) kind)
  = case (pprRegRelative False{-no sign wanted-} reg_rel) of
	(pp_reg, Nothing)     -> panic "ppr_amode: CIndex"
	(pp_reg, Just offset) -> 
	   hcat [ char '*', parens (pprPrimKind kind <> char '*'),
		  parens (pp_reg <> char '+' <> offset) ]
\end{code}

Now the rest of the cases for ``workhorse'' @ppr_amode@:

\begin{code}
ppr_amode (CVal reg_rel _)
  = case (pprRegRelative False{-no sign wanted-} reg_rel) of
	(pp_reg, Nothing)     -> (<>)  (char '*') pp_reg
	(pp_reg, Just offset) -> hcat [ pp_reg, brackets offset ]

ppr_amode (CAddr reg_rel)
  = case (pprRegRelative True{-sign wanted-} reg_rel) of
	(pp_reg, Nothing)     -> pp_reg
	(pp_reg, Just offset) -> pp_reg <> offset

ppr_amode (CReg magic_id) = pprMagicId magic_id

ppr_amode (CTemp uniq kind) = char '_' <> pprUnique uniq <> char '_'

ppr_amode (CLbl lbl kind) = pprCLabelAddr lbl 

ppr_amode (CCharLike ch)
  = hcat [ptext SLIT("CHARLIKE_CLOSURE"), char '(', pprAmode ch, rparen ]
ppr_amode (CIntLike int)
  = hcat [ptext SLIT("INTLIKE_CLOSURE"), char '(', pprAmode int, rparen ]

ppr_amode (CLit lit) = pprBasicLit lit

ppr_amode (CJoinPoint _)
  = panic "ppr_amode: CJoinPoint"

ppr_amode (CMacroExpr pk macro as)
  = parens (ptext (cExprMacroText macro) <> 
	    parens (hcat (punctuate comma (map pprAmode as))))
\end{code}

\begin{code}
cExprMacroText ENTRY_CODE 		= SLIT("ENTRY_CODE")
cExprMacroText ARG_TAG    		= SLIT("ARG_TAG")
cExprMacroText GET_TAG    		= SLIT("GET_TAG")
cExprMacroText CCS_HDR		 	= SLIT("CCS_HDR")
cExprMacroText BYTE_ARR_CTS	 	= SLIT("BYTE_ARR_CTS")
cExprMacroText PTRS_ARR_CTS	 	= SLIT("PTRS_ARR_CTS")
cExprMacroText ForeignObj_CLOSURE_DATA  = SLIT("ForeignObj_CLOSURE_DATA")

cStmtMacroText UPD_CAF			= SLIT("UPD_CAF")
cStmtMacroText UPD_BH_UPDATABLE		= SLIT("UPD_BH_UPDATABLE")
cStmtMacroText UPD_BH_SINGLE_ENTRY	= SLIT("UPD_BH_SINGLE_ENTRY")
cStmtMacroText PUSH_UPD_FRAME		= SLIT("PUSH_UPD_FRAME")
cStmtMacroText SET_TAG			= SLIT("SET_TAG")
cStmtMacroText DATA_TO_TAGZH            = SLIT("dataToTagzh")
cStmtMacroText REGISTER_FOREIGN_EXPORT	= SLIT("REGISTER_FOREIGN_EXPORT")
cStmtMacroText REGISTER_IMPORT		= SLIT("REGISTER_IMPORT")
cStmtMacroText REGISTER_DIMPORT		= SLIT("REGISTER_DIMPORT")
cStmtMacroText GRAN_FETCH	    	= SLIT("GRAN_FETCH")
cStmtMacroText GRAN_RESCHEDULE   	= SLIT("GRAN_RESCHEDULE")
cStmtMacroText GRAN_FETCH_AND_RESCHEDULE= SLIT("GRAN_FETCH_AND_RESCHEDULE")
cStmtMacroText THREAD_CONTEXT_SWITCH   	= SLIT("THREAD_CONTEXT_SWITCH")
cStmtMacroText GRAN_YIELD   		= SLIT("GRAN_YIELD")

cCheckMacroText	HP_CHK_NP		= SLIT("HP_CHK_NP")
cCheckMacroText	STK_CHK_NP		= SLIT("STK_CHK_NP")
cCheckMacroText	HP_STK_CHK_NP		= SLIT("HP_STK_CHK_NP")
cCheckMacroText	HP_CHK_FUN		= SLIT("HP_CHK_FUN")
cCheckMacroText	STK_CHK_FUN		= SLIT("STK_CHK_FUN")
cCheckMacroText	HP_STK_CHK_FUN		= SLIT("HP_STK_CHK_FUN")
cCheckMacroText	HP_CHK_NOREGS		= SLIT("HP_CHK_NOREGS")
cCheckMacroText	HP_CHK_UNPT_R1		= SLIT("HP_CHK_UNPT_R1")
cCheckMacroText	HP_CHK_UNBX_R1		= SLIT("HP_CHK_UNBX_R1")
cCheckMacroText	HP_CHK_F1		= SLIT("HP_CHK_F1")
cCheckMacroText	HP_CHK_D1		= SLIT("HP_CHK_D1")
cCheckMacroText	HP_CHK_L1		= SLIT("HP_CHK_L1")
cCheckMacroText	HP_CHK_UNBX_TUPLE	= SLIT("HP_CHK_UNBX_TUPLE")
\end{code}

%************************************************************************
%*									*
\subsection[ppr-liveness-masks]{Liveness Masks}
%*									*
%************************************************************************

\begin{code}
bitmapAddrModes [] = [mkWordCLit 0]
bitmapAddrModes xs = map mkWordCLit xs
\end{code}

%************************************************************************
%*									*
\subsection[a2r-MagicIds]{Magic ids}
%*									*
%************************************************************************

@pprRegRelative@ returns a pair of the @Doc@ for the register
(some casting may be required), and a @Maybe Doc@ for the offset
(zero offset gives a @Nothing@).

\begin{code}
addPlusSign :: Bool -> SDoc -> SDoc
addPlusSign False p = p
addPlusSign True  p = (<>) (char '+') p

pprSignedInt :: Bool -> Int -> Maybe SDoc	-- Nothing => 0
pprSignedInt sign_wanted n
 = if n == 0 then Nothing else
   if n > 0  then Just (addPlusSign sign_wanted (int n))
   else 	  Just (int n)

pprRegRelative :: Bool		-- True <=> Print leading plus sign (if +ve)
	       -> RegRelative
	       -> (SDoc, Maybe SDoc)

pprRegRelative sign_wanted (SpRel off)
  = (pprMagicId Sp, pprSignedInt sign_wanted (I# off))

pprRegRelative sign_wanted r@(HpRel o)
  = let pp_Hp	 = pprMagicId Hp; off = I# o
    in
    if off == 0 then
	(pp_Hp, Nothing)
    else
	(pp_Hp, Just ((<>) (char '-') (int off)))

pprRegRelative sign_wanted (NodeRel o)
  = let pp_Node = pprMagicId node; off = I# o
    in
    if off == 0 then
	(pp_Node, Nothing)
    else
	(pp_Node, Just (addPlusSign sign_wanted (int off)))

pprRegRelative sign_wanted (CIndex base offset kind)
  = ( hcat [text "((", pprPrimKind kind, text " *)(", ppr_amode base, text "))"]
    , Just (hcat [if sign_wanted then char '+' else empty,
	    text "(I_)(", ppr_amode offset, ptext SLIT(")")])
    )
\end{code}

@pprMagicId@ just prints the register name.  @VanillaReg@ registers are
represented by a discriminated union (@StgUnion@), so we use the @PrimRep@
to select the union tag.

\begin{code}
pprMagicId :: MagicId -> SDoc

pprMagicId BaseReg	    	    = ptext SLIT("BaseReg")
pprMagicId (VanillaReg pk n)
				    = hcat [ pprVanillaReg n, char '.',
						  pprUnionTag pk ]
pprMagicId (FloatReg  n)            = ptext SLIT("F") <> int (I# n)
pprMagicId (DoubleReg n)	    = ptext SLIT("D") <> int (I# n)
pprMagicId (LongReg _ n)	    = ptext SLIT("L") <> int (I# n)
pprMagicId Sp			    = ptext SLIT("Sp")
pprMagicId SpLim		    = ptext SLIT("SpLim")
pprMagicId Hp			    = ptext SLIT("Hp")
pprMagicId HpLim		    = ptext SLIT("HpLim")
pprMagicId CurCostCentre	    = ptext SLIT("CCCS")
pprMagicId VoidReg		    = ptext SLIT("VoidReg")

pprVanillaReg :: Int# -> SDoc
pprVanillaReg n = char 'R' <> int (I# n)

pprUnionTag :: PrimRep -> SDoc

pprUnionTag PtrRep		= char 'p'
pprUnionTag CodePtrRep	    	= ptext SLIT("fp")
pprUnionTag DataPtrRep	    	= char 'd'
pprUnionTag RetRep 	    	= char 'p'
pprUnionTag CostCentreRep	= panic "pprUnionTag:CostCentre?"

pprUnionTag CharRep		= char 'c'
pprUnionTag Int8Rep		= ptext SLIT("i8")
pprUnionTag IntRep		= char 'i'
pprUnionTag WordRep		= char 'w'
pprUnionTag Int32Rep		= char 'i'
pprUnionTag Word32Rep		= char 'w'
pprUnionTag AddrRep		= char 'a'
pprUnionTag FloatRep		= char 'f'
pprUnionTag DoubleRep		= panic "pprUnionTag:Double?"

pprUnionTag StablePtrRep	= char 'p'

pprUnionTag _                   = panic "pprUnionTag:Odd kind"
\end{code}


Find and print local and external declarations for a list of
Abstract~C statements.
\begin{code}
pprTempAndExternDecls :: AbstractC -> (SDoc{-temps-}, SDoc{-externs-})
pprTempAndExternDecls AbsCNop = (empty, empty)

pprTempAndExternDecls (AbsCStmts stmt1 stmt2)
  = initTE (ppr_decls_AbsC stmt1	`thenTE` \ (t_p1, e_p1) ->
	    ppr_decls_AbsC stmt2	`thenTE` \ (t_p2, e_p2) ->
	    case (catMaybes [t_p1, t_p2])	 of { real_temps ->
	    case (catMaybes [e_p1, e_p2])	 of { real_exts ->
	    returnTE (vcat real_temps, vcat real_exts) }}
	   )

pprTempAndExternDecls other_stmt
  = initTE (ppr_decls_AbsC other_stmt `thenTE` \ (maybe_t, maybe_e) ->
	    returnTE (
		case maybe_t of
		  Nothing -> empty
		  Just pp -> pp,

		case maybe_e of
		  Nothing -> empty
		  Just pp -> pp )
	   )

pprBasicLit :: Literal -> SDoc
pprPrimKind :: PrimRep -> SDoc

pprBasicLit  lit = ppr lit
pprPrimKind  k   = ppr k
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

type TEenv = (UniqSet Unique, CLabelSet)

type TeM result =  TEenv -> (TEenv, result)

initTE :: TeM a -> a
initTE sa
  = case sa (emptyUniqSet, emptyCLabelSet) of { (_, result) ->
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
  = if (uniq `elementOfUniqSet` seen_uniqs)
    then (env, True)
    else ((addOneToUniqSet seen_uniqs uniq,
	  seen_labels),
	  False)

labelSeenTE :: CLabel -> TeM Bool
labelSeenTE lbl env@(seen_uniqs, seen_labels)
  = if (lbl `elementOfCLabelSet` seen_labels)
    then (env, True)
    else ((seen_uniqs,
	  addToCLabelSet seen_labels lbl),
	  False)
\end{code}

\begin{code}
pprTempDecl :: Unique -> PrimRep -> SDoc
pprTempDecl uniq kind
  = hcat [ pprPrimKind kind, space, char '_', pprUnique uniq, ptext SLIT("_;") ]

pprExternDecl :: Bool -> CLabel -> SDoc
pprExternDecl in_srt clabel
  | not (needsCDecl clabel) = empty -- do not print anything for "known external" things
  | otherwise		    = 
	hcat [ ppLocalnessMacro (not in_srt) clabel, 
	       lparen, dyn_wrapper (pprCLabel clabel), pp_paren_semi ]
 where
  dyn_wrapper d
    | in_srt && labelDynamic clabel = text "DLL_IMPORT_DATA_VAR" <> parens d
    | otherwise			    = d

\end{code}

\begin{code}
ppr_decls_AbsC :: AbstractC -> TeM (Maybe SDoc{-temps-}, Maybe SDoc{-externs-})

ppr_decls_AbsC AbsCNop		= returnTE (Nothing, Nothing)

ppr_decls_AbsC (AbsCStmts stmts_1 stmts_2)
  = ppr_decls_AbsC stmts_1  `thenTE` \ p1 ->
    ppr_decls_AbsC stmts_2  `thenTE` \ p2 ->
    returnTE (maybe_vcat [p1, p2])

ppr_decls_AbsC (CSplitMarker) = returnTE (Nothing, Nothing)

ppr_decls_AbsC (CAssign dest source)
  = ppr_decls_Amode dest    `thenTE` \ p1 ->
    ppr_decls_Amode source  `thenTE` \ p2 ->
    returnTE (maybe_vcat [p1, p2])

ppr_decls_AbsC (CJump target) = ppr_decls_Amode target

ppr_decls_AbsC (CFallThrough target) = ppr_decls_Amode target

ppr_decls_AbsC (CReturn target _) = ppr_decls_Amode target

ppr_decls_AbsC (CSwitch discrim alts deflt)
  = ppr_decls_Amode discrim	`thenTE` \ pdisc ->
    mapTE ppr_alt_stuff alts	`thenTE` \ palts  ->
    ppr_decls_AbsC deflt	`thenTE` \ pdeflt ->
    returnTE (maybe_vcat (pdisc:pdeflt:palts))
  where
    ppr_alt_stuff (_, absC) = ppr_decls_AbsC absC

ppr_decls_AbsC (CCodeBlock lbl absC)
  = ppr_decls_AbsC absC

ppr_decls_AbsC (CInitHdr cl_info reg_rel cost_centre _)
	-- ToDo: strictly speaking, should chk "cost_centre" amode
  = labelSeenTE info_lbl     `thenTE` \  label_seen ->
    returnTE (Nothing,
	      if label_seen then
		  Nothing
	      else
		  Just (pprExternDecl False{-not in an SRT decl-} info_lbl))
  where
    info_lbl = infoTableLabelFromCI cl_info

ppr_decls_AbsC (CMachOpStmt res	_ args _) = ppr_decls_Amodes (res : args)
ppr_decls_AbsC (COpStmt	results	_ args _) = ppr_decls_Amodes (results ++ args)

ppr_decls_AbsC (CSimultaneous abc)  	  = ppr_decls_AbsC abc

ppr_decls_AbsC (CSequential abcs) 
  = mapTE ppr_decls_AbsC abcs	`thenTE` \ t_and_e_s ->
    returnTE (maybe_vcat t_and_e_s)

ppr_decls_AbsC (CCheck  	    _ amodes code) = 
     ppr_decls_Amodes amodes `thenTE` \p1 ->
     ppr_decls_AbsC code     `thenTE` \p2 ->
     returnTE (maybe_vcat [p1,p2])

ppr_decls_AbsC (CMacroStmt	    _ amodes)	= ppr_decls_Amodes amodes

ppr_decls_AbsC (CCallProfCtrMacro   _ amodes)	= ppr_decls_Amodes [] -- *****!!!
  -- you get some nasty re-decls of stdio.h if you compile
  -- the prelude while looking inside those amodes;
  -- no real reason to, anyway.
ppr_decls_AbsC (CCallProfCCMacro    _ amodes)	= ppr_decls_Amodes amodes

ppr_decls_AbsC (CStaticClosure _ closure_info cost_centre amodes)
	-- ToDo: strictly speaking, should chk "cost_centre" amode
  = ppr_decls_Amodes amodes

ppr_decls_AbsC (CClosureInfoAndCode cl_info entry)
  = ppr_decls_Amodes [entry_lbl]	    	`thenTE` \ p1 ->
    ppr_decls_AbsC entry			`thenTE` \ p2 ->
    returnTE (maybe_vcat [p1, p2])
  where
    entry_lbl = CLbl (entryLabelFromCI cl_info) CodePtrRep

ppr_decls_AbsC (CSRT _ closure_lbls)
  = mapTE labelSeenTE closure_lbls		`thenTE` \ seen ->
    returnTE (Nothing, 
	      if and seen then Nothing
		else Just (vcat [ pprExternDecl True{-in SRT decl-} l
				| (l,False) <- zip closure_lbls seen ]))

ppr_decls_AbsC (CRetDirect     _ code _ _)   = ppr_decls_AbsC code
ppr_decls_AbsC (CRetVector _ amodes _ _)     = ppr_decls_Amodes amodes
ppr_decls_AbsC (CModuleInitBlock _ _ code)   = ppr_decls_AbsC code

ppr_decls_AbsC (_) = returnTE (Nothing, Nothing)
\end{code}

\begin{code}
ppr_decls_Amode :: CAddrMode -> TeM (Maybe SDoc, Maybe SDoc)
ppr_decls_Amode (CVal  (CIndex base offset _) _) = ppr_decls_Amodes [base,offset]
ppr_decls_Amode (CAddr (CIndex base offset _))   = ppr_decls_Amodes [base,offset]
ppr_decls_Amode (CVal _ _)	= returnTE (Nothing, Nothing)
ppr_decls_Amode (CAddr _)	= returnTE (Nothing, Nothing)
ppr_decls_Amode (CReg _)	= returnTE (Nothing, Nothing)
ppr_decls_Amode (CLit _)	= returnTE (Nothing, Nothing)

-- CIntLike must be a literal -- no decls
ppr_decls_Amode (CIntLike int)	= returnTE (Nothing, Nothing)

-- CCharLike too
ppr_decls_Amode (CCharLike char) = returnTE (Nothing, Nothing)

-- now, the only place where we actually print temps/externs...
ppr_decls_Amode (CTemp uniq kind)
  = case kind of
      VoidRep -> returnTE (Nothing, Nothing)
      other ->
	tempSeenTE uniq `thenTE` \ temp_seen ->
	returnTE
	  (if temp_seen then Nothing else Just (pprTempDecl uniq kind), Nothing)

ppr_decls_Amode (CLbl lbl VoidRep)
  = returnTE (Nothing, Nothing)

ppr_decls_Amode (CLbl lbl kind)
  = labelSeenTE lbl `thenTE` \ label_seen ->
    returnTE (Nothing,
	      if label_seen then Nothing else Just (pprExternDecl False{-not in an SRT decl-} lbl))

ppr_decls_Amode (CMacroExpr _ _ amodes)
  = ppr_decls_Amodes amodes

ppr_decls_Amode other = returnTE (Nothing, Nothing)


maybe_vcat :: [(Maybe SDoc, Maybe SDoc)] -> (Maybe SDoc, Maybe SDoc)
maybe_vcat ps
  = case (unzip ps)	of { (ts, es) ->
    case (catMaybes ts)	of { real_ts  ->
    case (catMaybes es)	of { real_es  ->
    (if (null real_ts) then Nothing else Just (vcat real_ts),
     if (null real_es) then Nothing else Just (vcat real_es))
    } } }
\end{code}

\begin{code}
ppr_decls_Amodes :: [CAddrMode] -> TeM (Maybe SDoc, Maybe SDoc)
ppr_decls_Amodes amodes
  = mapTE ppr_decls_Amode amodes `thenTE` \ ps ->
    returnTE ( maybe_vcat ps )
\end{code}

Print out a C Label where you want the *address* of the label, not the
object it refers to.  The distinction is important when the label may
refer to a C structure (info tables and closures, for instance).

When just generating a declaration for the label, use pprCLabel.

\begin{code}
pprCLabelAddr :: CLabel -> SDoc
pprCLabelAddr clabel =
  case labelType clabel of
     InfoTblType    -> addr_of_label
     RetInfoTblType -> addr_of_label
     ClosureType    -> addr_of_label
     VecTblType     -> addr_of_label
     DataType	    -> addr_of_label

     _              -> pp_label
  where
    addr_of_label = ptext SLIT("(P_)&") <> pp_label
    pp_label = pprCLabel clabel
\end{code}

-----------------------------------------------------------------------------
Initialising static objects with floating-point numbers.  We can't
just emit the floating point number, because C will cast it to an int
by rounding it.  We want the actual bit-representation of the float.

This is a hack to turn the floating point numbers into ints that we
can safely initialise to static locations.

\begin{code}
big_doubles = (getPrimRepSize DoubleRep) /= 1

#if __GLASGOW_HASKELL__ >= 504
newFloatArray :: (Int,Int) -> ST s (STUArray s Int Float)
newFloatArray = newArray_

newDoubleArray :: (Int,Int) -> ST s (STUArray s Int Double)
newDoubleArray = newArray_

castFloatToIntArray :: STUArray s Int Float -> ST s (STUArray s Int Int)
castFloatToIntArray = castSTUArray

castDoubleToIntArray :: STUArray s Int Double -> ST s (STUArray s Int Int)
castDoubleToIntArray = castSTUArray

writeFloatArray :: STUArray s Int Float -> Int -> Float -> ST s ()
writeFloatArray = writeArray

writeDoubleArray :: STUArray s Int Double -> Int -> Double -> ST s ()
writeDoubleArray = writeArray

readIntArray :: STUArray s Int Int -> Int -> ST s Int
readIntArray = readArray

#else

castFloatToIntArray :: MutableByteArray s t -> ST s (MutableByteArray s t)
castFloatToIntArray = return

castDoubleToIntArray :: MutableByteArray s t -> ST s (MutableByteArray s t)
castDoubleToIntArray = return

#endif

-- floats are always 1 word
floatToWord :: CAddrMode -> CAddrMode
floatToWord (CLit (MachFloat r))
  = runST (do
	arr <- newFloatArray ((0::Int),0)
	writeFloatArray arr 0 (fromRational r)
	arr' <- castFloatToIntArray arr
	i <- readIntArray arr' 0
	return (CLit (MachInt (toInteger i)))
    )

doubleToWords :: CAddrMode -> [CAddrMode]
doubleToWords (CLit (MachDouble r))
  | big_doubles				-- doubles are 2 words
  = runST (do
	arr <- newDoubleArray ((0::Int),1)
	writeDoubleArray arr 0 (fromRational r)
	arr' <- castDoubleToIntArray arr
	i1 <- readIntArray arr' 0
	i2 <- readIntArray arr' 1
	return [ CLit (MachInt (toInteger i1))
	       , CLit (MachInt (toInteger i2))
	       ]
    )
  | otherwise				-- doubles are 1 word
  = runST (do
	arr <- newDoubleArray ((0::Int),0)
	writeDoubleArray arr 0 (fromRational r)
	arr' <- castDoubleToIntArray arr
	i <- readIntArray arr' 0
	return [ CLit (MachInt (toInteger i)) ]
    )
\end{code}
