%
% (c) The AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module StixMacro ( macroCode, checkCode ) where

#include "HsVersions.h"
#include "nativeGen/NCG.h"

import {-# SOURCE #-} StixPrim ( amodeToStix )

import MachMisc
import MachRegs
import AbsCSyn		( CStmtMacro(..), MagicId(..), CAddrMode, tagreg,
			  CCheckMacro(..) )
import Constants	( uF_RET, uF_SU, uF_UPDATEE, uF_SIZE, sEQ_FRAME_SIZE )
import CallConv		( cCallConv )
import PrimOp		( PrimOp(..) )
import PrimRep		( PrimRep(..) )
import Stix
import UniqSupply	( returnUs, thenUs, UniqSM )
import Outputable
import CLabel		( mkBlackHoleInfoTableLabel, mkIndStaticInfoLabel,
			  mkIndInfoLabel, mkUpdInfoLabel, mkSeqInfoLabel,
			  mkRtsGCEntryLabel, mkStgUpdatePAPLabel )
\end{code}

The @ARGS_CHK_A{_LOAD_NODE}@ macros check for sufficient arguments on
the A stack, and perform a tail call to @UpdatePAP@ if the arguments are
not there.  The @_LOAD_NODE@ version also loads R1 with an appropriate
closure address.

\begin{code}
macroCode
    :: CStmtMacro   	    -- statement macro
    -> [CAddrMode]  	    -- args
    -> UniqSM StixTreeList
\end{code}

-----------------------------------------------------------------------------
Argument satisfaction checks.

\begin{code}
macroCode ARGS_CHK_LOAD_NODE args
  = getUniqLabelNCG					`thenUs` \ ulbl ->
    let
	  [words, lbl] = map amodeToStix args
	  temp = StIndex PtrRep stgSp words
	  test = StPrim AddrGeOp [stgSu, temp]
	  cjmp = StCondJump ulbl test
	  assign = StAssign PtrRep stgNode lbl
	  join = StLabel ulbl
    in
    returnUs (\xs -> cjmp : assign : updatePAP : join : xs)

macroCode ARGS_CHK [words]
  = getUniqLabelNCG					`thenUs` \ ulbl ->
    let temp = StIndex PtrRep stgSp (amodeToStix words)
	test = StPrim AddrGeOp [stgSu, temp]
	cjmp = StCondJump ulbl test
	join = StLabel ulbl
    in
    returnUs (\xs -> cjmp : updatePAP : join : xs)
\end{code}

-----------------------------------------------------------------------------
Updating a CAF

@UPD_CAF@ involves changing the info pointer of the closure, and
adding an indirection.

\begin{code}
macroCode UPD_CAF args
  = let
	[cafptr,bhptr] = map amodeToStix args
    	w0 = StInd PtrRep cafptr
	w1 = StInd PtrRep (StIndex PtrRep cafptr fixedHS)
	blocking_queue = StInd PtrRep (StIndex PtrRep bhptr fixedHS)
	a1 = StAssign PtrRep w0 ind_static_info
	a2 = StAssign PtrRep w1 bhptr
	a3 = StCall SLIT("newCAF") cCallConv VoidRep [cafptr]
    in
    returnUs (\xs -> a1 : a2 : a3 : xs)
\end{code}

-----------------------------------------------------------------------------
Blackholing

We do lazy blackholing: no need to overwrite thunks with blackholes
the minute they're entered, as long as we do it before a context
switch or garbage collection, that's ok.

Don't blackhole single entry closures, for the following reasons:
	
	- if the compiler has decided that they won't be entered again,
	  that probably means that nothing has a pointer to it
	  (not necessarily true, but...)

	- no need to blackhole for concurrency reasons, because nothing
	  can block on the result of this computation.

\begin{code}
macroCode UPD_BH_UPDATABLE args = returnUs id

macroCode UPD_BH_SINGLE_ENTRY args = returnUs id
{-
  = let
    	update = StAssign PtrRep (StInd PtrRep (amodeToStix arg)) bh_info
    in
    returnUs (\xs -> update : xs)
-}
\end{code}

-----------------------------------------------------------------------------
Update frames

Push a four word update frame on the stack and slide the Su registers
to the current Sp location.

\begin{code}
macroCode PUSH_UPD_FRAME args
  = let
	[bhptr, _{-0-}] = map amodeToStix args
    	frame n = StInd PtrRep
	    (StIndex PtrRep stgSp (StInt (toInteger (n-uF_SIZE))))

        -- HWL: these values are *wrong* in a GranSim setup; ToDo: fix
	a1 = StAssign PtrRep (frame uF_RET)     upd_frame_info
	a3 = StAssign PtrRep (frame uF_SU)      stgSu
	a4 = StAssign PtrRep (frame uF_UPDATEE) bhptr

	updSu = StAssign PtrRep stgSu
		(StIndex PtrRep stgSp (StInt (toInteger (-uF_SIZE))))
    in
    returnUs (\xs -> a1 : a3 : a4 : updSu : xs)


macroCode PUSH_SEQ_FRAME args
   = let [arg_frame] = map amodeToStix args
         frame n = StInd PtrRep
                     (StIndex PtrRep arg_frame (StInt (toInteger n)))
         a1 = StAssign PtrRep (frame 0) seq_frame_info
         a2 = StAssign PtrRep (frame 1) stgSu
         updSu = StAssign PtrRep stgSu arg_frame 
     in
     returnUs (\xs -> a1 : a2 : updSu : xs)


macroCode UPDATE_SU_FROM_UPD_FRAME args
   = let [arg_frame] = map amodeToStix args
         frame n = StInd PtrRep
                      (StIndex PtrRep arg_frame (StInt (toInteger n)))
         updSu
            = StAssign PtrRep stgSu (frame uF_SU)
     in
     returnUs (\xs -> updSu : xs)
\end{code}

-----------------------------------------------------------------------------
Setting the tag register

This one only applies if we have a machine register devoted to TagReg.

\begin{code}
macroCode SET_TAG [tag]
  = let set_tag = StAssign IntRep stgTagReg (amodeToStix tag)
    in
    case stgReg tagreg of
      Always _ -> returnUs id
      Save   _ -> returnUs (\ xs -> set_tag : xs)
\end{code}

-----------------------------------------------------------------------------

\begin{code}
macroCode REGISTER_IMPORT [arg]
   = returnUs (
	\xs -> StAssign WordRep (StInd WordRep stgSp) (amodeToStix arg)
	     : StAssign PtrRep  stgSp (StPrim IntAddOp [stgSp, StInt 4])
	     : xs
     )

macroCode REGISTER_FOREIGN_EXPORT [arg]
   = returnUs (
	\xs -> StCall SLIT("getStablePtr") cCallConv VoidRep [amodeToStix arg]
	     : xs
     )

macroCode other args
   = case other of
        SET_TAG -> error "foobarxyzzy8"
	_       -> error "StixMacro.macroCode: unknown macro/args"
\end{code}


Do the business for a @HEAP_CHK@, having converted the args to Trees
of StixOp.

-----------------------------------------------------------------------------
Let's make sure that these CAFs are lifted out, shall we?

\begin{code}
-- Some common labels

bh_info, ind_static_info, ind_info :: StixTree

bh_info   	= StCLbl mkBlackHoleInfoTableLabel
ind_static_info	= StCLbl mkIndStaticInfoLabel
ind_info  	= StCLbl mkIndInfoLabel
upd_frame_info	= StCLbl mkUpdInfoLabel
seq_frame_info	= StCLbl mkSeqInfoLabel
stg_update_PAP  = StCLbl mkStgUpdatePAPLabel
-- Some common call trees

updatePAP, stackOverflow :: StixTree

updatePAP     = StJump stg_update_PAP
stackOverflow = StCall SLIT("StackOverflow") cCallConv VoidRep []
\end{code}

-----------------------------------------------------------------------------
Heap/Stack checks

\begin{code}
checkCode :: CCheckMacro -> [CAddrMode] -> StixTreeList -> UniqSM StixTreeList
checkCode macro args assts
  = getUniqLabelNCG		`thenUs` \ ulbl_fail ->
    getUniqLabelNCG		`thenUs` \ ulbl_pass ->

    let args_stix = map amodeToStix args
	newHp wds = StIndex PtrRep stgHp wds
	assign_hp wds = StAssign PtrRep stgHp (newHp wds)
	test_hp = StPrim AddrLeOp [stgHp, stgHpLim]
	cjmp_hp = StCondJump ulbl_pass test_hp

	newSp wds = StIndex PtrRep stgSp (StPrim IntNegOp [wds])
	test_sp_pass wds = StPrim AddrGeOp [newSp wds, stgSpLim]
	test_sp_fail wds = StPrim AddrLtOp [newSp wds, stgSpLim]
	cjmp_sp_pass wds = StCondJump ulbl_pass (test_sp_pass wds)
	cjmp_sp_fail wds = StCondJump ulbl_fail (test_sp_fail wds)

	assign_ret r ret = StAssign CodePtrRep r ret

	fail = StLabel ulbl_fail
	join = StLabel ulbl_pass

        -- see includes/StgMacros.h for explaination of these magic consts
        aLL_NON_PTRS
           = IF_ARCH_alpha(16383,65535)

        assign_liveness ptr_regs 
           = StAssign WordRep stgR9
                      (StPrim XorOp [StInt aLL_NON_PTRS, ptr_regs])
        assign_reentry reentry 
           = StAssign WordRep stgR10 reentry
    in	

    returnUs (
    case macro of
	HP_CHK_NP      -> 
		let [words,ptrs] = args_stix
		in  (\xs -> assign_hp words : cjmp_hp : 
			    assts (gc_enter ptrs : join : xs))

	HP_CHK_SEQ_NP  -> 
		let [words,ptrs] = args_stix
		in  (\xs -> assign_hp words : cjmp_hp : 
			    assts (gc_seq ptrs : join : xs))

	STK_CHK_NP     -> 
		let [words,ptrs] = args_stix
		in  (\xs -> cjmp_sp_pass words :
			    assts (gc_enter ptrs : join : xs))

	HP_STK_CHK_NP  -> 
		let [sp_words,hp_words,ptrs] = args_stix
		in  (\xs -> cjmp_sp_fail sp_words : 
			    assign_hp hp_words : cjmp_hp :
			    fail :
			    assts (gc_enter ptrs : join : xs))

	HP_CHK	       -> 
		let [words,ret,r,ptrs] = args_stix
		in  (\xs -> assign_hp words : cjmp_hp :
			    assts (assign_ret r ret : gc_chk ptrs : join : xs))

	STK_CHK	       -> 
		let [words,ret,r,ptrs] = args_stix
		in  (\xs -> cjmp_sp_pass words :
			    assts (assign_ret r ret : gc_chk ptrs : join : xs))

	HP_STK_CHK     -> 
		let [sp_words,hp_words,ret,r,ptrs] = args_stix
		in  (\xs -> cjmp_sp_fail sp_words :
			    assign_hp hp_words : cjmp_hp :
			    fail :
			    assts (assign_ret r ret : gc_chk ptrs : join : xs))

	HP_CHK_NOREGS  -> 
		let [words] = args_stix
		in  (\xs -> assign_hp words : cjmp_hp : 
			    assts (gc_noregs : join : xs))

	HP_CHK_UNPT_R1 -> 
		let [words] = args_stix
		in  (\xs -> assign_hp words : cjmp_hp : 
			    assts (gc_unpt_r1 : join : xs))

	HP_CHK_UNBX_R1 -> 
		let [words] = args_stix
		in  (\xs -> assign_hp words : cjmp_hp : 
			    assts (gc_unbx_r1 : join : xs))

	HP_CHK_F1      -> 
		let [words] = args_stix
		in  (\xs -> assign_hp words : cjmp_hp : 
			    assts (gc_f1 : join : xs))

	HP_CHK_D1      -> 
		let [words] = args_stix
		in  (\xs -> assign_hp words : cjmp_hp : 
			    assts (gc_d1 : join : xs))

	HP_CHK_UT_ALT  -> 
                let [words,ptrs,nonptrs,r,ret] = args_stix
                in (\xs -> assign_hp words : cjmp_hp :
                           assts (assign_ret r ret : gc_ut ptrs nonptrs 
                                  : join : xs))

	HP_CHK_GEN     -> 
                let [words,liveness,reentry] = args_stix
                in (\xs -> assign_hp words : cjmp_hp :
                           assts (assign_liveness liveness :
                                  assign_reentry reentry :
                                  gc_gen : join : xs))
    )
	
-- Various canned heap-check routines

mkStJump_to_GCentry :: String -> StixTree
mkStJump_to_GCentry gcname
--   | opt_Static
   = StJump (StCLbl (mkRtsGCEntryLabel gcname))
--   | otherwise -- it's in a different DLL
--   = StJump (StInd PtrRep (StLitLbl True sdoc))

gc_chk (StInt n)   = mkStJump_to_GCentry ("stg_chk_" ++ show n)
gc_enter (StInt n) = mkStJump_to_GCentry ("stg_gc_enter_" ++ show n)
gc_seq (StInt n)   = mkStJump_to_GCentry ("stg_gc_seq_" ++ show n)
gc_noregs          = mkStJump_to_GCentry "stg_gc_noregs"
gc_unpt_r1         = mkStJump_to_GCentry "stg_gc_unpt_r1"
gc_unbx_r1         = mkStJump_to_GCentry "stg_gc_unbx_r1"
gc_f1              = mkStJump_to_GCentry "stg_gc_f1"
gc_d1              = mkStJump_to_GCentry "stg_gc_d1"
gc_gen             = mkStJump_to_GCentry "stg_gen_chk"
gc_ut (StInt p) (StInt np)
                   = mkStJump_to_GCentry ("stg_gc_ut_" ++ show p 
                                          ++ "_" ++ show np)
\end{code}
