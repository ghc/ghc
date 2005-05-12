-----------------------------------------------------------------------------
--
-- Code generation for foreign calls.
--
-- (c) The University of Glasgow 2004
--
-----------------------------------------------------------------------------

module CgForeignCall (
  emitForeignCall,
  cgForeignCall,
  shimForeignCallArg,
  emitSaveThreadState, -- will be needed by the Cmm parser
  emitLoadThreadState, -- ditto
  emitCloseNursery,
  emitOpenNursery,
 ) where

#include "HsVersions.h"

import StgSyn		( StgLiveVars, StgArg, stgArgType )
import CgProf		( curCCS, curCCSAddr )
import CgBindery	( getVolatileRegs, getArgAmodes )
import CgMonad
import CgUtils		( cmmOffsetW, cmmOffsetB, cmmLoadIndexW, newTemp )
import Type		( tyConAppTyCon, repType )
import TysPrim
import CLabel		( mkForeignLabel, mkRtsCodeLabel )
import Cmm
import CmmUtils
import MachOp
import SMRep
import ForeignCall
import Constants
import StaticFlags	( opt_SccProfilingOn, opt_SMP )
import Outputable

import Monad		( when )

-- -----------------------------------------------------------------------------
-- Code generation for Foreign Calls

cgForeignCall
	:: [(CmmReg,MachHint)]	-- where to put the results
	-> ForeignCall		-- the op
	-> [StgArg]		-- arguments
	-> StgLiveVars	-- live vars, in case we need to save them
	-> Code
cgForeignCall results fcall stg_args live
  = do 
  reps_n_amodes <- getArgAmodes stg_args
  let
	-- Get the *non-void* args, and jiggle them with shimForeignCall
	arg_exprs = [ shimForeignCallArg stg_arg expr 
	  	    | (stg_arg, (rep,expr)) <- stg_args `zip` reps_n_amodes, 
	               nonVoidArg rep]

	arg_hints = zip arg_exprs (map (typeHint.stgArgType) stg_args)
  -- in
  emitForeignCall results fcall arg_hints live


emitForeignCall
	:: [(CmmReg,MachHint)]	-- where to put the results
	-> ForeignCall		-- the op
	-> [(CmmExpr,MachHint)] -- arguments
	-> StgLiveVars	-- live vars, in case we need to save them
	-> Code

emitForeignCall results (CCall (CCallSpec target cconv safety)) args live
  | not (playSafe safety) 
  = do 
    vols <- getVolatileRegs live
    stmtC (the_call vols)
  
  | otherwise -- it's a safe foreign call
  = do
    vols <- getVolatileRegs live
    id <- newTemp wordRep
    emitSaveThreadState
    stmtC (CmmCall (CmmForeignCall suspendThread CCallConv) 
			[(id,NoHint)]
			[ (CmmReg (CmmGlobal BaseReg), PtrHint) ] 
			(Just vols)
			)
    stmtC (the_call vols)
    stmtC (CmmCall (CmmForeignCall resumeThread CCallConv) 
			(if opt_SMP then [(CmmGlobal BaseReg, PtrHint)] else [])
				-- Assign the result to BaseReg: we might now have
				-- a different Capability!  Small optimisation:
				-- only do this in SMP mode, where there are >1
				-- Capabilities.
			[ (CmmReg id, NoHint) ]
			(Just vols)
			)
    emitLoadThreadState

  where
      (call_args, cmm_target)
	= case target of
	   StaticTarget lbl -> (args, CmmLit (CmmLabel 
					(mkForeignLabel lbl call_size False)))
	   DynamicTarget    ->  case args of (fn,_):rest -> (rest, fn)

      the_call vols = CmmCall (CmmForeignCall cmm_target cconv) 
			  results call_args (Just vols)

	-- in the stdcall calling convention, the symbol needs @size appended
	-- to it, where size is the total number of bytes of arguments.  We
	-- attach this info to the CLabel here, and the CLabel pretty printer
	-- will generate the suffix when the label is printed.
      call_size
	| StdCallConv <- cconv = Just (sum (map (arg_size.cmmExprRep.fst) args))
	| otherwise            = Nothing

	-- ToDo: this might not be correct for 64-bit API
      arg_size rep = max (machRepByteWidth rep) wORD_SIZE


emitForeignCall results (DNCall _) args live
  = panic "emitForeignCall: DNCall"

suspendThread = CmmLit (CmmLabel (mkRtsCodeLabel SLIT("suspendThread")))
resumeThread  = CmmLit (CmmLabel (mkRtsCodeLabel SLIT("resumeThread")))

-- -----------------------------------------------------------------------------
-- Save/restore the thread state in the TSO

-- This stuff can't be done in suspendThread/resumeThread, because it
-- refers to global registers which aren't available in the C world.

emitSaveThreadState = do
  -- CurrentTSO->sp = Sp;
  stmtC $ CmmStore (cmmOffset stgCurrentTSO tso_SP) stgSp
  emitCloseNursery
  -- and save the current cost centre stack in the TSO when profiling:
  when opt_SccProfilingOn $
	stmtC (CmmStore (cmmOffset stgCurrentTSO tso_CCCS) curCCS)

   -- CurrentNursery->free = Hp+1;
emitCloseNursery = stmtC $ CmmStore nursery_bdescr_free (cmmOffsetW stgHp 1)

emitLoadThreadState = do
  tso <- newTemp wordRep
  stmtsC [
	-- tso = CurrentTSO;
  	CmmAssign tso stgCurrentTSO,
	-- Sp = tso->sp;
	CmmAssign sp (CmmLoad (cmmOffset (CmmReg tso) tso_SP)
	                      wordRep),
	-- SpLim = tso->stack + RESERVED_STACK_WORDS;
	CmmAssign spLim (cmmOffsetW (cmmOffset (CmmReg tso) tso_STACK)
			            rESERVED_STACK_WORDS)
    ]
  emitOpenNursery
  -- and load the current cost centre stack from the TSO when profiling:
  when opt_SccProfilingOn $
	stmtC (CmmStore curCCSAddr 
		(CmmLoad (cmmOffset (CmmReg tso) tso_CCCS) wordRep))

emitOpenNursery = stmtsC [
        -- Hp = CurrentNursery->free - 1;
	CmmAssign hp (cmmOffsetW (CmmLoad nursery_bdescr_free wordRep) (-1)),

        -- HpLim = CurrentNursery->start + 
	--		CurrentNursery->blocks*BLOCK_SIZE_W - 1;
	CmmAssign hpLim
	    (cmmOffsetExpr
		(CmmLoad nursery_bdescr_start wordRep)
		(cmmOffset
		  (CmmMachOp mo_wordMul [
		    CmmMachOp (MO_S_Conv I32 wordRep)
		      [CmmLoad nursery_bdescr_blocks I32],
		    CmmLit (mkIntCLit bLOCK_SIZE)
		   ])
		  (-1)
		)
	    )
   ]


nursery_bdescr_free   = cmmOffset stgCurrentNursery oFFSET_bdescr_free
nursery_bdescr_start  = cmmOffset stgCurrentNursery oFFSET_bdescr_start
nursery_bdescr_blocks = cmmOffset stgCurrentNursery oFFSET_bdescr_blocks

tso_SP    = tsoFieldB     oFFSET_StgTSO_sp
tso_STACK = tsoFieldB     oFFSET_StgTSO_stack
tso_CCCS  = tsoProfFieldB oFFSET_StgTSO_CCCS

-- The TSO struct has a variable header, and an optional StgTSOProfInfo in
-- the middle.  The fields we're interested in are after the StgTSOProfInfo.
tsoFieldB :: ByteOff -> ByteOff
tsoFieldB off
  | opt_SccProfilingOn = off + sIZEOF_StgTSOProfInfo + fixedHdrSize * wORD_SIZE
  | otherwise          = off + fixedHdrSize * wORD_SIZE

tsoProfFieldB :: ByteOff -> ByteOff
tsoProfFieldB off = off + fixedHdrSize * wORD_SIZE

stgSp		  = CmmReg sp
stgHp		  = CmmReg hp
stgCurrentTSO	  = CmmReg currentTSO
stgCurrentNursery = CmmReg currentNursery

sp		  = CmmGlobal Sp
spLim		  = CmmGlobal SpLim
hp		  = CmmGlobal Hp
hpLim		  = CmmGlobal HpLim
currentTSO	  = CmmGlobal CurrentTSO
currentNursery 	  = CmmGlobal CurrentNursery

-- -----------------------------------------------------------------------------
-- For certain types passed to foreign calls, we adjust the actual
-- value passed to the call.  Two main cases: for ForeignObj# we pass
-- the pointer inside the ForeignObj# closure, and for ByteArray#/Array# we
-- pass the address of the actual array, not the address of the heap object.

shimForeignCallArg :: StgArg -> CmmExpr -> CmmExpr
shimForeignCallArg arg expr
  | tycon == foreignObjPrimTyCon
	= cmmLoadIndexW expr fixedHdrSize

  | tycon == arrayPrimTyCon || tycon == mutableArrayPrimTyCon
	= cmmOffsetB expr arrPtrsHdrSize

  | tycon == byteArrayPrimTyCon || tycon == mutableByteArrayPrimTyCon
	= cmmOffsetB expr arrWordsHdrSize

  | otherwise = expr
  where	
	-- should be a tycon app, since this is a foreign call
	tycon = tyConAppTyCon (repType (stgArgType arg))
