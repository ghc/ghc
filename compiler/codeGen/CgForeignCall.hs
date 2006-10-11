-----------------------------------------------------------------------------
--
-- Code generation for foreign calls.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module CgForeignCall (
  cgForeignCall,
  emitForeignCall,
  emitForeignCall',
  shimForeignCallArg,
  emitSaveThreadState, -- will be needed by the Cmm parser
  emitLoadThreadState, -- ditto
  emitCloseNursery,
  emitOpenNursery,
 ) where

#include "HsVersions.h"

import StgSyn
import CgProf
import CgBindery
import CgMonad
import CgUtils
import Type
import TysPrim
import CLabel
import Cmm
import CmmUtils
import MachOp
import SMRep
import ForeignCall
import Constants
import StaticFlags
import Outputable

import Control.Monad

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
  = do vols <- getVolatileRegs live
       emitForeignCall' safety results
		(CmmForeignCall cmm_target cconv) call_args (Just vols)
  where
      (call_args, cmm_target)
	= case target of
	   StaticTarget lbl -> (args, CmmLit (CmmLabel 
					(mkForeignLabel lbl call_size False)))
	   DynamicTarget    ->  case args of (fn,_):rest -> (rest, fn)

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


-- alternative entry point, used by CmmParse
emitForeignCall'
	:: Safety
	-> [(CmmReg,MachHint)]	-- where to put the results
	-> CmmCallTarget	-- the op
	-> [(CmmExpr,MachHint)] -- arguments
	-> Maybe [GlobalReg]	-- live vars, in case we need to save them
	-> Code
emitForeignCall' safety results target args vols 
  | not (playSafe safety) = do
    temp_args <- load_args_into_temps args
    stmtC (CmmCall target results temp_args vols)

  | otherwise = do
    id <- newTemp wordRep
    temp_args <- load_args_into_temps args
    temp_target <- load_target_into_temp target
    emitSaveThreadState
    stmtC (CmmCall (CmmForeignCall suspendThread CCallConv) 
			[(id,PtrHint)]
			[ (CmmReg (CmmGlobal BaseReg), PtrHint) ] 
			vols
			)
    stmtC (CmmCall temp_target results temp_args vols)
    stmtC (CmmCall (CmmForeignCall resumeThread CCallConv) 
			[ (CmmGlobal BaseReg, PtrHint) ]
				-- Assign the result to BaseReg: we
				-- might now have a different
				-- Capability!
			[ (CmmReg id, PtrHint) ]
			vols
			)
    emitLoadThreadState


suspendThread = CmmLit (CmmLabel (mkRtsCodeLabel SLIT("suspendThread")))
resumeThread  = CmmLit (CmmLabel (mkRtsCodeLabel SLIT("resumeThread")))


-- we might need to load arguments into temporaries before
-- making the call, because certain global registers might
-- overlap with registers that the C calling convention uses
-- for passing arguments.
--
-- This is a HACK; really it should be done in the back end, but
-- it's easier to generate the temporaries here.
load_args_into_temps = mapM arg_assign_temp
  where arg_assign_temp (e,hint) = do
	   tmp <- maybe_assign_temp e
	   return (tmp,hint)
	
load_target_into_temp (CmmForeignCall expr conv) = do 
  tmp <- maybe_assign_temp expr
  return (CmmForeignCall tmp conv)
load_target_info_temp other_target =
  return other_target

maybe_assign_temp e
  | hasNoGlobalRegs e = return e
  | otherwise          = do 
	-- don't use assignTemp, it uses its own notion of "trivial"
	-- expressions, which are wrong here
	reg <- newTemp (cmmExprRep e)
	stmtC (CmmAssign reg e)
	return (CmmReg reg)

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
-- value passed to the call.  For ByteArray#/Array# we pass the
-- address of the actual array, not the address of the heap object.

shimForeignCallArg :: StgArg -> CmmExpr -> CmmExpr
shimForeignCallArg arg expr
  | tycon == arrayPrimTyCon || tycon == mutableArrayPrimTyCon
	= cmmOffsetB expr arrPtrsHdrSize

  | tycon == byteArrayPrimTyCon || tycon == mutableByteArrayPrimTyCon
	= cmmOffsetB expr arrWordsHdrSize

  | otherwise = expr
  where	
	-- should be a tycon app, since this is a foreign call
	tycon = tyConAppTyCon (repType (stgArgType arg))
