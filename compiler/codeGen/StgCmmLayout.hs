-----------------------------------------------------------------------------
--
-- Building info tables.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module StgCmmLayout (
	mkArgDescr, 
	emitCall, emitReturn,

	emitClosureProcAndInfoTable,
	emitClosureAndInfoTable,

	slowCall, directCall, 

	mkVirtHeapOffsets, getHpRelOffset, hpRel,

	stdInfoTableSizeB,
	entryCode, closureInfoPtr,
	getConstrTag,
        cmmGetClosureType,
	infoTable, infoTableClosureType,
	infoTablePtrs, infoTableNonPtrs,
	funInfoTable, makeRelativeRefTo
  ) where


#include "HsVersions.h"

import StgCmmClosure
import StgCmmEnv
import StgCmmTicky
import StgCmmUtils
import StgCmmMonad

import MkGraph
import SMRep
import CmmDecl
import CmmExpr
import CmmUtils
import CLabel
import StgSyn
import DataCon
import Id
import Name
import TyCon		( PrimRep(..) )
import Unique
import BasicTypes	( Arity )
import StaticFlags

import Bitmap
import Data.Bits

import Constants
import Util
import Data.List
import Outputable
import FastString	( mkFastString, FastString, fsLit )

------------------------------------------------------------------------
--		Call and return sequences
------------------------------------------------------------------------

emitReturn :: [CmmExpr] -> FCode ()
-- Return multiple values to the sequel
--
-- If the sequel is Return
--	return (x,y)
-- If the sequel is AssignTo [p,q]
--	p=x; q=y; 
emitReturn results
  = do { sequel    <- getSequel;
       ; updfr_off <- getUpdFrameOff
       ; emit $ mkComment $ mkFastString ("emitReturn: " ++ show sequel)
       ; case sequel of
           Return _ ->
             do { adjustHpBackwards
                ; emit (mkReturnSimple results updfr_off) }
           AssignTo regs adjust ->
             do { if adjust then adjustHpBackwards else return ()
                ; emit (mkMultiAssign  regs results) }
       }

emitCall :: (Convention, Convention) -> CmmExpr -> [CmmExpr] -> FCode ()
-- (cgCall fun args) makes a call to the entry-code of 'fun', 
-- passing 'args', and returning the results to the current sequel
emitCall convs@(callConv, _) fun args
  = do	{ adjustHpBackwards
	; sequel <- getSequel
	; updfr_off <- getUpdFrameOff
        ; emit $ mkComment $ mkFastString ("emitCall: " ++ show sequel)
	; case sequel of
	    Return _            -> emit (mkForeignJump callConv fun args updfr_off)
	    AssignTo res_regs _ -> emit (mkCall fun convs res_regs args updfr_off)
    }

adjustHpBackwards :: FCode ()
-- This function adjusts and heap pointers just before a tail call or
-- return.  At a call or return, the virtual heap pointer may be less 
-- than the real Hp, because the latter was advanced to deal with 
-- the worst-case branch of the code, and we may be in a better-case 
-- branch.  In that case, move the real Hp *back* and retract some 
-- ticky allocation count.
--
-- It *does not* deal with high-water-mark adjustment.
-- That's done by functions which allocate heap.
adjustHpBackwards
  = do	{ hp_usg <- getHpUsage
	; let rHp = realHp hp_usg
	      vHp = virtHp hp_usg
	      adjust_words = vHp -rHp
	; new_hp <- getHpRelOffset vHp

	; emit (if adjust_words == 0
		then mkNop
		else mkAssign hpReg new_hp)	-- Generates nothing when vHp==rHp

	; tickyAllocHeap adjust_words		-- ...ditto

	; setRealHp vHp
	}


-------------------------------------------------------------------------
--	Making calls: directCall and slowCall
-------------------------------------------------------------------------

directCall :: CLabel -> Arity -> [StgArg] -> FCode ()
-- (directCall f n args)
-- calls f(arg1, ..., argn), and applies the result to the remaining args
-- The function f has arity n, and there are guaranteed at least n args
-- Both arity and args include void args
directCall lbl arity stg_args 
  = do	{ cmm_args <- getNonVoidArgAmodes stg_args
	; direct_call "directCall" lbl arity cmm_args (argsLReps stg_args) }

slowCall :: CmmExpr -> [StgArg] -> FCode ()
-- (slowCall fun args) applies fun to args, returning the results to Sequel
slowCall fun stg_args 
  = do	{ cmm_args <- getNonVoidArgAmodes stg_args
	; slow_call fun cmm_args (argsLReps stg_args) }

--------------
direct_call :: String -> CLabel -> Arity -> [CmmExpr] -> [LRep] -> FCode ()
-- NB1: (length args) may be less than (length reps), because
--     the args exclude the void ones
-- NB2: 'arity' refers to the *reps* 
direct_call caller lbl arity args reps
  | debugIsOn && arity > length reps	-- Too few args
  =  	    -- Caller should ensure that there enough args!  
    pprPanic "direct_call" (text caller <+> ppr arity <+> ppr lbl <+> ppr (length reps)
	     	  	    <+> ppr args <+> ppr reps )

  | null rest_reps     -- Precisely the right number of arguments
  = emitCall (NativeDirectCall, NativeReturn) target args

  | otherwise		-- Over-saturated call
  = ASSERT( arity == length initial_reps )
    do	{ pap_id <- newTemp gcWord
	; withSequel (AssignTo [pap_id] True)
		     (emitCall (NativeDirectCall, NativeReturn) target fast_args)
	; slow_call (CmmReg (CmmLocal pap_id)) 
		    rest_args rest_reps }
  where
    target = CmmLit (CmmLabel lbl)
    (initial_reps, rest_reps) = splitAt arity reps
    arg_arity = count isNonV initial_reps
    (fast_args, rest_args) = splitAt arg_arity args

--------------
slow_call :: CmmExpr -> [CmmExpr] -> [LRep] -> FCode ()
slow_call fun args reps
  = do call <- getCode $ direct_call "slow_call" (mkRtsApFastLabel rts_fun) arity args reps
       emit $ mkComment $ mkFastString ("slow_call for " ++ showSDoc (ppr fun) ++
                                        " with pat " ++ showSDoc (ftext rts_fun))
       emit (mkAssign nodeReg fun <*> call)
  where
    (rts_fun, arity) = slowCallPattern reps

-- These cases were found to cover about 99% of all slow calls:
slowCallPattern :: [LRep] -> (FastString, Arity)
-- Returns the generic apply function and arity
slowCallPattern (P: P: P: P: P: P: _) = (fsLit "stg_ap_pppppp", 6)
slowCallPattern (P: P: P: P: P: _)    = (fsLit "stg_ap_ppppp", 5)
slowCallPattern (P: P: P: P: _)       = (fsLit "stg_ap_pppp", 4)
slowCallPattern (P: P: P: V: _)       = (fsLit "stg_ap_pppv", 4)
slowCallPattern (P: P: P: _)          = (fsLit "stg_ap_ppp", 3)
slowCallPattern (P: P: V: _)          = (fsLit "stg_ap_ppv", 3)
slowCallPattern (P: P: _)	      = (fsLit "stg_ap_pp", 2)
slowCallPattern (P: V: _)	      = (fsLit "stg_ap_pv", 2)
slowCallPattern (P: _)		      = (fsLit "stg_ap_p", 1)
slowCallPattern (V: _)		      = (fsLit "stg_ap_v", 1)
slowCallPattern (N: _)		      = (fsLit "stg_ap_n", 1)
slowCallPattern (F: _)		      = (fsLit "stg_ap_f", 1)
slowCallPattern (D: _)		      = (fsLit "stg_ap_d", 1)
slowCallPattern (L: _)		      = (fsLit "stg_ap_l", 1)
slowCallPattern []		      = (fsLit "stg_ap_0", 0)


-------------------------------------------------------------------------
--	Classifying arguments: LRep
-------------------------------------------------------------------------

-- LRep is not exported (even abstractly)
-- It's a local helper type for classification

data LRep = P 	-- GC Ptr
	  | N   -- One-word non-ptr
	  | L	-- Two-word non-ptr (long)
	  | V	-- Void
	  | F	-- Float
	  | D	-- Double
instance Outputable LRep where
  ppr P = text "P"
  ppr N = text "N"
  ppr L = text "L"
  ppr V = text "V"
  ppr F = text "F"
  ppr D = text "D"

toLRep :: PrimRep -> LRep
toLRep VoidRep 	 = V
toLRep PtrRep  	 = P
toLRep IntRep  	 = N
toLRep WordRep 	 = N
toLRep AddrRep 	 = N
toLRep Int64Rep  = L
toLRep Word64Rep = L
toLRep FloatRep  = F
toLRep DoubleRep = D

isNonV :: LRep -> Bool
isNonV V = False
isNonV _ = True

argsLReps :: [StgArg] -> [LRep]
argsLReps = map (toLRep . argPrimRep)

lRepSizeW :: LRep -> WordOff		-- Size in words
lRepSizeW N = 1
lRepSizeW P = 1
lRepSizeW F = 1
lRepSizeW L = wORD64_SIZE `quot` wORD_SIZE
lRepSizeW D = dOUBLE_SIZE `quot` wORD_SIZE
lRepSizeW V = 0

-------------------------------------------------------------------------
----	Laying out objects on the heap and stack
-------------------------------------------------------------------------

-- The heap always grows upwards, so hpRel is easy
hpRel :: VirtualHpOffset 	-- virtual offset of Hp
      -> VirtualHpOffset 	-- virtual offset of The Thing
      -> WordOff		-- integer word offset
hpRel hp off = off - hp

getHpRelOffset :: VirtualHpOffset -> FCode CmmExpr
getHpRelOffset virtual_offset
  = do	{ hp_usg <- getHpUsage
	; return (cmmRegOffW hpReg (hpRel (realHp hp_usg) virtual_offset)) }

mkVirtHeapOffsets
  :: Bool		-- True <=> is a thunk
  -> [(PrimRep,a)]	-- Things to make offsets for
  -> (WordOff,		-- _Total_ number of words allocated
      WordOff,		-- Number of words allocated for *pointers*
      [(NonVoid a, VirtualHpOffset)])

-- Things with their offsets from start of object in order of
-- increasing offset; BUT THIS MAY BE DIFFERENT TO INPUT ORDER
-- First in list gets lowest offset, which is initial offset + 1.
--
-- Void arguments are removed, so output list may be shorter than
-- input list
--
-- mkVirtHeapOffsets always returns boxed things with smaller offsets
-- than the unboxed things

mkVirtHeapOffsets is_thunk things
  = let non_void_things		      = filterOut (isVoidRep . fst)  things
	(ptrs, non_ptrs)    	      = partition (isGcPtrRep . fst) non_void_things
    	(wds_of_ptrs, ptrs_w_offsets) = mapAccumL computeOffset 0 ptrs
	(tot_wds, non_ptrs_w_offsets) = mapAccumL computeOffset wds_of_ptrs non_ptrs
    in
    (tot_wds, wds_of_ptrs, ptrs_w_offsets ++ non_ptrs_w_offsets)
  where
    hdr_size 	| is_thunk   = thunkHdrSize
		| otherwise  = fixedHdrSize

    computeOffset wds_so_far (rep, thing)
      = (wds_so_far + lRepSizeW (toLRep rep), 
	 (NonVoid thing, hdr_size + wds_so_far))


-------------------------------------------------------------------------
--
--	Making argument descriptors
--
--  An argument descriptor describes the layout of args on the stack,
--  both for 	* GC (stack-layout) purposes, and 
--		* saving/restoring registers when a heap-check fails
--
-- Void arguments aren't important, therefore (contrast constructSlowCall)
--
-------------------------------------------------------------------------

-- bring in ARG_P, ARG_N, etc.
#include "../includes/rts/storage/FunTypes.h"

-------------------------
-- argDescrType :: ArgDescr -> StgHalfWord
-- -- The "argument type" RTS field type
-- argDescrType (ArgSpec n) = n
-- argDescrType (ArgGen liveness)
--   | isBigLiveness liveness = ARG_GEN_BIG
--   | otherwise		   = ARG_GEN


mkArgDescr :: Name -> [Id] -> FCode ArgDescr
mkArgDescr nm args 
  = case stdPattern arg_reps of
	Just spec_id -> return (ArgSpec spec_id)
	Nothing      -> do { liveness <- mkLiveness nm size bitmap
			   ; return (ArgGen liveness) }
  where
    arg_reps = filter isNonV (map (toLRep . idPrimRep) args)
	-- Getting rid of voids eases matching of standard patterns

    bitmap   = mkBitmap arg_bits
    arg_bits = argBits arg_reps
    size     = length arg_bits

argBits :: [LRep] -> [Bool]	-- True for non-ptr, False for ptr
argBits [] 		= []
argBits (P   : args) = False : argBits args
argBits (arg : args) = take (lRepSizeW arg) (repeat True) ++ argBits args

----------------------
stdPattern :: [LRep] -> Maybe StgHalfWord
stdPattern reps 
  = case reps of
	[]  -> Just ARG_NONE	-- just void args, probably
	[N] -> Just ARG_N
	[P] -> Just ARG_P
	[F] -> Just ARG_F
	[D] -> Just ARG_D
	[L] -> Just ARG_L

	[N,N] -> Just ARG_NN
	[N,P] -> Just ARG_NP
	[P,N] -> Just ARG_PN
	[P,P] -> Just ARG_PP

	[N,N,N] -> Just ARG_NNN
	[N,N,P] -> Just ARG_NNP
	[N,P,N] -> Just ARG_NPN
	[N,P,P] -> Just ARG_NPP
	[P,N,N] -> Just ARG_PNN
	[P,N,P] -> Just ARG_PNP
	[P,P,N] -> Just ARG_PPN
	[P,P,P] -> Just ARG_PPP

	[P,P,P,P]     -> Just ARG_PPPP
	[P,P,P,P,P]   -> Just ARG_PPPPP
	[P,P,P,P,P,P] -> Just ARG_PPPPPP
	
	_ -> Nothing

-------------------------------------------------------------------------
--
--	Liveness info
--
-------------------------------------------------------------------------

-- TODO: This along with 'mkArgDescr' should be unified
-- with 'CmmInfo.mkLiveness'.  However that would require
-- potentially invasive changes to the 'ClosureInfo' type.
-- For now, 'CmmInfo.mkLiveness' handles only continuations and
-- this one handles liveness everything else.  Another distinction
-- between these two is that 'CmmInfo.mkLiveness' information
-- about the stack layout, and this one is information about
-- the heap layout of PAPs.
mkLiveness :: Name -> Int -> Bitmap -> FCode Liveness
mkLiveness name size bits
  | size > mAX_SMALL_BITMAP_SIZE		-- Bitmap does not fit in one word
  = do	{ let lbl = mkBitmapLabel (getUnique name)
	; emitRODataLits lbl ( mkWordCLit (fromIntegral size)
		             : map mkWordCLit bits)
	; return (BigLiveness lbl) }
  
  | otherwise		-- Bitmap fits in one word
  = let
        small_bits = case bits of 
			[]  -> 0
                        [b] -> b
			_   -> panic "livenessToAddrMode"
    in
    return (smallLiveness size small_bits)

smallLiveness :: Int -> StgWord -> Liveness
smallLiveness size small_bits = SmallLiveness bits
  where bits = fromIntegral size .|. (small_bits `shiftL` bITMAP_BITS_SHIFT)

-------------------
-- isBigLiveness :: Liveness -> Bool
-- isBigLiveness (BigLiveness _)   = True
-- isBigLiveness (SmallLiveness _) = False

-------------------
-- mkLivenessCLit :: Liveness -> CmmLit
-- mkLivenessCLit (BigLiveness lbl)    = CmmLabel lbl
-- mkLivenessCLit (SmallLiveness bits) = mkWordCLit bits


-------------------------------------------------------------------------
--
--		Bitmap describing register liveness
--		across GC when doing a "generic" heap check
--		(a RET_DYN stack frame).
--
-- NB. Must agree with these macros (currently in StgMacros.h): 
-- GET_NON_PTRS(), GET_PTRS(), GET_LIVENESS().
-------------------------------------------------------------------------

{- 	Not used in new code gen
mkRegLiveness :: [(Id, GlobalReg)] -> Int -> Int -> StgWord
mkRegLiveness regs ptrs nptrs
  = (fromIntegral nptrs `shiftL` 16) .|. 
    (fromIntegral ptrs  `shiftL` 24) .|.
    all_non_ptrs `xor` reg_bits regs
  where
    all_non_ptrs = 0xff

    reg_bits [] = 0
    reg_bits ((id, VanillaReg i) : regs) | isGcPtrRep (idPrimRep id)
  	= (1 `shiftL` (i - 1)) .|. reg_bits regs
    reg_bits (_ : regs)
	= reg_bits regs
-}
 
-------------------------------------------------------------------------
--
--	Generating the info table and code for a closure
--
-------------------------------------------------------------------------

-- Here we make an info table of type 'CmmInfo'.  The concrete
-- representation as a list of 'CmmAddr' is handled later
-- in the pipeline by 'cmmToRawCmm'.
-- When loading the free variables, a function closure pointer may be tagged,
-- so we must take it into account.

emitClosureProcAndInfoTable :: Bool                    -- top-level? 
                            -> Id                      -- name of the closure
                            -> ClosureInfo             -- lots of info abt the closure
                            -> [NonVoid Id]            -- incoming arguments
                            -> ((Int, LocalReg, [LocalReg]) -> FCode ()) -- function body
                            -> FCode ()
emitClosureProcAndInfoTable top_lvl bndr cl_info args body
 = do	{ let lf_info = closureLFInfo cl_info
        -- Bind the binder itself, but only if it's not a top-level
        -- binding. We need non-top let-bindings to refer to the
        -- top-level binding, which this binding would incorrectly shadow.
        ; node <- if top_lvl then return $ idToReg (NonVoid bndr)
                  else bindToReg (NonVoid bndr) lf_info
        ; let node_points = nodeMustPointToIt lf_info
        ; arg_regs <- bindArgsToRegs args
        ; let args' = if node_points then (node : arg_regs) else arg_regs
              conv  = if nodeMustPointToIt lf_info then NativeNodeCall
                                                   else NativeDirectCall
              (offset, _) = mkCallEntry conv args'
        ; emitClosureAndInfoTable cl_info conv args' $ body (offset, node, arg_regs)
        }

-- Data constructors need closures, but not with all the argument handling
-- needed for functions. The shared part goes here.
emitClosureAndInfoTable ::
  ClosureInfo -> Convention -> [LocalReg] -> FCode () -> FCode ()
emitClosureAndInfoTable cl_info conv args body
  = do { info <- mkCmmInfo cl_info
       ; blks <- getCode body
       ; emitProcWithConvention conv info (infoLblToEntryLbl info_lbl) args blks
       }
  where
    info_lbl = infoTableLabelFromCI cl_info

-- Convert from 'ClosureInfo' to 'CmmInfoTable'.
-- Not used for return points.  (The 'smRepClosureTypeInt' call would panic.)
mkCmmInfo :: ClosureInfo -> FCode CmmInfoTable
mkCmmInfo cl_info
  = do	{ info <- closureTypeInfo cl_info k_with_con_name return 
        ; prof <- if opt_SccProfilingOn then
                    do fd_lit <- mkStringCLit (closureTypeDescr cl_info)
	               ad_lit <- mkStringCLit (closureValDescr  cl_info)
	               return $ ProfilingInfo fd_lit ad_lit
                  else return $ ProfilingInfo (mkIntCLit 0) (mkIntCLit 0)
	; return (CmmInfoTable (isStaticClosure cl_info) prof cl_type info) }
  where
    k_with_con_name con_info con info_lbl =
      do cstr <- mkByteStringCLit $ dataConIdentity con
         return $ con_info $ makeRelativeRefTo info_lbl cstr
    cl_type  = smRepClosureTypeInt (closureSMRep cl_info)

-----------------------------------------------------------------------------
--
--	Info table offsets
--
-----------------------------------------------------------------------------
	
stdInfoTableSizeW :: WordOff
-- The size of a standard info table varies with profiling/ticky etc,
-- so we can't get it from Constants
-- It must vary in sync with mkStdInfoTable
stdInfoTableSizeW
  = size_fixed + size_prof
  where
    size_fixed = 2	-- layout, type
    size_prof | opt_SccProfilingOn = 2
	      | otherwise	   = 0

stdInfoTableSizeB  :: ByteOff
stdInfoTableSizeB = stdInfoTableSizeW * wORD_SIZE :: ByteOff

stdSrtBitmapOffset :: ByteOff
-- Byte offset of the SRT bitmap half-word which is 
-- in the *higher-addressed* part of the type_lit
stdSrtBitmapOffset = stdInfoTableSizeB - hALF_WORD_SIZE

stdClosureTypeOffset :: ByteOff
-- Byte offset of the closure type half-word 
stdClosureTypeOffset = stdInfoTableSizeB - wORD_SIZE

stdPtrsOffset, stdNonPtrsOffset :: ByteOff
stdPtrsOffset    = stdInfoTableSizeB - 2*wORD_SIZE
stdNonPtrsOffset = stdInfoTableSizeB - 2*wORD_SIZE + hALF_WORD_SIZE

-------------------------------------------------------------------------
--
--	Accessing fields of an info table
--
-------------------------------------------------------------------------

closureInfoPtr :: CmmExpr -> CmmExpr
-- Takes a closure pointer and returns the info table pointer
closureInfoPtr e = CmmLoad e bWord

entryCode :: CmmExpr -> CmmExpr
-- Takes an info pointer (the first word of a closure)
-- and returns its entry code
entryCode e | tablesNextToCode = e
	    | otherwise	       = CmmLoad e bWord

getConstrTag :: CmmExpr -> CmmExpr
-- Takes a closure pointer, and return the *zero-indexed*
-- constructor tag obtained from the info table
-- This lives in the SRT field of the info table
-- (constructors don't need SRTs).
getConstrTag closure_ptr 
  = CmmMachOp (MO_UU_Conv halfWordWidth wordWidth) [infoTableConstrTag info_table]
  where
    info_table = infoTable (closureInfoPtr closure_ptr)

cmmGetClosureType :: CmmExpr -> CmmExpr
-- Takes a closure pointer, and return the closure type
-- obtained from the info table
cmmGetClosureType closure_ptr 
  = CmmMachOp (MO_UU_Conv halfWordWidth wordWidth) [infoTableClosureType info_table]
  where
    info_table = infoTable (closureInfoPtr closure_ptr)

infoTable :: CmmExpr -> CmmExpr
-- Takes an info pointer (the first word of a closure)
-- and returns a pointer to the first word of the standard-form
-- info table, excluding the entry-code word (if present)
infoTable info_ptr
  | tablesNextToCode = cmmOffsetB info_ptr (- stdInfoTableSizeB)
  | otherwise	     = cmmOffsetW info_ptr 1	-- Past the entry code pointer

infoTableConstrTag :: CmmExpr -> CmmExpr
-- Takes an info table pointer (from infoTable) and returns the constr tag
-- field of the info table (same as the srt_bitmap field)
infoTableConstrTag = infoTableSrtBitmap

infoTableSrtBitmap :: CmmExpr -> CmmExpr
-- Takes an info table pointer (from infoTable) and returns the srt_bitmap
-- field of the info table
infoTableSrtBitmap info_tbl
  = CmmLoad (cmmOffsetB info_tbl stdSrtBitmapOffset) bHalfWord

infoTableClosureType :: CmmExpr -> CmmExpr
-- Takes an info table pointer (from infoTable) and returns the closure type
-- field of the info table.
infoTableClosureType info_tbl 
  = CmmLoad (cmmOffsetB info_tbl stdClosureTypeOffset) bHalfWord

infoTablePtrs :: CmmExpr -> CmmExpr
infoTablePtrs info_tbl 
  = CmmLoad (cmmOffsetB info_tbl stdPtrsOffset) bHalfWord

infoTableNonPtrs :: CmmExpr -> CmmExpr
infoTableNonPtrs info_tbl 
  = CmmLoad (cmmOffsetB info_tbl stdNonPtrsOffset) bHalfWord

funInfoTable :: CmmExpr -> CmmExpr
-- Takes the info pointer of a function,
-- and returns a pointer to the first word of the StgFunInfoExtra struct
-- in the info table.
funInfoTable info_ptr
  | tablesNextToCode
  = cmmOffsetB info_ptr (- stdInfoTableSizeB - sIZEOF_StgFunInfoExtraRev)
  | otherwise
  = cmmOffsetW info_ptr (1 + stdInfoTableSizeW)
				-- Past the entry code pointer

-------------------------------------------------------------------------
--
--	Static reference tables
--
-------------------------------------------------------------------------

-- srtLabelAndLength :: C_SRT -> CLabel -> (CmmLit, StgHalfWord)
-- srtLabelAndLength NoC_SRT _		
--   = (zeroCLit, 0)
-- srtLabelAndLength (C_SRT lbl off bitmap) info_lbl
--   = (makeRelativeRefTo info_lbl $ cmmLabelOffW lbl off, bitmap)

-------------------------------------------------------------------------
--
--	Position independent code
--
-------------------------------------------------------------------------
-- In order to support position independent code, we mustn't put absolute
-- references into read-only space. Info tables in the tablesNextToCode
-- case must be in .text, which is read-only, so we doctor the CmmLits
-- to use relative offsets instead.

-- Note that this is done even when the -fPIC flag is not specified,
-- as we want to keep binary compatibility between PIC and non-PIC.

makeRelativeRefTo :: CLabel -> CmmLit -> CmmLit
        
makeRelativeRefTo info_lbl (CmmLabel lbl)
  | tablesNextToCode
  = CmmLabelDiffOff lbl info_lbl 0
makeRelativeRefTo info_lbl (CmmLabelOff lbl off)
  | tablesNextToCode
  = CmmLabelDiffOff lbl info_lbl off
makeRelativeRefTo _ lit = lit
