-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2004-2006
--
-- CgCallConv
--
-- The datatypes and functions here encapsulate the 
-- calling and return conventions used by the code generator.
--
-----------------------------------------------------------------------------


module CgCallConv (
	-- Argument descriptors
	mkArgDescr, argDescrType,

	-- Liveness
	isBigLiveness, buildContLiveness, mkRegLiveness, 
	smallLiveness, mkLivenessCLit,

	-- Register assignment
	assignCallRegs, assignReturnRegs, assignPrimOpCallRegs,

	-- Calls
	constructSlowCall, slowArgs, slowCallPattern,

	-- Returns
	CtrlReturnConvention(..),
	ctrlReturnConvAlg,
	dataReturnConvPrim,
	getSequelAmode
    ) where

#include "HsVersions.h"

import CgUtils
import CgMonad
import SMRep

import MachOp
import Cmm
import CLabel

import Constants
import ClosureInfo
import CgStackery
import CmmUtils
import Maybes
import Id
import Name
import TyCon
import Bitmap
import Util
import StaticFlags
import FastString
import Outputable

import Data.Bits

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
#include "../includes/StgFun.h"

-------------------------
argDescrType :: ArgDescr -> Int
-- The "argument type" RTS field type
argDescrType (ArgSpec n) = n
argDescrType (ArgGen liveness)
  | isBigLiveness liveness = ARG_GEN_BIG
  | otherwise		   = ARG_GEN


mkArgDescr :: Name -> [Id] -> FCode ArgDescr
mkArgDescr nm args 
  = case stdPattern arg_reps of
	Just spec_id -> return (ArgSpec spec_id)
	Nothing      -> do { liveness <- mkLiveness nm size bitmap
			   ; return (ArgGen liveness) }
  where
    arg_reps = filter nonVoidArg (map idCgRep args)
	-- Getting rid of voids eases matching of standard patterns

    bitmap   = mkBitmap arg_bits
    arg_bits = argBits arg_reps
    size     = length arg_bits

argBits :: [CgRep] -> [Bool]	-- True for non-ptr, False for ptr
argBits [] 		= []
argBits (PtrArg : args) = False : argBits args
argBits (arg    : args) = take (cgRepSizeW arg) (repeat True) ++ argBits args

stdPattern :: [CgRep] -> Maybe Int
stdPattern []          = Just ARG_NONE	-- just void args, probably

stdPattern [PtrArg]    = Just ARG_P
stdPattern [FloatArg]  = Just ARG_F
stdPattern [DoubleArg] = Just ARG_D
stdPattern [LongArg]   = Just ARG_L
stdPattern [NonPtrArg] = Just ARG_N
	 
stdPattern [NonPtrArg,NonPtrArg] = Just ARG_NN
stdPattern [NonPtrArg,PtrArg]    = Just ARG_NP
stdPattern [PtrArg,NonPtrArg]    = Just ARG_PN
stdPattern [PtrArg,PtrArg]       = Just ARG_PP

stdPattern [NonPtrArg,NonPtrArg,NonPtrArg] = Just ARG_NNN
stdPattern [NonPtrArg,NonPtrArg,PtrArg]    = Just ARG_NNP
stdPattern [NonPtrArg,PtrArg,NonPtrArg]    = Just ARG_NPN
stdPattern [NonPtrArg,PtrArg,PtrArg]	   = Just ARG_NPP
stdPattern [PtrArg,NonPtrArg,NonPtrArg]    = Just ARG_PNN
stdPattern [PtrArg,NonPtrArg,PtrArg]	   = Just ARG_PNP
stdPattern [PtrArg,PtrArg,NonPtrArg]	   = Just ARG_PPN
stdPattern [PtrArg,PtrArg,PtrArg]	   = Just ARG_PPP
	 
stdPattern [PtrArg,PtrArg,PtrArg,PtrArg]	       = Just ARG_PPPP
stdPattern [PtrArg,PtrArg,PtrArg,PtrArg,PtrArg]        = Just ARG_PPPPP
stdPattern [PtrArg,PtrArg,PtrArg,PtrArg,PtrArg,PtrArg] = Just ARG_PPPPPP
stdPattern other = Nothing


-------------------------------------------------------------------------
--
--	Liveness info
--
-------------------------------------------------------------------------

mkLiveness :: Name -> Int -> Bitmap -> FCode Liveness
mkLiveness name size bits
  | size > mAX_SMALL_BITMAP_SIZE		-- Bitmap does not fit in one word
  = do	{ let lbl = mkBitmapLabel name
	; emitRODataLits lbl ( mkWordCLit (fromIntegral size)
		             : map mkWordCLit bits)
	; return (BigLiveness lbl) }
  
  | otherwise		-- Bitmap fits in one word
  = let
        small_bits = case bits of 
			[]  -> 0
			[b] -> fromIntegral b
			_   -> panic "livenessToAddrMode"
    in
    return (smallLiveness size small_bits)

smallLiveness :: Int -> StgWord -> Liveness
smallLiveness size small_bits = SmallLiveness bits
  where bits = fromIntegral size .|. (small_bits `shiftL` bITMAP_BITS_SHIFT)

-------------------
isBigLiveness :: Liveness -> Bool
isBigLiveness (BigLiveness _)   = True
isBigLiveness (SmallLiveness _) = False

-------------------
mkLivenessCLit :: Liveness -> CmmLit
mkLivenessCLit (BigLiveness lbl)    = CmmLabel lbl
mkLivenessCLit (SmallLiveness bits) = mkWordCLit bits


-------------------------------------------------------------------------
--
--		Bitmap describing register liveness
--		across GC when doing a "generic" heap check
--		(a RET_DYN stack frame).
--
-- NB. Must agree with these macros (currently in StgMacros.h): 
-- GET_NON_PTRS(), GET_PTRS(), GET_LIVENESS().
-------------------------------------------------------------------------

mkRegLiveness :: [(Id, GlobalReg)] -> Int -> Int -> StgWord
mkRegLiveness regs ptrs nptrs
  = (fromIntegral nptrs `shiftL` 16) .|. 
    (fromIntegral ptrs  `shiftL` 24) .|.
    all_non_ptrs `xor` reg_bits regs
  where
    all_non_ptrs = 0xff

    reg_bits [] = 0
    reg_bits ((id, VanillaReg i) : regs) | isFollowableArg (idCgRep id)
  	= (1 `shiftL` (i - 1)) .|. reg_bits regs
    reg_bits (_ : regs)
	= reg_bits regs
  
-------------------------------------------------------------------------
--
--		Pushing the arguments for a slow call
--
-------------------------------------------------------------------------

-- For a slow call, we must take a bunch of arguments and intersperse
-- some stg_ap_<pattern>_ret_info return addresses.
constructSlowCall
	:: [(CgRep,CmmExpr)]
	-> (CLabel,		-- RTS entry point for call
	   [(CgRep,CmmExpr)],	-- args to pass to the entry point
	   [(CgRep,CmmExpr)])	-- stuff to save on the stack

   -- don't forget the zero case
constructSlowCall [] 
  = (mkRtsApFastLabel SLIT("stg_ap_0"), [], [])

constructSlowCall amodes
  = (stg_ap_pat, these, rest)
  where 
    stg_ap_pat = mkRtsApFastLabel arg_pat
    (arg_pat, these, rest) = matchSlowPattern amodes

enterRtsRetLabel arg_pat
  | tablesNextToCode = mkRtsRetInfoLabel arg_pat
  | otherwise        = mkRtsRetLabel arg_pat

-- | 'slowArgs' takes a list of function arguments and prepares them for
-- pushing on the stack for "extra" arguments to a function which requires
-- fewer arguments than we currently have.
slowArgs :: [(CgRep,CmmExpr)] -> [(CgRep,CmmExpr)]
slowArgs [] = []
slowArgs amodes = (NonPtrArg, mkLblExpr stg_ap_pat) : args ++ slowArgs rest
  where	(arg_pat, args, rest) = matchSlowPattern amodes
	stg_ap_pat = mkRtsRetInfoLabel arg_pat
  
matchSlowPattern :: [(CgRep,CmmExpr)] 
		 -> (LitString, [(CgRep,CmmExpr)], [(CgRep,CmmExpr)])
matchSlowPattern amodes = (arg_pat, these, rest)
  where (arg_pat, n)  = slowCallPattern (map fst amodes)
	(these, rest) = splitAt n amodes

-- These cases were found to cover about 99% of all slow calls:
slowCallPattern (PtrArg: PtrArg: PtrArg: PtrArg: PtrArg: PtrArg: _) = (SLIT("stg_ap_pppppp"), 6)
slowCallPattern (PtrArg: PtrArg: PtrArg: PtrArg: PtrArg: _) 	= (SLIT("stg_ap_ppppp"), 5)
slowCallPattern (PtrArg: PtrArg: PtrArg: PtrArg: _) 	= (SLIT("stg_ap_pppp"), 4)
slowCallPattern (PtrArg: PtrArg: PtrArg: VoidArg: _) 	= (SLIT("stg_ap_pppv"), 4)
slowCallPattern (PtrArg: PtrArg: PtrArg: _)       	= (SLIT("stg_ap_ppp"), 3)
slowCallPattern (PtrArg: PtrArg: VoidArg: _)       	= (SLIT("stg_ap_ppv"), 3)
slowCallPattern (PtrArg: PtrArg: _)			= (SLIT("stg_ap_pp"), 2)
slowCallPattern (PtrArg: VoidArg: _)			= (SLIT("stg_ap_pv"), 2)
slowCallPattern (PtrArg: _)				= (SLIT("stg_ap_p"), 1)
slowCallPattern (VoidArg: _)				= (SLIT("stg_ap_v"), 1)
slowCallPattern (NonPtrArg: _)				= (SLIT("stg_ap_n"), 1)
slowCallPattern (FloatArg: _)				= (SLIT("stg_ap_f"), 1)
slowCallPattern (DoubleArg: _)				= (SLIT("stg_ap_d"), 1)
slowCallPattern (LongArg: _)				= (SLIT("stg_ap_l"), 1)
slowCallPattern _  = panic "CgStackery.slowCallPattern"

-------------------------------------------------------------------------
--
--		Return conventions
--
-------------------------------------------------------------------------

-- A @CtrlReturnConvention@ says how {\em control} is returned.

data CtrlReturnConvention
  = VectoredReturn	Int	-- size of the vector table (family size)
  | UnvectoredReturn    Int 	-- family size

ctrlReturnConvAlg :: TyCon -> CtrlReturnConvention
ctrlReturnConvAlg tycon
  = case (tyConFamilySize tycon) of
      size -> -- we're supposed to know...
	if (size > (1::Int) && size <= mAX_FAMILY_SIZE_FOR_VEC_RETURNS) then
	    VectoredReturn size
	else
	    UnvectoredReturn size	
  -- NB: unvectored returns Include size 0 (no constructors), so that
  --     the following perverse code compiles (it crashed GHC in 5.02)
  -- 	    data T1
  --	    data T2 = T2 !T1 Int
  --     The only value of type T1 is bottom, which never returns anyway.

dataReturnConvPrim :: CgRep -> CmmReg
dataReturnConvPrim PtrArg    = CmmGlobal (VanillaReg 1)
dataReturnConvPrim NonPtrArg = CmmGlobal (VanillaReg 1)
dataReturnConvPrim LongArg   = CmmGlobal (LongReg 1)
dataReturnConvPrim FloatArg  = CmmGlobal (FloatReg 1)
dataReturnConvPrim DoubleArg = CmmGlobal (DoubleReg 1)
dataReturnConvPrim VoidArg   = panic "dataReturnConvPrim: void"


-- getSequelAmode returns an amode which refers to an info table.  The info
-- table will always be of the RET(_VEC)?_(BIG|SMALL) kind.  We're careful
-- not to handle real code pointers, just in case we're compiling for 
-- an unregisterised/untailcallish architecture, where info pointers and
-- code pointers aren't the same.
-- DIRE WARNING.
-- The OnStack case of sequelToAmode delivers an Amode which is only
-- valid just before the final control transfer, because it assumes
-- that Sp is pointing to the top word of the return address.  This
-- seems unclean but there you go.

getSequelAmode :: FCode CmmExpr
getSequelAmode
  = do	{ EndOfBlockInfo virt_sp sequel <- getEndOfBlockInfo
	; case sequel of
	    OnStack -> do { sp_rel <- getSpRelOffset virt_sp
			  ; returnFC (CmmLoad sp_rel wordRep) }

	    UpdateCode 	           -> returnFC (CmmLit (CmmLabel mkUpdInfoLabel))
	    CaseAlts lbl _ _ True  -> returnFC (CmmLit (CmmLabel mkSeqInfoLabel))
	    CaseAlts lbl _ _ False -> returnFC (CmmLit (CmmLabel lbl))
	}

-------------------------------------------------------------------------
--
--		Build a liveness mask for the current stack
--
-------------------------------------------------------------------------

-- There are four kinds of things on the stack:
--
--	- pointer variables (bound in the environment)
-- 	- non-pointer variables (boudn in the environment)
-- 	- free slots (recorded in the stack free list)
-- 	- non-pointer data slots (recorded in the stack free list)
-- 
-- We build up a bitmap of non-pointer slots by searching the environment
-- for all the pointer variables, and subtracting these from a bitmap
-- with initially all bits set (up to the size of the stack frame).

buildContLiveness :: Name		-- Basis for label (only)
		  -> [VirtualSpOffset] 	-- Live stack slots
		  -> FCode Liveness
buildContLiveness name live_slots
 = do	{ stk_usg    <- getStkUsage
	; let	StackUsage { realSp = real_sp, 
			     frameSp = frame_sp } = stk_usg

		start_sp :: VirtualSpOffset
		start_sp = real_sp - retAddrSizeW
		-- In a continuation, we want a liveness mask that 
		-- starts from just after the return address, which is 
		-- on the stack at real_sp.

		frame_size :: WordOff
		frame_size = start_sp - frame_sp
		-- real_sp points to the frame-header for the current
		-- stack frame, and the end of this frame is frame_sp.
		-- The size is therefore real_sp - frame_sp - retAddrSizeW
		-- (subtract one for the frame-header = return address).
	
		rel_slots :: [WordOff]
	 	rel_slots = sortLe (<=) 
	    	    [ start_sp - ofs  -- Get slots relative to top of frame
	    	    | ofs <- live_slots ]

		bitmap = intsToReverseBitmap frame_size rel_slots

	; WARN( not (all (>=0) rel_slots), 
		ppr name $$ ppr live_slots $$ ppr frame_size $$ ppr start_sp $$ ppr rel_slots )
	  mkLiveness name frame_size bitmap }


-------------------------------------------------------------------------
--
--		Register assignment
--
-------------------------------------------------------------------------

--  How to assign registers for 
--
--	1) Calling a fast entry point.
--	2) Returning an unboxed tuple.
--	3) Invoking an out-of-line PrimOp.
--
-- Registers are assigned in order.
-- 
-- If we run out, we don't attempt to assign any further registers (even
-- though we might have run out of only one kind of register); we just
-- return immediately with the left-overs specified.
-- 
-- The alternative version @assignAllRegs@ uses the complete set of
-- registers, including those that aren't mapped to real machine
-- registers.  This is used for calling special RTS functions and PrimOps
-- which expect their arguments to always be in the same registers.

assignCallRegs, assignPrimOpCallRegs, assignReturnRegs
	:: [(CgRep,a)]		-- Arg or result values to assign
	-> ([(a, GlobalReg)],	-- Register assignment in same order
				-- for *initial segment of* input list
				--   (but reversed; doesn't matter)
				-- VoidRep args do not appear here
	    [(CgRep,a)])	-- Leftover arg or result values

assignCallRegs args
  = assign_regs args (mkRegTbl [node])
	-- The entry convention for a function closure
	-- never uses Node for argument passing; instead
	-- Node points to the function closure itself

assignPrimOpCallRegs args
 = assign_regs args (mkRegTbl_allRegs [])
	-- For primops, *all* arguments must be passed in registers

assignReturnRegs args
 = assign_regs args (mkRegTbl [])
	-- For returning unboxed tuples etc, 
	-- we use all regs

assign_regs :: [(CgRep,a)]     	-- Arg or result values to assign
	    -> AvailRegs	-- Regs still avail: Vanilla, Float, Double, Longs
	    -> ([(a, GlobalReg)], [(CgRep, a)])
assign_regs args supply
  = go args [] supply
  where
    go [] acc supply = (acc, [])	-- Return the results reversed (doesn't matter)
    go ((VoidArg,_) : args) acc supply 	-- Skip void arguments; they aren't passed, and
	= go args acc supply		-- there's nothign to bind them to
    go ((rep,arg) : args) acc supply 
	= case assign_reg rep supply of
		Just (reg, supply') -> go args ((arg,reg):acc) supply'
		Nothing	   	    -> (acc, (rep,arg):args) 	-- No more regs

assign_reg :: CgRep -> AvailRegs -> Maybe (GlobalReg, AvailRegs)
assign_reg FloatArg  (vs, f:fs, ds, ls) = Just (FloatReg f,   (vs, fs, ds, ls))
assign_reg DoubleArg (vs, fs, d:ds, ls) = Just (DoubleReg d,  (vs, fs, ds, ls))
assign_reg LongArg   (vs, fs, ds, l:ls) = Just (LongReg l,    (vs, fs, ds, ls))
assign_reg PtrArg    (v:vs, fs, ds, ls) = Just (VanillaReg v, (vs, fs, ds, ls))
assign_reg NonPtrArg (v:vs, fs, ds, ls) = Just (VanillaReg v, (vs, fs, ds, ls))
    -- PtrArg and NonPtrArg both go in a vanilla register
assign_reg other     not_enough_regs    = Nothing


-------------------------------------------------------------------------
--
--		Register supplies
--
-------------------------------------------------------------------------

-- Vanilla registers can contain pointers, Ints, Chars.
-- Floats and doubles have separate register supplies.
--
-- We take these register supplies from the *real* registers, i.e. those
-- that are guaranteed to map to machine registers.

useVanillaRegs | opt_Unregisterised = 0
	       | otherwise          = mAX_Real_Vanilla_REG
useFloatRegs   | opt_Unregisterised = 0
	       | otherwise          = mAX_Real_Float_REG
useDoubleRegs  | opt_Unregisterised = 0
	       | otherwise          = mAX_Real_Double_REG
useLongRegs    | opt_Unregisterised = 0
	       | otherwise          = mAX_Real_Long_REG

vanillaRegNos, floatRegNos, doubleRegNos, longRegNos :: [Int]
vanillaRegNos	 = regList useVanillaRegs
floatRegNos	 = regList useFloatRegs
doubleRegNos	 = regList useDoubleRegs
longRegNos       = regList useLongRegs

allVanillaRegNos, allFloatRegNos, allDoubleRegNos, allLongRegNos :: [Int]
allVanillaRegNos = regList mAX_Vanilla_REG
allFloatRegNos	 = regList mAX_Float_REG
allDoubleRegNos	 = regList mAX_Double_REG
allLongRegNos	 = regList mAX_Long_REG

regList 0 = []
regList n = [1 .. n]

type AvailRegs = ( [Int]   -- available vanilla regs.
		 , [Int]   -- floats
		 , [Int]   -- doubles
		 , [Int]   -- longs (int64 and word64)
		 )

mkRegTbl :: [GlobalReg] -> AvailRegs
mkRegTbl regs_in_use
  = mkRegTbl' regs_in_use vanillaRegNos floatRegNos doubleRegNos longRegNos

mkRegTbl_allRegs :: [GlobalReg] -> AvailRegs
mkRegTbl_allRegs regs_in_use
  = mkRegTbl' regs_in_use allVanillaRegNos allFloatRegNos allDoubleRegNos allLongRegNos

mkRegTbl' regs_in_use vanillas floats doubles longs
  = (ok_vanilla, ok_float, ok_double, ok_long)
  where
    ok_vanilla = mapCatMaybes (select VanillaReg) vanillas
    ok_float   = mapCatMaybes (select FloatReg)	  floats
    ok_double  = mapCatMaybes (select DoubleReg)  doubles
    ok_long    = mapCatMaybes (select LongReg)    longs   
				    -- rep isn't looked at, hence we can use any old rep.

    select :: (Int -> GlobalReg) -> Int{-cand-} -> Maybe Int
	-- one we've unboxed the Int, we make a GlobalReg
	-- and see if it is already in use; if not, return its number.

    select mk_reg_fun cand
      = let
	    reg = mk_reg_fun cand
	in
	if reg `not_elem` regs_in_use
	then Just cand
	else Nothing
      where
	not_elem = isn'tIn "mkRegTbl"


