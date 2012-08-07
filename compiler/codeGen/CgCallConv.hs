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
        mkArgDescr,

        -- Liveness
        mkRegLiveness,

        -- Register assignment
        assignCallRegs, assignReturnRegs, assignPrimOpCallRegs,

        -- Calls
        constructSlowCall, slowArgs, slowCallPattern,

        -- Returns
        dataReturnConvPrim,
        getSequelAmode
    ) where

import CgMonad
import CgProf
import SMRep

import OldCmm
import CLabel

import Constants
import CgStackery
import ClosureInfo( CgRep(..), nonVoidArg, idCgRep, cgRepSizeW, isFollowableArg )
import OldCmmUtils
import Maybes
import Id
import Name
import Util
import DynFlags
import Module
import FastString
import Outputable
import Platform
import Data.Bits

-------------------------------------------------------------------------
--
--      Making argument descriptors
--
--  An argument descriptor describes the layout of args on the stack,
--  both for    * GC (stack-layout) purposes, and
--              * saving/restoring registers when a heap-check fails
--
-- Void arguments aren't important, therefore (contrast constructSlowCall)
--
-------------------------------------------------------------------------

-- bring in ARG_P, ARG_N, etc.
#include "../includes/rts/storage/FunTypes.h"

-------------------------
mkArgDescr :: Name -> [Id] -> FCode ArgDescr
mkArgDescr _nm args
  = case stdPattern arg_reps of
        Just spec_id -> return (ArgSpec spec_id)
        Nothing      -> return (ArgGen arg_bits)
  where
    arg_bits = argBits arg_reps
    arg_reps = filter nonVoidArg (map idCgRep args)
        -- Getting rid of voids eases matching of standard patterns

argBits :: [CgRep] -> [Bool]    -- True for non-ptr, False for ptr
argBits []              = []
argBits (PtrArg : args) = False : argBits args
argBits (arg    : args) = take (cgRepSizeW arg) (repeat True) ++ argBits args

stdPattern :: [CgRep] -> Maybe StgHalfWord
stdPattern []          = Just ARG_NONE  -- just void args, probably

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
stdPattern [NonPtrArg,PtrArg,PtrArg]       = Just ARG_NPP
stdPattern [PtrArg,NonPtrArg,NonPtrArg]    = Just ARG_PNN
stdPattern [PtrArg,NonPtrArg,PtrArg]       = Just ARG_PNP
stdPattern [PtrArg,PtrArg,NonPtrArg]       = Just ARG_PPN
stdPattern [PtrArg,PtrArg,PtrArg]          = Just ARG_PPP

stdPattern [PtrArg,PtrArg,PtrArg,PtrArg]               = Just ARG_PPPP
stdPattern [PtrArg,PtrArg,PtrArg,PtrArg,PtrArg]        = Just ARG_PPPPP
stdPattern [PtrArg,PtrArg,PtrArg,PtrArg,PtrArg,PtrArg] = Just ARG_PPPPPP
stdPattern _ = Nothing


-------------------------------------------------------------------------
--
--              Bitmap describing register liveness
--              across GC when doing a "generic" heap check
--              (a RET_DYN stack frame).
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
    reg_bits ((id, VanillaReg i _) : regs) | isFollowableArg (idCgRep id)
        = (1 `shiftL` (i - 1)) .|. reg_bits regs
    reg_bits (_ : regs)
        = reg_bits regs

-------------------------------------------------------------------------
--
--              Pushing the arguments for a slow call
--
-------------------------------------------------------------------------

-- For a slow call, we must take a bunch of arguments and intersperse
-- some stg_ap_<pattern>_ret_info return addresses.
constructSlowCall
        :: [(CgRep,CmmExpr)]
        -> (CLabel,             -- RTS entry point for call
           [(CgRep,CmmExpr)],   -- args to pass to the entry point
           [(CgRep,CmmExpr)])   -- stuff to save on the stack

   -- don't forget the zero case
constructSlowCall []
  = (mkRtsApFastLabel (fsLit "stg_ap_0"), [], [])

constructSlowCall amodes
  = (stg_ap_pat, these, rest)
  where
    stg_ap_pat = mkRtsApFastLabel arg_pat
    (arg_pat, these, rest) = matchSlowPattern amodes

-- | 'slowArgs' takes a list of function arguments and prepares them for
-- pushing on the stack for "extra" arguments to a function which requires
-- fewer arguments than we currently have.
slowArgs :: DynFlags -> [(CgRep,CmmExpr)] -> [(CgRep,CmmExpr)]
slowArgs _ [] = []
slowArgs dflags amodes
  | dopt Opt_SccProfilingOn dflags = save_cccs ++ this_pat ++ slowArgs dflags rest
  | otherwise                      =              this_pat ++ slowArgs dflags rest
  where
    (arg_pat, args, rest) = matchSlowPattern amodes
    stg_ap_pat = mkCmmRetInfoLabel rtsPackageId arg_pat
    this_pat   = (NonPtrArg, mkLblExpr stg_ap_pat) : args
    save_cccs  = [(NonPtrArg, mkLblExpr save_cccs_lbl), (NonPtrArg, curCCS)]
    save_cccs_lbl = mkCmmRetInfoLabel rtsPackageId (fsLit "stg_restore_cccs")

matchSlowPattern :: [(CgRep,CmmExpr)]
                 -> (FastString, [(CgRep,CmmExpr)], [(CgRep,CmmExpr)])
matchSlowPattern amodes = (arg_pat, these, rest)
  where (arg_pat, n)  = slowCallPattern (map fst amodes)
        (these, rest) = splitAt n amodes

-- These cases were found to cover about 99% of all slow calls:
slowCallPattern :: [CgRep] -> (FastString, Int)
slowCallPattern (PtrArg: PtrArg: PtrArg: PtrArg: PtrArg: PtrArg: _) = (fsLit "stg_ap_pppppp", 6)
slowCallPattern (PtrArg: PtrArg: PtrArg: PtrArg: PtrArg: _)         = (fsLit "stg_ap_ppppp", 5)
slowCallPattern (PtrArg: PtrArg: PtrArg: PtrArg: _)     = (fsLit "stg_ap_pppp", 4)
slowCallPattern (PtrArg: PtrArg: PtrArg: VoidArg: _)    = (fsLit "stg_ap_pppv", 4)
slowCallPattern (PtrArg: PtrArg: PtrArg: _)             = (fsLit "stg_ap_ppp", 3)
slowCallPattern (PtrArg: PtrArg: VoidArg: _)            = (fsLit "stg_ap_ppv", 3)
slowCallPattern (PtrArg: PtrArg: _)                     = (fsLit "stg_ap_pp", 2)
slowCallPattern (PtrArg: VoidArg: _)                    = (fsLit "stg_ap_pv", 2)
slowCallPattern (PtrArg: _)                             = (fsLit "stg_ap_p", 1)
slowCallPattern (VoidArg: _)                            = (fsLit "stg_ap_v", 1)
slowCallPattern (NonPtrArg: _)                          = (fsLit "stg_ap_n", 1)
slowCallPattern (FloatArg: _)                           = (fsLit "stg_ap_f", 1)
slowCallPattern (DoubleArg: _)                          = (fsLit "stg_ap_d", 1)
slowCallPattern (LongArg: _)                            = (fsLit "stg_ap_l", 1)
slowCallPattern _                                       = panic "CgStackery.slowCallPattern"

-------------------------------------------------------------------------
--
--              Return conventions
--
-------------------------------------------------------------------------

dataReturnConvPrim :: CgRep -> CmmReg
dataReturnConvPrim PtrArg    = CmmGlobal (VanillaReg 1 VGcPtr)
dataReturnConvPrim NonPtrArg = CmmGlobal (VanillaReg 1 VNonGcPtr)
dataReturnConvPrim LongArg   = CmmGlobal (LongReg 1)
dataReturnConvPrim FloatArg  = CmmGlobal (FloatReg 1)
dataReturnConvPrim DoubleArg = CmmGlobal (DoubleReg 1)
dataReturnConvPrim VoidArg   = panic "dataReturnConvPrim: void"


-- getSequelAmode returns an amode which refers to an info table.  The info
-- table will always be of the RET_(BIG|SMALL) kind.  We're careful
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
  = do  { EndOfBlockInfo virt_sp sequel <- getEndOfBlockInfo
        ; case sequel of
            OnStack -> do { sp_rel <- getSpRelOffset virt_sp
                          ; returnFC (CmmLoad sp_rel bWord) }

            CaseAlts lbl _ _  -> returnFC (CmmLit (CmmLabel lbl))
        }

-------------------------------------------------------------------------
--
--              Register assignment
--
-------------------------------------------------------------------------

--  How to assign registers for
--
--      1) Calling a fast entry point.
--      2) Returning an unboxed tuple.
--      3) Invoking an out-of-line PrimOp.
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

type AssignRegs a = [(CgRep,a)]          -- Arg or result values to assign
                 -> ([(a, GlobalReg)],   -- Register assignment in same order
                                         -- for *initial segment of* input list
                                         --   (but reversed; doesn't matter)
                                         -- VoidRep args do not appear here
                     [(CgRep,a)])        -- Leftover arg or result values

assignCallRegs       :: DynFlags -> AssignRegs a
assignPrimOpCallRegs ::             AssignRegs a
assignReturnRegs     :: DynFlags -> AssignRegs a

assignCallRegs dflags args
  = assign_regs args (mkRegTbl dflags [node])
        -- The entry convention for a function closure
        -- never uses Node for argument passing; instead
        -- Node points to the function closure itself

assignPrimOpCallRegs args
 = assign_regs args (mkRegTbl_allRegs [])
        -- For primops, *all* arguments must be passed in registers

assignReturnRegs dflags args
 -- when we have a single non-void component to return, use the normal
 -- unpointed return convention.  This make various things simpler: it
 -- means we can assume a consistent convention for IO, which is useful
 -- when writing code that relies on knowing the IO return convention in
 -- the RTS (primops, especially exception-related primops).
 -- Also, the bytecode compiler assumes this when compiling
 -- case expressions and ccalls, so it only needs to know one set of
 -- return conventions.
 | [(rep,arg)] <- non_void_args, CmmGlobal r <- dataReturnConvPrim rep
    = ([(arg, r)], [])
 | otherwise
    = assign_regs args (mkRegTbl dflags [])
        -- For returning unboxed tuples etc,
        -- we use all regs
 where
       non_void_args = filter ((/= VoidArg).fst) args

assign_regs :: [(CgRep,a)]      -- Arg or result values to assign
            -> AvailRegs        -- Regs still avail: Vanilla, Float, Double, Longs
            -> ([(a, GlobalReg)], [(CgRep, a)])
assign_regs args supply
  = go args [] supply
  where
    go [] acc _ = (acc, [])     -- Return the results reversed (doesn't matter)
    go ((VoidArg,_) : args) acc supply  -- Skip void arguments; they aren't passed, and
        = go args acc supply            -- there's nothing to bind them to
    go ((rep,arg) : args) acc supply
        = case assign_reg rep supply of
                Just (reg, supply') -> go args ((arg,reg):acc) supply'
                Nothing             -> (acc, (rep,arg):args)    -- No more regs

assign_reg :: CgRep -> AvailRegs -> Maybe (GlobalReg, AvailRegs)
assign_reg FloatArg  (vs, f:fs, ds, ls) = Just (FloatReg f,   (vs, fs, ds, ls))
assign_reg DoubleArg (vs, fs, d:ds, ls) = Just (DoubleReg d,  (vs, fs, ds, ls))
assign_reg LongArg   (vs, fs, ds, l:ls) = Just (LongReg l,    (vs, fs, ds, ls))
assign_reg PtrArg    (v:vs, fs, ds, ls) = Just (VanillaReg v VGcPtr, (vs, fs, ds, ls))
assign_reg NonPtrArg (v:vs, fs, ds, ls) = Just (VanillaReg v VNonGcPtr, (vs, fs, ds, ls))
    -- PtrArg and NonPtrArg both go in a vanilla register
assign_reg _         _                  = Nothing


-------------------------------------------------------------------------
--
--              Register supplies
--
-------------------------------------------------------------------------

-- Vanilla registers can contain pointers, Ints, Chars.
-- Floats and doubles have separate register supplies.
--
-- We take these register supplies from the *real* registers, i.e. those
-- that are guaranteed to map to machine registers.

useVanillaRegs :: DynFlags -> Int
useVanillaRegs dflags
 | platformUnregisterised (targetPlatform dflags) = 0
 | otherwise                                      = mAX_Real_Vanilla_REG
useFloatRegs :: DynFlags -> Int
useFloatRegs dflags
 | platformUnregisterised (targetPlatform dflags) = 0
 | otherwise                                      = mAX_Real_Float_REG
useDoubleRegs :: DynFlags -> Int
useDoubleRegs dflags
 | platformUnregisterised (targetPlatform dflags) = 0
 | otherwise                                      = mAX_Real_Double_REG
useLongRegs :: DynFlags -> Int
useLongRegs dflags
 | platformUnregisterised (targetPlatform dflags) = 0
 | otherwise                                      = mAX_Real_Long_REG

vanillaRegNos, floatRegNos, doubleRegNos, longRegNos :: DynFlags -> [Int]
vanillaRegNos dflags = regList $ useVanillaRegs dflags
floatRegNos   dflags = regList $ useFloatRegs   dflags
doubleRegNos  dflags = regList $ useDoubleRegs  dflags
longRegNos    dflags = regList $ useLongRegs    dflags

allVanillaRegNos, allFloatRegNos, allDoubleRegNos, allLongRegNos :: [Int]
allVanillaRegNos = regList mAX_Vanilla_REG
allFloatRegNos   = regList mAX_Float_REG
allDoubleRegNos  = regList mAX_Double_REG
allLongRegNos    = regList mAX_Long_REG

regList :: Int -> [Int]
regList n = [1 .. n]

type AvailRegs = ( [Int]   -- available vanilla regs.
                 , [Int]   -- floats
                 , [Int]   -- doubles
                 , [Int]   -- longs (int64 and word64)
                 )

mkRegTbl :: DynFlags -> [GlobalReg] -> AvailRegs
mkRegTbl dflags regs_in_use
  = mkRegTbl' regs_in_use (vanillaRegNos dflags)
                          (floatRegNos   dflags)
                          (doubleRegNos  dflags)
                          (longRegNos    dflags)

mkRegTbl_allRegs :: [GlobalReg] -> AvailRegs
mkRegTbl_allRegs regs_in_use
  = mkRegTbl' regs_in_use allVanillaRegNos allFloatRegNos allDoubleRegNos allLongRegNos

mkRegTbl' :: [GlobalReg] -> [Int] -> [Int] -> [Int] -> [Int]
          -> ([Int], [Int], [Int], [Int])
mkRegTbl' regs_in_use vanillas floats doubles longs
  = (ok_vanilla, ok_float, ok_double, ok_long)
  where
    ok_vanilla = mapCatMaybes (select (\i -> VanillaReg i VNonGcPtr)) vanillas
                    -- ptrhood isn't looked at, hence we can use any old rep.
    ok_float   = mapCatMaybes (select FloatReg)   floats
    ok_double  = mapCatMaybes (select DoubleReg)  doubles
    ok_long    = mapCatMaybes (select LongReg)    longs

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


