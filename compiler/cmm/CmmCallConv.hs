module CmmCallConv (
  ParamLocation(..),
  ArgumentFormat,
  assignArguments,
  assignArgumentsPos,
  argumentsSize,
) where

#include "HsVersions.h"

import Cmm
import SMRep
import ZipCfgCmmRep (Convention(..))

import Constants
import StaticFlags (opt_Unregisterised)
import Outputable

-- Calculate the 'GlobalReg' or stack locations for function call
-- parameters as used by the Cmm calling convention.

data ParamLocation a
  = RegisterParam GlobalReg
  | StackParam a

instance (Outputable a) => Outputable (ParamLocation a) where
  ppr (RegisterParam g) = ppr g
  ppr (StackParam p)    = ppr p

type ArgumentFormat a b = [(a, ParamLocation b)]

-- Stack parameters are returned as word offsets.
assignArguments :: (a -> CmmType) -> [a] -> ArgumentFormat a WordOff
assignArguments f reps = assignments
    where
      availRegs = getRegsWithNode
      (sizes, assignments) = unzip $ assignArguments' reps (negate (sum sizes)) availRegs
      assignArguments' [] _ _ = []
      assignArguments' (r:rs) offset availRegs =
          (size,(r,assignment)):assignArguments' rs new_offset remaining
          where 
            (assignment, new_offset, size, remaining) =
                assign_reg assign_slot_neg (f r) offset availRegs

-- | JD: For the new stack story, I want arguments passed on the stack to manifest as
-- positive offsets in a CallArea, not negative offsets from the stack pointer.
-- Also, I want byte offsets, not word offsets.
assignArgumentsPos :: (Outputable a) => Convention -> (a -> CmmType) -> [a] ->
                      ArgumentFormat a ByteOff
assignArgumentsPos conv arg_ty reps = map cvt assignments
    where -- The calling conventions (CgCallConv.hs) are complicated, to say the least
      regs = case (reps, conv) of
               (_,   NativeNodeCall)   -> getRegsWithNode
               (_,   NativeDirectCall) -> getRegsWithoutNode
               ([_], NativeReturn)     -> allRegs
               (_,   NativeReturn)     -> getRegsWithNode
               (_,   GC)               -> getRegsWithNode
               (_,   PrimOpCall)       -> allRegs
               ([_], PrimOpReturn)     -> allRegs
               (_,   PrimOpReturn)     -> getRegsWithNode
               (_,   Slow)             -> noRegs
               _ -> pprPanic "Unknown calling convention" (ppr conv)
      (sizes, assignments) = unzip $ assignArguments' reps (sum sizes) regs
      assignArguments' [] _ _ = []
      assignArguments' (r:rs) offset avails =
          (size, (r,assignment)):assignArguments' rs new_offset remaining
          where 
            (assignment, new_offset, size, remaining) =
                assign_reg assign_slot_pos (arg_ty r) offset avails
      cvt (l, RegisterParam r) = (l, RegisterParam r)
      cvt (l, StackParam off)  = (l, StackParam $ off * wORD_SIZE)

argumentsSize :: (a -> CmmType) -> [a] -> WordOff
argumentsSize f reps = maximum (0 : map arg_top args)
    where
      args = assignArguments f reps
      arg_top (_, StackParam offset) = -offset
      arg_top (_, RegisterParam _) = 0

-----------------------------------------------------------------------------
-- Local information about the registers available

type AvailRegs = ( [VGcPtr -> GlobalReg]   -- available vanilla regs.
		 , [GlobalReg]   -- floats
		 , [GlobalReg]   -- doubles
		 , [GlobalReg]   -- longs (int64 and word64)
		 )

-- Vanilla registers can contain pointers, Ints, Chars.
-- Floats and doubles have separate register supplies.
--
-- We take these register supplies from the *real* registers, i.e. those
-- that are guaranteed to map to machine registers.

vanillaRegNos, floatRegNos, doubleRegNos, longRegNos :: [Int]
vanillaRegNos | opt_Unregisterised = []
              | otherwise          = regList mAX_Real_Vanilla_REG
floatRegNos	  | opt_Unregisterised = []
              | otherwise          = regList mAX_Real_Float_REG
doubleRegNos  | opt_Unregisterised = []
              | otherwise          = regList mAX_Real_Double_REG
longRegNos	  | opt_Unregisterised = []
              | otherwise          = regList mAX_Real_Long_REG

-- 
getRegsWithoutNode, getRegsWithNode :: AvailRegs
getRegsWithoutNode =
  (filter (\r -> r VGcPtr /= node) intRegs,
   map FloatReg  floatRegNos, map DoubleReg doubleRegNos, map LongReg longRegNos)
    where intRegs = map VanillaReg vanillaRegNos
getRegsWithNode =
  (intRegs, map FloatReg  floatRegNos, map DoubleReg doubleRegNos, map LongReg longRegNos)
    where intRegs = map VanillaReg vanillaRegNos

allVanillaRegNos, allFloatRegNos, allDoubleRegNos, allLongRegNos :: [Int]
allVanillaRegNos = regList mAX_Vanilla_REG
allFloatRegNos	 = regList mAX_Float_REG
allDoubleRegNos	 = regList mAX_Double_REG
allLongRegNos	   = regList mAX_Long_REG

regList :: Int -> [Int]
regList n = [1 .. n]

allRegs :: AvailRegs
allRegs = (map VanillaReg allVanillaRegNos, map FloatReg allFloatRegNos,
           map DoubleReg  allDoubleRegNos,  map LongReg  allLongRegNos)

noRegs :: AvailRegs
noRegs    = ([], [], [], [])

-- Round the size of a local register up to the nearest word.
{-
UNUSED 2008-12-29

slot_size :: LocalReg -> Int
slot_size reg = slot_size' (typeWidth (localRegType reg))
-}

slot_size' :: Width -> Int
slot_size' reg = ((widthInBytes reg - 1) `div` wORD_SIZE) + 1

type Assignment = (ParamLocation WordOff, WordOff, WordOff, AvailRegs)
type SlotAssigner = Width -> Int -> AvailRegs -> Assignment

assign_reg :: SlotAssigner -> CmmType -> WordOff -> AvailRegs -> Assignment
assign_reg slot ty off avails
  | isFloatType ty = assign_float_reg slot width off avails
  | otherwise      = assign_bits_reg  slot width off gcp avails
  where
    width = typeWidth ty
    gcp | isGcPtrType ty = VGcPtr
	| otherwise  	 = VNonGcPtr

-- Assigning a slot using negative offsets from the stack pointer.
-- JD: I don't know why this convention stops using all the registers
--     after running out of one class of registers.
assign_slot_neg :: SlotAssigner
assign_slot_neg width off _regs =
  (StackParam $ off, off + size, size, ([], [], [], [])) where size = slot_size' width

-- Assigning a slot using positive offsets into a CallArea.
assign_slot_pos :: SlotAssigner
assign_slot_pos width off _regs =
  (StackParam $ off, off - size, size, ([], [], [], []))
  where size = slot_size' width

-- On calls in the native convention, `node` is used to hold the environment
-- for the closure, so we can't pass arguments in that register.
assign_bits_reg :: SlotAssigner -> Width -> WordOff -> VGcPtr -> AvailRegs -> Assignment
assign_bits_reg _ W128 _ _ _ = panic "W128 is not a supported register type"
assign_bits_reg _ w off gcp (v:vs, fs, ds, ls)
  | widthInBits w <= widthInBits wordWidth =
        (RegisterParam (v gcp), off, 0, (vs, fs, ds, ls))
assign_bits_reg _ w off _ (vs, fs, ds, l:ls)
  | widthInBits w > widthInBits wordWidth =
        (RegisterParam l, off, 0, (vs, fs, ds, ls))
assign_bits_reg assign_slot w off _ regs@(_, _, _, _) = assign_slot w off regs

assign_float_reg :: SlotAssigner -> Width -> WordOff -> AvailRegs -> Assignment
assign_float_reg _ W32 off (vs, f:fs, ds, ls) = (RegisterParam $ f, off, 0, (vs, fs, ds, ls))
assign_float_reg _ W64 off (vs, fs, d:ds, ls) = (RegisterParam $ d, off, 0, (vs, fs, ds, ls))
assign_float_reg _ W80 _   _                  = panic "F80 is not a supported register type"
assign_float_reg assign_slot width off r = assign_slot width off r
