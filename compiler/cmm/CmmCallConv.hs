module CmmCallConv (
  ParamLocation(..),
  ArgumentFormat,
  assignRegs,
  assignArguments,
) where

#include "HsVersions.h"

import Cmm
import MachOp
import SMRep

import Constants
import StaticFlags (opt_Unregisterised)
import Panic

data ParamLocation
  = RegisterParam GlobalReg
  | StackParam WordOff

assignRegs :: [LocalReg] -> ArgumentFormat LocalReg
assignRegs regs = assignRegs' regs 0 availRegs
    where
      assignRegs' (r:rs) offset availRegs = (r,assignment):assignRegs' rs new_offset remaining
          where 
            (assignment, new_offset, remaining) = assign_reg (localRegRep r) offset availRegs

assignArguments :: (a -> MachRep) -> [a] -> ArgumentFormat a
assignArguments f reps = assignArguments' reps 0 availRegs
    where
      assignArguments' [] offset availRegs = []
      assignArguments' (r:rs) offset availRegs = (r,assignment):assignArguments' rs new_offset remaining
          where 
            (assignment, new_offset, remaining) = assign_reg (f r) offset availRegs

type ArgumentFormat a = [(a, ParamLocation)]

type AvailRegs = ( [GlobalReg]   -- available vanilla regs.
		 , [GlobalReg]   -- floats
		 , [GlobalReg]   -- doubles
		 , [GlobalReg]   -- longs (int64 and word64)
		 )

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

availRegs = (regList VanillaReg useVanillaRegs,
             regList FloatReg useFloatRegs,
             regList DoubleReg useDoubleRegs,
             regList LongReg useLongRegs)
    where
      regList f max = map f [1 .. max]

slot_size :: LocalReg -> Int
slot_size reg = ((machRepByteWidth (localRegRep reg) - 1) `div` wORD_SIZE) + 1

slot_size' :: MachRep -> Int
slot_size' reg = ((machRepByteWidth reg - 1) `div` wORD_SIZE) + 1

assign_reg :: MachRep -> WordOff -> AvailRegs -> (ParamLocation, WordOff, AvailRegs)
assign_reg I8  off (v:vs, fs, ds, ls) = (RegisterParam $ v, off, (vs, fs, ds, ls))
assign_reg I16 off (v:vs, fs, ds, ls) = (RegisterParam $ v, off, (vs, fs, ds, ls))
assign_reg I32 off (v:vs, fs, ds, ls) = (RegisterParam $ v, off, (vs, fs, ds, ls))
assign_reg I64 off (vs, fs, ds, l:ls) = (RegisterParam $ l, off, (vs, fs, ds, ls))
assign_reg I128 off _                 = panic "I128 is not a supported register type"
assign_reg F32 off (vs, f:fs, ds, ls) = (RegisterParam $ f, off, (vs, fs, ds, ls))
assign_reg F64 off (vs, fs, d:ds, ls) = (RegisterParam $ d, off, (vs, fs, ds, ls))
assign_reg F80 off _                  = panic "F80 is not a supported register type"
assign_reg reg off _                  = (StackParam $ off - size, off - size, ([], [], [], [])) where size = slot_size' reg
