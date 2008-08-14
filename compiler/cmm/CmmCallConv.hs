{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/CodingStyle#Warnings
-- for details

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

import Constants
import StaticFlags (opt_Unregisterised)
import Outputable
import Panic

-- Calculate the 'GlobalReg' or stack locations for function call
-- parameters as used by the Cmm calling convention.

data ParamLocation a
  = RegisterParam GlobalReg
  | StackParam a

type ArgumentFormat a b = [(a, ParamLocation b)]

-- Stack parameters are returned as word offsets.
assignArguments :: (a -> CmmType) -> [a] -> ArgumentFormat a WordOff
assignArguments f reps = assignments
    where
      (sizes, assignments) = unzip $ assignArguments' reps (negate (sum sizes)) availRegs
      assignArguments' [] offset availRegs = []
      assignArguments' (r:rs) offset availRegs =
          (size,(r,assignment)):assignArguments' rs new_offset remaining
          where 
            (assignment, new_offset, size, remaining) =
                assign_reg False assign_slot_up (f r) offset availRegs

-- | JD: For the new stack story, I want arguments passed on the stack to manifest as
-- positive offsets in a CallArea, not negative offsets from the stack pointer.
-- Also, I want byte offsets, not word offsets.
-- The first argument tells us whether we are assigning positions for call arguments
-- or return results. The distinction matters because we reserve different
-- global registers in each case.
assignArgumentsPos :: Bool -> (a -> CmmType) -> [a] -> ArgumentFormat a ByteOff
assignArgumentsPos isCall arg_ty reps = map cvt assignments
    where
      (sizes, assignments) = unzip $ assignArguments' reps 0 availRegs
      assignArguments' [] _ _ = []
      assignArguments' (r:rs) offset avails =
          (size,(r,assignment)):assignArguments' rs new_offset remaining
          where 
            (assignment, new_offset, size, remaining) =
                assign_reg isCall assign_slot_down (arg_ty r) offset avails
      cvt (l, RegisterParam r) = (l, RegisterParam r)
      cvt (l, StackParam off)  = (l, StackParam $ off * wORD_SIZE)

argumentsSize :: (a -> CmmType) -> [a] -> WordOff
argumentsSize f reps = maximum (0 : map arg_top args)
    where
      args = assignArguments f reps
      arg_top (a, StackParam offset) = -offset
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

-- Round the size of a local register up to the nearest word.
slot_size :: LocalReg -> Int
slot_size reg = slot_size' (typeWidth (localRegType reg))

slot_size' :: Width -> Int
slot_size' reg = ((widthInBytes reg - 1) `div` wORD_SIZE) + 1

type Assignment = (ParamLocation WordOff, WordOff, WordOff, AvailRegs)
type SlotAssigner = Width -> Int -> AvailRegs -> Assignment

assign_reg :: Bool -> SlotAssigner -> CmmType -> WordOff -> AvailRegs -> Assignment
assign_reg isCall slot ty off avails
  | isFloatType ty = assign_float_reg        slot width off avails
  | otherwise      = assign_bits_reg  isCall slot width off gcp avails
  where
    width = typeWidth ty
    gcp | isGcPtrType ty = VGcPtr
	| otherwise  	 = VNonGcPtr

-- Assigning a slot on a stack that grows up:
-- JD: I don't know why this convention stops using all the registers
--     after running out of one class of registers.
assign_slot_up :: SlotAssigner
assign_slot_up width off regs =
  (StackParam $ off, off + size, size, ([], [], [], [])) where size = slot_size' width

-- Assigning a slot on a stack that grows down:
assign_slot_down :: SlotAssigner
assign_slot_down width off regs =
  (StackParam $ off + size, off + size, size, ([], [], [], []))
  where size = slot_size' width

-- On calls, `node` is used to hold the closure that is entered, so we can't
-- pass arguments in that register.
assign_bits_reg _ _ W128 _ _ _ = panic "I128 is not a supported register type"
assign_bits_reg isCall assign_slot w off gcp regs@(v:vs, fs, ds, ls) =
  if isCall && v gcp == node then
    assign_bits_reg isCall assign_slot w off gcp (vs, fs, ds, ls)
  else if widthInBits w <= widthInBits wordWidth then
    (RegisterParam (v gcp), off, 0, (vs, fs, ds, ls))
  else assign_slot w off regs

assign_float_reg _ W32 off (vs, f:fs, ds, ls) = (RegisterParam $ f, off, 0, (vs, fs, ds, ls))
assign_float_reg _ W64 off (vs, fs, d:ds, ls) = (RegisterParam $ d, off, 0, (vs, fs, ds, ls))
assign_float_reg _ W80 off _                  = panic "F80 is not a supported register type"
assign_float_reg assign_slot width off r = assign_slot width off r
