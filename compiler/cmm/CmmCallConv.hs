{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module CmmCallConv (
  ParamLocation(..),
  assignArgumentsPos,
  globalArgRegs
) where

#include "HsVersions.h"

import CmmExpr
import SMRep
import Cmm (Convention(..))
import PprCmm ()

import Constants
import qualified Data.List as L
import StaticFlags (opt_Unregisterised)
import Outputable

-- Calculate the 'GlobalReg' or stack locations for function call
-- parameters as used by the Cmm calling convention.

data ParamLocation
  = RegisterParam GlobalReg
  | StackParam ByteOff

instance Outputable ParamLocation where
  ppr (RegisterParam g) = ppr g
  ppr (StackParam p)    = ppr p

-- | JD: For the new stack story, I want arguments passed on the stack to manifest as
-- positive offsets in a CallArea, not negative offsets from the stack pointer.
-- Also, I want byte offsets, not word offsets.
assignArgumentsPos :: Convention -> (a -> CmmType) -> [a] ->
                      [(a, ParamLocation)]
-- Given a list of arguments, and a function that tells their types,
-- return a list showing where each argument is passed
assignArgumentsPos conv arg_ty reps = assignments
    where -- The calling conventions (CgCallConv.hs) are complicated, to say the least
      regs = case (reps, conv) of
               (_,   NativeNodeCall)   -> getRegsWithNode
               (_,   NativeDirectCall) -> getRegsWithoutNode
               ([_], NativeReturn)     -> allRegs
               (_,   NativeReturn)     -> getRegsWithNode
               -- GC calling convention *must* put values in registers
               (_,   GC)               -> allRegs
               (_,   PrimOpCall)       -> allRegs
               ([_], PrimOpReturn)     -> allRegs
               (_,   PrimOpReturn)     -> getRegsWithNode
               (_,   Slow)             -> noRegs
      -- The calling conventions first assign arguments to registers,
      -- then switch to the stack when we first run out of registers
      -- (even if there are still available registers for args of a different type).
      -- When returning an unboxed tuple, we also separate the stack
      -- arguments by pointerhood.
      (reg_assts, stk_args) = assign_regs [] reps regs
      stk_args' = case conv of NativeReturn -> part
                               PrimOpReturn -> part
                               GC | length stk_args /= 0 -> panic "Failed to allocate registers for GC call"
                               _            -> stk_args
                  where part = uncurry (++)
                                       (L.partition (not . isGcPtrType . arg_ty) stk_args)
      stk_assts = assign_stk 0 [] (reverse stk_args')
      assignments = reg_assts ++ stk_assts

      assign_regs assts []     _    = (assts, [])
      assign_regs assts (r:rs) regs = if isFloatType ty then float else int
        where float = case (w, regs) of
                        (W32, (vs, f:fs, ds, ls)) -> k (RegisterParam f, (vs, fs, ds, ls))
                        (W64, (vs, fs, d:ds, ls)) -> k (RegisterParam d, (vs, fs, ds, ls))
                        (W80, _) -> panic "F80 unsupported register type"
                        _ -> (assts, (r:rs))
              int = case (w, regs) of
                      (W128, _) -> panic "W128 unsupported register type"
                      (_, (v:vs, fs, ds, ls)) | widthInBits w <= widthInBits wordWidth
                          -> k (RegisterParam (v gcp), (vs, fs, ds, ls))
                      (_, (vs, fs, ds, l:ls)) | widthInBits w > widthInBits wordWidth
                          -> k (RegisterParam l, (vs, fs, ds, ls))
                      _   -> (assts, (r:rs))
              k (asst, regs') = assign_regs ((r, asst) : assts) rs regs'
              ty = arg_ty r
              w  = typeWidth ty
              gcp | isGcPtrType ty = VGcPtr
                  | otherwise  	   = VNonGcPtr

      assign_stk _      assts [] = assts
      assign_stk offset assts (r:rs) = assign_stk off' ((r, StackParam off') : assts) rs
        where w    = typeWidth (arg_ty r)
              size = (((widthInBytes w - 1) `div` wORD_SIZE) + 1) * wORD_SIZE
              off' = offset + size

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

allFloatRegs, allDoubleRegs, allLongRegs :: [GlobalReg]
allVanillaRegs :: [VGcPtr -> GlobalReg]

allVanillaRegs = map VanillaReg $ regList mAX_Vanilla_REG
allFloatRegs   = map FloatReg   $ regList mAX_Float_REG
allDoubleRegs  = map DoubleReg  $ regList mAX_Double_REG
allLongRegs    = map LongReg    $ regList mAX_Long_REG

regList :: Int -> [Int]
regList n = [1 .. n]

allRegs :: AvailRegs
allRegs  = (allVanillaRegs, allFloatRegs, allDoubleRegs, allLongRegs)

noRegs :: AvailRegs
noRegs  = ([], [], [], [])

globalArgRegs :: [GlobalReg]
globalArgRegs = map ($VGcPtr) allVanillaRegs ++
                allFloatRegs ++
                allDoubleRegs ++
                allLongRegs
