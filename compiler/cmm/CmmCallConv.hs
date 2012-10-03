{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module CmmCallConv (
  ParamLocation(..),
  assignArgumentsPos,
  assignStack,
  globalArgRegs, realArgRegs
) where

#include "HsVersions.h"

import CmmExpr
import SMRep
import Cmm (Convention(..))
import PprCmm ()

import DynFlags
import Outputable

-- Calculate the 'GlobalReg' or stack locations for function call
-- parameters as used by the Cmm calling convention.

data ParamLocation
  = RegisterParam GlobalReg
  | StackParam ByteOff

instance Outputable ParamLocation where
  ppr (RegisterParam g) = ppr g
  ppr (StackParam p)    = ppr p

-- |
-- Given a list of arguments, and a function that tells their types,
-- return a list showing where each argument is passed
--
assignArgumentsPos :: DynFlags
                   -> ByteOff           -- stack offset to start with
                   -> Convention
                   -> (a -> CmmType)    -- how to get a type from an arg
                   -> [a]               -- args
                   -> (
                        ByteOff              -- bytes of stack args
                      , [(a, ParamLocation)] -- args and locations
                      )

assignArgumentsPos dflags off conv arg_ty reps = (stk_off, assignments)
    where
      regs = case (reps, conv) of
               (_,   NativeNodeCall)   -> getRegsWithNode dflags
               (_,   NativeDirectCall) -> getRegsWithoutNode dflags
               ([_], NativeReturn)     -> allRegs dflags
               (_,   NativeReturn)     -> getRegsWithNode dflags
               -- GC calling convention *must* put values in registers
               (_,   GC)               -> allRegs dflags
               (_,   Slow)             -> noRegs
      -- The calling conventions first assign arguments to registers,
      -- then switch to the stack when we first run out of registers
      -- (even if there are still available registers for args of a
      -- different type).  When returning an unboxed tuple, we also
      -- separate the stack arguments by pointerhood.
      (reg_assts, stk_args)  = assign_regs [] reps regs
      (stk_off,   stk_assts) = assignStack dflags off arg_ty stk_args
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
                      (_, (v:vs, fs, ds, ls)) | widthInBits w <= widthInBits (wordWidth dflags)
                          -> k (RegisterParam (v gcp), (vs, fs, ds, ls))
                      (_, (vs, fs, ds, l:ls)) | widthInBits w > widthInBits (wordWidth dflags)
                          -> k (RegisterParam l, (vs, fs, ds, ls))
                      _   -> (assts, (r:rs))
              k (asst, regs') = assign_regs ((r, asst) : assts) rs regs'
              ty = arg_ty r
              w  = typeWidth ty
              gcp | isGcPtrType ty = VGcPtr
                  | otherwise  	   = VNonGcPtr


assignStack :: DynFlags -> ByteOff -> (a -> CmmType) -> [a]
            -> (
                 ByteOff              -- bytes of stack args
               , [(a, ParamLocation)] -- args and locations
               )
assignStack dflags offset arg_ty args = assign_stk offset [] (reverse args)
 where
      assign_stk offset assts [] = (offset, assts)
      assign_stk offset assts (r:rs)
        = assign_stk off' ((r, StackParam off') : assts) rs
        where w    = typeWidth (arg_ty r)
              size = (((widthInBytes w - 1) `div` word_size) + 1) * word_size
              off' = offset + size
              word_size = wORD_SIZE dflags

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

getRegsWithoutNode, getRegsWithNode :: DynFlags -> AvailRegs
getRegsWithoutNode dflags =
  ( filter (\r -> r VGcPtr /= node) (realVanillaRegs dflags)
  , realFloatRegs dflags
  , realDoubleRegs dflags
  , realLongRegs dflags)

-- getRegsWithNode uses R1/node even if it isn't a register
getRegsWithNode dflags =
  ( if null (realVanillaRegs dflags)
    then [VanillaReg 1]
    else realVanillaRegs dflags
  , realFloatRegs dflags
  , realDoubleRegs dflags
  , realLongRegs dflags)

allFloatRegs, allDoubleRegs, allLongRegs :: DynFlags -> [GlobalReg]
allVanillaRegs :: DynFlags -> [VGcPtr -> GlobalReg]

allVanillaRegs dflags = map VanillaReg $ regList (mAX_Vanilla_REG dflags)
allFloatRegs   dflags = map FloatReg   $ regList (mAX_Float_REG   dflags)
allDoubleRegs  dflags = map DoubleReg  $ regList (mAX_Double_REG  dflags)
allLongRegs    dflags = map LongReg    $ regList (mAX_Long_REG    dflags)

realFloatRegs, realDoubleRegs, realLongRegs :: DynFlags -> [GlobalReg]
realVanillaRegs :: DynFlags -> [VGcPtr -> GlobalReg]

realVanillaRegs dflags = map VanillaReg $ regList (mAX_Real_Vanilla_REG dflags)
realFloatRegs   dflags = map FloatReg   $ regList (mAX_Real_Float_REG   dflags)
realDoubleRegs  dflags = map DoubleReg  $ regList (mAX_Real_Double_REG  dflags)
realLongRegs    dflags = map LongReg    $ regList (mAX_Real_Long_REG    dflags)

regList :: Int -> [Int]
regList n = [1 .. n]

allRegs :: DynFlags -> AvailRegs
allRegs dflags = (allVanillaRegs dflags,
                  allFloatRegs dflags,
                  allDoubleRegs dflags,
                  allLongRegs dflags)

noRegs :: AvailRegs
noRegs  = ([], [], [], [])

globalArgRegs :: DynFlags -> [GlobalReg]
globalArgRegs dflags = map ($ VGcPtr) (allVanillaRegs dflags) ++
                       allFloatRegs dflags ++
                       allDoubleRegs dflags ++
                       allLongRegs dflags

realArgRegs :: DynFlags -> [GlobalReg]
realArgRegs dflags = map ($VGcPtr) (realVanillaRegs dflags) ++
                realFloatRegs dflags ++
                realDoubleRegs dflags ++
                realLongRegs dflags
