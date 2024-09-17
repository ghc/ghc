module GHC.Cmm.CallConv (
  ParamLocation(..),
  assignArgumentsPos,
  assignStack,
  realArgRegsCover,
  allArgRegsCover
) where

import GHC.Prelude

import GHC.Cmm.Expr
import GHC.Runtime.Heap.Layout
import GHC.Cmm (Convention(..))

import GHC.Platform
import GHC.Platform.Profile
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.List (nub)

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
assignArgumentsPos :: Profile
                   -> ByteOff           -- stack offset to start with
                   -> Convention
                   -> (a -> CmmType)    -- how to get a type from an arg
                   -> [a]               -- args
                   -> (
                        ByteOff              -- bytes of stack args
                      , [(a, ParamLocation)] -- args and locations
                      )

assignArgumentsPos profile off conv arg_ty reps = (stk_off, assignments)
    where
      platform = profilePlatform profile
      regs = case (reps, conv) of
               (_,   NativeNodeCall)   -> getRegsWithNode platform
               (_,   NativeDirectCall) -> getRegsWithoutNode platform
               ([_], NativeReturn)     -> allRegs platform
               (_,   NativeReturn)     -> getRegsWithNode platform
               -- GC calling convention *must* put values in registers
               (_,   GC)               -> allRegs platform
               (_,   Slow)             -> nodeOnly
      -- The calling conventions first assign arguments to registers,
      -- then switch to the stack when we first run out of registers
      -- (even if there are still available registers for args of a
      -- different type).  When returning an unboxed tuple, we also
      -- separate the stack arguments by pointerhood.
      (reg_assts, stk_args)  = assign_regs [] reps regs
      (stk_off,   stk_assts) = assignStack platform off arg_ty stk_args
      assignments = reg_assts ++ stk_assts

      assign_regs assts []     _    = (assts, [])
      assign_regs assts (r:rs) regs | isVecType ty   = vec
                                    | isFloatType ty = float
                                    | otherwise      = int
        where vec = case regs of
                      AvailRegs vs fs ds ls (s:ss)
                        | passVectorInReg w profile
                          -> let reg_class = case w of
                                    W128 -> XmmReg
                                    W256 -> YmmReg
                                    W512 -> ZmmReg
                                    _    -> panic "CmmCallConv.assignArgumentsPos: Invalid vector width"
                              in k (RegisterParam (reg_class s), AvailRegs vs fs ds ls ss)
                      _ -> (assts, r:rs)
              float = case (w, regs) of
                        (W32, AvailRegs vs fs ds ls (s:ss))
                            | passFloatInXmm          -> k (RegisterParam (FloatReg s), AvailRegs vs fs ds ls ss)
                        (W32, AvailRegs vs (f:fs) ds ls ss)
                            | not passFloatInXmm      -> k (RegisterParam f, AvailRegs vs fs ds ls ss)
                        (W64, AvailRegs vs fs ds ls (s:ss))
                            | passFloatInXmm          -> k (RegisterParam (DoubleReg s), AvailRegs vs fs ds ls ss)
                        (W64, AvailRegs vs fs (d:ds) ls ss)
                            | not passFloatInXmm      -> k (RegisterParam d, AvailRegs vs fs ds ls ss)
                        _ -> (assts, (r:rs))
              int = case (w, regs) of
                      (W128, _) -> panic "W128 unsupported register type"
                      (_, AvailRegs (v:vs) fs ds ls ss) | widthInBits w <= widthInBits (wordWidth platform)
                          -> k (RegisterParam v, AvailRegs vs fs ds ls ss)
                      (_, AvailRegs vs fs ds (l:ls) ss) | widthInBits w > widthInBits (wordWidth platform)
                          -> k (RegisterParam l, AvailRegs vs fs ds ls ss)
                      _   -> (assts, (r:rs))
              k (asst, regs') = assign_regs ((r, asst) : assts) rs regs'
              ty = arg_ty r
              w  = typeWidth ty
              passFloatInXmm = passFloatArgsInXmm platform

passFloatArgsInXmm :: Platform -> Bool
passFloatArgsInXmm platform =
  -- TODO: replace the following logic by casing on @registerArch (platformArch platform)@.
  --
  -- This will mean we start saying "True" for AArch64, which the rest of the AArch64
  -- compilation pipeline will need to be able to handle (e.g. the AArch64 NCG).
  case platformArch platform of
    ArchX86_64 -> True
    ArchX86    -> False
    _          -> False

-- We used to spill vector registers to the stack since the LLVM backend didn't
-- support vector registers in its calling convention. However, this has now
-- been fixed. This function remains only as a convenient way to re-enable
-- spilling when debugging code generation.
passVectorInReg :: Width -> Profile -> Bool
passVectorInReg _ _ = True

assignStack :: Platform -> ByteOff -> (a -> CmmType) -> [a]
            -> (
                 ByteOff              -- bytes of stack args
               , [(a, ParamLocation)] -- args and locations
               )
assignStack platform offset arg_ty args = assign_stk offset [] (reverse args)
 where
      assign_stk offset assts [] = (offset, assts)
      assign_stk offset assts (r:rs)
        = assign_stk off' ((r, StackParam off') : assts) rs
        where w    = typeWidth (arg_ty r)
              off' = offset + size
              -- Stack arguments always take a whole number of words, we never
              -- pack them unlike constructor fields.
              size = roundUpToWords platform (widthInBytes w)

-----------------------------------------------------------------------------
-- Local information about the registers available

-- | Keep track of locally available registers.
data AvailRegs
  = AvailRegs
    { availVanillaRegs :: [GlobalReg]
       -- ^ Available vanilla registers
    , availFloatRegs   :: [GlobalReg]
       -- ^ Available float registers
    , availDoubleRegs  :: [GlobalReg]
       -- ^ Available double registers
    , availLongRegs    :: [GlobalReg]
       -- ^ Available long registers
    , availXMMRegs     :: [Int]
       -- ^ Available vector XMM registers
    }

noAvailRegs :: AvailRegs
noAvailRegs = AvailRegs [] [] [] [] []

-- Vanilla registers can contain pointers, Ints, Chars.
-- Floats and doubles have separate register supplies.
--
-- We take these register supplies from the *real* registers, i.e. those
-- that are guaranteed to map to machine registers.

getRegsWithoutNode, getRegsWithNode :: Platform -> AvailRegs
getRegsWithoutNode platform =
  AvailRegs
   { availVanillaRegs = filter (\r -> r /= node) (realVanillaRegs platform)
   , availFloatRegs   = realFloatRegs platform
   , availDoubleRegs  = realDoubleRegs platform
   , availLongRegs    = realLongRegs platform
   , availXMMRegs     = realXmmRegNos platform }

-- getRegsWithNode uses R1/node even if it isn't a register
getRegsWithNode platform =
  AvailRegs
   { availVanillaRegs = if null (realVanillaRegs platform)
                        then [VanillaReg 1]
                        else realVanillaRegs platform
   , availFloatRegs   = realFloatRegs platform
   , availDoubleRegs  = realDoubleRegs platform
   , availLongRegs    = realLongRegs platform
   , availXMMRegs     = realXmmRegNos platform }

allFloatRegs, allDoubleRegs, allLongRegs :: Platform -> [GlobalReg]
allVanillaRegs :: Platform -> [GlobalReg]
allXmmRegs :: Platform -> [Int]

allVanillaRegs platform = map VanillaReg $ regList (pc_MAX_Vanilla_REG (platformConstants platform))
allFloatRegs   platform = map FloatReg   $ regList (pc_MAX_Float_REG   (platformConstants platform))
allDoubleRegs  platform = map DoubleReg  $ regList (pc_MAX_Double_REG  (platformConstants platform))
allLongRegs    platform = map LongReg    $ regList (pc_MAX_Long_REG    (platformConstants platform))
allXmmRegs     platform =                  regList (pc_MAX_XMM_REG     (platformConstants platform))

realFloatRegs, realDoubleRegs, realLongRegs :: Platform -> [GlobalReg]
realVanillaRegs :: Platform -> [GlobalReg]

realVanillaRegs platform = map VanillaReg $ regList (pc_MAX_Real_Vanilla_REG (platformConstants platform))
realFloatRegs   platform = map FloatReg   $ regList (pc_MAX_Real_Float_REG   (platformConstants platform))
realDoubleRegs  platform = map DoubleReg  $ regList (pc_MAX_Real_Double_REG  (platformConstants platform))
realLongRegs    platform = map LongReg    $ regList (pc_MAX_Real_Long_REG    (platformConstants platform))

realXmmRegNos :: Platform -> [Int]
realXmmRegNos platform
    | isSse2Enabled platform || platformArch platform == ArchAArch64
    = regList (pc_MAX_Real_XMM_REG (platformConstants platform))
    | otherwise
    = []

regList :: Int -> [Int]
regList n = [1 .. n]

allRegs :: Platform -> AvailRegs
allRegs platform =
  AvailRegs
   { availVanillaRegs = allVanillaRegs platform
   , availFloatRegs   = allFloatRegs   platform
   , availDoubleRegs  = allDoubleRegs  platform
   , availLongRegs    = allLongRegs    platform
   , availXMMRegs     = allXmmRegs     platform }

nodeOnly :: AvailRegs
nodeOnly = noAvailRegs { availVanillaRegs = [VanillaReg 1] }

-- | A set of global registers that cover the machine registers used
-- for argument passing.
--
-- **Warning:** does not cover vector registers, just the part used for
-- passing doubles.
realArgRegsCover :: Platform -> [GlobalReg]
realArgRegsCover platform
 | passFloatArgsInXmm platform
 = realVanillaRegs    platform ++
   realLongRegs       platform ++
   realDoubleRegs     platform
 | otherwise
 = realVanillaRegs platform ++
   realFloatRegs   platform ++
   realDoubleRegs  platform ++
   realLongRegs    platform

-- | Like "realArgRegsCover", but always includes the node.
allArgRegsCover :: Platform -> [GlobalReg]
allArgRegsCover platform =
  nub (node : realArgRegsCover platform)
  where
    node = VanillaReg 1
