module GHC.Cmm.CallConv (
  ParamLocation(..),
  assignArgumentsPos,
  assignStack,
  realArgRegsCover,
  allArgRegsCover
) where

import GHC.Prelude

import GHC.Cmm.Expr
import GHC.Cmm.Reg (GlobalArgRegs(..))
import GHC.Runtime.Heap.Layout
import GHC.Cmm (Convention(..))

import GHC.Platform
import GHC.Platform.Reg.Class
import GHC.Platform.Profile
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.Maybe ( maybeToList )
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
-- See Note [realArgRegsCover].
realArgRegsCover :: Platform
                 -> GlobalArgRegs
                    -- ^ which kinds of registers do we want to cover?
                 -> [GlobalReg]
realArgRegsCover platform argRegs
  =  realVanillaRegs platform
  ++ realLongRegs    platform
  ++ concat
      (  [ realFloatRegs platform  | wantFP, not (passFloatArgsInXmm platform) ]
           -- TODO: the line above is legacy logic, but removing it breaks
           -- the bytecode interpreter on AArch64. Probably easy to fix.
           -- AK: I believe this might be because we map REG_F1..4 and REG_D1..4 to different
           -- machine registers on AArch64.
      ++ [ realDoubleRegs platform | wantFP ]
      )
  ++ [ mkVecReg i | mkVecReg <- maybeToList mbMkVecReg
                  , i <- realXmmRegNos platform ]

  where
    wantFP = case registerArch (platformArch platform) of
      Unified   -> argRegs == SCALAR_ARG_REGS
      Separate  -> argRegs >= SCALAR_ARG_REGS
      NoVectors -> argRegs >= SCALAR_ARG_REGS
    mbMkVecReg = case registerArch (platformArch platform) of
      Unified   -> mb_xyzmm
      Separate  -> mb_xyzmm
      NoVectors -> Nothing
    mb_xyzmm = case argRegs of
      V16_ARG_REGS -> Just XmmReg
      V32_ARG_REGS -> Just YmmReg
      V64_ARG_REGS -> Just ZmmReg
      _ -> Nothing

-- | Like "realArgRegsCover", but always includes the node.
--
-- See Note [realArgRegsCover].
allArgRegsCover :: Platform
                -> GlobalArgRegs
                    -- ^ which kinds of registers do we want to cover?
                -> [GlobalReg]
allArgRegsCover platform argRegs =
  nub (node : realArgRegsCover platform argRegs)
  where
    node = VanillaReg 1

{- Note [realArgRegsCover]
~~~~~~~~~~~~~~~~~~~~~~~~~~
In low-level Cmm, jumps must be annotated with a set of live registers,
allowing precise control of global STG register contents across function calls.
However, in some places (in particular in the RTS), the registers we want to
preserve depend on the *caller*. For example, if we intercept a function call
via a stack underflow frame, we want to preserve exactly those registers
containing function arguments.
Since we can't know exactly how many arguments the caller passed, we settle on
simply preserving all global regs which might be used for argument passing.
To do this, we specify a collection of registers that *covers* all the registers
we want to preserve; this is done by "realArgRegsCover".

The situation is made somewhat tricky by the need to handle vector registers.
For example, on X86_64, the F, D, XMM, YMM, ZMM overlap in the following way
          ┌─┬─┬───┬───────┬───────────────┐
          │F┆D┆XMM┆  YMM  ┆     ZMM       │
          └─┴─┴───┴───────┴───────────────┘
where each register extends all the way to the left.

Based on this register architecture, on X86_64 we might want to annotate a jump
in which we (might) want to preserve the contents of all argument-passing
registers with [R1, ..., R6, ZMM1, ..., ZMM6]. This, however, is not possible
in general, because preserving e.g. a ZMM register across a C call requires the
availability of the AVX-512F instruction set. If we did this, the RTS would
crash at runtime with an "invalid instruction" error on X86_64 machines which
do not support AVX-512F.

Instead, we parametrise "realArgRegsCover" on the 'GlobalArgRegs' datatype, which
specifies which registers it is sufficient to preserve. For example, it might
suffice to only preserve general-purpose registers, or to only preserve up to
XMM (not YMM or ZMM).

Then, to handle certain functions in the RTS such as "stack_underflow_frame", we
proceed by defining 4 variants, stack_underflow_frame_{d,v16,v32,v64}, which
respectively annotate the jump at the end of the function with SCALAR_ARG_REGS,
V16_ARG_REGS, V32_ARG_REGS and V64_ARG_REGS. Compiling these variants, in effect,
amounts to compiling "stack_underflow_frame" four times, once for each level of
vector support. Then, in the RTS, we dispatch at runtime based on the support
for vectors provided by the architecture on the current machine (see e.g.
'threadStackOverflow' and its 'switch (vectorSupportGlobalVar)'.)

Note that, like in Note [AutoApply.cmm for vectors], it is **critical** that we
compile e.g. stack_underflow_frame_v64 with -mavx512f. If we don't, the LLVM
backend is liable to compile code using e.g. the ZMM1 STG register to uses of
X86 machine registers xmm1, xmm2, xmm3, xmm4, instead of just zmm1. This would
mean that LLVM produces ABI-incompatible code that would result in segfaults in
the RTS.
-}