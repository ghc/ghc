module GHC.Cmm.CallConv (
  ParamLocation(..),
  assignArgumentsPos,
  assignStack,
  realArgRegsCover,
  allArgRegsCover
) where

import GHC.Prelude
import Data.List (nub)

import GHC.Cmm.Expr
import GHC.Runtime.Heap.Layout
import GHC.Cmm (Convention(..))

import GHC.Platform
import GHC.Platform.Profile
import GHC.Utils.Outputable
import GHC.Utils.Panic

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
passFloatArgsInXmm platform = case platformArch platform of
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

-- This returns the set of global registers that *cover* the machine registers
-- used for argument passing. On platforms where registers can overlap---right
-- now just x86-64, where Float and Double registers overlap---passing this set
-- of registers is guaranteed to preserve the contents of all live registers. We
-- only use this functionality in hand-written C-- code in the RTS.
realArgRegsCover :: Platform -> [GlobalReg]
realArgRegsCover platform
    | passFloatArgsInXmm platform
    = realVanillaRegs    platform ++
      realLongRegs       platform ++
      realDoubleRegs     platform
        -- we only need to save the low Double part of XMM registers.
        -- Moreover, the NCG can't load/store full XMM
        -- registers for now...

    | otherwise
    = realVanillaRegs platform ++
      realFloatRegs   platform ++
      realDoubleRegs  platform ++
      realLongRegs    platform
        -- we don't save XMM registers if they are not used for parameter passing
-- SLD TODO: do we need to save xmm/ymm registers now as well?


{-

  Note [GHCi and native call registers]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  The GHCi bytecode interpreter does not have access to the STG registers
  that the native calling convention uses for passing arguments. It uses
  helper stack frames to move values between the stack and registers.

  If only a single register needs to be moved, GHCi uses a specific stack
  frame. For example stg_ctoi_R1p saves a heap pointer value from STG register
  R1 and stg_ctoi_D1 saves a double precision floating point value from D1.
  In the other direction, helpers stg_ret_p and stg_ret_d move a value from
  the stack to the R1 and D1 registers, respectively.

  When GHCi needs to move more than one register it cannot use a specific
  helper frame. It would simply be impossible to create a helper for all
  possible combinations of register values. Instead, there are generic helper
  stack frames that use a call_info word that describes the active registers
  and the number of stack words used by the arguments of a call.

  These helper stack frames are currently:

      - stg_ret_t:    return a tuple to the continuation at the top of
                          the stack
      - stg_ctoi_t:   convert a tuple return value to be used in
                          bytecode
      - stg_primcall: call a function


  The call_info word contains a bitmap of the active registers
  for the call and and a stack offset. The layout is as follows:

      - bit 0-23:  Bitmap of active registers for the call, the
                   order corresponds to the list returned by
                   allArgRegsCover. For example if bit 0 (the least
                   significant bit) is set, the first register in the
                   allArgRegsCover list is active. Bit 1 for the
                   second register in the list and so on.

      - bit 24-31: Unsigned byte indicating the stack offset
                   of the continuation in words. For tuple returns
                   this is the number of words returned on the
                   stack. For primcalls this field is unused, since
                   we don't jump to a continuation.

    The upper 32 bits on 64 bit platforms are currently unused.

    If a register is smaller than a word on the stack (for example a
    single precision float on a 64 bit system), then the stack slot
    is padded to a whole word.

    Example:

        If a tuple is returned in three registers and an additional two
        words on the stack, then three bits in the register bitmap
        (bits 0-23) would be set. And bit 24-31 would be
        00000010 (two in binary).

        The values on the stack before a call to POP_ARG_REGS would
        be as follows:

            ...
            continuation
            stack_arg_1
            stack_arg_2
            register_arg_3
            register_arg_2
            register_arg_1 <- Sp

        A call to POP_ARG_REGS(call_info) would move register_arg_1
        to the register corresponding to the lowest set bit in the
        call_info word. register_arg_2 would be moved to the register
        corresponding to the second lowest set bit, and so on.

        After POP_ARG_REGS(call_info), the stack pointer Sp points
        to the topmost stack argument, so the stack looks as follows:

            ...
            continuation
            stack_arg_1
            stack_arg_2 <- Sp

        At this point all the arguments are in place and we are ready
        to jump to the continuation, the location (offset from Sp) of
        which is found by inspecting the value of bits 24-31. In this
        case the offset is two words.

    On x86_64, the double precision (Dn) and single precision
    floating (Fn) point registers overlap, e.g. D1 uses the same
    physical register as F1. On this platform, the list returned
    by allArgRegsCover contains only entries for the double
    precision registers. If an argument is passed in register
    Fn, the bit corresponding to Dn should be set.

  Note: if anything changes in how registers for native calls overlap,
           make sure to also update GHC.StgToByteCode.layoutNativeCall
 -}

-- Like realArgRegsCover but always includes the node. This covers all real
-- and virtual registers actually used for passing arguments.

allArgRegsCover :: Platform -> [GlobalReg]
allArgRegsCover platform =
  nub (VanillaReg 1 : realArgRegsCover platform)
