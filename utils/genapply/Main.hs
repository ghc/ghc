{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Prelude hiding ((<>))

import Control.Arrow      ( (***) )
import Data.Bits          ( (.|.), shiftL )
import Data.List          ( intercalate, intersperse, nub, sort, sortOn )
import Data.Maybe         ( isNothing, mapMaybe )
import Data.Ord           ( Down(..) )
import Data.Word          ( Word32 )
import System.Environment ( getArgs )

import Text.PrettyPrint

{- Note [How genapply gets target info]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
genapply generates AutoApply.cmm for the target rts, so it needs
access to target constants like word size, MAX_REAL_VANILLA_REG, etc.
These constants are computed by the deriveConstants program, which
outputs:

1. DerivedConstants.h containing the constants
2. Constants.hs, which is the GHC.Platform.Constants module used by
   ghc to parse the header

It's quite tricky to import Constants.hs and reuse the same parsing
logic, therefore we take one step back and do our own parsing, while
still regarding DerivedConstants.h as the source of truth for target
info. The deriveConstants program will emit lines like these in the
header:

// MAX_Real_Vanilla_REG 10
// WORD_SIZE 4

They will be parsed by parseTargetInfo at runtime, the resulting
TargetInfo record is passed to other places in genapply. hadrian
passes the DerivedConstants.h path as genapply's command line
argument, while also ensuring that DerivedConstants.h is a dependency
of AutoApply.cmm, and only the header in the same stage's rts build
directory is passed.

In the past, genapply used to bake in these target constants at
compile-time via CPP. This is horrifically fragile when it comes to
cross-compilation! (See #24347) People invented hacks like making the
build system pass -I flags to override CPP include path and make it
favor the target headers, but host info may still leak into genapply
because ghc passes CPP flags like -Dx86_64_HOST_ARCH when building
genapply, and of course it should because genapply is meant to run on
the host. Should we add even more CPP hacks like passing flags like
-Ux86_64_HOST_ARCH to get it right? Please, no. Before we move
genapply logic into hadrian at some point, at least we should make it
less hacky by nuking all CPP logic in it from the orbit.

Note [AutoApply.cmm for vectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generating and compiling stg_ap functions for vectors (e.g. stg_ap_v16_fast)
is quite tricky. The build platform generates Cmm code for the target platform,
and the requirements are:

  1. the host platform must be able to compile this code,
  2. the target platform must be able to:
      a. run this code when it passes the appropriate CPU flags such as -mavx
      b. not run into any problems in other code that does not use vector registers.

This is achieved by several means. The first step is to use CPP in the generated
Cmm code. Specifically, any mention of XMM/YMM/ZMM registers is wrapped
in a conditional. For example, stg_stk_save_v32 looks like:

    stg_stk_save_v32
    {
        #if defined(REG_YMM1)
        Sp_adj(-7);
        V32_[Sp+WDS(3)] = YMM1;
        ...
        #else
        foreign "C" barf("stg_stk_save_v32: unsupported vector register", NULL) never returns;
        #endif
    }

This means that:

  - the RTS unconditionally defines symbols such as 'stg_ap_v32_fast',
  - but they may throw an error at runtime if vector registers are not supported.

This is a lot more straightforward than attempting to defer the generation of
this code. In particular, GHC.StgToCmm.ArgRep.slowCallPattern currently assumes
that 1-argument stg_ap functions exist for all representations, and revisiting
that design would constitute a fair amount of work.

Case in point: this CPP allows the generated application code to be compiled on
AArch64, for which REG_XMM1 is defined but both REG_YMM1 and REG_ZMM1 are not.
Note that GHC will complain at compile-time if we try to use 256/512 bit wide
vectors on AArch64, so it is immaterial that stg_ap_v32_fast will crash at runtime.

How about on X86_64? There are several points to consider:

  - The X86 NCG only partially supports vector registers; most operations
    only work on 128-bit wide vectors.

    However all that is needed is support for MOV instructions to perform the
    stack save/load; so it suffices to have (working register allocation and)
    support for move instructions for XMM/YMM/ZMM;
    see GHC.CmmToAsm.X86.Instr.movInstr.

  - For XMM load/stores, we can't assume that the target supports AVX, only SSE2.
    So we have to pessimistically emit SSE2 instructions instead of AVX instructions.
    On the other hand, YMM load/stores require -mavx2/-mavx512f, respectively.

    To do this, we must compile e.g stg_ap_v16_fast WITHOUT -mavx.
    (Were we to compile with -mavx, we would emit assembly containing AVX
    instructions, and these might not be available on the target platform.)

  - For YMM/ZMM load/stores, we must emit the appropriate AVX2/AVX512F
    instructions (as GHC.CmmToAsm.X86.Instr.movInstr does).

This means that:

  - stg_ap_v16_fast (and friends) should be compiled WITHOUT -mavx.
    (Were we to compile with -mavx, we would emit assembly containing AVX
    instructions, and these might not be available on the target platform.)
  - stg_ap_v32_fast (and friends) should be compiled with -mavx2.
  - stg_ap_v64_fast (and friends) should be compiled with -mavx512f.

However, there isn't currently a way to set CPU flags per function in Cmm, à la

  __attribute__(("target"="avx2"))
  stg_ap_v32_fast ...

Instead, we put all V16 code in AutoApply_V16.cmm, all V32 code into
AutoApply_V32.cmm, and all V64 code in AutoApply_V64.cmm.
On X86, we then compile AutoApply_V32.cmm with -mavx2, and AutoApply_V64.cmm
with -mavx512f. See references to AutoApply in Hadrian, Settings/Packages.hs.

Note that it is very important to set these flags. For example, were we to
compile AutoApply_V32.cmm without -mavx2 using the LLVM backend, LLVM would
attempt to compile usage of ymm registers into usage of pairs of xmm registers.
This violates the expected calling convention, and leads to segfaults
(e.g. in test T25062_V32).
-}

data TargetInfo = TargetInfo
  { maxRealVanillaReg,
    maxRealFloatReg,
    maxRealDoubleReg,
    maxRealLongReg,
    maxRealXmmReg,
    wordSize,
    tagBits,
    tagBitsMax,
    bitmapBitsShift :: !Int
  }

parseTargetInfo :: FilePath -> IO TargetInfo
parseTargetInfo path = do
  header <- readFile path
  let tups :: [(String, Int)]
      tups = [ (k, read v) | '/':'/':' ':l <- lines header, [k, v] <- [words l] ]
      tups_get k = case lookup k tups of
                    Nothing -> error "genapply.parseTargetInfo: Missing key"
                    Just v  -> v
      tag_bits = tups_get "TAG_BITS"
  pure TargetInfo {
    maxRealVanillaReg = tups_get "MAX_Real_Vanilla_REG",
    maxRealFloatReg = tups_get "MAX_Real_Float_REG",
    maxRealDoubleReg = tups_get "MAX_Real_Double_REG",
    maxRealLongReg = tups_get "MAX_Real_Long_REG",
    maxRealXmmReg = tups_get "MAX_Real_XMM_REG",
    wordSize = tups_get "WORD_SIZE",
    tagBits = tag_bits,
    tagBitsMax = 1 `shiftL` tag_bits,
    bitmapBitsShift = tups_get "BITMAP_BITS_SHIFT"
  }

-- -----------------------------------------------------------------------------
-- Argument kinds (roughly equivalent to PrimRep)

data ArgRep
  = N   -- ^ non-ptr
  | P   -- ^ ptr
  | V   -- ^ void
  | F   -- ^ float
  | D   -- ^ double
  | L   -- ^ long (64 bit)
  | V16 -- ^ 16 byte (128 bit) vector
  | V32 -- ^ 32 byte (256 bit) vector
  | V64 -- ^ 64 byte (512 bit) vector
  deriving stock (Eq, Ord, Show)

-- size of a value in *words*
argSize :: TargetInfo -> ArgRep -> Int
argSize _ N   = 1
argSize _ P   = 1
argSize _ V   = 0
argSize _ F   = 1
argSize TargetInfo {..} D   = 8 `quot` wordSize
argSize TargetInfo {..} L   = 8 `quot` wordSize
argSize TargetInfo {..} V16 = 16 `quot` wordSize
argSize TargetInfo {..} V32 = 32 `quot` wordSize
argSize TargetInfo {..} V64 = 64 `quot` wordSize

showArg :: ArgRep -> String
showArg N   = "n"
showArg P   = "p"
showArg V   = "v"
showArg F   = "f"
showArg D   = "d"
showArg L   = "l"
showArg V16 = "v16"
showArg V32 = "v32"
showArg V64 = "v64"

-- is a value a pointer?
isPtr :: ArgRep -> Bool
isPtr P = True
isPtr _ = False

-- -----------------------------------------------------------------------------
-- Registers

type Reg = String

-- Available registers.
--
-- Assumes we have only two types of assignable registers:
--
--  - general purpose registers (RegClass RcInteger)
--  - floating point / vector registers (RegClass RcFloatOrVector)
--
-- Target architectures with different register layouts (e.g. different RegClass
-- structure) will need adjustments here.
data AvailRegs =
  AvailRegs
    { availIntegerRegNos :: [Int]
    , availFloatOrVectorRegNos :: [Int]
    }

allAvailRegs :: AvailRegs
allAvailRegs =
  AvailRegs
    { availIntegerRegNos       = [1..]
    , availFloatOrVectorRegNos = [1..]
    }

findAvailIntegerReg :: ArgRep -> (Int, Int) -> AvailRegs -> Maybe (Reg, AvailRegs)
findAvailIntegerReg rep (min_regno, max_regno) regs =
  case mid of
    []       -> Nothing
    regNo:nos -> Just (regName rep regNo, regs { availIntegerRegNos = small ++ nos ++ big })
  where
    (small, rest) = span (< min_regno) $ availIntegerRegNos regs
    (mid, big) = span (<= max_regno) rest

findAvailFloatOrVectorReg :: ArgRep -> (Int, Int) -> AvailRegs -> Maybe (Reg, AvailRegs)
findAvailFloatOrVectorReg reg (min_regno, max_regno) regs =
  case mid of
    []        -> Nothing
    regNo:nos -> Just (regName reg regNo, regs { availFloatOrVectorRegNos = small ++ nos ++ big })
  where
    (small, rest) = span (< min_regno) $ availFloatOrVectorRegNos regs
    (mid, big) = span (<= max_regno) rest

-- -----------------------------------------------------------------------------
-- Loading/saving register arguments to the stack

loadRegArgs :: TargetInfo -> Int -> [ArgRep] -> (Doc,Int)
loadRegArgs targetInfo sp args = (loadRegOffs reg_locs, sp')
  where (reg_locs, _, sp') = assignRegs targetInfo sp args

loadRegOffs :: [(Reg,Int)] -> Doc
loadRegOffs reg_locs =
  vcat $ map (uncurry assign_stk_to_reg) reg_locs

saveRegOffs :: [(Reg,Int)] -> Doc
saveRegOffs reg_locs =
  vcat $ map (uncurry assign_reg_to_stk) reg_locs

assignRegs
        :: TargetInfo
        -> Int                  -- Sp of first arg
        -> [ArgRep]             -- args
        -> ([(Reg,Int)],        -- regs and offsets to load
            [ArgRep],           -- left-over args
            Int)                -- Sp of left-over args
assignRegs targetInfo sp args = assign targetInfo sp args allAvailRegs []

assign :: TargetInfo -> Int -> [ArgRep] -> AvailRegs -> [(Reg, Int)] -> ([(Reg, Int)], [ArgRep], Int)
assign _ sp [] _regs doc = (doc, [], sp)
assign targetInfo sp (V : args) regs doc = assign targetInfo sp args regs doc
assign targetInfo sp (arg : args) regs doc
 = case findAvailableReg targetInfo arg regs of
    Just (reg, regs') -> assign targetInfo (sp + argSize targetInfo arg) args regs'
                            ((reg, sp) : doc)
    Nothing -> (doc, (arg:args), sp)

findAvailableReg :: TargetInfo -> ArgRep -> AvailRegs -> Maybe (Reg, AvailRegs)
findAvailableReg tgt N   = findAvailIntegerReg       N   (2, maxRealVanillaReg tgt) -- don't use R1
findAvailableReg tgt P   = findAvailIntegerReg       P   (2, maxRealVanillaReg tgt) --     ''
findAvailableReg tgt L   = findAvailIntegerReg       L   (1, maxRealLongReg tgt)
findAvailableReg tgt F   = findAvailFloatOrVectorReg F   (1, maxRealFloatReg tgt)
findAvailableReg tgt D   = findAvailFloatOrVectorReg D   (1, maxRealDoubleReg tgt)
findAvailableReg tgt V16 = findAvailFloatOrVectorReg V16 (1, maxRealXmmReg tgt)
findAvailableReg tgt V32 = findAvailFloatOrVectorReg V32 (1, maxRealXmmReg tgt)
findAvailableReg tgt V64 = findAvailFloatOrVectorReg V64 (1, maxRealXmmReg tgt)
findAvailableReg _   V   = error "genapply: findAvailableReg void arg (V)"

regName :: ArgRep -> Int -> Reg
regName rep regNo = regNm ++ show regNo
  where
    regNm :: String
    regNm = case rep of
      N   -> "R"
      P   -> "R"
      L   -> "L"
      F   -> "F"
      D   -> "D"
      V16 -> "XMM"
      V32 -> "YMM"
      V64 -> "ZMM"
      V  -> error "regName Void"

vectorReg_maybe :: Reg -> Maybe Reg
vectorReg_maybe r = case r of
  xyz:'M':'M':_
    | xyz `elem` ['X','Y','Z']
    -> Just r
  _ -> Nothing

vecsCpp :: Doc -> [Reg] -> [Doc] -> [Doc]
vecsCpp fun regs code =
  case mapMaybe vectorReg_maybe regs of
    [] -> code
    vs ->
      let cond = text (intercalate " && " [ "defined(REG_" ++ r ++ ")" | r <- vs ])
      in [ text "// Guard usage of vector registers"
         , text "#if" <+> cond ]
         ++ code
         ++ [ text "#else //" <+> cond
            , text "foreign \"C\" barf(\"" <> fun <> text ": unsupported vector register\", NULL) never returns;"
            , text "#endif //" <+> cond
            ]

vecRep_maybe :: ArgRep -> Maybe ArgRep
vecRep_maybe = \case
  V16 -> Just V16
  V32 -> Just V32
  V64 -> Just V64
  _   -> Nothing


largestVec :: [ArgRep] -> Maybe ArgRep
largestVec reps
  | r:_ <- sortOn Down reps
  = vecRep_maybe r
  | otherwise
  = Nothing

assign_reg_to_stk :: String -> Int -> Doc
assign_reg_to_stk reg sp
   = loadSpWordOff (regRep reg) sp <> text " = " <> text reg <> semi

assign_stk_to_reg :: String -> Int -> Doc
assign_stk_to_reg reg sp
   = text reg <> text " = " <> loadSpWordOff (regRep reg) sp <> semi

regRep :: String -> String
regRep ('F':_) = "F_"
regRep ('D':_) = "D_"
regRep ('L':_) = "L_"
regRep ('X':'M':'M':_) = "V16_"
regRep ('Y':'M':'M':_) = "V32_"
regRep ('Z':'M':'M':_) = "V64_"
regRep _ = "W_"

loadSpWordOff :: String -> Int -> Doc
loadSpWordOff rep off = text rep <> text "[Sp+WDS(" <> int off <> text ")]"

-- Make a jump
mkJump :: TargetInfo
       -> Doc       -- Jump target
       -> [Reg]     -- Registers that are definitely live
       -> [ArgRep]  -- Jump arguments
       -> Doc
mkJump targetInfo jump live args =
  text "jump" <+> jump <+> brackets (hcat (punctuate comma (map text liveRegs))) <+> semi
  where
    liveRegs = mkJumpLiveRegs targetInfo live args

-- Make a jump, saving CCCS and restoring it on return
mkJumpSaveCCCS :: TargetInfo
               -> Doc       -- Jump target
               -> [Reg]     -- Registers that are definitely live
               -> [ArgRep]  -- Jump arguments
               -> Doc
mkJumpSaveCCCS targetInfo jump live args =
  text "jump_SAVE_CCCS" <> parens (hcat (punctuate comma (jump : map text liveRegs))) <+> semi
  where
    liveRegs = mkJumpLiveRegs targetInfo live args

-- Calculate live registers for a jump
mkJumpLiveRegs :: TargetInfo
               -> [Reg]     -- Registers that are definitely live
               -> [ArgRep]  -- Jump arguments
               -> [String]
mkJumpLiveRegs targetInfo live args = (nub . sort) (live ++ map fst reg_locs)
  where
    (reg_locs, _, _) = assignRegs targetInfo 0 args

-- make a ptr/non-ptr bitmap from a list of argument types
mkBitmap :: TargetInfo -> [ArgRep] -> Word32
mkBitmap targetInfo args = foldr f 0 args
 where
  f :: ArgRep -> Word32 -> Word32
  f arg bm | isPtr arg = bm `shiftL` 1
           | otherwise = (bm `shiftL` size) .|. ((1 `shiftL` size) - 1)
           where size = argSize targetInfo arg

-- -----------------------------------------------------------------------------
-- Generating the application functions

-- A SUBTLE POINT about stg_ap functions (can't think of a better
-- place to put this comment --SDM):
--
-- The entry convention to an stg_ap_ function is as follows: all the
-- arguments are on the stack (we might revisit this at some point,
-- but it doesn't make any difference on x86), and THERE IS AN EXTRA
-- EMPTY STACK SLOT at the top of the stack.
--
-- Why?  Because in several cases, stg_ap_* will need an extra stack
-- slot, eg. to push a return address in the THUNK case, and this is a
-- way of pushing the stack check up into the caller which is probably
-- doing one anyway.  Allocating the extra stack slot in the caller is
-- also probably free, because it will be adjusting Sp after pushing
-- the args anyway (this might not be true of register-rich machines
-- when we start passing args to stg_ap_* in regs).

mkApplyName :: [ArgRep] -> Doc
mkApplyName args
  = text "stg_ap_" <> text (concatMap showArg args)

mkApplyRetName :: [ArgRep] -> Doc
mkApplyRetName args
  = mkApplyName args <> text "_ret"

mkApplyFastName :: [ArgRep] -> Doc
mkApplyFastName args
  = mkApplyName args <> text "_fast"

mkApplyInfoName :: [ArgRep] -> Doc
mkApplyInfoName args
  = mkApplyName args <> text "_info"

mb_tag_node :: TargetInfo -> Int -> Doc
mb_tag_node targetInfo arity | Just tag <- tagForArity targetInfo arity = mkTagStmt tag <> semi
                             | otherwise = empty

mkTagStmt :: Int -> Doc
mkTagStmt tag = text ("R1 = R1 + " ++ show tag)

type StackUsage = (Int, Int)  -- PROFILING, normal

maxStack :: [StackUsage] -> StackUsage
maxStack = (maximum *** maximum) . unzip

stackCheck
   :: TargetInfo
   -> [ArgRep]
   -> Bool       -- args in regs?
   -> Doc        -- fun_info_label
   -> StackUsage
   -> Doc
stackCheck targetInfo args args_in_regs fun_info_label (prof_sp, norm_sp) =
  let
     (reg_locs, _leftovers, sp_offset) = assignRegs targetInfo 1 args

     cmp_sp n
       | n > 0 =
          text "if (Sp - WDS(" <> int n <> text ") < SpLim) {" $$
          nest 4 (vcat [
            if args_in_regs
               then
                 text "Sp_adj" <> parens (int (-sp_offset)) <> semi $$
                 saveRegOffs reg_locs
               else
                 empty,
            text "Sp(0) = " <> fun_info_label <> char ';',
            mkJump targetInfo (text "__stg_gc_enter_1") ["R1"] []
            ]) $$
          char '}'
       | otherwise = empty
  in
  vcat [ text "#if defined(PROFILING)",
         cmp_sp prof_sp,
         text "#else // defined(PROFILING)",
         cmp_sp norm_sp,
         text "#endif // defined(PROFILING)"
       ]

genMkPAP :: TargetInfo
         -> String    -- Macro
         -> String    -- Jump target
         -> [Reg]     -- Registers that are definitely live
         -> String    -- Ticker
         -> String    -- Disamb
         -> Bool      -- Don't load argument registers before jump if True
         -> Bool      -- Arguments already in registers if True
         -> Bool      -- Is a PAP if True
         -> [ArgRep]  -- Arguments
         -> Int       -- Size of all arguments
         -> Doc       -- info label
         -> Bool      -- Is a function
         -> (Doc, StackUsage)
genMkPAP targetInfo@TargetInfo {..} macro jump live _ticker disamb
        no_load_regs    -- don't load argument regs before jumping
        args_in_regs    -- arguments are already in regs
        is_pap args all_args_size fun_info_label
        is_fun_case
  = (doc, stack_usage)

  where
    doc = vcat smaller_arity_doc $$ exact_arity_case $$ larger_arity_doc

    stack_usage = maxStack (larger_arity_stack : smaller_arity_stack)

    n_args = length args

        -- offset of arguments on the stack at slow apply calls.
    stk_args_slow_offset = 1

    stk_args_offset
        | args_in_regs = 0
        | otherwise    = stk_args_slow_offset

-- The SMALLER ARITY cases:
--      if (arity == 1) {
--          Sp[0] = Sp[1];
--          Sp[1] = (W_)&stg_ap_1_info;
--          JMP_(GET_ENTRY(R1.cl));
    (smaller_arity_doc, smaller_arity_stack)
       = unzip [ smaller_arity i | i <- [1..n_args-1] ]

    smaller_arity arity = (smaller_doc, smaller_stack_usage)
      where
        (save_regs, smaller_stack_usage)
          | overflow_regs = save_extra_regs
          | otherwise     = shuffle_extra_args

        smaller_doc =
           text "if (arity == " <> int arity <> text ") {" $$
           nest 4 (vcat [
           --  text "TICK_SLOW_CALL_" <> text ticker <> text "_TOO_MANY();",

                -- load up regs for the call, if necessary
             load_regs,

                -- If we have more args in registers than are required
                -- for the call, then we must save some on the stack,
                -- and set up the stack for the follow-up call.
                -- If the extra arguments are on the stack, then we must
                -- instead shuffle them down to make room for the info
                -- table for the follow-on call.
             save_regs,

                -- for a PAP, we have to arrange that the stack contains a
                -- return address in the event that stg_PAP_entry fails its
                -- heap check.  See stg_PAP_entry in Apply.cmm for details.
             if is_pap
                then text "R2 = " <> mkApplyInfoName this_call_args <> semi

                else empty,
            if is_fun_case then mb_tag_node targetInfo arity else empty,
            if overflow_regs
                then mkJumpSaveCCCS targetInfo
                       (text jump) live (take arity args)
                else mkJump targetInfo (text jump) live (if no_load_regs then [] else args)
            ]) $$
           text "}"

           -- offsets in case we need to save regs:
        (reg_locs, _, _)
           = assignRegs targetInfo stk_args_offset args

           -- register assignment for *this function call*
        (reg_locs', reg_call_leftovers, reg_call_sp_stk_args)
           = assignRegs targetInfo stk_args_offset (take arity args)

        load_regs
           | no_load_regs || args_in_regs = empty
           | otherwise                    = loadRegOffs reg_locs'

        (this_call_args, rest_args) = splitAt arity args

           -- the offset of the stack args from initial Sp
        sp_stk_args
           | args_in_regs = stk_args_offset
           | no_load_regs = stk_args_offset
           | otherwise    = reg_call_sp_stk_args

           -- the stack args themselves
        this_call_stack_args
           | args_in_regs = reg_call_leftovers -- sp offsets are wrong
           | no_load_regs = this_call_args
           | otherwise    = reg_call_leftovers

        stack_args_size = sum (map (argSize targetInfo) this_call_stack_args)

        overflow_regs = args_in_regs && length reg_locs > length reg_locs'

        save_extra_regs = (save_extra_doc, (size,size))
          where
             -- we have extra arguments in registers to save
              extra_reg_locs = drop (length reg_locs') (reverse reg_locs)
              adj_reg_locs = [ (reg, off - adj + 1) |
                               (reg,off) <- extra_reg_locs ]
              adj = case extra_reg_locs of
                      (_reg, fst_off):_ -> fst_off
                      [] -> error "Impossible: genapply.hs : No extra register locations"
              size = snd (last adj_reg_locs) + 1

              save_extra_doc =
                text "Sp_adj(" <> int (-size) <> text ");" $$
                saveRegOffs adj_reg_locs $$
                loadSpWordOff "W_" 0 <> text " = " <>
                             mkApplyInfoName rest_args <> semi

        shuffle_extra_args = (shuffle_extra_doc, (shuffle_prof_stack, shuffle_norm_stack))
          where
           shuffle_extra_doc =
            vcat [ text "#if defined(PROFILING)"
                 , shuffle_prof_doc
                 , text "#else // defined(PROFILING)"
                 , shuffle_norm_doc
                 , text "#endif // defined(PROFILING)"
                 ]

           (shuffle_prof_doc, shuffle_prof_stack) = shuffle True
           (shuffle_norm_doc, shuffle_norm_stack) = shuffle False

           -- Sadly here we have to insert an stg_restore_cccs frame
           -- just underneath the stg_ap_*_info frame if we're
           -- profiling; see Note [jump_SAVE_CCCS]
           shuffle prof = (shuffle_doc, -sp_adj)
             where
             sp_adj = sp_stk_args - 1 - offset
             offset = if prof then 2 else 0
             shuffle_doc =
               vcat (map (shuffle_down (offset + 1))
                      [sp_stk_args .. sp_stk_args + stack_args_size - 1]) $$
               (if prof
                 then
                   loadSpWordOff "W_" (sp_stk_args + stack_args_size - 3)
                     <> text " = stg_restore_cccs_info;" $$
                   loadSpWordOff "W_" (sp_stk_args + stack_args_size - 2)
                     <> text " = CCCS;"
                 else empty) $$
               loadSpWordOff "W_" (sp_stk_args + stack_args_size-1)
                     <> text " = "
                     <> mkApplyInfoName rest_args <> semi $$
               text "Sp_adj(" <> int sp_adj <> text ");"

        shuffle_down j i =
             loadSpWordOff "W_" (i-j) <> text " = " <>
             loadSpWordOff "W_" i <> semi


-- The EXACT ARITY case
--
--      if (arity == 1) {
--          Sp++;
--          JMP_(GET_ENTRY(R1.cl));

    exact_arity_case
        = text "if (arity == " <> int n_args <> text ") {" $$
          let
             (reg_doc, sp')
                | no_load_regs || args_in_regs = (empty, stk_args_offset)
                | otherwise    = loadRegArgs targetInfo stk_args_offset args
          in
          nest 4 (vcat [
--          text "TICK_SLOW_CALL_" <> text ticker <> text "_CORRECT();",
            reg_doc,
            text "Sp_adj(" <> int sp' <> text ");",
            if is_pap
                then text "R2 = " <> fun_info_label <> semi
                else empty,
            if is_fun_case then mb_tag_node targetInfo n_args else empty,
            mkJump targetInfo (text jump) live (if no_load_regs then [] else args)
          ])

-- The LARGER ARITY cases:
--
--      } else /* arity > 1 */ {
--          BUILD_PAP(1,0,(W_)&stg_ap_v_info);
--      }

    (larger_arity_doc, larger_arity_stack) = (larger_doc, stack)
     where
       -- offsets in case we need to save regs:
       (reg_locs, _leftovers, sp_offset)
           = assignRegs targetInfo stk_args_slow_offset args
           -- BUILD_PAP assumes args start at offset 1

       stack | args_in_regs = (sp_offset, sp_offset)
             | otherwise    = (0,0)

       larger_doc =
           text "} else {" $$
           let
             save_regs
                | args_in_regs =
                        text "Sp_adj(" <> int (-sp_offset) <> text ");" $$
                        saveRegOffs  reg_locs
                | otherwise =
                        empty
           in
           nest 4 (vcat [
--              text "TICK_SLOW_CALL_" <> text ticker <> text "_TOO_FEW();",
                save_regs,
                -- Before building the PAP, tag the function closure pointer
                if is_fun_case then
                  vcat [
                     text "if (arity < " <> int tagBitsMax <> text ") {",
                     text "  R1 = R1 + arity" <> semi,
                     text "}"
                   ]
                  else empty
                ,
                text macro <> char '(' <> int n_args <> comma <>
                                        int all_args_size <>
                                        text "," <> fun_info_label <>
                                        text "," <> text disamb <>
                                        text ");"
           ]) $$
           char '}'


-- Note [jump_SAVE_CCCS]
-- ~~~~~~~~~~~~~~~~~~~~~
-- When profiling, if we have some extra arguments to apply that we
-- save to the stack, we must also save the current cost centre stack
-- and restore it when applying the extra arguments.  This is all
-- handled by the macro jump_SAVE_CCCS(target), defined in
-- rts/AutoApply.h.
--
-- At the jump, the stack will look like this:
--
--      ... extra args ...
--      stg_ap_pp_info
--      CCCS
--      stg_restore_cccs_info

-- --------------------------------------
-- Examine tag bits of function pointer and enter it
-- directly if needed.
-- TODO: remove the redundant case in the original code.
enterFastPath :: TargetInfo -> Bool -> Bool -> [ArgRep] -> Doc
enterFastPath targetInfo no_load_regs args_in_regs args
    | Just tag <- tagForArity targetInfo (length args)
    = enterFastPathHelper targetInfo tag no_load_regs args_in_regs args
enterFastPath _ _ _ _ = empty

tagForArity :: TargetInfo -> Int -> Maybe Int
tagForArity TargetInfo {..} i | i < tagBitsMax = Just i
                              | otherwise      = Nothing

enterFastPathHelper :: TargetInfo
                    -> Int
                    -> Bool
                    -> Bool
                    -> [ArgRep]
                    -> Doc
enterFastPathHelper targetInfo tag no_load_regs args_in_regs args =
  text "if (GETTAG(R1)==" <> int tag <> text ") {" $$
  nest 4 (vcat [
    reg_doc,
    text "Sp_adj(" <> int sp' <> text ");",
    -- enter, but adjust offset with tag
    mkJump targetInfo (text "%GET_ENTRY(R1-" <> int tag <> text ")") ["R1"] args
  ]) $$
  text "}"
  -- I don't totally understand this code, I copied it from
  -- exact_arity_case
  -- TODO: refactor
    where
        -- offset of arguments on the stack at slow apply calls.
    stk_args_slow_offset = 1

    stk_args_offset
        | args_in_regs = 0
        | otherwise    = stk_args_slow_offset

    (reg_doc, sp')
        | no_load_regs || args_in_regs = (empty, stk_args_offset)
        | otherwise    = loadRegArgs targetInfo stk_args_offset args

tickForArity :: TargetInfo -> Int -> Doc
tickForArity _targetInfo _arity = empty
{-
    | Just tag <- tagForArity targetInfo arity
    = vcat [
            text "W_[TOTAL_CALLS] = W_[TOTAL_CALLS] + 1;",
            text "W_[SLOW_CALLS_" <> int arity <> text "] = W_[SLOW_CALLS_" <> int arity <> text "] + 1;",
            text "if (TO_W_(StgFunInfoExtra_arity(%FUN_INFO(%INFO_PTR(UNTAG(R1))))) == " <> int arity <> text " ) {",
            text "  W_[RIGHT_ARITY_" <> int arity <> text "] = W_[RIGHT_ARITY_" <> int arity <> text "] + 1;",
            text "  if (GETTAG(R1)==" <> int tag <> text ") {",
            text "    W_[TAGGED_PTR_" <> int arity <> text "] = W_[TAGGED_PTR_" <> int arity <> text "] + 1;",
            text "  } else {",
            -- force a halt when not tagged!
--          text "    W_[0]=0;",
            text "  }",
            text "}"
          ]
tickForArity _ _ = text "W_[TOTAL_CALLS] = W_[TOTAL_CALLS] + 1;"
-}

-- -----------------------------------------------------------------------------
-- generate an apply function

-- args is a list of 'p', 'n', 'f', 'd' or 'l'
formalParam :: ArgRep -> Int -> Doc
formalParam V _ = empty
formalParam arg n =
    formalParamType arg <> space <>
    text "arg" <> int n <> text ", "

formalParamType :: ArgRep -> Doc
formalParamType arg = argRep arg

argRep :: ArgRep -> Doc
argRep F   = text "F_"
argRep D   = text "D_"
argRep L   = text "L_"
argRep P   = text "gcptr"
argRep V16 = text "V16_"
argRep V32 = text "V32_"
argRep V64 = text "V64_"
argRep _   = text "W_"

genApply :: TargetInfo -> [ArgRep] -> Doc
genApply targetInfo args =
   let
    fun_ret_label  = mkApplyRetName args
    fun_info_label = mkApplyInfoName args
    all_args_size  = sum (map (argSize targetInfo) args)

    (bco_doc, bco_stack) =
       genMkPAP targetInfo "BUILD_PAP" "ENTRY_LBL(stg_BCO)" ["R1"] "FUN" "BCO"
             True{-stack apply-} False{-args on stack-} False{-not a PAP-}
             args all_args_size fun_info_label {- tag stmt -}False

    (fun_doc, fun_stack) =
       genMkPAP targetInfo "BUILD_PAP" "%GET_ENTRY(UNTAG(R1))" ["R1"] "FUN" "FUN"
             False{-reg apply-} False{-args on stack-} False{-not a PAP-}
             args all_args_size fun_info_label {- tag stmt -}True

    (pap_doc, pap_stack) =
       genMkPAP targetInfo "NEW_PAP" "stg_PAP_apply" ["R1", "R2"] "PAP" "PAP"
             True{-stack apply-} False{-args on stack-} True{-is a PAP-}
             args all_args_size fun_info_label {- tag stmt -}False

    stack_usage = maxStack [bco_stack, fun_stack, pap_stack]
    applyName = mkApplyName args
    (regsOffs,_,_) =assignRegs targetInfo 1 args
    regs = map fst regsOffs
   in
    vcat [
      text "INFO_TABLE_RET(" <> applyName <> text ", " <>
        text "RET_SMALL, W_ info_ptr, " <> (cat $ zipWith formalParam args [1..]) <>
        text ")\n{",
      nest 4 (vcat $ vecsCpp applyName regs [
       text "W_ _unused;",
       text "W_ info;",
       text "W_ arity;",
       text "unwind Sp = Sp + WDS(" <> int (1 + all_args_size) <> text ");",

--    if fast == 1:
--        print "static void *lbls[] ="
--        print "  { [FUN]             &&fun_lbl,"
--        print "    [FUN_1_0]         &&fun_lbl,"
--        print "    [FUN_0_1]        &&fun_lbl,"
--        print "    [FUN_2_0]        &&fun_lbl,"
--        print "    [FUN_1_1]        &&fun_lbl,"
--        print "    [FUN_0_2]        &&fun_lbl,"
--        print "    [FUN_STATIC]      &&fun_lbl,"
--        print "    [PAP]             &&pap_lbl,"
--        print "    [THUNK]           &&thunk_lbl,"
--        print "    [THUNK_1_0]              &&thunk_lbl,"
--        print "    [THUNK_0_1]              &&thunk_lbl,"
--        print "    [THUNK_2_0]              &&thunk_lbl,"
--        print "    [THUNK_1_1]              &&thunk_lbl,"
--        print "    [THUNK_0_2]              &&thunk_lbl,"
--        print "    [THUNK_STATIC]    &&thunk_lbl,"
--        print "    [THUNK_SELECTOR]  &&thunk_lbl,"
--        print "    [IND]            &&ind_lbl,"
--        print "    [IND_STATIC]      &&ind_lbl,"
--        print "  };"

       tickForArity targetInfo (length args),
       text "",
       text "IF_DEBUG(apply,foreign \"C\" debugBelch(\"" <> fun_ret_label <>
          text "... \", NULL); foreign \"C\" printClosure(R1 \"ptr\"));",

       text "IF_DEBUG(sanity,(_unused) = foreign \"C\" checkStackFrame(Sp+WDS(" <> int (1 + all_args_size)
        <> text ")\"ptr\"));",

--       text "IF_DEBUG(sanity,checkStackChunk(Sp+" <> int (1 + all_args_size) <>
--        text ", CurrentTSO->stack + CurrentTSO->stack_size));",

--       text "TICK_SLOW_CALL(" <> int (length args) <> text ");",

       let do_assert [] _ = []
           do_assert (a:as) offset
                | isPtr a   = this : rest
                | otherwise = rest
                where this = text "ASSERT(LOOKS_LIKE_CLOSURE_PTR(Sp("
                                 <> int offset <> text ")));"
                      rest = do_assert as (offset + argSize targetInfo a)
       in
       vcat (do_assert args 1),

       text  "again:",

       -- if pointer is tagged enter it fast!
       enterFastPath targetInfo False False args,

       stackCheck targetInfo args False{-args on stack-}
                  fun_info_label stack_usage,

       -- Functions can be tagged, so we untag them!
       text  "R1 = UNTAG(R1);",
       text  "info = %INFO_PTR(R1);",

--    if fast == 1:
--        print "    goto *lbls[info->type];";
--    else:
        text "switch [INVALID_OBJECT .. N_CLOSURE_TYPES] (TO_W_(%INFO_TYPE(%STD_INFO(info)))) {",
        nest 4 (vcat [

--    if fast == 1:
--        print "    bco_lbl:"
--    else:
        text "case BCO: {",
        nest 4 (vcat [
          text "arity = TO_W_(StgBCO_arity(R1));",
          text "ASSERT(arity > 0);",
          bco_doc
         ]),
        text "}",

--    if fast == 1:
--        print "    fun_lbl:"
--    else:
        text "case FUN,",
        text "     FUN_1_0,",
        text "     FUN_0_1,",
        text "     FUN_2_0,",
        text "     FUN_1_1,",
        text "     FUN_0_2,",
        text "     FUN_STATIC,",
        text "     CONTINUATION: {",
        nest 4 (vcat [
          text "arity = TO_W_(StgFunInfoExtra_arity(%FUN_INFO(info)));",
          text "ASSERT(arity > 0);",
          fun_doc
         ]),
        text "}",

--    if fast == 1:
--        print "    pap_lbl:"
--    else:

        text "case PAP: {",
        nest 4 (vcat [
          text "arity = TO_W_(StgPAP_arity(R1));",
          text "ASSERT(arity > 0);",
          pap_doc
         ]),
        text "}",

        text "",

--    if fast == 1:
--        print "    thunk_lbl:"
--    else:
        text "case AP,",
        text "     AP_STACK,",
        text "     BLACKHOLE,",
        text "     WHITEHOLE,",
        text "     THUNK,",
        text "     THUNK_1_0,",
        text "     THUNK_0_1,",
        text "     THUNK_2_0,",
        text "     THUNK_1_1,",
        text "     THUNK_0_2,",
        text "     THUNK_STATIC,",
        text "     THUNK_SELECTOR: {",
        nest 4 (vcat [
--          text "TICK_SLOW_CALL_UNEVALD(" <> int (length args) <> text ");",
          text "Sp(0) = " <> fun_info_label <> semi,
          -- CAREFUL! in SMP mode, the info table may already have been
          -- overwritten by an indirection, so we must enter the original
          -- info pointer we read, don't read it again, because it might
          -- not be enterable any more.
          mkJumpSaveCCCS targetInfo
            (text "%ENTRY_CODE(info)") ["R1"] args,
            -- see Note [jump_SAVE_CCCS]
          text ""
         ]),
        text "}",

--    if fast == 1:
--        print "    ind_lbl:"
--    else:
        text "case IND,",
        text "     IND_STATIC: {",
        nest 4 (vcat [
          -- N.B. annoyingly the %acquire syntax must place its result in a local register
          -- as it is a Cmm prim call node.
          text "P_ p;",
          text "p = %acquire StgInd_indirectee(R1);",
          text "R1 = p;",
            -- An indirection node might contain a tagged pointer
          text "goto again;"
         ]),
        text "}",
        text "",

--    if fast == 0:

       text "default: {",
       nest 4 (
         text "foreign \"C\" barf(\"" <> fun_ret_label <> text "\", NULL) never returns;"
       ),
       text "}"

        ]),
       text "}"
      ]),

      text "}"
    ]

-- -----------------------------------------------------------------------------
-- Making a fast unknown application, args are in regs

genApplyFast :: TargetInfo -> [ArgRep] -> Doc
genApplyFast targetInfo args =
   let
    fun_fast_label = mkApplyFastName args
    fun_ret_label  = text "RET_LBL" <> parens (mkApplyName args)
    fun_info_label = mkApplyInfoName args
    all_args_size  = sum (map (argSize targetInfo) args)

    (fun_doc, fun_stack) =
       genMkPAP targetInfo "BUILD_PAP" "%GET_ENTRY(UNTAG(R1))" ["R1"] "FUN" "FUN"
            False{-reg apply-} True{-args in regs-} False{-not a PAP-}
            args all_args_size fun_info_label {- tag stmt -}True

    (reg_locs, _leftovers, sp_offset) = assignRegs targetInfo 1 args

    stack_usage = maxStack [fun_stack, (sp_offset,sp_offset)]

   in
    vcat $ [
     fun_fast_label,
     char '{',
     nest 4 (vcat $ vecsCpp fun_fast_label (map fst reg_locs) [
        text "W_ info;",
        text "W_ arity;",

        tickForArity targetInfo (length args),

        -- if pointer is tagged enter it fast!
        enterFastPath targetInfo False True args,

        stackCheck targetInfo args True{-args in regs-}
                   fun_info_label stack_usage,

        -- Functions can be tagged, so we untag them!
        text  "R1 = UNTAG(R1);",
        text  "info = %GET_STD_INFO(R1);",
        text "switch [INVALID_OBJECT .. N_CLOSURE_TYPES] (TO_W_(%INFO_TYPE(info))) {",
        nest 4 (vcat [
          text "case FUN,",
          text "     FUN_1_0,",
          text "     FUN_0_1,",
          text "     FUN_2_0,",
          text "     FUN_1_1,",
          text "     FUN_0_2,",
          text "     FUN_STATIC,",
          text "     CONTINUATION: {",
          nest 4 (vcat [
            text "arity = TO_W_(StgFunInfoExtra_arity(%GET_FUN_INFO(R1)));",
            text "ASSERT(arity > 0);",
            fun_doc
           ]),
          char '}',

          text "default: {",
          nest 4 (vcat [
             text "Sp_adj" <> parens (int (-sp_offset)) <> semi,
             saveRegOffs reg_locs,
             mkJump targetInfo fun_ret_label [] args
          ]),
          char '}'
        ]),

       char '}'
     ]),
     char '}'
  ]

-- -----------------------------------------------------------------------------
-- Making a stack apply

-- These little functions are like slow entry points.  They provide
-- the layer between the PAP entry code and the function's fast entry
-- point: namely they load arguments off the stack into registers (if
-- available) and jump to the function's entry code.
--
-- On entry: R1 points to the function closure
--           arguments are on the stack starting at Sp
--
-- Invariant: the list of arguments never contains void.  Since we're only
-- interested in loading arguments off the stack here, we can ignore
-- void arguments.

mkStackApplyEntryLabel:: [ArgRep] -> Doc
mkStackApplyEntryLabel args = text "stg_ap_stk_" <> text (concatMap showArg args)

genStackApply :: TargetInfo -> [ArgRep] -> Doc
genStackApply targetInfo args =
  vcat [
    fn_entry_label,
    text "{", nest 4 body, text "}"
   ]
 where
   fn_entry_label = mkStackApplyEntryLabel args
   (assign_regs, sp') = loadRegArgs targetInfo 0 args
   (regs, _, _) = assignRegs targetInfo 0 args
   body = vcat $
     vecsCpp fn_entry_label (map fst regs)
       [ assign_regs
       , text "Sp_adj" <> parens (int sp') <> semi
       , mkJump targetInfo (text "%GET_ENTRY(UNTAG(R1))") ["R1"] args
       ]

-- -----------------------------------------------------------------------------
-- Stack save entry points.
--
-- These code fragments are used to save registers on the stack at a heap
-- check failure in the entry code for a function.  We also have to save R1
-- and the return address (stg_gc_fun_info) on the stack.  See __stg_gc_fun
-- in HeapStackCheck.cmm for more details.

mkStackSaveEntryLabel :: [ArgRep] -> Doc
mkStackSaveEntryLabel args = text "stg_stk_save_" <> text (concatMap showArg args)

genStackSave :: TargetInfo -> [ArgRep] -> Doc
genStackSave targetInfo args =
  vcat [
    fn_entry_label,
    text "{", nest 4 body, text "}"
   ]
 where
   fn_entry_label = mkStackSaveEntryLabel args
   body = vcat $
     vecsCpp fn_entry_label (map fst reg_locs)
       [ text "Sp_adj" <> parens (int (-sp_offset)) <> semi
       , saveRegOffs reg_locs
       , text "Sp(2) = R1;"
       , text "Sp(1) =" <+> int stk_args <> semi
       , text "Sp(0) = stg_gc_fun_info;"
       , text "jump stg_gc_noregs [];"
       ]

   std_frame_size = 3 -- the std bits of the frame. See StgRetFun in Closures.h,
                      -- and the comment on stg_fun_gc_gen
                      -- in HeapStackCheck.cmm.
   (reg_locs, leftovers, sp_offset) = assignRegs targetInfo std_frame_size args

   -- number of words of arguments on the stack.
   stk_args = sum (map (argSize targetInfo) leftovers) + sp_offset - std_frame_size

-- -----------------------------------------------------------------------------
-- The prologue...

main :: IO ()
main = do
  (path:args) <- getArgs
  -- Because of Note [AutoApply.cmm for vectors],
  -- we want to generate the code for V16/V32/V64 into separate files.
  --
  -- So GenApply takes an optional second argument (after the target info argument).
  --
  --  * No argument: generate code for all ArgReps except vectors
  --  * -V16: generate code involving V16 vectors (at the widest)
  --  * -V32: generate code involving V32 vectors (at the widest)
  --  * -V64: generate code involving V64 vectors (at the widest)
  let mbVec = case args of
                arg:_ -> case arg of
                  "-V16" -> Just V16
                  "-V32" -> Just V32
                  "-V64" -> Just V64
                  _      -> error $ "genapply arg: " ++ arg
                _ -> Nothing
      wantArgs :: [ArgRep] -> Bool
      wantArgs reps =
        case mbVec of
          Nothing
            -> isNothing $ largestVec reps
          Just v
            | v' : _ <- sortOn Down reps
            , v' == v
            -> True
            | otherwise
            -> False
  targetInfo <- parseTargetInfo path
  let the_code = vcat [
                text "// DO NOT EDIT!",
                text "// Automatically generated by utils/genapply/Main.hs",
                text "",
                text "#include \"Cmm.h\"",
                text "#include \"AutoApply.h\"",
                text "#if !defined(UnregisterisedCompiler)",
                text "import CLOSURE ALLOC_RTS_ctr;",
                text "import CLOSURE ALLOC_RTS_tot;",
                text "import CLOSURE HEAP_CHK_ctr;",
                text "import CLOSURE RtsFlags;",
                text "import CLOSURE stg_PAP_info;",
         vcat [ text "import CLOSURE stg_ap_" <> text str <> text "_info;"
              | argReps <- applyTypes
              , wantArgs argReps
              , let str = concatMap showArg argReps ],
                text "import CLOSURE stg_gc_fun_info;",
                text "import CLOSURE stg_restore_cccs_info;",
                text "#endif // !defined(UnregisterisedCompiler)",

         -- NB: the vector apply/save functions are defined in separate modules,
         -- as per Note [AutoApply.cmm for vectors], so we import them here.
         vcat [ text "import" <+> fun <> semi
              | isNothing mbVec
              , argReps <- stackApplyTypes
              , not (wantArgs argReps)
              , fun <- [ mkStackApplyEntryLabel argReps
                       , mkStackSaveEntryLabel argReps ]
              ],

                vcat $ intersperse (text "") $
                   [ genApply targetInfo argReps
                   | argReps <- applyTypes
                   , wantArgs argReps ],
                vcat $ intersperse (text "") $
                   [ genStackFns targetInfo argReps
                   | argReps <- stackApplyTypes
                   , wantArgs argReps ],
                vcat $ intersperse (text "") $
                   [ genApplyFast targetInfo argReps
                   | argReps <- applyTypes
                   , wantArgs argReps ],

                if isNothing mbVec
                then
         vcat [ genStackApplyArray stackApplyTypes,
                genStackSaveArray stackApplyTypes,
                genBitmapArray targetInfo stackApplyTypes
              ]
                else empty,
                text ""  -- add a newline at the end of the file
              ]

  putStr (render the_code)

-- These have been shown to cover about 99% of cases in practice...
applyTypes :: [[ArgRep]]
applyTypes = [
        [V],
        [F],
        [D],
        [L],
        [V16],
        [V32],
        [V64],
        [N],
        [P],
        [P,V],
        [P,P],
        [P,P,V],
        [P,P,P],
        [P,P,P,V],
        [P,P,P,P],
        [P,P,P,P,P],
        [P,P,P,P,P,P]
   ]

-- No need for V args in the stack apply cases.
-- ToDo: the stack apply and stack save code doesn't make a distinction
-- between N and P (they both live in the same register), only the bitmap
-- changes, so we could share the apply/save code between lots of cases.
--
--  NOTE: other places to change if you change stackApplyTypes:
--       - rts/include/rts/storage/FunTypes.h
--       - GHC.StgToCmm.Layout: stdPattern
stackApplyTypes :: [[ArgRep]]
stackApplyTypes = [
        [],
        [N],
        [P],
        [F],
        [D],
        [L],
        [V16],
        [V32],
        [V64],
        [N,N],
        [N,P],
        [P,N],
        [P,P],
        [N,N,N],
        [N,N,P],
        [N,P,N],
        [N,P,P],
        [P,N,N],
        [P,N,P],
        [P,P,N],
        [P,P,P],
        [P,P,P,P],
        [P,P,P,P,P],
        [P,P,P,P,P,P],
        [P,P,P,P,P,P,P],
        [P,P,P,P,P,P,P,P]
   ]

genStackFns :: TargetInfo -> [ArgRep] -> Doc
genStackFns targetInfo args
  =  genStackApply targetInfo args
  $$ genStackSave targetInfo args

genStackApplyArray :: [[ArgRep]] -> Doc
genStackApplyArray types =
  vcat [
    text "section \"relrodata\" {",
    text "stg_ap_stack_entries:",
    text "W_ 0; W_ 0; W_ 0;", -- ARG_GEN, ARG_GEN_BIG, ARG_BCO
    vcat (map arr_ent types),
    text "}"
  ]
 where
  arr_ent ty = text "W_" <+> mkStackApplyEntryLabel ty <> semi

genStackSaveArray :: [[ArgRep]] -> Doc
genStackSaveArray types =
  vcat [
    text "section \"relrodata\" {",
    text "stg_stack_save_entries:",
    text "W_ 0; W_ 0; W_ 0;", -- ARG_GEN, ARG_GEN_BIG, ARG_BCO
    vcat (map arr_ent types),
    text "}"
  ]
 where
  arr_ent ty = text "W_" <+> mkStackSaveEntryLabel ty <> semi

genBitmapArray :: TargetInfo -> [[ArgRep]] -> Doc
genBitmapArray targetInfo@TargetInfo {..} types =
  vcat [
    text "section \"rodata\" {",
    text "stg_arg_bitmaps:",
    text "W_ 0; W_ 0; W_ 0;", -- ARG_GEN, ARG_GEN_BIG, ARG_BCO
    vcat (map gen_bitmap types),
    text "}"
  ]
  where
   gen_bitmap ty = text "W_" <+> int bitmap_val <> semi
        where bitmap_val =
                (fromIntegral (mkBitmap targetInfo ty) `shiftL` bitmapBitsShift)
                 .|. sum (map (argSize targetInfo) ty)
