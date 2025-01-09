{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.CmmToAsm.LA64.Instr where

import GHC.Prelude

import GHC.CmmToAsm.LA64.Cond
import GHC.CmmToAsm.LA64.Regs

import GHC.CmmToAsm.Instr (RegUsage(..))
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Utils
import GHC.CmmToAsm.Config
import GHC.Platform.Reg

import GHC.Platform.Regs
import GHC.Platform.Reg.Class.Separate
import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Label
import GHC.Cmm
import GHC.Cmm.CLabel
import GHC.Utils.Outputable
import GHC.Platform
import GHC.Types.Unique.DSM

import GHC.Utils.Panic
import Data.Maybe
import GHC.Stack
import GHC.Data.FastString (LexicalFastString)

-- | Stack frame header size
-- Each stack frame contains ra and fp -- prologue.
stackFrameHeaderSize :: Int
stackFrameHeaderSize = 2 * spillSlotSize

-- | All registers are 8 byte wide.
spillSlotSize :: Int
spillSlotSize = 8

-- | The number of bytes that the stack pointer should be aligned to.
stackAlign :: Int
stackAlign = 16

-- | The number of spill slots available without allocating more.
maxSpillSlots :: NCGConfig -> Int
maxSpillSlots config
    = (
        (ncgSpillPreallocSize config - stackFrameHeaderSize)
         `div`
         spillSlotSize
      ) - 1

-- | Convert a spill slot number to a *byte* offset.
spillSlotToOffset :: Int -> Int
spillSlotToOffset slot
   = stackFrameHeaderSize + spillSlotSize * slot

instance Outputable RegUsage where
    ppr (RU reads writes) = text "RegUsage(reads:" <+> ppr reads <> comma <+> text "writes:" <+> ppr writes <> char ')'

-- | Get the registers that are being used by this instruction.
-- regUsage doesn't need to do any trickery for jumps and such.
-- Just state precisely the regs read and written by that insn.
-- The consequences of control flow transfers, as far as register
-- allocation goes, are taken care of by the register allocator.
--
-- RegUsage = RU [<read regs>] [<write regs>]
regUsageOfInstr :: Platform -> Instr -> RegUsage
regUsageOfInstr platform instr = case instr of
  -- Pseudo Instructions
  ANN _ i                  -> regUsageOfInstr platform i
  COMMENT{}                -> usage ([], [])
  MULTILINE_COMMENT{}      -> usage ([], [])
  PUSH_STACK_FRAME         -> usage ([], [])
  POP_STACK_FRAME          -> usage ([], [])
  DELTA{}                  -> usage ([], [])
  LOCATION{}               -> usage ([], [])

  -- 1. Arithmetic Instructions ------------------------------------------------
  ADD dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  SUB dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  ALSL dst src1 src2 src3  -> usage (regOp src1 ++ regOp src2 ++ regOp src3, regOp dst)
  ALSLU dst src1 src2 src3 -> usage (regOp src1 ++ regOp src2 ++ regOp src3, regOp dst)
  LU12I dst src1           -> usage (regOp src1, regOp dst)
  LU32I dst src1           -> usage (regOp src1, regOp dst)
  LU52I dst src1 src2      -> usage (regOp src1 ++ regOp src2, regOp dst)
  SSLT dst src1 src2       -> usage (regOp src1 ++ regOp src2, regOp dst)
  SSLTU dst src1 src2      -> usage (regOp src1 ++ regOp src2, regOp dst)
  PCADDI dst src1          -> usage (regOp src1, regOp dst)
  PCADDU12I dst src1       -> usage (regOp src1, regOp dst)
  PCADDU18I dst src1       -> usage (regOp src1, regOp dst)
  PCALAU12I dst src1       -> usage (regOp src1, regOp dst)
  AND dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  OR dst src1 src2         -> usage (regOp src1 ++ regOp src2, regOp dst)
  XOR dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  NOR dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  ANDN dst src1 src2       -> usage (regOp src1 ++ regOp src2, regOp dst)
  ORN dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  MUL dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  MULW dst src1 src2       -> usage (regOp src1 ++ regOp src2, regOp dst)
  MULWU dst src1 src2      -> usage (regOp src1 ++ regOp src2, regOp dst)
  MULH dst src1 src2       -> usage (regOp src1 ++ regOp src2, regOp dst)
  MULHU dst src1 src2      -> usage (regOp src1 ++ regOp src2, regOp dst)
  DIV dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  DIVU dst src1 src2       -> usage (regOp src1 ++ regOp src2, regOp dst)
  MOD dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  MODU dst src1 src2       -> usage (regOp src1 ++ regOp src2, regOp dst)
  -- 2. Bit-shift Instructions ------------------------------------------
  SLL dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  SRL dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  SRA dst src1 src2        -> usage (regOp src1 ++ regOp src2, regOp dst)
  ROTR dst src1 src2       -> usage (regOp src1 ++ regOp src2, regOp dst)
  -- 3. Bit Manipulation Instructions ------------------------------------------
  EXT dst src1             -> usage (regOp src1, regOp dst)
  CLO dst src1             -> usage (regOp src1, regOp dst)
  CLZ dst src1             -> usage (regOp src1, regOp dst)
  CTO dst src1             -> usage (regOp src1, regOp dst)
  CTZ dst src1             -> usage (regOp src1, regOp dst)
  BYTEPICK dst src1 src2 src3 -> usage (regOp src1 ++ regOp src2 ++ regOp src3, regOp dst)
  REVB2H dst src1          -> usage (regOp src1, regOp dst)
  REVB4H dst src1          -> usage (regOp src1, regOp dst)
  REVB2W dst src1          -> usage (regOp src1, regOp dst)
  REVBD  dst src1          -> usage (regOp src1, regOp dst)
  REVH2W dst src1          -> usage (regOp src1, regOp dst)
  REVHD  dst src1          -> usage (regOp src1, regOp dst)
  BITREV4B dst src1        -> usage (regOp src1, regOp dst)
  BITREV8B dst src1        -> usage (regOp src1, regOp dst)
  BITREVW dst src1         -> usage (regOp src1, regOp dst)
  BITREVD dst src1         -> usage (regOp src1, regOp dst)
  BSTRINS _ dst src1 src2 src3  -> usage (regOp src1 ++ regOp src2 ++ regOp src3, regOp dst)
  BSTRPICK _ dst src1 src2 src3 -> usage (regOp src1 ++ regOp src2 ++ regOp src3, regOp dst)
  MASKEQZ dst src1 src2         -> usage (regOp src1 ++ regOp src2, regOp dst)
  MASKNEZ dst src1 src2         -> usage (regOp src1 ++ regOp src2, regOp dst)
  --
  -- Pseudo instructions
  NOP                      -> usage ([], [])
  MOV dst src              -> usage (regOp src, regOp dst)
  NEG dst src              -> usage (regOp src, regOp dst)
  CSET _cond dst src1 src2  -> usage (regOp src1 ++ regOp src2 , regOp dst)
  -- 4. Branch Instructions ----------------------------------------------------
  J t                      -> usage (regTarget t, [])
  J_TBL _ _ t              -> usage ([t], [])
  B t                      -> usage (regTarget t, [])
  BL t ps                  -> usage (regTarget t ++ ps, callerSavedRegisters)
  CALL36 t                 -> usage (regTarget t, [])
  TAIL36 r t               -> usage (regTarget t, regOp r)
  BCOND _ j d t tmp        -> usage (regTarget t ++ regOp j ++ regOp d ++ regOp tmp, regOp tmp)
  BEQZ j t                 -> usage (regTarget t ++ regOp j, [])
  BNEZ j t                 -> usage (regTarget t ++ regOp j, [])
  -- 5. Common Memory Access Instructions --------------------------------------
  LD _ dst src             -> usage (regOp src, regOp dst)
  LDU _ dst src            -> usage (regOp src, regOp dst)
  ST _ dst src             -> usage (regOp src ++ regOp dst, [])
  LDX _ dst src            -> usage (regOp src, regOp dst)
  LDXU _ dst src           -> usage (regOp src, regOp dst)
  STX _ dst src            -> usage (regOp src ++ regOp dst, [])
  LDPTR _ dst src          -> usage (regOp src, regOp dst)
  STPTR _ dst src          -> usage (regOp src ++ regOp dst, [])
  -- 6. Bound Check Memory Access Instructions ---------------------------------
  -- LDCOND dst src1 src2     -> usage (regOp src1 ++ regOp src2, regOp dst)
  -- STCOND dst src1 src2     -> usage (regOp src1 ++ regOp src2, regOp dst)
  -- 7. Atomic Memory Access Instructions --------------------------------------
  -- 8. Barrier Instructions ---------------------------------------------------
  DBAR _hint               -> usage ([], [])
  IBAR _hint               -> usage ([], [])
  -- 11. Floating Point Instructions -------------------------------------------
  FMAX dst src1 src2       -> usage (regOp src1 ++ regOp src2, regOp dst)
  FMIN dst src1 src2       -> usage (regOp src1 ++ regOp src2, regOp dst)
  FMAXA dst src1 src2      -> usage (regOp src1 ++ regOp src2, regOp dst)
  FMINA dst src1 src2      -> usage (regOp src1 ++ regOp src2, regOp dst)
  FNEG dst src1            -> usage (regOp src1, regOp dst)

  FCVT dst src             -> usage (regOp src, regOp dst)
  -- SCVTF dst src            -> usage (regOp src, regOp src ++ regOp dst)
  SCVTF dst src            -> usage (regOp src, regOp dst)
  FCVTZS dst src1 src2     -> usage (regOp src2, regOp src1 ++ regOp dst)
  FABS dst src             -> usage (regOp src, regOp dst)
  FMA _ dst src1 src2 src3 -> usage (regOp src1 ++ regOp src2 ++ regOp src3, regOp dst)

  _ -> panic $ "regUsageOfInstr: " ++ instrCon instr

  where
        -- filtering the usage is necessary, otherwise the register
        -- allocator will try to allocate pre-defined fixed stg
        -- registers as well, as they show up.
        usage :: ([Reg], [Reg]) -> RegUsage
        usage (srcRegs, dstRegs) =
          RU
            (map mkFmt $ filter (interesting platform) srcRegs)
            (map mkFmt $ filter (interesting platform) dstRegs)

        mkFmt r = RegWithFormat r fmt
          where
            fmt = case cls of
              RcInteger -> II64
              RcFloat   -> FF64
              RcVector  -> sorry "The LoongArch64 NCG does not (yet) support vectors; please use -fllvm."
            cls = case r of
              RegVirtual vr -> classOfVirtualReg (platformArch platform) vr
              RegReal rr -> classOfRealReg rr

        regAddr :: AddrMode -> [Reg]
        regAddr (AddrRegReg r1 r2) = [r1, r2]
        regAddr (AddrRegImm r1 _)  = [r1]
        regAddr (AddrReg r1)       = [r1]

        regOp :: Operand -> [Reg]
        regOp (OpReg _ r1) = [r1]
        regOp (OpAddr a)    = regAddr a
        regOp (OpImm _)     = []

        regTarget :: Target -> [Reg]
        regTarget (TBlock _) = []
        regTarget (TLabel _) = []
        regTarget (TReg r1)  = [r1]

        -- Is this register interesting for the register allocator?
        interesting :: Platform -> Reg -> Bool
        interesting _        (RegVirtual _)                 = True
        interesting platform (RegReal (RealRegSingle i))    = freeReg platform i

-- | Caller-saved registers (according to calling convention)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- |  0 |  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 |
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- |zero| ra | tp | sp | a0 | a1 | a2 | a3 | a4 | a5 | a6 | a7 | t0 | t1 | t2 | t3 | t4 | t5 | t6 | t7 | t8 | Rv | fp | s0 | s1 | s2 | s3 | s4 | s5 | s6 | s7 | s8 |
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 | 41 | 42 | 42 | 44 | 45 | 46 | 47 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 |
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--f| a0 | a1 | a2 | a3 | a4 | a5 | a6 | a7 | t0 | t1 | t2 | t3 | t4 | t5 | t6 | t7 | t8 | t9 | t10| t11| t12| t13| t14| t15| s0 | s1 | s2 | s3 | s4 | s5 | s6 | s7 |
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
callerSavedRegisters :: [Reg]
callerSavedRegisters =
    -- TODO: Not sure.
    [regSingle 1]                 -- ra
    ++ map regSingle [4 .. 11]    -- a0 - a7
    ++ map regSingle [12 .. 20]   -- t0 - t8
    ++ map regSingle [32 .. 39]   -- fa0 - fa7
    ++ map regSingle [40 .. 55]   -- ft0 - ft15

-- | Apply a given mapping to all the register references in this instruction.
patchRegsOfInstr :: Instr -> (Reg -> Reg) -> Instr
patchRegsOfInstr instr env = case instr of
    -- 0. Meta Instructions
    ANN d i             -> ANN d (patchRegsOfInstr i env)
    COMMENT{}           -> instr
    MULTILINE_COMMENT{} -> instr
    PUSH_STACK_FRAME    -> instr
    POP_STACK_FRAME     -> instr
    DELTA{}             -> instr
    LOCATION{}          -> instr
    -- 1. Arithmetic Instructions ------------------------------------------------
    ADD o1 o2 o3        -> ADD  (patchOp o1)  (patchOp o2)  (patchOp o3)
    SUB o1 o2 o3        -> SUB  (patchOp o1)  (patchOp o2)  (patchOp o3)
    ALSL o1 o2 o3 o4    -> ALSL  (patchOp o1)  (patchOp o2)  (patchOp o3)  (patchOp o4)
    ALSLU o1 o2 o3 o4   -> ALSLU (patchOp o1)  (patchOp o2)  (patchOp o3)  (patchOp o4)
    LU12I o1 o2         -> LU12I  (patchOp o1)  (patchOp o2)
    LU32I o1 o2         -> LU32I  (patchOp o1)  (patchOp o2)
    LU52I o1 o2 o3      -> LU52I  (patchOp o1)  (patchOp o2)  (patchOp o3)
    SSLT o1 o2 o3       -> SSLT  (patchOp o1)  (patchOp o2)  (patchOp o3)
    SSLTU o1 o2 o3      -> SSLTU  (patchOp o1)  (patchOp o2)  (patchOp o3)
    PCADDI o1 o2        -> PCADDI  (patchOp o1)  (patchOp o2)
    PCADDU12I o1 o2     -> PCADDU12I  (patchOp o1)  (patchOp o2)
    PCADDU18I o1 o2     -> PCADDU18I  (patchOp o1)  (patchOp o2)
    PCALAU12I o1 o2     -> PCALAU12I  (patchOp o1)  (patchOp o2)
    AND o1 o2 o3        -> AND  (patchOp o1)  (patchOp o2)  (patchOp o3)
    OR o1 o2 o3         -> OR  (patchOp o1)  (patchOp o2)  (patchOp o3)
    XOR o1 o2 o3        -> XOR  (patchOp o1)  (patchOp o2)  (patchOp o3)
    NOR o1 o2 o3        -> NOR  (patchOp o1)  (patchOp o2)  (patchOp o3)
    ANDN o1 o2 o3       -> ANDN  (patchOp o1)  (patchOp o2)  (patchOp o3)
    ORN o1 o2 o3        -> ORN  (patchOp o1)  (patchOp o2)  (patchOp o3)
    MUL o1 o2 o3        -> MUL  (patchOp o1)  (patchOp o2)  (patchOp o3)
    MULW o1 o2 o3       -> MULW  (patchOp o1)  (patchOp o2)  (patchOp o3)
    MULWU o1 o2 o3      -> MULWU (patchOp o1)  (patchOp o2)  (patchOp o3)
    MULH o1 o2 o3       -> MULH  (patchOp o1)  (patchOp o2)  (patchOp o3)
    MULHU o1 o2 o3      -> MULHU  (patchOp o1)  (patchOp o2)  (patchOp o3)
    DIV o1 o2 o3        -> DIV  (patchOp o1)  (patchOp o2)  (patchOp o3)
    MOD o1 o2 o3        -> MOD  (patchOp o1)  (patchOp o2)  (patchOp o3)
    DIVU o1 o2 o3       -> DIVU  (patchOp o1)  (patchOp o2)  (patchOp o3)
    MODU o1 o2 o3       -> MODU  (patchOp o1)  (patchOp o2)  (patchOp o3)
    -- 2. Bit-shift Instructions ------------------------------------------
    SLL o1 o2 o3        -> SLL  (patchOp o1)  (patchOp o2)  (patchOp o3)
    SRL o1 o2 o3        -> SRL  (patchOp o1)  (patchOp o2)  (patchOp o3)
    SRA o1 o2 o3        -> SRA  (patchOp o1)  (patchOp o2)  (patchOp o3)
    ROTR o1 o2 o3       -> ROTR  (patchOp o1)  (patchOp o2)  (patchOp o3)
    -- 3. Bit Manipulation Instructions ------------------------------------------
    EXT o1 o2             -> EXT  (patchOp o1)  (patchOp o2)
    CLO o1 o2             -> CLO  (patchOp o1)  (patchOp o2)
    CLZ o1 o2             -> CLZ  (patchOp o1)  (patchOp o2)
    CTO o1 o2             -> CTO  (patchOp o1)  (patchOp o2)
    CTZ o1 o2             -> CTZ  (patchOp o1)  (patchOp o2)
    BYTEPICK o1 o2 o3 o4  -> BYTEPICK  (patchOp o1)  (patchOp o2) (patchOp o3) (patchOp o4)
    REVB2H o1 o2          -> REVB2H  (patchOp o1)  (patchOp o2)
    REVB4H o1 o2          -> REVB4H  (patchOp o1)  (patchOp o2)
    REVB2W o1 o2          -> REVB2W  (patchOp o1)  (patchOp o2)
    REVBD  o1 o2          -> REVBD  (patchOp o1)  (patchOp o2)
    REVH2W o1 o2          -> REVH2W  (patchOp o1)  (patchOp o2)
    REVHD  o1 o2          -> REVHD  (patchOp o1)  (patchOp o2)
    BITREV4B o1 o2         -> BITREV4B  (patchOp o1)  (patchOp o2)
    BITREV8B o1 o2         -> BITREV8B  (patchOp o1)  (patchOp o2)
    BITREVW o1 o2          -> BITREVW  (patchOp o1)  (patchOp o2)
    BITREVD o1 o2          -> BITREVD  (patchOp o1)  (patchOp o2)
    BSTRINS f o1 o2 o3 o4  -> BSTRINS f (patchOp o1)  (patchOp o2)  (patchOp o3)  (patchOp o4)
    BSTRPICK f o1 o2 o3 o4 -> BSTRPICK f (patchOp o1)  (patchOp o2)  (patchOp o3)  (patchOp o4)
    MASKEQZ o1 o2 o3       -> MASKEQZ  (patchOp o1)  (patchOp o2)  (patchOp o3)
    MASKNEZ o1 o2 o3       -> MASKNEZ  (patchOp o1)  (patchOp o2)  (patchOp o3)
    --
    -- Pseudo instrcutions
    NOP                 -> NOP
    MOV o1 o2           -> MOV  (patchOp o1)  (patchOp o2)
    NEG o1 o2           -> NEG  (patchOp o1)  (patchOp o2)
    CSET cond o1 o2 o3  -> CSET cond (patchOp o1) (patchOp o2) (patchOp o3)
    -- 4. Branch Instructions ----------------------------------------------------
    -- TODO:
    J t            -> J (patchTarget t)
    J_TBL ids mbLbl t  -> J_TBL ids mbLbl (env t)
    B t            -> B (patchTarget t)
    BL t ps        -> BL (patchTarget t) ps
    CALL36 t       -> CALL36 (patchTarget t)
    TAIL36 r t     -> TAIL36 (patchOp r) (patchTarget t)
    BCOND c j d t tmp -> BCOND c (patchOp j) (patchOp d) (patchTarget t) (patchOp tmp)
    BEQZ j t       -> BEQZ (patchOp j) (patchTarget t)
    BNEZ j t       -> BNEZ (patchOp j) (patchTarget t)
    -- 5. Common Memory Access Instructions --------------------------------------
    -- TODO:
    LD f o1 o2         -> LD f (patchOp o1)  (patchOp o2)
    LDU f o1 o2        -> LDU f (patchOp o1)  (patchOp o2)
    ST f o1 o2         -> ST f (patchOp o1)  (patchOp o2)
    LDX f o1 o2        -> LDX f (patchOp o1)  (patchOp o2)
    LDXU f o1 o2       -> LDXU f (patchOp o1)  (patchOp o2)
    STX f o1 o2        -> STX f (patchOp o1)  (patchOp o2)
    LDPTR f o1 o2      -> LDPTR f (patchOp o1)  (patchOp o2)
    STPTR f o1 o2      -> STPTR f (patchOp o1)  (patchOp o2)
    -- 6. Bound Check Memory Access Instructions ---------------------------------
    -- LDCOND o1 o2 o3       -> LDCOND  (patchOp o1)  (patchOp o2)  (patchOp o3)
    -- STCOND o1 o2 o3       -> STCOND  (patchOp o1)  (patchOp o2)  (patchOp o3)
    -- 7. Atomic Memory Access Instructions --------------------------------------
    -- 8. Barrier Instructions ---------------------------------------------------
    -- TODO: need fix
    DBAR o1             -> DBAR o1
    IBAR o1             -> IBAR o1
    -- 11. Floating Point Instructions -------------------------------------------
    FCVT o1 o2          -> FCVT  (patchOp o1)  (patchOp o2)
    SCVTF o1 o2         -> SCVTF  (patchOp o1)  (patchOp o2)
    FCVTZS o1 o2 o3     -> FCVTZS  (patchOp o1)  (patchOp o2) (patchOp o3)
    FMIN o1 o2 o3       -> FMIN  (patchOp o1)  (patchOp o2)  (patchOp o3)
    FMAX o1 o2 o3       -> FMAX  (patchOp o1)  (patchOp o2)  (patchOp o3)
    FMINA o1 o2 o3      -> FMINA  (patchOp o1)  (patchOp o2)  (patchOp o3)
    FMAXA o1 o2 o3      -> FMAXA  (patchOp o1)  (patchOp o2)  (patchOp o3)
    FNEG o1 o2          -> FNEG  (patchOp o1)  (patchOp o2)
    FABS o1 o2          -> FABS  (patchOp o1)  (patchOp o2)
    FMA s o1 o2 o3 o4   -> FMA s (patchOp o1)  (patchOp o2)  (patchOp o3)  (patchOp o4)

    _                   -> panic $ "patchRegsOfInstr: " ++ instrCon instr
    where
        -- TODO:
        patchOp :: Operand -> Operand
        patchOp (OpReg w r) = OpReg w (env r)
        patchOp (OpAddr a) = OpAddr (patchAddr a)
        patchOp opImm = opImm

        patchTarget :: Target -> Target
        patchTarget (TReg r) = TReg (env r)
        patchTarget t = t

        patchAddr :: AddrMode -> AddrMode
        patchAddr (AddrRegReg r1 r2)  = AddrRegReg (env r1) (env r2)
        patchAddr (AddrRegImm r1 imm) = AddrRegImm (env r1) imm
        patchAddr (AddrReg r) = AddrReg (env r)

--------------------------------------------------------------------------------

-- | Checks whether this instruction is a jump/branch instruction.
-- One that can change the flow of control in a way that the
-- register allocator needs to worry about.
isJumpishInstr :: Instr -> Bool
isJumpishInstr instr = case instr of
  ANN _ i -> isJumpishInstr i
  J {} -> True
  J_TBL {} -> True
  B {} -> True
  BL {} -> True
  CALL36 {} -> True
  TAIL36 {} -> True
  BCOND {} -> True
  BEQZ {} -> True
  BNEZ {} -> True
  _ -> False

-- | Get the `BlockId`s of the jump destinations (if any)
jumpDestsOfInstr :: Instr -> [BlockId]
jumpDestsOfInstr (ANN _ i) = jumpDestsOfInstr i
jumpDestsOfInstr (J t) = [id | TBlock id <- [t]]
jumpDestsOfInstr (J_TBL ids _mbLbl _r) = catMaybes ids
jumpDestsOfInstr (B t) = [id | TBlock id <- [t]]
jumpDestsOfInstr (BL t _) = [id | TBlock id <- [t]]
jumpDestsOfInstr (CALL36 t) = [id | TBlock id <- [t]]
jumpDestsOfInstr (TAIL36 _ t) = [id | TBlock id <- [t]]
jumpDestsOfInstr (BCOND _ _ _ t _) = [id | TBlock id <- [t]]
jumpDestsOfInstr (BEQZ _ t) = [id | TBlock id <- [t]]
jumpDestsOfInstr (BNEZ _ t) = [id | TBlock id <- [t]]
jumpDestsOfInstr _ = []

-- | Change the destination of this (potential) jump instruction.
-- Used in the linear allocator when adding fixup blocks for join
-- points.
patchJumpInstr :: Instr -> (BlockId -> BlockId) -> Instr
patchJumpInstr instr patchF =
  case instr of
    ANN d i -> ANN d (patchJumpInstr i patchF)
    J (TBlock bid) -> J (TBlock (patchF bid))
    J_TBL ids mbLbl r -> J_TBL (map (fmap patchF) ids) mbLbl r
    B (TBlock bid) -> B (TBlock (patchF bid))
    BL (TBlock bid) ps -> BL (TBlock (patchF bid)) ps
    CALL36 (TBlock bid) -> CALL36 (TBlock (patchF bid))
    TAIL36 r (TBlock bid) -> TAIL36 r (TBlock (patchF bid))
    BCOND c o1 o2 (TBlock bid) tmp -> BCOND c o1 o2 (TBlock (patchF bid)) tmp
    BEQZ j (TBlock bid) -> BEQZ j (TBlock (patchF bid))
    BNEZ j (TBlock bid) -> BNEZ j (TBlock (patchF bid))
    _ -> panic $ "patchJumpInstr: " ++ instrCon instr

-- -----------------------------------------------------------------------------
-- | Make a spill instruction, spill a register into spill slot.
mkSpillInstr
   :: HasCallStack
   => NCGConfig
   -> RegWithFormat -- register to spill
   -> Int       -- current stack delta
   -> Int       -- spill slot to use
   -> [Instr]

mkSpillInstr _config (RegWithFormat reg _fmt) delta slot =
  case off - delta of
    imm | fitsInNbits 12 imm -> [mkStrSpImm imm]
    imm ->
      [ movImmToIp imm,
        addSpToIp,
        mkStrIp
      ]
  where
    fmt = case reg of
      RegReal (RealRegSingle n) | n < 32 -> II64
      _ -> FF64
    mkStrSpImm imm = ANN (text "Spill@" <> int (off - delta)) $ ST fmt (OpReg W64 reg) (OpAddr (AddrRegImm spMachReg (ImmInt imm)))
    movImmToIp imm = ANN (text "Spill: TMP <- " <> int imm) $ MOV tmp (OpImm (ImmInt imm))
    addSpToIp = ANN (text "Spill: TMP <- SP + TMP ") $ ADD tmp tmp sp
    mkStrIp = ANN (text "Spill@" <> int (off - delta)) $ ST fmt (OpReg W64 reg) (OpAddr (AddrReg tmpReg))
    off = spillSlotToOffset slot

-- | Make a reload instruction, reload from spill slot to a register.
mkLoadInstr
   :: NCGConfig
   -> RegWithFormat
   -> Int       -- current stack delta
   -> Int       -- spill slot to use
   -> [Instr]

mkLoadInstr _config (RegWithFormat reg _fmt) delta slot =
  case off - delta of
    imm | fitsInNbits 12 imm -> [mkLdrSpImm imm]
    imm ->
      [ movImmToIp imm,
        addSpToIp,
        mkLdrIp
      ]
  where
    fmt = case reg of
      RegReal (RealRegSingle n) | n < 32 -> II64
      _ -> FF64
    mkLdrSpImm imm = ANN (text "Reload@" <> int (off - delta)) $ LD fmt (OpReg W64 reg) (OpAddr (AddrRegImm spMachReg (ImmInt imm)))
    movImmToIp imm = ANN (text "Reload: TMP <- " <> int imm) $ MOV tmp (OpImm (ImmInt imm))
    addSpToIp = ANN (text "Reload: TMP <- SP + TMP ") $ ADD tmp tmp sp
    mkLdrIp = ANN (text "Reload@" <> int (off - delta)) $ LD fmt (OpReg W64 reg) (OpAddr (AddrReg tmpReg))
    off = spillSlotToOffset slot

-- | See if this instruction is telling us the current C stack delta
takeDeltaInstr :: Instr -> Maybe Int
takeDeltaInstr (ANN _ i) = takeDeltaInstr i
takeDeltaInstr (DELTA i) = Just i
takeDeltaInstr _         = Nothing

-- | Not real instructions.  Just meta data
isMetaInstr :: Instr -> Bool
isMetaInstr instr =
  case instr of
    ANN _ i -> isMetaInstr i
    COMMENT {} -> True
    MULTILINE_COMMENT {} -> True
    LOCATION {} -> True
    NEWBLOCK {} -> True
    DELTA {} -> True
    LDATA {} -> True
    PUSH_STACK_FRAME -> True
    POP_STACK_FRAME -> True
    _ -> False

canFallthroughTo :: Instr -> BlockId -> Bool
canFallthroughTo insn bid =
  case insn of
    J (TBlock target) -> bid == target
    J_TBL targets _ _ -> all isTargetBid targets
    B (TBlock target) -> bid == target
    CALL36 (TBlock target) -> bid == target
    TAIL36 _ (TBlock target) -> bid == target
    BCOND _ _ _ (TBlock target) _ -> bid == target
    _ -> False
  where
    isTargetBid target = case target of
      Nothing -> True
      Just target -> target == bid

-- | Copy the value in a register to another one.
-- Must work for all register classes.
mkRegRegMoveInstr :: Reg -> Reg -> Instr
mkRegRegMoveInstr src dst = ANN desc instr
  where
    desc = text "Reg->Reg Move: " <> ppr src <> text " -> " <> ppr dst
    instr = MOV (OpReg W64 dst) (OpReg W64 src)

-- | Take the source and destination from this (potential) reg -> reg move instruction
-- We have to be a bit careful here: A `MOV` can also mean an implicit
-- conversion. This case is filtered out.
takeRegRegMoveInstr :: Instr -> Maybe (Reg, Reg)
takeRegRegMoveInstr (MOV (OpReg width dst) (OpReg width' src))
  | width == width' && (isFloatReg dst == isFloatReg src) = pure (src, dst)
takeRegRegMoveInstr _ = Nothing

-- | Make an unconditional jump instruction.
mkJumpInstr :: BlockId -> [Instr]
mkJumpInstr id = [TAIL36 (OpReg W64 tmpReg) (TBlock (id))]

-- | Decrement @sp@ to allocate stack space.
mkStackAllocInstr :: Platform -> Int -> [Instr]
mkStackAllocInstr _platform n
  | n == 0 = []
  | n > 0 && fitsInNbits 12 (fromIntegral n) =
      [ ANN (text "Alloc stack") $ SUB sp sp (OpImm (ImmInt n)) ]
  | n > 0 =
     [
         ANN (text "Alloc more stack") (MOV tmp (OpImm (ImmInt n))),
         SUB sp sp tmp
     ]
mkStackAllocInstr _platform n = pprPanic "mkStackAllocInstr" (int n)

-- | Increment SP to deallocate stack space.
mkStackDeallocInstr :: Platform -> Int -> [Instr]
mkStackDeallocInstr _platform  n
  | n == 0 = []
  | n > 0 && fitsInNbits 12 (fromIntegral n) =
      [ ANN (text "Dealloc stack") $ ADD sp sp (OpImm (ImmInt n)) ]
  | n > 0 =
     [
         ANN (text "Dealloc more stack") (MOV tmp (OpImm (ImmInt n))),
         ADD sp sp tmp
     ]
mkStackDeallocInstr _platform n = pprPanic "mkStackDeallocInstr" (int n)

allocMoreStack
  :: Platform
  -> Int
  -> NatCmmDecl statics GHC.CmmToAsm.LA64.Instr.Instr
  -> UniqDSM (NatCmmDecl statics GHC.CmmToAsm.LA64.Instr.Instr, [(BlockId,BlockId)])

allocMoreStack _ _ top@(CmmData _ _) = return (top, [])
allocMoreStack platform slots proc@(CmmProc info lbl live (ListGraph code)) = do
  let entries = entryBlocks proc

  retargetList <- mapM (\e -> (e,) <$> newBlockId) entries

  let
    delta = ((x + stackAlign - 1) `quot` stackAlign) * stackAlign -- round up
      where x = slots * spillSlotSize -- sp delta

    alloc   = mkStackAllocInstr   platform delta
    dealloc = mkStackDeallocInstr platform delta

    new_blockmap :: LabelMap BlockId
    new_blockmap = mapFromList retargetList

    insert_stack_insn (BasicBlock id insns)
      | Just new_blockid <- mapLookup id new_blockmap =
        [ BasicBlock id $ alloc ++ [ B (TBlock new_blockid) ],
          BasicBlock new_blockid block' ]
      | otherwise =
        [ BasicBlock id block' ]
      where
        block' = foldr insert_dealloc [] insns

    insert_dealloc insn r = case insn of
      J {} -> dealloc ++ (insn : r)
      J_TBL {} -> dealloc ++ (insn : r)
      ANN _ e -> insert_dealloc e r
      _other | jumpDestsOfInstr insn /= [] ->
        patchJumpInstr insn retarget : r
      _other -> insn : r

      where retarget b = fromMaybe b (mapLookup b new_blockmap)

    new_code = concatMap insert_stack_insn code
  return (CmmProc info lbl live (ListGraph new_code), retargetList)

-- -----------------------------------------------------------------------------
-- Machine's assembly language

-- We have a few common "instructions" (nearly all the pseudo-ops) but
-- mostly all of 'Instr' is machine-specific.

data Instr
    -- comment pseudo-op
    = COMMENT SDoc
    | MULTILINE_COMMENT SDoc

    -- Annotated instruction. Should print <instr> # <doc>
    | ANN SDoc Instr

    -- location pseudo-op (file, line, col, name)
    | LOCATION Int Int Int LexicalFastString

    -- start a new basic block.  Useful during codegen, removed later.
    -- Preceding instruction should be a jump, as per the invariants
    -- for a BasicBlock (see Cmm).
    | NEWBLOCK BlockId

    -- specify current stack offset for benefit of subsequent passes.
    -- This carries a BlockId so it can be used in unwinding information.
    | DELTA   Int

    -- | Static data spat out during code generation.
    | LDATA Section RawCmmStatics

    | PUSH_STACK_FRAME
    | POP_STACK_FRAME
    -- Basic Integer Instructions ------------------------------------------------
    -- 1. Arithmetic Instructions ------------------------------------------------
    | ADD Operand Operand Operand
    | SUB Operand Operand Operand
    | ALSL Operand Operand Operand Operand
    | ALSLU Operand Operand Operand Operand
    | LU12I Operand Operand
    | LU32I Operand Operand
    | LU52I Operand Operand Operand
    | SSLT Operand Operand Operand
    | SSLTU Operand Operand Operand
    | PCADDI Operand Operand
    | PCADDU12I Operand Operand
    | PCADDU18I Operand Operand
    | PCALAU12I Operand Operand
    | AND Operand Operand Operand
    | OR Operand Operand Operand
    | XOR Operand Operand Operand
    | NOR Operand Operand Operand
    | ANDN Operand Operand Operand
    | ORN Operand Operand Operand
    | MUL Operand Operand Operand
    | MULW Operand Operand Operand
    | MULWU Operand Operand Operand
    | MULH Operand Operand Operand
    | MULHU Operand Operand Operand
    | DIV Operand Operand Operand
    | DIVU Operand Operand Operand
    | MOD Operand Operand Operand
    | MODU Operand Operand Operand
    -- 2. Bit-shift Instuctions --------------------------------------------------
    | SLL Operand Operand Operand
    | SRL Operand Operand Operand
    | SRA Operand Operand Operand
    | ROTR Operand Operand Operand
    -- 3. Bit-manupulation Instructions ------------------------------------------
    | EXT Operand Operand
    | CLO Operand Operand
    | CTO Operand Operand
    | CLZ Operand Operand
    | CTZ Operand Operand
    | BYTEPICK Operand Operand Operand Operand
    | REVB2H Operand Operand
    | REVB4H Operand Operand
    | REVB2W Operand Operand
    | REVBD Operand Operand
    | REVH2W Operand Operand
    | REVHD Operand Operand
    | BITREV4B Operand Operand
    | BITREV8B Operand Operand
    | BITREVW Operand Operand
    | BITREVD Operand Operand
    | BSTRINS Format Operand Operand Operand Operand
    | BSTRPICK Format Operand Operand Operand Operand
    | MASKEQZ Operand Operand Operand
    | MASKNEZ Operand Operand Operand
    -- Pseudo instructions
    | NOP
    | MOV Operand Operand
    | NEG Operand Operand
    | CSET Cond Operand Operand Operand
    -- 4. Branch Instructions ----------------------------------------------------
    | J Target
    | J_TBL [Maybe BlockId] (Maybe CLabel) Reg
    | B Target
    | BL Target [Reg]
    | CALL36 Target
    | TAIL36 Operand Target
    | BCOND Cond Operand Operand Target Operand
    | BEQZ Operand Target
    | BNEZ Operand Target
    -- 5. Common Memory Access Instructions --------------------------------------
    | LD Format Operand Operand
    | LDU Format Operand Operand
    | ST Format Operand Operand
    | LDX Format Operand Operand
    | LDXU Format Operand Operand
    | STX Format Operand Operand
    | LDPTR Format Operand Operand
    | STPTR Format Operand Operand
    -- 6. Bound Check Memory Access Instructions ---------------------------------
    -- 7. Atomic Memory Access Instructions --------------------------------------
    -- 8. Barrier Instructions ---------------------------------------------------
    | DBAR BarrierType
    | IBAR BarrierType
    -- Basic Floating Point Instructions -----------------------------------------
    | FCVT    Operand Operand
    | SCVTF   Operand Operand
    | FCVTZS  Operand Operand Operand
    | FMAX Operand Operand Operand
    | FMIN Operand Operand Operand
    | FMAXA Operand Operand Operand
    | FMINA Operand Operand Operand
    | FNEG Operand Operand
    | FABS Operand Operand
    -- Floating-point fused multiply-add instructions
    --  fmadd : d =   r1 * r2 + r3
    --  fnmsub: d =   r1 * r2 - r3
    --  fmsub : d = - r1 * r2 + r3
    --  fnmadd: d = - r1 * r2 - r3
    | FMA FMASign Operand Operand Operand Operand

-- TODO: Not complete.
data BarrierType = Hint0

instrCon :: Instr -> String
instrCon i =
    case i of
      COMMENT{} -> "COMMENT"
      MULTILINE_COMMENT{} -> "COMMENT"
      ANN{} -> "ANN"
      LOCATION{} -> "LOCATION"
      NEWBLOCK{} -> "NEWBLOCK"
      DELTA{} -> "DELTA"
      LDATA {} -> "LDATA"
      PUSH_STACK_FRAME{} -> "PUSH_STACK_FRAME"
      POP_STACK_FRAME{} -> "POP_STACK_FRAME"

      ADD{} -> "ADD"
      SUB{} -> "SUB"
      ALSL{} -> "ALSL"
      ALSLU{} -> "ALSLU"
      LU12I{} -> "LU12I"
      LU32I{} -> "LU32I"
      LU52I{} -> "LU52I"
      SSLT{} -> "SSLT"
      SSLTU{} -> "SSLTU"
      PCADDI{} -> "PCADDI"
      PCADDU12I{} -> "PCADDU12I"
      PCADDU18I{} -> "PCADDU18I"
      PCALAU12I{} -> "PCALAU12I"
      AND{} -> "AND"
      OR{} -> "OR"
      XOR{} -> "XOR"
      NOR{} -> "NOR"
      ANDN{} -> "ANDN"
      ORN{} -> "ORN"
      MUL{} -> "MUL"
      MULW{} -> "MULW"
      MULWU{} -> "MULWU"
      MULH{} -> "MULH"
      MULHU{} -> "MULHU"
      DIV{} -> "DIV"
      MOD{} -> "MOD"
      DIVU{} -> "DIVU"
      MODU{} -> "MODU"
      SLL{} -> "SLL"
      SRL{} -> "SRL"
      SRA{} -> "SRA"
      ROTR{} -> "ROTR"
      EXT{} -> "EXT"
      CLO{} -> "CLO"
      CLZ{} -> "CLZ"
      CTO{} -> "CTO"
      CTZ{} -> "CTZ"
      BYTEPICK{} -> "BYTEPICK"
      REVB2H{} -> "REVB2H"
      REVB4H{} -> "REVB4H"
      REVB2W{} -> "REVB2W"
      REVBD{} -> "REVBD"
      REVH2W{} -> "REVH2W"
      REVHD{} -> "REVHD"
      BITREV4B{} -> "BITREV4B"
      BITREV8B{} -> "BITREV8B"
      BITREVW{} -> "BITREVW"
      BITREVD{} -> "BITREVD"
      BSTRINS{} -> "BSTRINS"
      BSTRPICK{} -> "BSTRPICK"
      MASKEQZ{} -> "MASKEQZ"
      MASKNEZ{} -> "MASKNEZ"
      NOP{} -> "NOP"
      MOV{} -> "MOV"
      NEG{} -> "NEG"
      CSET{} -> "CSET"
      J{} -> "J"
      J_TBL{} -> "J_TBL"
      B{} -> "B"
      BL{} -> "BL"
      CALL36{} -> "CALL36"
      TAIL36{} -> "TAIL36"
      BCOND{} -> "BCOND"
      BEQZ{} -> "BEQZ"
      BNEZ{} -> "BNEZ"
      LD{} -> "LD"
      LDU{} -> "LDU"
      ST{} -> "ST"
      LDX{} -> "LDX"
      LDXU{} -> "LDXU"
      STX{} -> "STX"
      LDPTR{} -> "LDPTR"
      STPTR{} -> "STPTR"
      DBAR{} -> "DBAR"
      IBAR{} -> "IBAR"
      FCVT{} -> "FCVT"
      SCVTF{} -> "SCVTF"
      FCVTZS{} -> "FCVTZS"
      FMAX{} -> "FMAX"
      FMIN{} -> "FMIN"
      FMAXA{} -> "FMAXA"
      FMINA{} -> "FMINA"
      FNEG{} -> "FNEG"
      FABS{} -> "FABS"
      FMA variant _ _ _ _ ->
        case variant of
          FMAdd  -> "FMADD"
          FMSub  -> "FMSUB"
          FNMAdd -> "FNMADD"
          FNMSub -> "FNMSUB"

data Target
    = TBlock BlockId
    | TLabel CLabel
    | TReg   Reg

data Operand
  = OpReg Width Reg -- register
  | OpImm Imm       -- immediate
  | OpAddr AddrMode -- address
  deriving (Eq, Show)

opReg :: Reg -> Operand
opReg = OpReg W64

opRegNo :: RegNo -> Operand
opRegNo = opReg . regSingle

-- LoongArch64 has no ip register in ABI. Here ip register is for spilling/
-- reloading register to/from slots. So make t8(r20) non-free for ip.
zero, ra, tp, sp, fp, tmp :: Operand
zero = opReg zeroReg
ra   = opReg raReg
sp   = opReg spMachReg
tp   = opReg tpMachReg
fp   = opReg fpMachReg
tmp  = opReg tmpReg

x0,  x1,  x2,  x3,  x4,  x5,  x6,  x7  :: Operand
x8,  x9,  x10, x11, x12, x13, x14, x15 :: Operand
x16, x17, x18, x19, x20, x21, x22, x23 :: Operand
x24, x25, x26, x27, x28, x29, x30, x31 :: Operand
x0  = opRegNo  0
x1  = opRegNo  1
x2  = opRegNo  2
x3  = opRegNo  3
x4  = opRegNo  4
x5  = opRegNo  5
x6  = opRegNo  6
x7  = opRegNo  7
x8  = opRegNo  8
x9  = opRegNo  9
x10 = opRegNo 10
x11 = opRegNo 11
x12 = opRegNo 12
x13 = opRegNo 13
x14 = opRegNo 14
x15 = opRegNo 15
x16 = opRegNo 16
x17 = opRegNo 17
x18 = opRegNo 18
x19 = opRegNo 19
x20 = opRegNo 20
x21 = opRegNo 21
x22 = opRegNo 22
x23 = opRegNo 23
x24 = opRegNo 24
x25 = opRegNo 25
x26 = opRegNo 26
x27 = opRegNo 27
x28 = opRegNo 18
x29 = opRegNo 29
x30 = opRegNo 30
x31 = opRegNo 31

d0,  d1,  d2,  d3,  d4,  d5,  d6,  d7  :: Operand
d8,  d9,  d10, d11, d12, d13, d14, d15 :: Operand
d16, d17, d18, d19, d20, d21, d22, d23 :: Operand
d24, d25, d26, d27, d28, d29, d30, d31 :: Operand
d0  = opRegNo 32
d1  = opRegNo 33
d2  = opRegNo 34
d3  = opRegNo 35
d4  = opRegNo 36
d5  = opRegNo 37
d6  = opRegNo 38
d7  = opRegNo 39
d8  = opRegNo 40
d9  = opRegNo 41
d10 = opRegNo 42
d11 = opRegNo 43
d12 = opRegNo 44
d13 = opRegNo 45
d14 = opRegNo 46
d15 = opRegNo 47
d16 = opRegNo 48
d17 = opRegNo 49
d18 = opRegNo 50
d19 = opRegNo 51
d20 = opRegNo 52
d21 = opRegNo 53
d22 = opRegNo 54
d23 = opRegNo 55
d24 = opRegNo 56
d25 = opRegNo 57
d26 = opRegNo 58
d27 = opRegNo 59
d28 = opRegNo 60
d29 = opRegNo 61
d30 = opRegNo 62
d31 = opRegNo 63

fitsInNbits :: Int -> Int -> Bool
fitsInNbits n i = (-1 `shiftL` (n - 1)) <= i && i <= (1 `shiftL` (n - 1) - 1)

isUnsignOp :: Int -> Bool
isUnsignOp i = (i >= 0)

isNbitEncodeable :: Int -> Integer -> Bool
isNbitEncodeable n i = let shift = n - 1 in (-1 `shiftL` shift) <= i && i < (1 `shiftL` shift)

isEncodeableInWidth :: Width -> Integer -> Bool
isEncodeableInWidth = isNbitEncodeable . widthInBits

isIntOp :: Operand -> Bool
isIntOp = not . isFloatOp

isFloatOp :: Operand -> Bool
isFloatOp (OpReg _ reg) | isFloatReg reg = True
isFloatOp _ = False

isFloatReg :: Reg -> Bool
isFloatReg (RegReal (RealRegSingle i)) | i > 31 = True
isFloatReg (RegVirtual (VirtualRegD _)) = True
isFloatReg _ = False

widthToInt :: Width -> Int
widthToInt W8   = 8
widthToInt W16  = 16
widthToInt W32  = 32
widthToInt W64  = 64
widthToInt _ = 64

widthFromOpReg :: Operand -> Width
widthFromOpReg (OpReg W8 _)  = W8
widthFromOpReg (OpReg W16 _) = W16
widthFromOpReg (OpReg W32 _) = W32
widthFromOpReg (OpReg W64 _) = W64
widthFromOpReg _ = W64

lessW64 :: Width -> Bool
lessW64 w | w == W8 || w == W16 || w == W32 = True
lessW64 _   = False
