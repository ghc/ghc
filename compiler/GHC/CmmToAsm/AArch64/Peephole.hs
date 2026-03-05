{- |
Module      : GHC.CmmToAsm.AArch64.Peephole
Description : Post-register-allocation peephole optimizer for AArch64
Copyright   : (c) Moritz Angermann <moritz.angermann@iohk.io>, 2026
License     : BSD-3

Post-register-allocation peephole optimizer. Runs over the final instruction
stream and rewrites instruction sequences to take advantage of AArch64-specific
instructions and idioms.

Optimizations performed:

  * LDP\/STP formation: Merges adjacent LDR\/STR at consecutive offsets
    into paired load\/store instructions, saving a decode slot.
    Only II32 and II64 formats are supported (AArch64 has no 8\/16-bit
    paired load\/store instructions).

  * STR-of-zero: Replaces MOV\/MOVZ reg,#0; STR reg,[addr] with
    STR xzr,[addr], eliminating the move instruction entirely.

  * Redundant move elimination: Removes identity moves (MOV r,r).

  * Redundant extension elimination: Removes consecutive identical
    sign\/zero extensions (e.g. UXTB w0,w0; UXTB w0,w0).

  * CMP+CSET+CB{N}Z→BCOND: Rewrites a comparison that produces a boolean
    followed by a conditional branch on that boolean into a single
    conditional branch, saving 2 instructions.
-}
module GHC.CmmToAsm.AArch64.Peephole
  ( peephole
  )
where

import GHC.Prelude hiding (EQ)
import GHC.CmmToAsm.AArch64.Instr
import GHC.CmmToAsm.AArch64.Regs (AddrMode(..), Imm(..))
import GHC.CmmToAsm.AArch64.Cond
import GHC.CmmToAsm.Format
import GHC.Platform.Reg

-- | Run the peephole optimizer over an instruction list.
-- This is designed to be called after register allocation, when final
-- register assignments are known and pattern matching is most effective.
peephole :: [Instr] -> [Instr]
peephole = peep []
  where
    -- Process instructions, building result in reverse for efficiency.
    -- We look ahead at 2-3 instructions for pattern matching.
    peep :: [Instr] -> [Instr] -> [Instr]
    peep acc [] = reverse acc
    peep acc (i:rest) = case matchPattern i rest of
      Just (replacement, remaining) -> peep acc (replacement ++ remaining)
      Nothing -> peep (i:acc) rest

-- | Try to match a peephole pattern starting at the given instruction.
-- Returns Just (replacement, remaining) if a pattern matches.
matchPattern :: Instr -> [Instr] -> Maybe ([Instr], [Instr])
matchPattern i rest = case i of

  -- Pattern: Identity move elimination
  -- MOV r, r → (removed)
  MOV o1 o2
    | o1 == o2
    -> Just ([], rest)

  -- Pattern: STR-of-zero
  -- MOV/MOVZ reg, #0; STR fmt reg addr → STR fmt xzr addr
  -- Uses the hardware zero register to avoid materializing zero.
  -- Restricted to II32/II64: pprReg only handles the zero register
  -- (RealRegSingle -1) at W32 (wzr) and W64 (xzr) widths, so we cannot
  -- substitute xzr into II8/II16 stores.
  _ | Just (dst, dst_n) <- isZeroMov i
    , (STR fmt (OpReg w' src) addr : rest') <- rest
    , RegReal (RealRegSingle src_n) <- src
    , dst_n == src_n
    , dst_n < 32  -- GP register
    , fmt == II32 || fmt == II64  -- zero register only printable at W32/W64
    , not (addrUsesReg dst addr)  -- dst not used in the address computation
    -> Just ([STR fmt (OpReg w' (RegReal (RealRegSingle (-1)))) addr], rest')
       -- regNo -1 is the zero register (xzr/wzr)

  -- Pattern: LDP formation from consecutive LDRs
  -- LDR fmt r1, [base, #off]; LDR fmt r2, [base, #off+size] → LDP fmt r1, r2, [base, #off]
  -- Note: LDP/STP only support 32-bit and 64-bit integer registers (no 8/16-bit).
  LDR fmt1 dst1@(OpReg _w1 r1) (OpAddr (AddrRegImm base1 (ImmInt off1)))
    | (LDR fmt2 dst2@(OpReg _w2 r2) (OpAddr (AddrRegImm base2 (ImmInt off2))) : rest') <- rest
    , fmt1 == fmt2
    , base1 == base2
    , r1 /= r2
    , off2 == off1 + formatInBytes fmt1
    , isLdpStpFormat fmt1
    , isLdpStpOffset off1 fmt1
    , not (isFloatFormat fmt1)  -- Only integer registers for now
    -> Just ([LDP fmt1 dst1 dst2 (OpAddr (AddrRegImm base1 (ImmInt off1)))], rest')

  -- Pattern: LDP formation (reversed order)
  -- LDR fmt r2, [base, #off+size]; LDR fmt r1, [base, #off] → LDP fmt r1, r2, [base, #off]
  LDR fmt1 dst1@(OpReg _w1 r1) (OpAddr (AddrRegImm base1 (ImmInt off1)))
    | (LDR fmt2 dst2@(OpReg _w2 r2) (OpAddr (AddrRegImm base2 (ImmInt off2))) : rest') <- rest
    , fmt1 == fmt2
    , base1 == base2
    , r1 /= r2
    , off1 == off2 + formatInBytes fmt1
    , isLdpStpFormat fmt1
    , isLdpStpOffset off2 fmt1
    , not (isFloatFormat fmt1)
    -- Ensure first load's destination isn't the base register
    , not (regUsedIn r1 base1)
    -> Just ([LDP fmt1 dst2 dst1 (OpAddr (AddrRegImm base1 (ImmInt off2)))], rest')

  -- Pattern: STP formation from consecutive STRs
  -- STR fmt r1, [base, #off]; STR fmt r2, [base, #off+size] → STP fmt r1, r2, [base, #off]
  STR fmt1 src1@(OpReg _w1 _) (OpAddr (AddrRegImm base1 (ImmInt off1)))
    | (STR fmt2 src2@(OpReg _w2 _) (OpAddr (AddrRegImm base2 (ImmInt off2))) : rest') <- rest
    , fmt1 == fmt2
    , base1 == base2
    , off2 == off1 + formatInBytes fmt1
    , isLdpStpFormat fmt1
    , isLdpStpOffset off1 fmt1
    , not (isFloatFormat fmt1)
    -> Just ([STP fmt1 src1 src2 (OpAddr (AddrRegImm base1 (ImmInt off1)))], rest')

  -- Pattern: STP formation (reversed order)
  STR fmt1 src1@(OpReg _w1 _) (OpAddr (AddrRegImm base1 (ImmInt off1)))
    | (STR fmt2 src2@(OpReg _w2 _) (OpAddr (AddrRegImm base2 (ImmInt off2))) : rest') <- rest
    , fmt1 == fmt2
    , base1 == base2
    , off1 == off2 + formatInBytes fmt1
    , isLdpStpFormat fmt1
    , isLdpStpOffset off2 fmt1
    , not (isFloatFormat fmt1)
    -> Just ([STP fmt1 src2 src1 (OpAddr (AddrRegImm base1 (ImmInt off2)))], rest')

  -- Pattern: Redundant extension elimination
  -- EXT dst, src; EXT dst', src' where dst==dst'==src'==src → EXT dst, src
  UXTB dst _src
    | (UXTB dst' src' : rest') <- rest
    , dst == dst', dst == src'
    -> Just ([i], rest')
  UXTH dst _src
    | (UXTH dst' src' : rest') <- rest
    , dst == dst', dst == src'
    -> Just ([i], rest')
  SXTB dst _src
    | (SXTB dst' src' : rest') <- rest
    , dst == dst', dst == src'
    -> Just ([i], rest')
  SXTH dst _src
    | (SXTH dst' src' : rest') <- rest
    , dst == dst', dst == src'
    -> Just ([i], rest')

  -- Pattern: CMP + CSET + CBNZ/CBZ → CMP + BCOND
  -- CMP x, y; CSET w, cond; CBNZ w, lbl → CMP x, y; BCOND cond lbl
  -- CMP x, y; CSET w, cond; CBZ w, lbl  → CMP x, y; BCOND !cond lbl
  --
  -- Safety: This removes the CSET, so cset_dst no longer gets written.
  -- This is safe because CBNZ/CBZ terminates the basic block, and
  -- the peephole runs per-block. The CMP+CSET+CB{N}Z pattern is
  -- generated by the backend specifically for conditional branches
  -- where the boolean register is only consumed by the branch.
  CMP _ _
    | (CSET (OpReg _ cset_dst) cond : branch : rest') <- rest
    , Just target <- cbnzTarget cset_dst branch
    -> Just ([i, BCOND (branchCond cond branch) target], rest')

  -- Annotations: look through annotations for pattern matching
  ANN doc inner
    | Just (replacement, remaining) <- matchPattern inner rest
    -> Just (map (ANN doc) replacement, remaining)

  _ -> Nothing

-- | Recognize a zero-materializing move instruction.
-- Matches both MOV reg, #0 and MOVZ reg, #0 (the latter is emitted
-- by inlineMemset for zero fills).
isZeroMov :: Instr -> Maybe (Reg, Int)
isZeroMov (MOV (OpReg _w dst) (OpImm (ImmInt 0)))
  | RegReal (RealRegSingle n) <- dst = Just (dst, n)
isZeroMov (MOVZ (OpReg _w dst) (OpImm (ImmInt 0)))
  | RegReal (RealRegSingle n) <- dst = Just (dst, n)
isZeroMov _ = Nothing

-- | Extract branch target from CBNZ/CBZ if it references the given register
cbnzTarget :: Reg -> Instr -> Maybe Target
cbnzTarget r (CBNZ (OpReg _ r') t) | r == r' = Just t
cbnzTarget r (CBZ (OpReg _ r') t)  | r == r' = Just t
cbnzTarget _ _ = Nothing

-- | Determine the effective condition for CMP+CSET+CBZ/CBNZ fusion.
-- CBNZ tests "not zero" → the CSET condition is used directly.
-- CBZ tests "zero" → the CSET condition is inverted.
branchCond :: Cond -> Instr -> Cond
branchCond cond (CBNZ _ _) = cond
branchCond cond (CBZ _ _)  = invertCond cond
branchCond _ _ = error "branchCond: not a CBZ/CBNZ"

-- | Check if a format is supported by LDP/STP.
-- AArch64 LDP/STP only support 32-bit and 64-bit integer registers
-- (and SIMD/FP registers, which we don't handle here). There are no
-- 8-bit or 16-bit paired load/store instructions.
isLdpStpFormat :: Format -> Bool
isLdpStpFormat II32 = True
isLdpStpFormat II64 = True
isLdpStpFormat _    = False

-- | Check if an offset is valid for LDP/STP.
-- LDP/STP use a 7-bit signed immediate, scaled by the access size.
-- 64-bit: offset must be multiple of 8, range [-512, 504]
-- 32-bit: offset must be multiple of 4, range [-256, 252]
isLdpStpOffset :: Int -> Format -> Bool
isLdpStpOffset off fmt =
  let scale = formatInBytes fmt
  in off `mod` scale == 0
     && off >= -64 * scale
     && off <= 63 * scale

-- | Check if a register is used in an address mode operand.
addrUsesReg :: Reg -> Operand -> Bool
addrUsesReg r (OpAddr (AddrRegReg r1 r2)) = r == r1 || r == r2
addrUsesReg r (OpAddr (AddrRegImm r1 _))  = r == r1
addrUsesReg r (OpAddr (AddrReg r1))       = r == r1
addrUsesReg _ _ = False

-- | Check if a register is the same as an address base register.
regUsedIn :: Reg -> Reg -> Bool
regUsedIn = (==)
