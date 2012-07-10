-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

module X86.Ppr (
        pprNatCmmDecl,
        pprBasicBlock,
        pprSectionHeader,
        pprData,
        pprInstr,
        pprSize,
        pprImm,
        pprDataItem,
)

where

#include "HsVersions.h"
#include "nativeGen/NCG.h"

import X86.Regs
import X86.Instr
import X86.Cond
import Instruction
import Size
import Reg
import PprBase


import BasicTypes       (Alignment)
import OldCmm
import CLabel
import Unique           ( pprUnique, Uniquable(..) )
import Platform
import FastString
import Outputable

import Data.Word

import Data.Bits

-- -----------------------------------------------------------------------------
-- Printing this stuff out

pprNatCmmDecl :: Platform -> NatCmmDecl (Alignment, CmmStatics) Instr -> SDoc
pprNatCmmDecl platform (CmmData section dats) =
  pprSectionHeader platform section $$ pprDatas platform dats

 -- special case for split markers:
pprNatCmmDecl platform (CmmProc Nothing lbl (ListGraph [])) = pprLabel platform lbl

 -- special case for code without info table:
pprNatCmmDecl platform (CmmProc Nothing lbl (ListGraph blocks)) =
  pprSectionHeader platform Text $$
  pprLabel platform lbl $$ -- blocks guaranteed not null, so label needed
  vcat (map (pprBasicBlock platform) blocks) $$
  pprSizeDecl platform lbl

pprNatCmmDecl platform (CmmProc (Just (Statics info_lbl info)) _entry_lbl (ListGraph blocks)) =
  pprSectionHeader platform Text $$
  (
       (if platformHasSubsectionsViaSymbols platform
        then pprCLabel platform (mkDeadStripPreventer info_lbl) <> char ':'
        else empty) $$
       vcat (map (pprData platform) info) $$
       pprLabel platform info_lbl
  ) $$
  vcat (map (pprBasicBlock platform) blocks) $$
     -- above: Even the first block gets a label, because with branch-chain
     -- elimination, it might be the target of a goto.
        (if platformHasSubsectionsViaSymbols platform
         then
         -- If we are using the .subsections_via_symbols directive
         -- (available on recent versions of Darwin),
         -- we have to make sure that there is some kind of reference
         -- from the entry code to a label on the _top_ of of the info table,
         -- so that the linker will not think it is unreferenced and dead-strip
         -- it. That's why the label is called a DeadStripPreventer (_dsp).
                  text "\t.long "
              <+> pprCLabel platform info_lbl
              <+> char '-'
              <+> pprCLabel platform (mkDeadStripPreventer info_lbl)
         else empty) $$
  pprSizeDecl platform info_lbl

-- | Output the ELF .size directive.
pprSizeDecl :: Platform -> CLabel -> SDoc
pprSizeDecl platform lbl
 | osElfTarget (platformOS platform) =
    ptext (sLit "\t.size") <+> pprCLabel platform lbl
    <> ptext (sLit ", .-") <> pprCLabel platform lbl
 | otherwise = empty

pprBasicBlock :: Platform -> NatBasicBlock Instr -> SDoc
pprBasicBlock platform (BasicBlock blockid instrs) =
  pprLabel platform (mkAsmTempLabel (getUnique blockid)) $$
  vcat (map (pprInstr platform) instrs)


pprDatas :: Platform -> (Alignment, CmmStatics) -> SDoc
pprDatas platform (align, (Statics lbl dats))
 = vcat (pprAlign platform align : pprLabel platform lbl : map (pprData platform) dats)
 -- TODO: could remove if align == 1

pprData :: Platform -> CmmStatic -> SDoc
pprData _ (CmmString str)          = pprASCII str

pprData platform (CmmUninitialised bytes)
 | platformOS platform == OSDarwin = ptext (sLit ".space ") <> int bytes
 | otherwise                       = ptext (sLit ".skip ")  <> int bytes

pprData platform (CmmStaticLit lit) = pprDataItem platform lit

pprGloblDecl :: Platform -> CLabel -> SDoc
pprGloblDecl platform lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = ptext (sLit ".globl ") <> pprCLabel platform lbl

pprTypeAndSizeDecl :: Platform -> CLabel -> SDoc
pprTypeAndSizeDecl platform lbl
 | osElfTarget (platformOS platform) && externallyVisibleCLabel lbl
    = ptext (sLit ".type ") <>
      pprCLabel platform lbl <> ptext (sLit ", @object")
 | otherwise = empty

pprLabel :: Platform -> CLabel -> SDoc
pprLabel platform lbl = pprGloblDecl platform lbl
                     $$ pprTypeAndSizeDecl platform lbl
                     $$ (pprCLabel platform lbl <> char ':')


pprASCII :: [Word8] -> SDoc
pprASCII str
  = vcat (map do1 str) $$ do1 0
    where
       do1 :: Word8 -> SDoc
       do1 w = ptext (sLit "\t.byte\t") <> int (fromIntegral w)

pprAlign :: Platform -> Int -> SDoc
pprAlign platform bytes
        = ptext (sLit ".align ") <> int alignment
  where
        alignment = if platformOS platform == OSDarwin
                    then log2 bytes
                    else      bytes

        log2 :: Int -> Int  -- cache the common ones
        log2 1 = 0
        log2 2 = 1
        log2 4 = 2
        log2 8 = 3
        log2 n = 1 + log2 (n `quot` 2)

-- -----------------------------------------------------------------------------
-- pprInstr: print an 'Instr'

instance Outputable Instr where
    ppr instr = sdocWithPlatform $ \platform -> pprInstr platform instr


pprReg :: Platform -> Size -> Reg -> SDoc
pprReg platform s r
  = case r of
      RegReal    (RealRegSingle i) ->
          if target32Bit platform then ppr32_reg_no s i
                                  else ppr64_reg_no s i
      RegReal    (RealRegPair _ _) -> panic "X86.Ppr: no reg pairs on this arch"
      RegVirtual (VirtualRegI  u)  -> text "%vI_" <> pprUnique u
      RegVirtual (VirtualRegHi u)  -> text "%vHi_" <> pprUnique u
      RegVirtual (VirtualRegF  u)  -> text "%vF_" <> pprUnique u
      RegVirtual (VirtualRegD  u)  -> text "%vD_" <> pprUnique u
      RegVirtual (VirtualRegSSE  u) -> text "%vSSE_" <> pprUnique u
  where
    ppr32_reg_no :: Size -> Int -> SDoc
    ppr32_reg_no II8   = ppr32_reg_byte
    ppr32_reg_no II16  = ppr32_reg_word
    ppr32_reg_no _     = ppr32_reg_long

    ppr32_reg_byte i = ptext
      (case i of {
         0 -> sLit "%al";     1 -> sLit "%bl";
         2 -> sLit "%cl";     3 -> sLit "%dl";
        _  -> sLit "very naughty I386 byte register"
      })

    ppr32_reg_word i = ptext
      (case i of {
         0 -> sLit "%ax";     1 -> sLit "%bx";
         2 -> sLit "%cx";     3 -> sLit "%dx";
         4 -> sLit "%si";     5 -> sLit "%di";
         6 -> sLit "%bp";     7 -> sLit "%sp";
        _  -> sLit "very naughty I386 word register"
      })

    ppr32_reg_long i = ptext
      (case i of {
         0 -> sLit "%eax";    1 -> sLit "%ebx";
         2 -> sLit "%ecx";    3 -> sLit "%edx";
         4 -> sLit "%esi";    5 -> sLit "%edi";
         6 -> sLit "%ebp";    7 -> sLit "%esp";
         _  -> ppr_reg_float i
      })

    ppr64_reg_no :: Size -> Int -> SDoc
    ppr64_reg_no II8   = ppr64_reg_byte
    ppr64_reg_no II16  = ppr64_reg_word
    ppr64_reg_no II32  = ppr64_reg_long
    ppr64_reg_no _     = ppr64_reg_quad

    ppr64_reg_byte i = ptext
      (case i of {
         0 -> sLit "%al";     1 -> sLit "%bl";
         2 -> sLit "%cl";     3 -> sLit "%dl";
         4 -> sLit "%sil";    5 -> sLit "%dil"; -- new 8-bit regs!
         6 -> sLit "%bpl";    7 -> sLit "%spl";
         8 -> sLit "%r8b";    9  -> sLit "%r9b";
        10 -> sLit "%r10b";   11 -> sLit "%r11b";
        12 -> sLit "%r12b";   13 -> sLit "%r13b";
        14 -> sLit "%r14b";   15 -> sLit "%r15b";
        _  -> sLit "very naughty x86_64 byte register"
      })

    ppr64_reg_word i = ptext
      (case i of {
         0 -> sLit "%ax";     1 -> sLit "%bx";
         2 -> sLit "%cx";     3 -> sLit "%dx";
         4 -> sLit "%si";     5 -> sLit "%di";
         6 -> sLit "%bp";     7 -> sLit "%sp";
         8 -> sLit "%r8w";    9  -> sLit "%r9w";
        10 -> sLit "%r10w";   11 -> sLit "%r11w";
        12 -> sLit "%r12w";   13 -> sLit "%r13w";
        14 -> sLit "%r14w";   15 -> sLit "%r15w";
        _  -> sLit "very naughty x86_64 word register"
      })

    ppr64_reg_long i = ptext
      (case i of {
         0 -> sLit "%eax";    1  -> sLit "%ebx";
         2 -> sLit "%ecx";    3  -> sLit "%edx";
         4 -> sLit "%esi";    5  -> sLit "%edi";
         6 -> sLit "%ebp";    7  -> sLit "%esp";
         8 -> sLit "%r8d";    9  -> sLit "%r9d";
        10 -> sLit "%r10d";   11 -> sLit "%r11d";
        12 -> sLit "%r12d";   13 -> sLit "%r13d";
        14 -> sLit "%r14d";   15 -> sLit "%r15d";
        _  -> sLit "very naughty x86_64 register"
      })

    ppr64_reg_quad i = ptext
      (case i of {
         0 -> sLit "%rax";      1 -> sLit "%rbx";
         2 -> sLit "%rcx";      3 -> sLit "%rdx";
         4 -> sLit "%rsi";      5 -> sLit "%rdi";
         6 -> sLit "%rbp";      7 -> sLit "%rsp";
         8 -> sLit "%r8";       9 -> sLit "%r9";
        10 -> sLit "%r10";    11 -> sLit "%r11";
        12 -> sLit "%r12";    13 -> sLit "%r13";
        14 -> sLit "%r14";    15 -> sLit "%r15";
        _  -> ppr_reg_float i
      })

ppr_reg_float :: Int -> LitString
ppr_reg_float i = case i of
        16 -> sLit "%fake0";  17 -> sLit "%fake1"
        18 -> sLit "%fake2";  19 -> sLit "%fake3"
        20 -> sLit "%fake4";  21 -> sLit "%fake5"
        24 -> sLit "%xmm0";   25 -> sLit "%xmm1"
        26 -> sLit "%xmm2";   27 -> sLit "%xmm3"
        28 -> sLit "%xmm4";   29 -> sLit "%xmm5"
        30 -> sLit "%xmm6";   31 -> sLit "%xmm7"
        32 -> sLit "%xmm8";   33 -> sLit "%xmm9"
        34 -> sLit "%xmm10";  35 -> sLit "%xmm11"
        36 -> sLit "%xmm12";  37 -> sLit "%xmm13"
        38 -> sLit "%xmm14";  39 -> sLit "%xmm15"
        _  -> sLit "very naughty x86 register"

pprSize :: Size -> SDoc
pprSize x
 = ptext (case x of
                II8   -> sLit "b"
                II16  -> sLit "w"
                II32  -> sLit "l"
                II64  -> sLit "q"
                FF32  -> sLit "ss"      -- "scalar single-precision float" (SSE2)
                FF64  -> sLit "sd"      -- "scalar double-precision float" (SSE2)
                FF80  -> sLit "t"
                )

pprSize_x87 :: Size -> SDoc
pprSize_x87 x
  = ptext $ case x of
                FF32  -> sLit "s"
                FF64  -> sLit "l"
                FF80  -> sLit "t"
                _     -> panic "X86.Ppr.pprSize_x87"

pprCond :: Cond -> SDoc
pprCond c
 = ptext (case c of {
                GEU     -> sLit "ae";   LU    -> sLit "b";
                EQQ     -> sLit "e";    GTT   -> sLit "g";
                GE      -> sLit "ge";   GU    -> sLit "a";
                LTT     -> sLit "l";    LE    -> sLit "le";
                LEU     -> sLit "be";   NE    -> sLit "ne";
                NEG     -> sLit "s";    POS   -> sLit "ns";
                CARRY   -> sLit "c";   OFLO  -> sLit "o";
                PARITY  -> sLit "p";   NOTPARITY -> sLit "np";
                ALWAYS  -> sLit "mp"})


pprImm :: Platform -> Imm -> SDoc
pprImm _        (ImmInt i)     = int i
pprImm _        (ImmInteger i) = integer i
pprImm platform (ImmCLbl l)    = pprCLabel platform l
pprImm platform (ImmIndex l i) = pprCLabel platform l <> char '+' <> int i
pprImm _        (ImmLit s)     = s

pprImm _        (ImmFloat _)  = ptext (sLit "naughty float immediate")
pprImm _        (ImmDouble _) = ptext (sLit "naughty double immediate")

pprImm platform (ImmConstantSum a b) = pprImm platform a <> char '+' <> pprImm platform b
pprImm platform (ImmConstantDiff a b) = pprImm platform a <> char '-'
                                     <> lparen <> pprImm platform b <> rparen



pprAddr :: Platform -> AddrMode -> SDoc
pprAddr platform (ImmAddr imm off)
  = let pp_imm = pprImm platform imm
    in
    if (off == 0) then
        pp_imm
    else if (off < 0) then
        pp_imm <> int off
    else
        pp_imm <> char '+' <> int off

pprAddr platform (AddrBaseIndex base index displacement)
  = let
        pp_disp  = ppr_disp displacement
        pp_off p = pp_disp <> char '(' <> p <> char ')'
        pp_reg r = pprReg platform (archWordSize (target32Bit platform)) r
    in
    case (base, index) of
      (EABaseNone,  EAIndexNone) -> pp_disp
      (EABaseReg b, EAIndexNone) -> pp_off (pp_reg b)
      (EABaseRip,   EAIndexNone) -> pp_off (ptext (sLit "%rip"))
      (EABaseNone,  EAIndex r i) -> pp_off (comma <> pp_reg r <> comma <> int i)
      (EABaseReg b, EAIndex r i) -> pp_off (pp_reg b <> comma <> pp_reg r
                                       <> comma <> int i)
      _                         -> panic "X86.Ppr.pprAddr: no match"

  where
    ppr_disp (ImmInt 0) = empty
    ppr_disp imm        = pprImm platform imm


pprSectionHeader :: Platform -> Section -> SDoc
pprSectionHeader platform seg
 = case platformOS platform of
   OSDarwin
    | target32Bit platform ->
       case seg of
           Text                    -> ptext (sLit ".text\n\t.align 2")
           Data                    -> ptext (sLit ".data\n\t.align 2")
           ReadOnlyData            -> ptext (sLit ".const\n.align 2")
           RelocatableReadOnlyData -> ptext (sLit ".const_data\n.align 2")
           UninitialisedData       -> ptext (sLit ".data\n\t.align 2")
           ReadOnlyData16          -> ptext (sLit ".const\n.align 4")
           OtherSection _          -> panic "X86.Ppr.pprSectionHeader: unknown section"
    | otherwise ->
       case seg of
           Text                    -> ptext (sLit ".text\n.align 3")
           Data                    -> ptext (sLit ".data\n.align 3")
           ReadOnlyData            -> ptext (sLit ".const\n.align 3")
           RelocatableReadOnlyData -> ptext (sLit ".const_data\n.align 3")
           UninitialisedData       -> ptext (sLit ".data\n\t.align 3")
           ReadOnlyData16          -> ptext (sLit ".const\n.align 4")
           OtherSection _          -> panic "PprMach.pprSectionHeader: unknown section"
   _
    | target32Bit platform ->
       case seg of
           Text                    -> ptext (sLit ".text\n\t.align 4,0x90")
           Data                    -> ptext (sLit ".data\n\t.align 4")
           ReadOnlyData            -> ptext (sLit ".section .rodata\n\t.align 4")
           RelocatableReadOnlyData -> ptext (sLit ".section .data\n\t.align 4")
           UninitialisedData       -> ptext (sLit ".section .bss\n\t.align 4")
           ReadOnlyData16          -> ptext (sLit ".section .rodata\n\t.align 16")
           OtherSection _          -> panic "X86.Ppr.pprSectionHeader: unknown section"
    | otherwise ->
       case seg of
           Text                    -> ptext (sLit ".text\n\t.align 8")
           Data                    -> ptext (sLit ".data\n\t.align 8")
           ReadOnlyData            -> ptext (sLit ".section .rodata\n\t.align 8")
           RelocatableReadOnlyData -> ptext (sLit ".section .data\n\t.align 8")
           UninitialisedData       -> ptext (sLit ".section .bss\n\t.align 8")
           ReadOnlyData16          -> ptext (sLit ".section .rodata.cst16\n\t.align 16")
           OtherSection _          -> panic "PprMach.pprSectionHeader: unknown section"




pprDataItem :: Platform -> CmmLit -> SDoc
pprDataItem platform lit
  = vcat (ppr_item (cmmTypeSize $ cmmLitType lit) lit)
    where
        imm = litToImm lit

        -- These seem to be common:
        ppr_item II8   _ = [ptext (sLit "\t.byte\t") <> pprImm platform imm]
        ppr_item II16  _ = [ptext (sLit "\t.word\t") <> pprImm platform imm]
        ppr_item II32  _ = [ptext (sLit "\t.long\t") <> pprImm platform imm]

        ppr_item FF32  (CmmFloat r _)
           = let bs = floatToBytes (fromRational r)
             in  map (\b -> ptext (sLit "\t.byte\t") <> pprImm platform (ImmInt b)) bs

        ppr_item FF64 (CmmFloat r _)
           = let bs = doubleToBytes (fromRational r)
             in  map (\b -> ptext (sLit "\t.byte\t") <> pprImm platform (ImmInt b)) bs

        ppr_item II64 _
            = case platformOS platform of
              OSDarwin
               | target32Bit platform ->
                  case lit of
                  CmmInt x _ ->
                      [ptext (sLit "\t.long\t")
                          <> int (fromIntegral (fromIntegral x :: Word32)),
                       ptext (sLit "\t.long\t")
                          <> int (fromIntegral
                              (fromIntegral (x `shiftR` 32) :: Word32))]
                  _ -> panic "X86.Ppr.ppr_item: no match for II64"
               | otherwise ->
                  [ptext (sLit "\t.quad\t") <> pprImm platform imm]
              _
               | target32Bit platform ->
                  [ptext (sLit "\t.quad\t") <> pprImm platform imm]
               | otherwise ->
                  -- x86_64: binutils can't handle the R_X86_64_PC64
                  -- relocation type, which means we can't do
                  -- pc-relative 64-bit addresses. Fortunately we're
                  -- assuming the small memory model, in which all such
                  -- offsets will fit into 32 bits, so we have to stick
                  -- to 32-bit offset fields and modify the RTS
                  -- appropriately
                  --
                  -- See Note [x86-64-relative] in includes/rts/storage/InfoTables.h
                  --
                  case lit of
                  -- A relative relocation:
                  CmmLabelDiffOff _ _ _ ->
                      [ptext (sLit "\t.long\t") <> pprImm platform imm,
                       ptext (sLit "\t.long\t0")]
                  _ ->
                      [ptext (sLit "\t.quad\t") <> pprImm platform imm]

        ppr_item _ _
                = panic "X86.Ppr.ppr_item: no match"



pprInstr :: Platform -> Instr -> SDoc

pprInstr _ (COMMENT _) = empty -- nuke 'em
{-
pprInstr _ (COMMENT s) = ptext (sLit "# ") <> ftext s
-}
pprInstr platform (DELTA d)
   = pprInstr platform (COMMENT (mkFastString ("\tdelta = " ++ show d)))

pprInstr _ (NEWBLOCK _)
   = panic "PprMach.pprInstr: NEWBLOCK"

pprInstr _ (LDATA _ _)
   = panic "PprMach.pprInstr: LDATA"

{-
pprInstr _ (SPILL reg slot)
   = hcat [
        ptext (sLit "\tSPILL"),
        char ' ',
        pprUserReg reg,
        comma,
        ptext (sLit "SLOT") <> parens (int slot)]

pprInstr _ (RELOAD slot reg)
   = hcat [
        ptext (sLit "\tRELOAD"),
        char ' ',
        ptext (sLit "SLOT") <> parens (int slot),
        comma,
        pprUserReg reg]
-}

pprInstr platform (MOV size src dst)
  = pprSizeOpOp platform (sLit "mov") size src dst

pprInstr platform (MOVZxL II32 src dst) = pprSizeOpOp platform (sLit "mov") II32 src dst
        -- 32-to-64 bit zero extension on x86_64 is accomplished by a simple
        -- movl.  But we represent it as a MOVZxL instruction, because
        -- the reg alloc would tend to throw away a plain reg-to-reg
        -- move, and we still want it to do that.

pprInstr platform (MOVZxL sizes src dst) = pprSizeOpOpCoerce platform (sLit "movz") sizes II32 src dst
        -- zero-extension only needs to extend to 32 bits: on x86_64,
        -- the remaining zero-extension to 64 bits is automatic, and the 32-bit
        -- instruction is shorter.

pprInstr platform (MOVSxL sizes src dst) = pprSizeOpOpCoerce platform (sLit "movs") sizes (archWordSize (target32Bit platform)) src dst

-- here we do some patching, since the physical registers are only set late
-- in the code generation.
pprInstr platform (LEA size (OpAddr (AddrBaseIndex (EABaseReg reg1) (EAIndex reg2 1) (ImmInt 0))) dst@(OpReg reg3))
  | reg1 == reg3
  = pprSizeOpOp platform (sLit "add") size (OpReg reg2) dst

pprInstr platform (LEA size (OpAddr (AddrBaseIndex (EABaseReg reg1) (EAIndex reg2 1) (ImmInt 0))) dst@(OpReg reg3))
  | reg2 == reg3
  = pprSizeOpOp platform (sLit "add") size (OpReg reg1) dst

pprInstr platform (LEA size (OpAddr (AddrBaseIndex (EABaseReg reg1) EAIndexNone displ)) dst@(OpReg reg3))
  | reg1 == reg3
  = pprInstr platform (ADD size (OpImm displ) dst)

pprInstr platform (LEA size src dst) = pprSizeOpOp platform (sLit "lea") size src dst

pprInstr platform (ADD size (OpImm (ImmInt (-1))) dst)
  = pprSizeOp platform (sLit "dec") size dst
pprInstr platform (ADD size (OpImm (ImmInt 1)) dst)
  = pprSizeOp platform (sLit "inc") size dst
pprInstr platform (ADD size src dst)
  = pprSizeOpOp platform (sLit "add") size src dst
pprInstr platform (ADC size src dst)
  = pprSizeOpOp platform (sLit "adc") size src dst
pprInstr platform (SUB size src dst) = pprSizeOpOp platform (sLit "sub") size src dst
pprInstr platform (IMUL size op1 op2) = pprSizeOpOp platform (sLit "imul") size op1 op2

{- A hack.  The Intel documentation says that "The two and three
   operand forms [of IMUL] may also be used with unsigned operands
   because the lower half of the product is the same regardless if
   (sic) the operands are signed or unsigned.  The CF and OF flags,
   however, cannot be used to determine if the upper half of the
   result is non-zero."  So there.
-}
pprInstr platform (AND size src dst) = pprSizeOpOp platform (sLit "and") size src dst
pprInstr platform (OR  size src dst) = pprSizeOpOp platform (sLit "or")  size src dst

pprInstr platform (XOR FF32 src dst) = pprOpOp platform (sLit "xorps") FF32 src dst
pprInstr platform (XOR FF64 src dst) = pprOpOp platform (sLit "xorpd") FF64 src dst
pprInstr platform (XOR size src dst) = pprSizeOpOp platform (sLit "xor")  size src dst

pprInstr platform (POPCNT size src dst) = pprOpOp platform (sLit "popcnt") size src (OpReg dst)

pprInstr platform (NOT size op) = pprSizeOp platform (sLit "not") size op
pprInstr platform (NEGI size op) = pprSizeOp platform (sLit "neg") size op

pprInstr platform (SHL size src dst) = pprShift platform (sLit "shl") size src dst
pprInstr platform (SAR size src dst) = pprShift platform (sLit "sar") size src dst
pprInstr platform (SHR size src dst) = pprShift platform (sLit "shr") size src dst

pprInstr platform (BT  size imm src) = pprSizeImmOp platform (sLit "bt") size imm src

pprInstr platform (CMP size src dst)
  | is_float size =  pprSizeOpOp platform (sLit "ucomi") size src dst -- SSE2
  | otherwise     =  pprSizeOpOp platform (sLit "cmp")   size src dst
  where
        -- This predicate is needed here and nowhere else
    is_float FF32       = True
    is_float FF64       = True
    is_float FF80       = True
    is_float _          = False

pprInstr platform (TEST size src dst) = pprSizeOpOp platform (sLit "test")  size src dst
pprInstr platform (PUSH size op) = pprSizeOp platform (sLit "push") size op
pprInstr platform (POP size op) = pprSizeOp platform (sLit "pop") size op

-- both unused (SDM):
-- pprInstr PUSHA = ptext (sLit "\tpushal")
-- pprInstr POPA = ptext (sLit "\tpopal")

pprInstr _ NOP = ptext (sLit "\tnop")
pprInstr _ (CLTD II32) = ptext (sLit "\tcltd")
pprInstr _ (CLTD II64) = ptext (sLit "\tcqto")

pprInstr platform (SETCC cond op) = pprCondInstr (sLit "set") cond (pprOperand platform II8 op)

pprInstr platform (JXX cond blockid)
  = pprCondInstr (sLit "j") cond (pprCLabel platform lab)
  where lab = mkAsmTempLabel (getUnique blockid)

pprInstr platform (JXX_GBL cond imm) = pprCondInstr (sLit "j") cond (pprImm platform imm)

pprInstr platform (JMP (OpImm imm) _) = (<>) (ptext (sLit "\tjmp ")) (pprImm platform imm)
pprInstr platform (JMP op _)          = (<>) (ptext (sLit "\tjmp *")) (pprOperand platform (archWordSize (target32Bit platform)) op)
pprInstr platform (JMP_TBL op _ _ _)  = pprInstr platform (JMP op [])
pprInstr platform (CALL (Left imm) _)    = (<>) (ptext (sLit "\tcall ")) (pprImm platform imm)
pprInstr platform (CALL (Right reg) _)   = (<>) (ptext (sLit "\tcall *")) (pprReg platform (archWordSize (target32Bit platform)) reg)

pprInstr platform (IDIV sz op)   = pprSizeOp platform (sLit "idiv") sz op
pprInstr platform (DIV sz op)    = pprSizeOp platform (sLit "div")  sz op
pprInstr platform (IMUL2 sz op)  = pprSizeOp platform (sLit "imul") sz op

-- x86_64 only
pprInstr platform (MUL size op1 op2) = pprSizeOpOp platform (sLit "mul") size op1 op2
pprInstr platform (MUL2 size op) = pprSizeOp platform (sLit "mul") size op

pprInstr platform (FDIV size op1 op2) = pprSizeOpOp platform (sLit "div") size op1 op2

pprInstr platform (CVTSS2SD from to)      = pprRegReg platform (sLit "cvtss2sd") from to
pprInstr platform (CVTSD2SS from to)      = pprRegReg platform (sLit "cvtsd2ss") from to
pprInstr platform (CVTTSS2SIQ sz from to) = pprSizeSizeOpReg platform (sLit "cvttss2si") FF32 sz from to
pprInstr platform (CVTTSD2SIQ sz from to) = pprSizeSizeOpReg platform (sLit "cvttsd2si") FF64 sz from to
pprInstr platform (CVTSI2SS sz from to)   = pprSizeOpReg platform (sLit "cvtsi2ss") sz from to
pprInstr platform (CVTSI2SD sz from to)   = pprSizeOpReg platform (sLit "cvtsi2sd") sz from to

    -- FETCHGOT for PIC on ELF platforms
pprInstr platform (FETCHGOT reg)
   = vcat [ ptext (sLit "\tcall 1f"),
            hcat [ ptext (sLit "1:\tpopl\t"), pprReg platform II32 reg ],
            hcat [ ptext (sLit "\taddl\t$_GLOBAL_OFFSET_TABLE_+(.-1b), "),
                   pprReg platform II32 reg ]
          ]

    -- FETCHPC for PIC on Darwin/x86
    -- get the instruction pointer into a register
    -- (Terminology note: the IP is called Program Counter on PPC,
    --  and it's a good thing to use the same name on both platforms)
pprInstr platform (FETCHPC reg)
   = vcat [ ptext (sLit "\tcall 1f"),
            hcat [ ptext (sLit "1:\tpopl\t"), pprReg platform II32 reg ]
          ]


-- -----------------------------------------------------------------------------
-- i386 floating-point

-- Simulating a flat register set on the x86 FP stack is tricky.
-- you have to free %st(7) before pushing anything on the FP reg stack
-- so as to preclude the possibility of a FP stack overflow exception.
pprInstr platform g@(GMOV src dst)
   | src == dst
   = empty
   | otherwise
   = pprG platform g (hcat [gtab, gpush src 0, gsemi, gpop dst 1])

-- GLD sz addr dst ==> FLDsz addr ; FSTP (dst+1)
pprInstr platform g@(GLD sz addr dst)
 = pprG platform g (hcat [gtab, text "fld", pprSize_x87 sz, gsp,
                          pprAddr platform addr, gsemi, gpop dst 1])

-- GST sz src addr ==> FLD dst ; FSTPsz addr
pprInstr platform g@(GST sz src addr)
 | src == fake0 && sz /= FF80 -- fstt instruction doesn't exist
 = pprG platform g (hcat [gtab,
                          text "fst", pprSize_x87 sz, gsp, pprAddr platform addr])
 | otherwise
 = pprG platform g (hcat [gtab, gpush src 0, gsemi,
                          text "fstp", pprSize_x87 sz, gsp, pprAddr platform addr])

pprInstr platform g@(GLDZ dst)
 = pprG platform g (hcat [gtab, text "fldz ; ", gpop dst 1])
pprInstr platform g@(GLD1 dst)
 = pprG platform g (hcat [gtab, text "fld1 ; ", gpop dst 1])

pprInstr platform (GFTOI src dst)
   = pprInstr platform (GDTOI src dst)

pprInstr platform g@(GDTOI src dst)
   = pprG platform g (vcat [
         hcat [gtab, text "subl $8, %esp ; fnstcw 4(%esp)"],
         hcat [gtab, gpush src 0],
         hcat [gtab, text "movzwl 4(%esp), ", reg,
                     text " ; orl $0xC00, ", reg],
         hcat [gtab, text "movl ", reg, text ", 0(%esp) ; fldcw 0(%esp)"],
         hcat [gtab, text "fistpl 0(%esp)"],
         hcat [gtab, text "fldcw 4(%esp) ; movl 0(%esp), ", reg],
         hcat [gtab, text "addl $8, %esp"]
     ])
   where
     reg = pprReg platform II32 dst

pprInstr platform (GITOF src dst)
   = pprInstr platform (GITOD src dst)

pprInstr platform g@(GITOD src dst)
   = pprG platform g (hcat [gtab, text "pushl ", pprReg platform II32 src,
                            text " ; fildl (%esp) ; ",
                            gpop dst 1, text " ; addl $4,%esp"])

pprInstr platform g@(GDTOF src dst)
  = pprG platform g (vcat [gtab <> gpush src 0,
                           gtab <> text "subl $4,%esp ; fstps (%esp) ; flds (%esp) ; addl $4,%esp ;",
                           gtab <> gpop dst 1])

{- Gruesome swamp follows.  If you're unfortunate enough to have ventured
   this far into the jungle AND you give a Rat's Ass (tm) what's going
   on, here's the deal.  Generate code to do a floating point comparison
   of src1 and src2, of kind cond, and set the Zero flag if true.

   The complications are to do with handling NaNs correctly.  We want the
   property that if either argument is NaN, then the result of the
   comparison is False ... except if we're comparing for inequality,
   in which case the answer is True.

   Here's how the general (non-inequality) case works.  As an
   example, consider generating the an equality test:

     pushl %eax         -- we need to mess with this
     <get src1 to top of FPU stack>
     fcomp <src2 location in FPU stack> and pop pushed src1
                -- Result of comparison is in FPU Status Register bits
                -- C3 C2 and C0
     fstsw %ax  -- Move FPU Status Reg to %ax
     sahf       -- move C3 C2 C0 from %ax to integer flag reg
     -- now the serious magic begins
     setpo %ah     -- %ah = if comparable(neither arg was NaN) then 1 else 0
     sete  %al     -- %al = if arg1 == arg2 then 1 else 0
     andb %ah,%al  -- %al &= %ah
                   -- so %al == 1 iff (comparable && same); else it holds 0
     decb %al      -- %al == 0, ZeroFlag=1  iff (comparable && same);
                      else %al == 0xFF, ZeroFlag=0
     -- the zero flag is now set as we desire.
     popl %eax

   The special case of inequality differs thusly:

     setpe %ah     -- %ah = if incomparable(either arg was NaN) then 1 else 0
     setne %al     -- %al = if arg1 /= arg2 then 1 else 0
     orb %ah,%al   -- %al = if (incomparable || different) then 1 else 0
     decb %al      -- if (incomparable || different) then (%al == 0, ZF=1)
                                                     else (%al == 0xFF, ZF=0)
-}
pprInstr platform g@(GCMP cond src1 src2)
   | case cond of { NE -> True; _ -> False }
   = pprG platform g (vcat [
        hcat [gtab, text "pushl %eax ; ",gpush src1 0],
        hcat [gtab, text "fcomp ", greg src2 1,
                    text "; fstsw %ax ; sahf ;  setpe %ah"],
        hcat [gtab, text "setne %al ;  ",
              text "orb %ah,%al ;  decb %al ;  popl %eax"]
    ])
   | otherwise
   = pprG platform g (vcat [
        hcat [gtab, text "pushl %eax ; ",gpush src1 0],
        hcat [gtab, text "fcomp ", greg src2 1,
                    text "; fstsw %ax ; sahf ;  setpo %ah"],
        hcat [gtab, text "set", pprCond (fix_FP_cond cond), text " %al ;  ",
              text "andb %ah,%al ;  decb %al ;  popl %eax"]
    ])
    where
        {- On the 486, the flags set by FP compare are the unsigned ones!
           (This looks like a HACK to me.  WDP 96/03)
        -}
        fix_FP_cond :: Cond -> Cond
        fix_FP_cond GE   = GEU
        fix_FP_cond GTT  = GU
        fix_FP_cond LTT  = LU
        fix_FP_cond LE   = LEU
        fix_FP_cond EQQ  = EQQ
        fix_FP_cond NE   = NE
        fix_FP_cond _    = panic "X86.Ppr.fix_FP_cond: no match"
        -- there should be no others


pprInstr platform g@(GABS _ src dst)
   = pprG platform g (hcat [gtab, gpush src 0, text " ; fabs ; ", gpop dst 1])

pprInstr platform g@(GNEG _ src dst)
   = pprG platform g (hcat [gtab, gpush src 0, text " ; fchs ; ", gpop dst 1])

pprInstr platform g@(GSQRT sz src dst)
   = pprG platform g (hcat [gtab, gpush src 0, text " ; fsqrt"] $$
                      hcat [gtab, gcoerceto sz, gpop dst 1])

pprInstr platform g@(GSIN sz l1 l2 src dst)
   = pprG platform g (pprTrigOp platform "fsin" False l1 l2 src dst sz)

pprInstr platform g@(GCOS sz l1 l2 src dst)
   = pprG platform g (pprTrigOp platform "fcos" False l1 l2 src dst sz)

pprInstr platform g@(GTAN sz l1 l2 src dst)
   = pprG platform g (pprTrigOp platform "fptan" True l1 l2 src dst sz)

-- In the translations for GADD, GMUL, GSUB and GDIV,
-- the first two cases are mere optimisations.  The otherwise clause
-- generates correct code under all circumstances.

pprInstr platform g@(GADD _ src1 src2 dst)
   | src1 == dst
   = pprG platform g (text "\t#GADD-xxxcase1" $$
                      hcat [gtab, gpush src2 0,
                            text " ; faddp %st(0),", greg src1 1])
   | src2 == dst
   = pprG platform g (text "\t#GADD-xxxcase2" $$
                      hcat [gtab, gpush src1 0,
                            text " ; faddp %st(0),", greg src2 1])
   | otherwise
   = pprG platform g (hcat [gtab, gpush src1 0,
                            text " ; fadd ", greg src2 1, text ",%st(0)",
                            gsemi, gpop dst 1])


pprInstr platform g@(GMUL _ src1 src2 dst)
   | src1 == dst
   = pprG platform g (text "\t#GMUL-xxxcase1" $$
                      hcat [gtab, gpush src2 0,
                            text " ; fmulp %st(0),", greg src1 1])
   | src2 == dst
   = pprG platform g (text "\t#GMUL-xxxcase2" $$
                      hcat [gtab, gpush src1 0,
                            text " ; fmulp %st(0),", greg src2 1])
   | otherwise
   = pprG platform g (hcat [gtab, gpush src1 0,
                            text " ; fmul ", greg src2 1, text ",%st(0)",
                            gsemi, gpop dst 1])


pprInstr platform g@(GSUB _ src1 src2 dst)
   | src1 == dst
   = pprG platform g (text "\t#GSUB-xxxcase1" $$
                      hcat [gtab, gpush src2 0,
                            text " ; fsubrp %st(0),", greg src1 1])
   | src2 == dst
   = pprG platform g (text "\t#GSUB-xxxcase2" $$
                      hcat [gtab, gpush src1 0,
                            text " ; fsubp %st(0),", greg src2 1])
   | otherwise
   = pprG platform g (hcat [gtab, gpush src1 0,
                            text " ; fsub ", greg src2 1, text ",%st(0)",
                            gsemi, gpop dst 1])


pprInstr platform g@(GDIV _ src1 src2 dst)
   | src1 == dst
   = pprG platform g (text "\t#GDIV-xxxcase1" $$
                      hcat [gtab, gpush src2 0,
                            text " ; fdivrp %st(0),", greg src1 1])
   | src2 == dst
   = pprG platform g (text "\t#GDIV-xxxcase2" $$
                      hcat [gtab, gpush src1 0,
                            text " ; fdivp %st(0),", greg src2 1])
   | otherwise
   = pprG platform g (hcat [gtab, gpush src1 0,
                            text " ; fdiv ", greg src2 1, text ",%st(0)",
                            gsemi, gpop dst 1])


pprInstr _ GFREE
   = vcat [ ptext (sLit "\tffree %st(0) ;ffree %st(1) ;ffree %st(2) ;ffree %st(3)"),
            ptext (sLit "\tffree %st(4) ;ffree %st(5)")
          ]

pprInstr _ _
        = panic "X86.Ppr.pprInstr: no match"


pprTrigOp :: Platform -> String -> Bool -> CLabel -> CLabel
          -> Reg -> Reg -> Size -> SDoc
pprTrigOp platform
          op -- fsin, fcos or fptan
          isTan -- we need a couple of extra steps if we're doing tan
          l1 l2 -- internal labels for us to use
          src dst sz
    = -- We'll be needing %eax later on
      hcat [gtab, text "pushl %eax;"] $$
      -- tan is going to use an extra space on the FP stack
      (if isTan then hcat [gtab, text "ffree %st(6)"] else empty) $$
      -- First put the value in %st(0) and try to apply the op to it
      hcat [gpush src 0, text ("; " ++ op)] $$
      -- Now look to see if C2 was set (overflow, |value| >= 2^63)
      hcat [gtab, text "fnstsw %ax"] $$
      hcat [gtab, text "test   $0x400,%eax"] $$
      -- If we were in bounds then jump to the end
      hcat [gtab, text "je     " <> pprCLabel platform l1] $$
      -- Otherwise we need to shrink the value. Start by
      -- loading pi, doubleing it (by adding it to itself),
      -- and then swapping pi with the value, so the value we
      -- want to apply op to is in %st(0) again
      hcat [gtab, text "ffree %st(7); fldpi"] $$
      hcat [gtab, text "fadd   %st(0),%st"] $$
      hcat [gtab, text "fxch   %st(1)"] $$
      -- Now we have a loop in which we make the value smaller,
      -- see if it's small enough, and loop if not
      (pprCLabel platform l2 <> char ':') $$
      hcat [gtab, text "fprem1"] $$
      -- My Debian libc uses fstsw here for the tan code, but I can't
      -- see any reason why it should need to be different for tan.
      hcat [gtab, text "fnstsw %ax"] $$
      hcat [gtab, text "test   $0x400,%eax"] $$
      hcat [gtab, text "jne    " <> pprCLabel platform l2] $$
      hcat [gtab, text "fstp   %st(1)"] $$
      hcat [gtab, text op] $$
      (pprCLabel platform l1 <> char ':') $$
      -- Pop the 1.0 tan gave us
      (if isTan then hcat [gtab, text "fstp %st(0)"] else empty) $$
      -- Restore %eax
      hcat [gtab, text "popl %eax;"] $$
      -- And finally make the result the right size
      hcat [gtab, gcoerceto sz, gpop dst 1]

--------------------------

-- coerce %st(0) to the specified size
gcoerceto :: Size -> SDoc
gcoerceto FF64 = empty
gcoerceto FF32 = empty --text "subl $4,%esp ; fstps (%esp) ; flds (%esp) ; addl $4,%esp ; "
gcoerceto _    = panic "X86.Ppr.gcoerceto: no match"

gpush :: Reg -> RegNo -> SDoc
gpush reg offset
   = hcat [text "fld ", greg reg offset]

gpop :: Reg -> RegNo -> SDoc
gpop reg offset
   = hcat [text "fstp ", greg reg offset]

greg :: Reg -> RegNo -> SDoc
greg reg offset = text "%st(" <> int (gregno reg - firstfake+offset) <> char ')'

gsemi :: SDoc
gsemi = text " ; "

gtab :: SDoc
gtab  = char '\t'

gsp :: SDoc
gsp   = char ' '

gregno :: Reg -> RegNo
gregno (RegReal (RealRegSingle i)) = i
gregno _           = --pprPanic "gregno" (ppr other)
                     999   -- bogus; only needed for debug printing

pprG :: Platform -> Instr -> SDoc -> SDoc
pprG platform fake actual
   = (char '#' <> pprGInstr platform fake) $$ actual


pprGInstr :: Platform -> Instr -> SDoc
pprGInstr platform (GMOV src dst)   = pprSizeRegReg platform (sLit "gmov") FF64 src dst
pprGInstr platform (GLD sz src dst) = pprSizeAddrReg platform (sLit "gld") sz src dst
pprGInstr platform (GST sz src dst) = pprSizeRegAddr platform (sLit "gst") sz src dst

pprGInstr platform (GLDZ dst) = pprSizeReg platform (sLit "gldz") FF64 dst
pprGInstr platform (GLD1 dst) = pprSizeReg platform (sLit "gld1") FF64 dst

pprGInstr platform (GFTOI src dst) = pprSizeSizeRegReg platform (sLit "gftoi") FF32 II32  src dst
pprGInstr platform (GDTOI src dst) = pprSizeSizeRegReg platform (sLit "gdtoi") FF64 II32 src dst

pprGInstr platform (GITOF src dst) = pprSizeSizeRegReg platform (sLit "gitof") II32 FF32  src dst
pprGInstr platform (GITOD src dst) = pprSizeSizeRegReg platform (sLit "gitod") II32 FF64 src dst
pprGInstr platform (GDTOF src dst) = pprSizeSizeRegReg platform (sLit "gdtof") FF64 FF32 src dst

pprGInstr platform (GCMP co src dst) = pprCondRegReg platform (sLit "gcmp_") FF64 co src dst
pprGInstr platform (GABS sz src dst) = pprSizeRegReg platform (sLit "gabs") sz src dst
pprGInstr platform (GNEG sz src dst) = pprSizeRegReg platform (sLit "gneg") sz src dst
pprGInstr platform (GSQRT sz src dst) = pprSizeRegReg platform (sLit "gsqrt") sz src dst
pprGInstr platform (GSIN sz _ _ src dst) = pprSizeRegReg platform (sLit "gsin") sz src dst
pprGInstr platform (GCOS sz _ _ src dst) = pprSizeRegReg platform (sLit "gcos") sz src dst
pprGInstr platform (GTAN sz _ _ src dst) = pprSizeRegReg platform (sLit "gtan") sz src dst

pprGInstr platform (GADD sz src1 src2 dst) = pprSizeRegRegReg platform (sLit "gadd") sz src1 src2 dst
pprGInstr platform (GSUB sz src1 src2 dst) = pprSizeRegRegReg platform (sLit "gsub") sz src1 src2 dst
pprGInstr platform (GMUL sz src1 src2 dst) = pprSizeRegRegReg platform (sLit "gmul") sz src1 src2 dst
pprGInstr platform (GDIV sz src1 src2 dst) = pprSizeRegRegReg platform (sLit "gdiv") sz src1 src2 dst

pprGInstr _ _ = panic "X86.Ppr.pprGInstr: no match"

pprDollImm :: Platform -> Imm -> SDoc
pprDollImm platform i = ptext (sLit "$") <> pprImm platform i


pprOperand :: Platform -> Size -> Operand -> SDoc
pprOperand platform s (OpReg r)   = pprReg platform s r
pprOperand platform _ (OpImm i)   = pprDollImm platform i
pprOperand platform _ (OpAddr ea) = pprAddr platform ea


pprMnemonic_  :: LitString -> SDoc
pprMnemonic_ name =
   char '\t' <> ptext name <> space


pprMnemonic  :: LitString -> Size -> SDoc
pprMnemonic name size =
   char '\t' <> ptext name <> pprSize size <> space


pprSizeImmOp :: Platform -> LitString -> Size -> Imm -> Operand -> SDoc
pprSizeImmOp platform name size imm op1
  = hcat [
        pprMnemonic name size,
        char '$',
        pprImm platform imm,
        comma,
        pprOperand platform size op1
    ]


pprSizeOp :: Platform -> LitString -> Size -> Operand -> SDoc
pprSizeOp platform name size op1
  = hcat [
        pprMnemonic name size,
        pprOperand platform size op1
    ]


pprSizeOpOp :: Platform -> LitString -> Size -> Operand -> Operand -> SDoc
pprSizeOpOp platform name size op1 op2
  = hcat [
        pprMnemonic name size,
        pprOperand platform size op1,
        comma,
        pprOperand platform size op2
    ]


pprOpOp :: Platform -> LitString -> Size -> Operand -> Operand -> SDoc
pprOpOp platform name size op1 op2
  = hcat [
        pprMnemonic_ name,
        pprOperand platform size op1,
        comma,
        pprOperand platform size op2
    ]


pprSizeReg :: Platform -> LitString -> Size -> Reg -> SDoc
pprSizeReg platform name size reg1
  = hcat [
        pprMnemonic name size,
        pprReg platform size reg1
    ]


pprSizeRegReg :: Platform -> LitString -> Size -> Reg -> Reg -> SDoc
pprSizeRegReg platform name size reg1 reg2
  = hcat [
        pprMnemonic name size,
        pprReg platform size reg1,
        comma,
        pprReg platform size reg2
    ]


pprRegReg :: Platform -> LitString -> Reg -> Reg -> SDoc
pprRegReg platform name reg1 reg2
  = hcat [
        pprMnemonic_ name,
        pprReg platform (archWordSize (target32Bit platform)) reg1,
        comma,
        pprReg platform (archWordSize (target32Bit platform)) reg2
    ]


pprSizeOpReg :: Platform -> LitString -> Size -> Operand -> Reg -> SDoc
pprSizeOpReg platform name size op1 reg2
  = hcat [
        pprMnemonic name size,
        pprOperand platform size op1,
        comma,
        pprReg platform (archWordSize (target32Bit platform)) reg2
    ]

pprCondRegReg :: Platform -> LitString -> Size -> Cond -> Reg -> Reg -> SDoc
pprCondRegReg platform name size cond reg1 reg2
  = hcat [
        char '\t',
        ptext name,
        pprCond cond,
        space,
        pprReg platform size reg1,
        comma,
        pprReg platform size reg2
    ]

pprSizeSizeRegReg :: Platform -> LitString -> Size -> Size -> Reg -> Reg -> SDoc
pprSizeSizeRegReg platform name size1 size2 reg1 reg2
  = hcat [
        char '\t',
        ptext name,
        pprSize size1,
        pprSize size2,
        space,
        pprReg platform size1 reg1,
        comma,
        pprReg platform size2 reg2
    ]

pprSizeSizeOpReg :: Platform -> LitString -> Size -> Size -> Operand -> Reg -> SDoc
pprSizeSizeOpReg platform name size1 size2 op1 reg2
  = hcat [
        pprMnemonic name size2,
        pprOperand platform size1 op1,
        comma,
        pprReg platform size2 reg2
    ]

pprSizeRegRegReg :: Platform -> LitString -> Size -> Reg -> Reg -> Reg -> SDoc
pprSizeRegRegReg platform name size reg1 reg2 reg3
  = hcat [
        pprMnemonic name size,
        pprReg platform size reg1,
        comma,
        pprReg platform size reg2,
        comma,
        pprReg platform size reg3
    ]


pprSizeAddrReg :: Platform -> LitString -> Size -> AddrMode -> Reg -> SDoc
pprSizeAddrReg platform name size op dst
  = hcat [
        pprMnemonic name size,
        pprAddr platform op,
        comma,
        pprReg platform size dst
    ]


pprSizeRegAddr :: Platform -> LitString -> Size -> Reg -> AddrMode -> SDoc
pprSizeRegAddr platform name size src op
  = hcat [
        pprMnemonic name size,
        pprReg platform size src,
        comma,
        pprAddr platform op
    ]


pprShift :: Platform -> LitString -> Size -> Operand -> Operand -> SDoc
pprShift platform name size src dest
  = hcat [
        pprMnemonic name size,
        pprOperand platform II8 src,  -- src is 8-bit sized
        comma,
        pprOperand platform size dest
    ]


pprSizeOpOpCoerce :: Platform -> LitString -> Size -> Size -> Operand -> Operand -> SDoc
pprSizeOpOpCoerce platform name size1 size2 op1 op2
  = hcat [ char '\t', ptext name, pprSize size1, pprSize size2, space,
        pprOperand platform size1 op1,
        comma,
        pprOperand platform size2 op2
    ]


pprCondInstr :: LitString -> Cond -> SDoc -> SDoc
pprCondInstr name cond arg
  = hcat [ char '\t', ptext name, pprCond cond, space, arg]

