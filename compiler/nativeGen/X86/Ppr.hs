-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
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


import BlockId
import BasicTypes       (Alignment)
import DynFlags
import Cmm              hiding (topInfoTable)
import CLabel
import Unique           ( pprUnique, Uniquable(..) )
import Platform
import FastString
import Outputable

import Data.Word

import Data.Bits

-- -----------------------------------------------------------------------------
-- Printing this stuff out

pprNatCmmDecl :: NatCmmDecl (Alignment, CmmStatics) Instr -> SDoc
pprNatCmmDecl (CmmData section dats) =
  pprSectionHeader section $$ pprDatas dats

pprNatCmmDecl proc@(CmmProc top_info lbl _ (ListGraph blocks)) =
  case topInfoTable proc of
    Nothing ->
       case blocks of
         []     -> -- special case for split markers:
           pprLabel lbl
         blocks -> -- special case for code without info table:
           pprSectionHeader Text $$
           pprLabel lbl $$ -- blocks guaranteed not null, so label needed
           vcat (map (pprBasicBlock top_info) blocks) $$
           pprSizeDecl lbl

    Just (Statics info_lbl _) ->
      sdocWithPlatform $ \platform ->
      (if platformHasSubsectionsViaSymbols platform
          then pprSectionHeader Text $$
               ppr (mkDeadStripPreventer info_lbl) <> char ':'
          else empty) $$
      vcat (map (pprBasicBlock top_info) blocks) $$
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
                  <+> ppr info_lbl
                  <+> char '-'
                  <+> ppr (mkDeadStripPreventer info_lbl)
             else empty) $$
      pprSizeDecl info_lbl

-- | Output the ELF .size directive.
pprSizeDecl :: CLabel -> SDoc
pprSizeDecl lbl
 = sdocWithPlatform $ \platform ->
   if osElfTarget (platformOS platform)
   then ptext (sLit "\t.size") <+> ppr lbl
     <> ptext (sLit ", .-") <> ppr lbl
   else empty

pprBasicBlock :: BlockEnv CmmStatics -> NatBasicBlock Instr -> SDoc
pprBasicBlock info_env (BasicBlock blockid instrs)
  = maybe_infotable $$
    pprLabel (mkAsmTempLabel (getUnique blockid)) $$
    vcat (map pprInstr instrs)
  where
    maybe_infotable = case mapLookup blockid info_env of
       Nothing   -> empty
       Just (Statics info_lbl info) ->
           pprSectionHeader Text $$
           vcat (map pprData info) $$
           pprLabel info_lbl

pprDatas :: (Alignment, CmmStatics) -> SDoc
pprDatas (align, (Statics lbl dats))
 = vcat (pprAlign align : pprLabel lbl : map pprData dats)
 -- TODO: could remove if align == 1

pprData :: CmmStatic -> SDoc
pprData (CmmString str) = pprASCII str

pprData (CmmUninitialised bytes)
 = sdocWithPlatform $ \platform ->
   if platformOS platform == OSDarwin then ptext (sLit ".space ") <> int bytes
                                      else ptext (sLit ".skip ")  <> int bytes

pprData (CmmStaticLit lit) = pprDataItem lit

pprGloblDecl :: CLabel -> SDoc
pprGloblDecl lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = ptext (sLit ".globl ") <> ppr lbl

pprTypeAndSizeDecl :: CLabel -> SDoc
pprTypeAndSizeDecl lbl
    = sdocWithPlatform $ \platform ->
      if osElfTarget (platformOS platform) && externallyVisibleCLabel lbl
      then ptext (sLit ".type ") <> ppr lbl <> ptext (sLit ", @object")
      else empty

pprLabel :: CLabel -> SDoc
pprLabel lbl = pprGloblDecl lbl
            $$ pprTypeAndSizeDecl lbl
            $$ (ppr lbl <> char ':')


pprASCII :: [Word8] -> SDoc
pprASCII str
  = vcat (map do1 str) $$ do1 0
    where
       do1 :: Word8 -> SDoc
       do1 w = ptext (sLit "\t.byte\t") <> int (fromIntegral w)

pprAlign :: Int -> SDoc
pprAlign bytes
        = sdocWithPlatform $ \platform ->
          ptext (sLit ".align ") <> int (alignment platform)
  where
        alignment platform = if platformOS platform == OSDarwin
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
    ppr instr = pprInstr instr


pprReg :: Size -> Reg -> SDoc
pprReg s r
  = case r of
      RegReal    (RealRegSingle i) ->
          sdocWithPlatform $ \platform ->
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


pprImm :: Imm -> SDoc
pprImm (ImmInt i)     = int i
pprImm (ImmInteger i) = integer i
pprImm (ImmCLbl l)    = ppr l
pprImm (ImmIndex l i) = ppr l <> char '+' <> int i
pprImm (ImmLit s)     = s

pprImm (ImmFloat _)  = ptext (sLit "naughty float immediate")
pprImm (ImmDouble _) = ptext (sLit "naughty double immediate")

pprImm (ImmConstantSum a b) = pprImm a <> char '+' <> pprImm b
pprImm (ImmConstantDiff a b) = pprImm a <> char '-'
                            <> lparen <> pprImm b <> rparen



pprAddr :: AddrMode -> SDoc
pprAddr (ImmAddr imm off)
  = let pp_imm = pprImm imm
    in
    if (off == 0) then
        pp_imm
    else if (off < 0) then
        pp_imm <> int off
    else
        pp_imm <> char '+' <> int off

pprAddr (AddrBaseIndex base index displacement)
  = sdocWithPlatform $ \platform ->
    let
        pp_disp  = ppr_disp displacement
        pp_off p = pp_disp <> char '(' <> p <> char ')'
        pp_reg r = pprReg (archWordSize (target32Bit platform)) r
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
    ppr_disp imm        = pprImm imm


pprSectionHeader :: Section -> SDoc
pprSectionHeader seg
 = sdocWithPlatform $ \platform ->
   case platformOS platform of
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




pprDataItem :: CmmLit -> SDoc
pprDataItem lit = sdocWithDynFlags $ \dflags -> pprDataItem' dflags lit

pprDataItem' :: DynFlags -> CmmLit -> SDoc
pprDataItem' dflags lit
  = vcat (ppr_item (cmmTypeSize $ cmmLitType dflags lit) lit)
    where
        platform = targetPlatform dflags
        imm = litToImm lit

        -- These seem to be common:
        ppr_item II8   _ = [ptext (sLit "\t.byte\t") <> pprImm imm]
        ppr_item II16  _ = [ptext (sLit "\t.word\t") <> pprImm imm]
        ppr_item II32  _ = [ptext (sLit "\t.long\t") <> pprImm imm]

        ppr_item FF32  (CmmFloat r _)
           = let bs = floatToBytes (fromRational r)
             in  map (\b -> ptext (sLit "\t.byte\t") <> pprImm (ImmInt b)) bs

        ppr_item FF64 (CmmFloat r _)
           = let bs = doubleToBytes (fromRational r)
             in  map (\b -> ptext (sLit "\t.byte\t") <> pprImm (ImmInt b)) bs

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
                  [ptext (sLit "\t.quad\t") <> pprImm imm]
              _
               | target32Bit platform ->
                  [ptext (sLit "\t.quad\t") <> pprImm imm]
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
                      [ptext (sLit "\t.long\t") <> pprImm imm,
                       ptext (sLit "\t.long\t0")]
                  _ ->
                      [ptext (sLit "\t.quad\t") <> pprImm imm]

        ppr_item _ _
                = panic "X86.Ppr.ppr_item: no match"



pprInstr :: Instr -> SDoc

pprInstr (COMMENT _) = empty -- nuke 'em
{-
pprInstr (COMMENT s) = ptext (sLit "# ") <> ftext s
-}
pprInstr (DELTA d)
   = pprInstr (COMMENT (mkFastString ("\tdelta = " ++ show d)))

pprInstr (NEWBLOCK _)
   = panic "PprMach.pprInstr: NEWBLOCK"

pprInstr (LDATA _ _)
   = panic "PprMach.pprInstr: LDATA"

{-
pprInstr (SPILL reg slot)
   = hcat [
        ptext (sLit "\tSPILL"),
        char ' ',
        pprUserReg reg,
        comma,
        ptext (sLit "SLOT") <> parens (int slot)]

pprInstr (RELOAD slot reg)
   = hcat [
        ptext (sLit "\tRELOAD"),
        char ' ',
        ptext (sLit "SLOT") <> parens (int slot),
        comma,
        pprUserReg reg]
-}

pprInstr (MOV size src dst)
  = pprSizeOpOp (sLit "mov") size src dst

pprInstr (MOVZxL II32 src dst) = pprSizeOpOp (sLit "mov") II32 src dst
        -- 32-to-64 bit zero extension on x86_64 is accomplished by a simple
        -- movl.  But we represent it as a MOVZxL instruction, because
        -- the reg alloc would tend to throw away a plain reg-to-reg
        -- move, and we still want it to do that.

pprInstr (MOVZxL sizes src dst) = pprSizeOpOpCoerce (sLit "movz") sizes II32 src dst
        -- zero-extension only needs to extend to 32 bits: on x86_64,
        -- the remaining zero-extension to 64 bits is automatic, and the 32-bit
        -- instruction is shorter.

pprInstr (MOVSxL sizes src dst)
  = sdocWithPlatform $ \platform ->
    pprSizeOpOpCoerce (sLit "movs") sizes (archWordSize (target32Bit platform)) src dst

-- here we do some patching, since the physical registers are only set late
-- in the code generation.
pprInstr (LEA size (OpAddr (AddrBaseIndex (EABaseReg reg1) (EAIndex reg2 1) (ImmInt 0))) dst@(OpReg reg3))
  | reg1 == reg3
  = pprSizeOpOp (sLit "add") size (OpReg reg2) dst

pprInstr (LEA size (OpAddr (AddrBaseIndex (EABaseReg reg1) (EAIndex reg2 1) (ImmInt 0))) dst@(OpReg reg3))
  | reg2 == reg3
  = pprSizeOpOp (sLit "add") size (OpReg reg1) dst

pprInstr (LEA size (OpAddr (AddrBaseIndex (EABaseReg reg1) EAIndexNone displ)) dst@(OpReg reg3))
  | reg1 == reg3
  = pprInstr (ADD size (OpImm displ) dst)

pprInstr (LEA size src dst) = pprSizeOpOp (sLit "lea") size src dst

pprInstr (ADD size (OpImm (ImmInt (-1))) dst)
  = pprSizeOp (sLit "dec") size dst
pprInstr (ADD size (OpImm (ImmInt 1)) dst)
  = pprSizeOp (sLit "inc") size dst
pprInstr (ADD size src dst)
  = pprSizeOpOp (sLit "add") size src dst
pprInstr (ADC size src dst)
  = pprSizeOpOp (sLit "adc") size src dst
pprInstr (SUB size src dst) = pprSizeOpOp (sLit "sub") size src dst
pprInstr (IMUL size op1 op2) = pprSizeOpOp (sLit "imul") size op1 op2

{- A hack.  The Intel documentation says that "The two and three
   operand forms [of IMUL] may also be used with unsigned operands
   because the lower half of the product is the same regardless if
   (sic) the operands are signed or unsigned.  The CF and OF flags,
   however, cannot be used to determine if the upper half of the
   result is non-zero."  So there.
-}
pprInstr (AND size src dst) = pprSizeOpOp (sLit "and") size src dst
pprInstr (OR  size src dst) = pprSizeOpOp (sLit "or")  size src dst

pprInstr (XOR FF32 src dst) = pprOpOp (sLit "xorps") FF32 src dst
pprInstr (XOR FF64 src dst) = pprOpOp (sLit "xorpd") FF64 src dst
pprInstr (XOR size src dst) = pprSizeOpOp (sLit "xor")  size src dst

pprInstr (POPCNT size src dst) = pprOpOp (sLit "popcnt") size src (OpReg dst)

pprInstr (PREFETCH NTA size src ) =  pprSizeOp_ (sLit "prefetchnta") size src
pprInstr (PREFETCH Lvl0 size src) = pprSizeOp_ (sLit "prefetcht0") size src
pprInstr (PREFETCH Lvl1 size src) = pprSizeOp_ (sLit "prefetcht1") size src
pprInstr (PREFETCH Lvl2 size src) = pprSizeOp_ (sLit "prefetcht2") size src

pprInstr (NOT size op) = pprSizeOp (sLit "not") size op
pprInstr (BSWAP size op) = pprSizeOp (sLit "bswap") size (OpReg op)
pprInstr (NEGI size op) = pprSizeOp (sLit "neg") size op

pprInstr (SHL size src dst) = pprShift (sLit "shl") size src dst
pprInstr (SAR size src dst) = pprShift (sLit "sar") size src dst
pprInstr (SHR size src dst) = pprShift (sLit "shr") size src dst

pprInstr (BT  size imm src) = pprSizeImmOp (sLit "bt") size imm src

pprInstr (CMP size src dst)
  | is_float size =  pprSizeOpOp (sLit "ucomi") size src dst -- SSE2
  | otherwise     =  pprSizeOpOp (sLit "cmp")   size src dst
  where
        -- This predicate is needed here and nowhere else
    is_float FF32       = True
    is_float FF64       = True
    is_float FF80       = True
    is_float _          = False

pprInstr (TEST size src dst) = pprSizeOpOp (sLit "test")  size src dst
pprInstr (PUSH size op) = pprSizeOp (sLit "push") size op
pprInstr (POP size op) = pprSizeOp (sLit "pop") size op

-- both unused (SDM):
-- pprInstr PUSHA = ptext (sLit "\tpushal")
-- pprInstr POPA = ptext (sLit "\tpopal")

pprInstr NOP = ptext (sLit "\tnop")
pprInstr (CLTD II32) = ptext (sLit "\tcltd")
pprInstr (CLTD II64) = ptext (sLit "\tcqto")

pprInstr (SETCC cond op) = pprCondInstr (sLit "set") cond (pprOperand II8 op)

pprInstr (JXX cond blockid)
  = pprCondInstr (sLit "j") cond (ppr lab)
  where lab = mkAsmTempLabel (getUnique blockid)

pprInstr        (JXX_GBL cond imm) = pprCondInstr (sLit "j") cond (pprImm imm)

pprInstr        (JMP (OpImm imm) _) = ptext (sLit "\tjmp ") <> pprImm imm
pprInstr (JMP op _)          = sdocWithPlatform $ \platform ->
                               ptext (sLit "\tjmp *") <> pprOperand (archWordSize (target32Bit platform)) op
pprInstr (JMP_TBL op _ _ _)  = pprInstr (JMP op [])
pprInstr        (CALL (Left imm) _)    = ptext (sLit "\tcall ") <> pprImm imm
pprInstr (CALL (Right reg) _)   = sdocWithPlatform $ \platform ->
                                  ptext (sLit "\tcall *") <> pprReg (archWordSize (target32Bit platform)) reg

pprInstr (IDIV sz op)   = pprSizeOp (sLit "idiv") sz op
pprInstr (DIV sz op)    = pprSizeOp (sLit "div")  sz op
pprInstr (IMUL2 sz op)  = pprSizeOp (sLit "imul") sz op

-- x86_64 only
pprInstr (MUL size op1 op2) = pprSizeOpOp (sLit "mul") size op1 op2
pprInstr (MUL2 size op) = pprSizeOp (sLit "mul") size op

pprInstr (FDIV size op1 op2) = pprSizeOpOp (sLit "div") size op1 op2

pprInstr (CVTSS2SD from to)      = pprRegReg (sLit "cvtss2sd") from to
pprInstr (CVTSD2SS from to)      = pprRegReg (sLit "cvtsd2ss") from to
pprInstr (CVTTSS2SIQ sz from to) = pprSizeSizeOpReg (sLit "cvttss2si") FF32 sz from to
pprInstr (CVTTSD2SIQ sz from to) = pprSizeSizeOpReg (sLit "cvttsd2si") FF64 sz from to
pprInstr (CVTSI2SS sz from to)   = pprSizeOpReg (sLit "cvtsi2ss") sz from to
pprInstr (CVTSI2SD sz from to)   = pprSizeOpReg (sLit "cvtsi2sd") sz from to

    -- FETCHGOT for PIC on ELF platforms
pprInstr (FETCHGOT reg)
   = vcat [ ptext (sLit "\tcall 1f"),
            hcat [ ptext (sLit "1:\tpopl\t"), pprReg II32 reg ],
            hcat [ ptext (sLit "\taddl\t$_GLOBAL_OFFSET_TABLE_+(.-1b), "),
                   pprReg II32 reg ]
          ]

    -- FETCHPC for PIC on Darwin/x86
    -- get the instruction pointer into a register
    -- (Terminology note: the IP is called Program Counter on PPC,
    --  and it's a good thing to use the same name on both platforms)
pprInstr (FETCHPC reg)
   = vcat [ ptext (sLit "\tcall 1f"),
            hcat [ ptext (sLit "1:\tpopl\t"), pprReg II32 reg ]
          ]


-- -----------------------------------------------------------------------------
-- i386 floating-point

-- Simulating a flat register set on the x86 FP stack is tricky.
-- you have to free %st(7) before pushing anything on the FP reg stack
-- so as to preclude the possibility of a FP stack overflow exception.
pprInstr g@(GMOV src dst)
   | src == dst
   = empty
   | otherwise
   = pprG g (hcat [gtab, gpush src 0, gsemi, gpop dst 1])

-- GLD sz addr dst ==> FLDsz addr ; FSTP (dst+1)
pprInstr g@(GLD sz addr dst)
 = pprG g (hcat [gtab, text "fld", pprSize_x87 sz, gsp,
                 pprAddr addr, gsemi, gpop dst 1])

-- GST sz src addr ==> FLD dst ; FSTPsz addr
pprInstr g@(GST sz src addr)
 | src == fake0 && sz /= FF80 -- fstt instruction doesn't exist
 = pprG g (hcat [gtab,
                 text "fst", pprSize_x87 sz, gsp, pprAddr addr])
 | otherwise
 = pprG g (hcat [gtab, gpush src 0, gsemi,
                 text "fstp", pprSize_x87 sz, gsp, pprAddr addr])

pprInstr g@(GLDZ dst)
 = pprG g (hcat [gtab, text "fldz ; ", gpop dst 1])
pprInstr g@(GLD1 dst)
 = pprG g (hcat [gtab, text "fld1 ; ", gpop dst 1])

pprInstr (GFTOI src dst)
   = pprInstr (GDTOI src dst)

pprInstr g@(GDTOI src dst)
   = pprG g (vcat [
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
     reg = pprReg II32 dst

pprInstr (GITOF src dst)
   = pprInstr (GITOD src dst)

pprInstr g@(GITOD src dst)
   = pprG g (hcat [gtab, text "pushl ", pprReg II32 src,
                   text " ; fildl (%esp) ; ",
                   gpop dst 1, text " ; addl $4,%esp"])

pprInstr g@(GDTOF src dst)
  = pprG g (vcat [gtab <> gpush src 0,
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
pprInstr g@(GCMP cond src1 src2)
   | case cond of { NE -> True; _ -> False }
   = pprG g (vcat [
        hcat [gtab, text "pushl %eax ; ",gpush src1 0],
        hcat [gtab, text "fcomp ", greg src2 1,
                    text "; fstsw %ax ; sahf ;  setpe %ah"],
        hcat [gtab, text "setne %al ;  ",
              text "orb %ah,%al ;  decb %al ;  popl %eax"]
    ])
   | otherwise
   = pprG g (vcat [
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


pprInstr g@(GABS _ src dst)
   = pprG g (hcat [gtab, gpush src 0, text " ; fabs ; ", gpop dst 1])

pprInstr g@(GNEG _ src dst)
   = pprG g (hcat [gtab, gpush src 0, text " ; fchs ; ", gpop dst 1])

pprInstr g@(GSQRT sz src dst)
   = pprG g (hcat [gtab, gpush src 0, text " ; fsqrt"] $$
             hcat [gtab, gcoerceto sz, gpop dst 1])

pprInstr g@(GSIN sz l1 l2 src dst)
   = pprG g (pprTrigOp "fsin" False l1 l2 src dst sz)

pprInstr g@(GCOS sz l1 l2 src dst)
   = pprG g (pprTrigOp "fcos" False l1 l2 src dst sz)

pprInstr g@(GTAN sz l1 l2 src dst)
   = pprG g (pprTrigOp "fptan" True l1 l2 src dst sz)

-- In the translations for GADD, GMUL, GSUB and GDIV,
-- the first two cases are mere optimisations.  The otherwise clause
-- generates correct code under all circumstances.

pprInstr g@(GADD _ src1 src2 dst)
   | src1 == dst
   = pprG g (text "\t#GADD-xxxcase1" $$
             hcat [gtab, gpush src2 0,
                   text " ; faddp %st(0),", greg src1 1])
   | src2 == dst
   = pprG g (text "\t#GADD-xxxcase2" $$
             hcat [gtab, gpush src1 0,
                   text " ; faddp %st(0),", greg src2 1])
   | otherwise
   = pprG g (hcat [gtab, gpush src1 0,
                   text " ; fadd ", greg src2 1, text ",%st(0)",
                   gsemi, gpop dst 1])


pprInstr g@(GMUL _ src1 src2 dst)
   | src1 == dst
   = pprG g (text "\t#GMUL-xxxcase1" $$
             hcat [gtab, gpush src2 0,
                   text " ; fmulp %st(0),", greg src1 1])
   | src2 == dst
   = pprG g (text "\t#GMUL-xxxcase2" $$
             hcat [gtab, gpush src1 0,
                   text " ; fmulp %st(0),", greg src2 1])
   | otherwise
   = pprG g (hcat [gtab, gpush src1 0,
             text " ; fmul ", greg src2 1, text ",%st(0)",
             gsemi, gpop dst 1])


pprInstr g@(GSUB _ src1 src2 dst)
   | src1 == dst
   = pprG g (text "\t#GSUB-xxxcase1" $$
             hcat [gtab, gpush src2 0,
                   text " ; fsubrp %st(0),", greg src1 1])
   | src2 == dst
   = pprG g (text "\t#GSUB-xxxcase2" $$
             hcat [gtab, gpush src1 0,
                   text " ; fsubp %st(0),", greg src2 1])
   | otherwise
   = pprG g (hcat [gtab, gpush src1 0,
                   text " ; fsub ", greg src2 1, text ",%st(0)",
                   gsemi, gpop dst 1])


pprInstr g@(GDIV _ src1 src2 dst)
   | src1 == dst
   = pprG g (text "\t#GDIV-xxxcase1" $$
             hcat [gtab, gpush src2 0,
                   text " ; fdivrp %st(0),", greg src1 1])
   | src2 == dst
   = pprG g (text "\t#GDIV-xxxcase2" $$
             hcat [gtab, gpush src1 0,
                   text " ; fdivp %st(0),", greg src2 1])
   | otherwise
   = pprG g (hcat [gtab, gpush src1 0,
                   text " ; fdiv ", greg src2 1, text ",%st(0)",
                   gsemi, gpop dst 1])


pprInstr GFREE
   = vcat [ ptext (sLit "\tffree %st(0) ;ffree %st(1) ;ffree %st(2) ;ffree %st(3)"),
            ptext (sLit "\tffree %st(4) ;ffree %st(5)")
          ]

pprInstr _
        = panic "X86.Ppr.pprInstr: no match"


pprTrigOp :: String -> Bool -> CLabel -> CLabel
          -> Reg -> Reg -> Size -> SDoc
pprTrigOp op -- fsin, fcos or fptan
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
      hcat [gtab, text "je     " <> ppr l1] $$
      -- Otherwise we need to shrink the value. Start by
      -- loading pi, doubleing it (by adding it to itself),
      -- and then swapping pi with the value, so the value we
      -- want to apply op to is in %st(0) again
      hcat [gtab, text "ffree %st(7); fldpi"] $$
      hcat [gtab, text "fadd   %st(0),%st"] $$
      hcat [gtab, text "fxch   %st(1)"] $$
      -- Now we have a loop in which we make the value smaller,
      -- see if it's small enough, and loop if not
      (ppr l2 <> char ':') $$
      hcat [gtab, text "fprem1"] $$
      -- My Debian libc uses fstsw here for the tan code, but I can't
      -- see any reason why it should need to be different for tan.
      hcat [gtab, text "fnstsw %ax"] $$
      hcat [gtab, text "test   $0x400,%eax"] $$
      hcat [gtab, text "jne    " <> ppr l2] $$
      hcat [gtab, text "fstp   %st(1)"] $$
      hcat [gtab, text op] $$
      (ppr l1 <> char ':') $$
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

pprG :: Instr -> SDoc -> SDoc
pprG fake actual
   = (char '#' <> pprGInstr fake) $$ actual


pprGInstr :: Instr -> SDoc
pprGInstr (GMOV src dst)   = pprSizeRegReg (sLit "gmov") FF64 src dst
pprGInstr (GLD sz src dst) = pprSizeAddrReg (sLit "gld") sz src dst
pprGInstr (GST sz src dst) = pprSizeRegAddr (sLit "gst") sz src dst

pprGInstr (GLDZ dst) = pprSizeReg (sLit "gldz") FF64 dst
pprGInstr (GLD1 dst) = pprSizeReg (sLit "gld1") FF64 dst

pprGInstr (GFTOI src dst) = pprSizeSizeRegReg (sLit "gftoi") FF32 II32  src dst
pprGInstr (GDTOI src dst) = pprSizeSizeRegReg (sLit "gdtoi") FF64 II32 src dst

pprGInstr (GITOF src dst) = pprSizeSizeRegReg (sLit "gitof") II32 FF32  src dst
pprGInstr (GITOD src dst) = pprSizeSizeRegReg (sLit "gitod") II32 FF64 src dst
pprGInstr (GDTOF src dst) = pprSizeSizeRegReg (sLit "gdtof") FF64 FF32 src dst

pprGInstr (GCMP co src dst) = pprCondRegReg (sLit "gcmp_") FF64 co src dst
pprGInstr (GABS sz src dst) = pprSizeRegReg (sLit "gabs") sz src dst
pprGInstr (GNEG sz src dst) = pprSizeRegReg (sLit "gneg") sz src dst
pprGInstr (GSQRT sz src dst) = pprSizeRegReg (sLit "gsqrt") sz src dst
pprGInstr (GSIN sz _ _ src dst) = pprSizeRegReg (sLit "gsin") sz src dst
pprGInstr (GCOS sz _ _ src dst) = pprSizeRegReg (sLit "gcos") sz src dst
pprGInstr (GTAN sz _ _ src dst) = pprSizeRegReg (sLit "gtan") sz src dst

pprGInstr (GADD sz src1 src2 dst) = pprSizeRegRegReg (sLit "gadd") sz src1 src2 dst
pprGInstr (GSUB sz src1 src2 dst) = pprSizeRegRegReg (sLit "gsub") sz src1 src2 dst
pprGInstr (GMUL sz src1 src2 dst) = pprSizeRegRegReg (sLit "gmul") sz src1 src2 dst
pprGInstr (GDIV sz src1 src2 dst) = pprSizeRegRegReg (sLit "gdiv") sz src1 src2 dst

pprGInstr _ = panic "X86.Ppr.pprGInstr: no match"

pprDollImm :: Imm -> SDoc
pprDollImm i = ptext (sLit "$") <> pprImm i


pprOperand :: Size -> Operand -> SDoc
pprOperand s (OpReg r)   = pprReg s r
pprOperand _ (OpImm i)   = pprDollImm i
pprOperand _ (OpAddr ea) = pprAddr ea


pprMnemonic_  :: LitString -> SDoc
pprMnemonic_ name =
   char '\t' <> ptext name <> space


pprMnemonic  :: LitString -> Size -> SDoc
pprMnemonic name size =
   char '\t' <> ptext name <> pprSize size <> space


pprSizeImmOp :: LitString -> Size -> Imm -> Operand -> SDoc
pprSizeImmOp name size imm op1
  = hcat [
        pprMnemonic name size,
        char '$',
        pprImm imm,
        comma,
        pprOperand size op1
    ]


pprSizeOp_ :: LitString -> Size -> Operand -> SDoc
pprSizeOp_ name size op1
  = hcat [
        pprMnemonic_ name ,
        pprOperand size op1
    ]

pprSizeOp :: LitString -> Size -> Operand -> SDoc
pprSizeOp name size op1
  = hcat [
        pprMnemonic name size,
        pprOperand size op1
    ]


pprSizeOpOp :: LitString -> Size -> Operand -> Operand -> SDoc
pprSizeOpOp name size op1 op2
  = hcat [
        pprMnemonic name size,
        pprOperand size op1,
        comma,
        pprOperand size op2
    ]


pprOpOp :: LitString -> Size -> Operand -> Operand -> SDoc
pprOpOp name size op1 op2
  = hcat [
        pprMnemonic_ name,
        pprOperand size op1,
        comma,
        pprOperand size op2
    ]


pprSizeReg :: LitString -> Size -> Reg -> SDoc
pprSizeReg name size reg1
  = hcat [
        pprMnemonic name size,
        pprReg size reg1
    ]


pprSizeRegReg :: LitString -> Size -> Reg -> Reg -> SDoc
pprSizeRegReg name size reg1 reg2
  = hcat [
        pprMnemonic name size,
        pprReg size reg1,
        comma,
        pprReg size reg2
    ]


pprRegReg :: LitString -> Reg -> Reg -> SDoc
pprRegReg name reg1 reg2
  = sdocWithPlatform $ \platform ->
    hcat [
        pprMnemonic_ name,
        pprReg (archWordSize (target32Bit platform)) reg1,
        comma,
        pprReg (archWordSize (target32Bit platform)) reg2
    ]


pprSizeOpReg :: LitString -> Size -> Operand -> Reg -> SDoc
pprSizeOpReg name size op1 reg2
  = sdocWithPlatform $ \platform ->
    hcat [
        pprMnemonic name size,
        pprOperand size op1,
        comma,
        pprReg (archWordSize (target32Bit platform)) reg2
    ]

pprCondRegReg :: LitString -> Size -> Cond -> Reg -> Reg -> SDoc
pprCondRegReg name size cond reg1 reg2
  = hcat [
        char '\t',
        ptext name,
        pprCond cond,
        space,
        pprReg size reg1,
        comma,
        pprReg size reg2
    ]

pprSizeSizeRegReg :: LitString -> Size -> Size -> Reg -> Reg -> SDoc
pprSizeSizeRegReg name size1 size2 reg1 reg2
  = hcat [
        char '\t',
        ptext name,
        pprSize size1,
        pprSize size2,
        space,
        pprReg size1 reg1,
        comma,
        pprReg size2 reg2
    ]

pprSizeSizeOpReg :: LitString -> Size -> Size -> Operand -> Reg -> SDoc
pprSizeSizeOpReg name size1 size2 op1 reg2
  = hcat [
        pprMnemonic name size2,
        pprOperand size1 op1,
        comma,
        pprReg size2 reg2
    ]

pprSizeRegRegReg :: LitString -> Size -> Reg -> Reg -> Reg -> SDoc
pprSizeRegRegReg name size reg1 reg2 reg3
  = hcat [
        pprMnemonic name size,
        pprReg size reg1,
        comma,
        pprReg size reg2,
        comma,
        pprReg size reg3
    ]


pprSizeAddrReg :: LitString -> Size -> AddrMode -> Reg -> SDoc
pprSizeAddrReg name size op dst
  = hcat [
        pprMnemonic name size,
        pprAddr op,
        comma,
        pprReg size dst
    ]


pprSizeRegAddr :: LitString -> Size -> Reg -> AddrMode -> SDoc
pprSizeRegAddr name size src op
  = hcat [
        pprMnemonic name size,
        pprReg size src,
        comma,
        pprAddr op
    ]


pprShift :: LitString -> Size -> Operand -> Operand -> SDoc
pprShift name size src dest
  = hcat [
        pprMnemonic name size,
        pprOperand II8 src,  -- src is 8-bit sized
        comma,
        pprOperand size dest
    ]


pprSizeOpOpCoerce :: LitString -> Size -> Size -> Operand -> Operand -> SDoc
pprSizeOpOpCoerce name size1 size2 op1 op2
  = hcat [ char '\t', ptext name, pprSize size1, pprSize size2, space,
        pprOperand size1 op1,
        comma,
        pprOperand size2 op2
    ]


pprCondInstr :: LitString -> Cond -> SDoc -> SDoc
pprCondInstr name cond arg
  = hcat [ char '\t', ptext name, pprCond cond, space, arg]

