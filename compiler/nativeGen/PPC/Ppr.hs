-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

module PPC.Ppr (
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

import PPC.Regs
import PPC.Instr
import PPC.Cond
import PprBase
import Instruction
import Size
import Reg
import RegClass
import TargetReg

import OldCmm

import CLabel

import Unique                ( pprUnique, Uniquable(..) )
import Platform
import FastString
import Outputable

import Data.Word
import Data.Bits


-- -----------------------------------------------------------------------------
-- Printing this stuff out

pprNatCmmDecl :: Platform -> NatCmmDecl CmmStatics Instr -> SDoc
pprNatCmmDecl platform (CmmData section dats) =
  pprSectionHeader platform section $$ pprDatas platform dats

 -- special case for split markers:
pprNatCmmDecl platform (CmmProc Nothing lbl (ListGraph []))
    = pprLabel platform lbl

 -- special case for code without an info table:
pprNatCmmDecl platform (CmmProc Nothing lbl (ListGraph blocks)) =
  pprSectionHeader platform Text $$
  pprLabel platform lbl $$ -- blocks guaranteed not null, so label needed
  vcat (map (pprBasicBlock platform) blocks)

pprNatCmmDecl platform (CmmProc (Just (Statics info_lbl info)) _entry_lbl (ListGraph blocks)) =
  pprSectionHeader platform Text $$
  (
       (if platformHasSubsectionsViaSymbols platform
        then ppr (mkDeadStripPreventer info_lbl) <> char ':'
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
              <+> ppr info_lbl
              <+> char '-'
              <+> ppr (mkDeadStripPreventer info_lbl)
         else empty)


pprBasicBlock :: Platform -> NatBasicBlock Instr -> SDoc
pprBasicBlock platform (BasicBlock blockid instrs) =
  pprLabel platform (mkAsmTempLabel (getUnique blockid)) $$
  vcat (map (pprInstr platform) instrs)



pprDatas :: Platform -> CmmStatics -> SDoc
pprDatas platform (Statics lbl dats) = vcat (pprLabel platform lbl : map (pprData platform) dats)

pprData :: Platform -> CmmStatic -> SDoc
pprData _ (CmmString str)          = pprASCII str
pprData platform (CmmUninitialised bytes) = ptext (sLit keyword) <> int bytes
    where keyword = case platformOS platform of
                    OSDarwin -> ".space "
                    _        -> ".skip "
pprData platform (CmmStaticLit lit)       = pprDataItem platform lit

pprGloblDecl :: CLabel -> SDoc
pprGloblDecl lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = ptext (sLit ".globl ") <> ppr lbl

pprTypeAndSizeDecl :: Platform -> CLabel -> SDoc
pprTypeAndSizeDecl platform lbl
  | platformOS platform == OSLinux && externallyVisibleCLabel lbl
    = ptext (sLit ".type ") <>
      ppr lbl <> ptext (sLit ", @object")
pprTypeAndSizeDecl _ _
  = empty

pprLabel :: Platform -> CLabel -> SDoc
pprLabel platform lbl = pprGloblDecl lbl
                     $$ pprTypeAndSizeDecl platform lbl
                     $$ (ppr lbl <> char ':')


pprASCII :: [Word8] -> SDoc
pprASCII str
  = vcat (map do1 str) $$ do1 0
    where
       do1 :: Word8 -> SDoc
       do1 w = ptext (sLit "\t.byte\t") <> int (fromIntegral w)


-- -----------------------------------------------------------------------------
-- pprInstr: print an 'Instr'

instance Outputable Instr where
    ppr instr = sdocWithPlatform $ \platform -> pprInstr platform instr


pprReg :: Platform -> Reg -> SDoc

pprReg platform r
  = case r of
      RegReal    (RealRegSingle i) -> ppr_reg_no i
      RegReal    (RealRegPair{})   -> panic "PPC.pprReg: no reg pairs on this arch"
      RegVirtual (VirtualRegI  u)  -> text "%vI_" <> pprUnique u
      RegVirtual (VirtualRegHi u)  -> text "%vHi_" <> pprUnique u
      RegVirtual (VirtualRegF  u)  -> text "%vF_" <> pprUnique u
      RegVirtual (VirtualRegD  u)  -> text "%vD_" <> pprUnique u
      RegVirtual (VirtualRegSSE  u) -> text "%vSSE_" <> pprUnique u
  where
    ppr_reg_no :: Int -> SDoc
    ppr_reg_no i =
        case platformOS platform of
        OSDarwin ->
            ptext
                (case i of {
                 0 -> sLit "r0";   1 -> sLit "r1";
                 2 -> sLit "r2";   3 -> sLit "r3";
                 4 -> sLit "r4";   5 -> sLit "r5";
                 6 -> sLit "r6";   7 -> sLit "r7";
                 8 -> sLit "r8";   9 -> sLit "r9";
                10 -> sLit "r10";  11 -> sLit "r11";
                12 -> sLit "r12";  13 -> sLit "r13";
                14 -> sLit "r14";  15 -> sLit "r15";
                16 -> sLit "r16";  17 -> sLit "r17";
                18 -> sLit "r18";  19 -> sLit "r19";
                20 -> sLit "r20";  21 -> sLit "r21";
                22 -> sLit "r22";  23 -> sLit "r23";
                24 -> sLit "r24";  25 -> sLit "r25";
                26 -> sLit "r26";  27 -> sLit "r27";
                28 -> sLit "r28";  29 -> sLit "r29";
                30 -> sLit "r30";  31 -> sLit "r31";
                32 -> sLit "f0";  33 -> sLit "f1";
                34 -> sLit "f2";  35 -> sLit "f3";
                36 -> sLit "f4";  37 -> sLit "f5";
                38 -> sLit "f6";  39 -> sLit "f7";
                40 -> sLit "f8";  41 -> sLit "f9";
                42 -> sLit "f10"; 43 -> sLit "f11";
                44 -> sLit "f12"; 45 -> sLit "f13";
                46 -> sLit "f14"; 47 -> sLit "f15";
                48 -> sLit "f16"; 49 -> sLit "f17";
                50 -> sLit "f18"; 51 -> sLit "f19";
                52 -> sLit "f20"; 53 -> sLit "f21";
                54 -> sLit "f22"; 55 -> sLit "f23";
                56 -> sLit "f24"; 57 -> sLit "f25";
                58 -> sLit "f26"; 59 -> sLit "f27";
                60 -> sLit "f28"; 61 -> sLit "f29";
                62 -> sLit "f30"; 63 -> sLit "f31";
                _  -> sLit "very naughty powerpc register"
              })
        _
         | i <= 31   -> int i      -- GPRs
         | i <= 63   -> int (i-32) -- FPRs
         | otherwise -> ptext (sLit "very naughty powerpc register")



pprSize :: Size -> SDoc
pprSize x 
 = ptext (case x of
                II8        -> sLit "b"
                II16        -> sLit "h"
                II32        -> sLit "w"
                FF32        -> sLit "fs"
                FF64        -> sLit "fd"
                _        -> panic "PPC.Ppr.pprSize: no match")
                
                
pprCond :: Cond -> SDoc
pprCond c 
 = ptext (case c of {
                ALWAYS  -> sLit "";
                EQQ        -> sLit "eq";        NE    -> sLit "ne";
                LTT     -> sLit "lt";  GE    -> sLit "ge";
                GTT     -> sLit "gt";  LE    -> sLit "le";
                LU      -> sLit "lt";  GEU   -> sLit "ge";
                GU      -> sLit "gt";  LEU   -> sLit "le"; })


pprImm :: Platform -> Imm -> SDoc

pprImm _        (ImmInt i)     = int i
pprImm _        (ImmInteger i) = integer i
pprImm _        (ImmCLbl l)    = ppr l
pprImm _        (ImmIndex l i) = ppr l <> char '+' <> int i
pprImm _        (ImmLit s)     = s

pprImm _        (ImmFloat _)  = ptext (sLit "naughty float immediate")
pprImm _        (ImmDouble _) = ptext (sLit "naughty double immediate")

pprImm platform (ImmConstantSum a b) = pprImm platform a <> char '+' <> pprImm platform b
pprImm platform (ImmConstantDiff a b) = pprImm platform a <> char '-'
                            <> lparen <> pprImm platform b <> rparen

pprImm platform (LO i)
  = if platformOS platform == OSDarwin
    then hcat [ text "lo16(", pprImm platform i, rparen ]
    else pprImm platform i <> text "@l"

pprImm platform (HI i)
  = if platformOS platform == OSDarwin
    then hcat [ text "hi16(", pprImm platform i, rparen ]
    else pprImm platform i <> text "@h"

pprImm platform (HA i)
  = if platformOS platform == OSDarwin
    then hcat [ text "ha16(", pprImm platform i, rparen ]
    else pprImm platform i <> text "@ha"


pprAddr :: Platform -> AddrMode -> SDoc
pprAddr platform (AddrRegReg r1 r2)
  = pprReg platform r1 <+> ptext (sLit ", ") <+> pprReg platform r2

pprAddr platform (AddrRegImm r1 (ImmInt i)) = hcat [ int i, char '(', pprReg platform r1, char ')' ]
pprAddr platform (AddrRegImm r1 (ImmInteger i)) = hcat [ integer i, char '(', pprReg platform r1, char ')' ]
pprAddr platform (AddrRegImm r1 imm) = hcat [ pprImm platform imm, char '(', pprReg platform r1, char ')' ]


pprSectionHeader :: Platform -> Section -> SDoc
pprSectionHeader platform seg
 = case seg of
        Text                    -> ptext (sLit ".text\n.align 2")
        Data                    -> ptext (sLit ".data\n.align 2")
        ReadOnlyData
         | osDarwin             -> ptext (sLit ".const\n.align 2")
         | otherwise            -> ptext (sLit ".section .rodata\n\t.align 2")
        RelocatableReadOnlyData
         | osDarwin             -> ptext (sLit ".const_data\n.align 2")
         | otherwise            -> ptext (sLit ".data\n\t.align 2")
        UninitialisedData
         | osDarwin             -> ptext (sLit ".const_data\n.align 2")
         | otherwise            -> ptext (sLit ".section .bss\n\t.align 2")
        ReadOnlyData16
         | osDarwin             -> ptext (sLit ".const\n.align 4")
         | otherwise            -> ptext (sLit ".section .rodata\n\t.align 4")
        OtherSection _          ->
            panic "PprMach.pprSectionHeader: unknown section"
    where osDarwin = platformOS platform == OSDarwin


pprDataItem :: Platform -> CmmLit -> SDoc
pprDataItem platform lit
  = vcat (ppr_item (cmmTypeSize $ cmmLitType lit) lit)
    where
        imm = litToImm lit

        ppr_item II8   _ = [ptext (sLit "\t.byte\t") <> pprImm platform imm]

        ppr_item II32  _ = [ptext (sLit "\t.long\t") <> pprImm platform imm]

        ppr_item FF32 (CmmFloat r _)
           = let bs = floatToBytes (fromRational r)
             in  map (\b -> ptext (sLit "\t.byte\t") <> pprImm platform (ImmInt b)) bs

        ppr_item FF64 (CmmFloat r _)
           = let bs = doubleToBytes (fromRational r)
             in  map (\b -> ptext (sLit "\t.byte\t") <> pprImm platform (ImmInt b)) bs

        ppr_item II16 _        = [ptext (sLit "\t.short\t") <> pprImm platform imm]

        ppr_item II64 (CmmInt x _)  =
                [ptext (sLit "\t.long\t")
                    <> int (fromIntegral 
                        (fromIntegral (x `shiftR` 32) :: Word32)),
                 ptext (sLit "\t.long\t")
                    <> int (fromIntegral (fromIntegral x :: Word32))]

        ppr_item _ _
                = panic "PPC.Ppr.pprDataItem: no match"


pprInstr :: Platform -> Instr -> SDoc

pprInstr _ (COMMENT _) = empty -- nuke 'em
{-
pprInstr platform (COMMENT s) =
     if platformOS platform == OSLinux
     then ptext (sLit "# ") <> ftext s
     else ptext (sLit "; ") <> ftext s
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
        char '\t',
        pprReg platform reg,
        comma,
        ptext (sLit "SLOT") <> parens (int slot)]

pprInstr _ (RELOAD slot reg)
   = hcat [
           ptext (sLit "\tRELOAD"),
        char '\t',
        ptext (sLit "SLOT") <> parens (int slot),
        comma,
        pprReg platform reg]
-}

pprInstr platform (LD sz reg addr) = hcat [
        char '\t',
        ptext (sLit "l"),
        ptext (case sz of
            II8  -> sLit "bz"
            II16 -> sLit "hz"
            II32 -> sLit "wz"
            FF32 -> sLit "fs"
            FF64 -> sLit "fd"
            _         -> panic "PPC.Ppr.pprInstr: no match"
            ),
        case addr of AddrRegImm _ _ -> empty
                     AddrRegReg _ _ -> char 'x',
        char '\t',
        pprReg platform reg,
        ptext (sLit ", "),
        pprAddr platform addr
    ]
pprInstr platform (LA sz reg addr) = hcat [
        char '\t',
        ptext (sLit "l"),
        ptext (case sz of
            II8  -> sLit "ba"
            II16 -> sLit "ha"
            II32 -> sLit "wa"
            FF32 -> sLit "fs"
            FF64 -> sLit "fd"
            _         -> panic "PPC.Ppr.pprInstr: no match"
            ),
        case addr of AddrRegImm _ _ -> empty
                     AddrRegReg _ _ -> char 'x',
        char '\t',
        pprReg platform reg,
        ptext (sLit ", "),
        pprAddr platform addr
    ]
pprInstr platform (ST sz reg addr) = hcat [
        char '\t',
        ptext (sLit "st"),
        pprSize sz,
        case addr of AddrRegImm _ _ -> empty
                     AddrRegReg _ _ -> char 'x',
        char '\t',
        pprReg platform reg,
        ptext (sLit ", "),
        pprAddr platform addr
    ]
pprInstr platform (STU sz reg addr) = hcat [
        char '\t',
        ptext (sLit "st"),
        pprSize sz,
        ptext (sLit "u\t"),
        case addr of AddrRegImm _ _ -> empty
                     AddrRegReg _ _ -> char 'x',
        pprReg platform reg,
        ptext (sLit ", "),
        pprAddr platform addr
    ]
pprInstr platform (LIS reg imm) = hcat [
        char '\t',
        ptext (sLit "lis"),
        char '\t',
        pprReg platform reg,
        ptext (sLit ", "),
        pprImm platform imm
    ]
pprInstr platform (LI reg imm) = hcat [
        char '\t',
        ptext (sLit "li"),
        char '\t',
        pprReg platform reg,
        ptext (sLit ", "),
        pprImm platform imm
    ]
pprInstr platform (MR reg1 reg2) 
    | reg1 == reg2 = empty
    | otherwise = hcat [
        char '\t',
        case targetClassOfReg platform reg1 of
            RcInteger -> ptext (sLit "mr")
            _ -> ptext (sLit "fmr"),
        char '\t',
        pprReg platform reg1,
        ptext (sLit ", "),
        pprReg platform reg2
    ]
pprInstr platform (CMP sz reg ri) = hcat [
        char '\t',
        op,
        char '\t',
        pprReg platform reg,
        ptext (sLit ", "),
        pprRI platform ri
    ]
    where
        op = hcat [
                ptext (sLit "cmp"),
                pprSize sz,
                case ri of
                    RIReg _ -> empty
                    RIImm _ -> char 'i'
            ]
pprInstr platform (CMPL sz reg ri) = hcat [
        char '\t',
        op,
        char '\t',
        pprReg platform reg,
        ptext (sLit ", "),
        pprRI platform ri
    ]
    where
        op = hcat [
                ptext (sLit "cmpl"),
                pprSize sz,
                case ri of
                    RIReg _ -> empty
                    RIImm _ -> char 'i'
            ]
pprInstr _ (BCC cond blockid) = hcat [
        char '\t',
        ptext (sLit "b"),
        pprCond cond,
        char '\t',
        ppr lbl
    ]
    where lbl = mkAsmTempLabel (getUnique blockid)

pprInstr _ (BCCFAR cond blockid) = vcat [
        hcat [
            ptext (sLit "\tb"),
            pprCond (condNegate cond),
            ptext (sLit "\t$+8")
        ],
        hcat [
            ptext (sLit "\tb\t"),
            ppr lbl
        ]
    ]
    where lbl = mkAsmTempLabel (getUnique blockid)

pprInstr _ (JMP lbl) = hcat [ -- an alias for b that takes a CLabel
        char '\t',
        ptext (sLit "b"),
        char '\t',
        ppr lbl
    ]

pprInstr platform (MTCTR reg) = hcat [
        char '\t',
        ptext (sLit "mtctr"),
        char '\t',
        pprReg platform reg
    ]
pprInstr _ (BCTR _ _) = hcat [
        char '\t',
        ptext (sLit "bctr")
    ]
pprInstr _ (BL lbl _) = hcat [
        ptext (sLit "\tbl\t"),
        ppr lbl
    ]
pprInstr _ (BCTRL _) = hcat [
        char '\t',
        ptext (sLit "bctrl")
    ]
pprInstr platform (ADD reg1 reg2 ri) = pprLogic platform (sLit "add") reg1 reg2 ri
pprInstr platform (ADDIS reg1 reg2 imm) = hcat [
        char '\t',
        ptext (sLit "addis"),
        char '\t',
        pprReg platform reg1,
        ptext (sLit ", "),
        pprReg platform reg2,
        ptext (sLit ", "),
        pprImm platform imm
    ]

pprInstr platform (ADDC reg1 reg2 reg3) = pprLogic platform (sLit "addc") reg1 reg2 (RIReg reg3)
pprInstr platform (ADDE reg1 reg2 reg3) = pprLogic platform (sLit "adde") reg1 reg2 (RIReg reg3)
pprInstr platform (SUBF reg1 reg2 reg3) = pprLogic platform (sLit "subf") reg1 reg2 (RIReg reg3)
pprInstr platform (MULLW reg1 reg2 ri@(RIReg _)) = pprLogic platform (sLit "mullw") reg1 reg2 ri
pprInstr platform (MULLW reg1 reg2 ri@(RIImm _)) = pprLogic platform (sLit "mull") reg1 reg2 ri
pprInstr platform (DIVW reg1 reg2 reg3) = pprLogic platform (sLit "divw") reg1 reg2 (RIReg reg3)
pprInstr platform (DIVWU reg1 reg2 reg3) = pprLogic platform (sLit "divwu") reg1 reg2 (RIReg reg3)

pprInstr platform (MULLW_MayOflo reg1 reg2 reg3) = vcat [
         hcat [ ptext (sLit "\tmullwo\t"), pprReg platform reg1, ptext (sLit ", "),
                                          pprReg platform reg2, ptext (sLit ", "),
                                          pprReg platform reg3 ],
         hcat [ ptext (sLit "\tmfxer\t"),  pprReg platform reg1 ],
         hcat [ ptext (sLit "\trlwinm\t"), pprReg platform reg1, ptext (sLit ", "),
                                          pprReg platform reg1, ptext (sLit ", "),
                                          ptext (sLit "2, 31, 31") ]
    ]

            -- for some reason, "andi" doesn't exist.
        -- we'll use "andi." instead.
pprInstr platform (AND reg1 reg2 (RIImm imm)) = hcat [
        char '\t',
        ptext (sLit "andi."),
        char '\t',
        pprReg platform reg1,
        ptext (sLit ", "),
        pprReg platform reg2,
        ptext (sLit ", "),
        pprImm platform imm
    ]
pprInstr platform (AND reg1 reg2 ri) = pprLogic platform (sLit "and") reg1 reg2 ri

pprInstr platform (OR reg1 reg2 ri) = pprLogic platform (sLit "or") reg1 reg2 ri
pprInstr platform (XOR reg1 reg2 ri) = pprLogic platform (sLit "xor") reg1 reg2 ri

pprInstr platform (XORIS reg1 reg2 imm) = hcat [
        char '\t',
        ptext (sLit "xoris"),
        char '\t',
        pprReg platform reg1,
        ptext (sLit ", "),
        pprReg platform reg2,
        ptext (sLit ", "),
        pprImm platform imm
    ]

pprInstr platform (EXTS sz reg1 reg2) = hcat [
        char '\t',
        ptext (sLit "exts"),
        pprSize sz,
        char '\t',
        pprReg platform reg1,
        ptext (sLit ", "),
        pprReg platform reg2
    ]

pprInstr platform (NEG reg1 reg2) = pprUnary platform (sLit "neg") reg1 reg2
pprInstr platform (NOT reg1 reg2) = pprUnary platform (sLit "not") reg1 reg2

pprInstr platform (SLW reg1 reg2 ri) = pprLogic platform (sLit "slw") reg1 reg2 (limitShiftRI ri)

pprInstr platform (SRW reg1 reg2 (RIImm (ImmInt i))) | i > 31 || i < 0 =
    -- Handle the case where we are asked to shift a 32 bit register by
    -- less than zero or more than 31 bits. We convert this into a clear
    -- of the destination register.
    -- Fixes ticket http://hackage.haskell.org/trac/ghc/ticket/5900
    pprInstr platform (XOR reg1 reg2 (RIReg reg2))
pprInstr platform (SRW reg1 reg2 ri) = pprLogic platform (sLit "srw") reg1 reg2 (limitShiftRI ri)

pprInstr platform (SRAW reg1 reg2 ri) = pprLogic platform (sLit "sraw") reg1 reg2 (limitShiftRI ri)
pprInstr platform (RLWINM reg1 reg2 sh mb me) = hcat [
        ptext (sLit "\trlwinm\t"),
        pprReg platform reg1,
        ptext (sLit ", "),
        pprReg platform reg2,
        ptext (sLit ", "),
        int sh,
        ptext (sLit ", "),
        int mb,
        ptext (sLit ", "),
        int me
    ]
    
pprInstr platform (FADD sz reg1 reg2 reg3) = pprBinaryF platform (sLit "fadd") sz reg1 reg2 reg3
pprInstr platform (FSUB sz reg1 reg2 reg3) = pprBinaryF platform (sLit "fsub") sz reg1 reg2 reg3
pprInstr platform (FMUL sz reg1 reg2 reg3) = pprBinaryF platform (sLit "fmul") sz reg1 reg2 reg3
pprInstr platform (FDIV sz reg1 reg2 reg3) = pprBinaryF platform (sLit "fdiv") sz reg1 reg2 reg3
pprInstr platform (FNEG reg1 reg2) = pprUnary platform (sLit "fneg") reg1 reg2

pprInstr platform (FCMP reg1 reg2) = hcat [
        char '\t',
        ptext (sLit "fcmpu\tcr0, "),
            -- Note: we're using fcmpu, not fcmpo
            -- The difference is with fcmpo, compare with NaN is an invalid operation.
            -- We don't handle invalid fp ops, so we don't care
        pprReg platform reg1,
        ptext (sLit ", "),
        pprReg platform reg2
    ]

pprInstr platform (FCTIWZ reg1 reg2) = pprUnary platform (sLit "fctiwz") reg1 reg2
pprInstr platform (FRSP reg1 reg2) = pprUnary platform (sLit "frsp") reg1 reg2

pprInstr _ (CRNOR dst src1 src2) = hcat [
        ptext (sLit "\tcrnor\t"),
        int dst,
        ptext (sLit ", "),
        int src1,
        ptext (sLit ", "),
        int src2
    ]

pprInstr platform (MFCR reg) = hcat [
        char '\t',
        ptext (sLit "mfcr"),
        char '\t',
        pprReg platform reg
    ]

pprInstr platform (MFLR reg) = hcat [
        char '\t',
        ptext (sLit "mflr"),
        char '\t',
        pprReg platform reg
    ]

pprInstr platform (FETCHPC reg) = vcat [
        ptext (sLit "\tbcl\t20,31,1f"),
        hcat [ ptext (sLit "1:\tmflr\t"), pprReg platform reg ]
    ]

pprInstr _ LWSYNC = ptext (sLit "\tlwsync")

-- pprInstr _ _ = panic "pprInstr (ppc)"


pprLogic :: Platform -> LitString -> Reg -> Reg -> RI -> SDoc
pprLogic platform op reg1 reg2 ri = hcat [
        char '\t',
        ptext op,
        case ri of
            RIReg _ -> empty
            RIImm _ -> char 'i',
        char '\t',
        pprReg platform reg1,
        ptext (sLit ", "),
        pprReg platform reg2,
        ptext (sLit ", "),
        pprRI platform ri
    ]


pprUnary :: Platform -> LitString -> Reg -> Reg -> SDoc
pprUnary platform op reg1 reg2 = hcat [
        char '\t',
        ptext op,
        char '\t',
        pprReg platform reg1,
        ptext (sLit ", "),
        pprReg platform reg2
    ]
    
    
pprBinaryF :: Platform -> LitString -> Size -> Reg -> Reg -> Reg -> SDoc
pprBinaryF platform op sz reg1 reg2 reg3 = hcat [
        char '\t',
        ptext op,
        pprFSize sz,
        char '\t',
        pprReg platform reg1,
        ptext (sLit ", "),
        pprReg platform reg2,
        ptext (sLit ", "),
        pprReg platform reg3
    ]
    
pprRI :: Platform -> RI -> SDoc
pprRI platform (RIReg r) = pprReg platform r
pprRI platform (RIImm r) = pprImm platform r


pprFSize :: Size -> SDoc
pprFSize FF64     = empty
pprFSize FF32     = char 's'
pprFSize _        = panic "PPC.Ppr.pprFSize: no match"

    -- limit immediate argument for shift instruction to range 0..31
limitShiftRI :: RI -> RI
limitShiftRI (RIImm (ImmInt i)) | i > 31 || i < 0 =
  panic $ "PPC.Ppr: Shift by " ++ show i ++ " bits is not allowed."
limitShiftRI x = x

