-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
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

import Cmm hiding (topInfoTable)
import BlockId

import CLabel

import Unique                ( pprUnique, Uniquable(..) )
import Platform
import FastString
import Outputable

import Data.Word
import Data.Bits


-- -----------------------------------------------------------------------------
-- Printing this stuff out

pprNatCmmDecl :: NatCmmDecl CmmStatics Instr -> SDoc
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
           vcat (map (pprBasicBlock top_info) blocks)

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
             else empty)


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



pprDatas :: CmmStatics -> SDoc
pprDatas (Statics lbl dats) = vcat (pprLabel lbl : map pprData dats)

pprData :: CmmStatic -> SDoc
pprData (CmmString str)          = pprASCII str
pprData (CmmUninitialised bytes) = keyword <> int bytes
    where keyword = sdocWithPlatform $ \platform ->
                    case platformOS platform of
                    OSDarwin -> ptext (sLit ".space ")
                    _        -> ptext (sLit ".skip ")
pprData (CmmStaticLit lit)       = pprDataItem lit

pprGloblDecl :: CLabel -> SDoc
pprGloblDecl lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = ptext (sLit ".globl ") <> ppr lbl

pprTypeAndSizeDecl :: CLabel -> SDoc
pprTypeAndSizeDecl lbl
  = sdocWithPlatform $ \platform ->
    if platformOS platform == OSLinux && externallyVisibleCLabel lbl
    then ptext (sLit ".type ") <>
         ppr lbl <> ptext (sLit ", @object")
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


-- -----------------------------------------------------------------------------
-- pprInstr: print an 'Instr'

instance Outputable Instr where
    ppr instr = pprInstr instr


pprReg :: Reg -> SDoc

pprReg r
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
        sdocWithPlatform $ \platform ->
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

pprImm (LO i)
  = sdocWithPlatform $ \platform ->
    if platformOS platform == OSDarwin
    then hcat [ text "lo16(", pprImm i, rparen ]
    else pprImm i <> text "@l"

pprImm (HI i)
  = sdocWithPlatform $ \platform ->
    if platformOS platform == OSDarwin
    then hcat [ text "hi16(", pprImm i, rparen ]
    else pprImm i <> text "@h"

pprImm (HA i)
  = sdocWithPlatform $ \platform ->
    if platformOS platform == OSDarwin
    then hcat [ text "ha16(", pprImm i, rparen ]
    else pprImm i <> text "@ha"


pprAddr :: AddrMode -> SDoc
pprAddr (AddrRegReg r1 r2)
  = pprReg r1 <+> ptext (sLit ", ") <+> pprReg r2

pprAddr (AddrRegImm r1 (ImmInt i)) = hcat [ int i, char '(', pprReg r1, char ')' ]
pprAddr (AddrRegImm r1 (ImmInteger i)) = hcat [ integer i, char '(', pprReg r1, char ')' ]
pprAddr (AddrRegImm r1 imm) = hcat [ pprImm imm, char '(', pprReg r1, char ')' ]


pprSectionHeader :: Section -> SDoc
pprSectionHeader seg
 = sdocWithPlatform $ \platform ->
   let osDarwin = platformOS platform == OSDarwin
   in   case seg of
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


pprDataItem :: CmmLit -> SDoc
pprDataItem lit
  = sdocWithDynFlags $ \dflags ->
    vcat (ppr_item (cmmTypeSize $ cmmLitType dflags lit) lit)
    where
        imm = litToImm lit

        ppr_item II8   _ = [ptext (sLit "\t.byte\t") <> pprImm imm]

        ppr_item II32  _ = [ptext (sLit "\t.long\t") <> pprImm imm]

        ppr_item FF32 (CmmFloat r _)
           = let bs = floatToBytes (fromRational r)
             in  map (\b -> ptext (sLit "\t.byte\t") <> pprImm (ImmInt b)) bs

        ppr_item FF64 (CmmFloat r _)
           = let bs = doubleToBytes (fromRational r)
             in  map (\b -> ptext (sLit "\t.byte\t") <> pprImm (ImmInt b)) bs

        ppr_item II16 _        = [ptext (sLit "\t.short\t") <> pprImm imm]

        ppr_item II64 (CmmInt x _)  =
                [ptext (sLit "\t.long\t")
                    <> int (fromIntegral 
                        (fromIntegral (x `shiftR` 32) :: Word32)),
                 ptext (sLit "\t.long\t")
                    <> int (fromIntegral (fromIntegral x :: Word32))]

        ppr_item _ _
                = panic "PPC.Ppr.pprDataItem: no match"


pprInstr :: Instr -> SDoc

pprInstr (COMMENT _) = empty -- nuke 'em
{-
pprInstr (COMMENT s) =
     if platformOS platform == OSLinux
     then ptext (sLit "# ") <> ftext s
     else ptext (sLit "; ") <> ftext s
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
        char '\t',
        pprReg reg,
        comma,
        ptext (sLit "SLOT") <> parens (int slot)]

pprInstr (RELOAD slot reg)
   = hcat [
           ptext (sLit "\tRELOAD"),
        char '\t',
        ptext (sLit "SLOT") <> parens (int slot),
        comma,
        pprReg reg]
-}

pprInstr (LD sz reg addr) = hcat [
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
        pprReg reg,
        ptext (sLit ", "),
        pprAddr addr
    ]
pprInstr (LA sz reg addr) = hcat [
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
        pprReg reg,
        ptext (sLit ", "),
        pprAddr addr
    ]
pprInstr (ST sz reg addr) = hcat [
        char '\t',
        ptext (sLit "st"),
        pprSize sz,
        case addr of AddrRegImm _ _ -> empty
                     AddrRegReg _ _ -> char 'x',
        char '\t',
        pprReg reg,
        ptext (sLit ", "),
        pprAddr addr
    ]
pprInstr (STU sz reg addr) = hcat [
        char '\t',
        ptext (sLit "st"),
        pprSize sz,
        ptext (sLit "u\t"),
        case addr of AddrRegImm _ _ -> empty
                     AddrRegReg _ _ -> char 'x',
        pprReg reg,
        ptext (sLit ", "),
        pprAddr addr
    ]
pprInstr (LIS reg imm) = hcat [
        char '\t',
        ptext (sLit "lis"),
        char '\t',
        pprReg reg,
        ptext (sLit ", "),
        pprImm imm
    ]
pprInstr (LI reg imm) = hcat [
        char '\t',
        ptext (sLit "li"),
        char '\t',
        pprReg reg,
        ptext (sLit ", "),
        pprImm imm
    ]
pprInstr (MR reg1 reg2) 
    | reg1 == reg2 = empty
    | otherwise = hcat [
        char '\t',
        sdocWithPlatform $ \platform ->
        case targetClassOfReg platform reg1 of
            RcInteger -> ptext (sLit "mr")
            _ -> ptext (sLit "fmr"),
        char '\t',
        pprReg reg1,
        ptext (sLit ", "),
        pprReg reg2
    ]
pprInstr (CMP sz reg ri) = hcat [
        char '\t',
        op,
        char '\t',
        pprReg reg,
        ptext (sLit ", "),
        pprRI ri
    ]
    where
        op = hcat [
                ptext (sLit "cmp"),
                pprSize sz,
                case ri of
                    RIReg _ -> empty
                    RIImm _ -> char 'i'
            ]
pprInstr (CMPL sz reg ri) = hcat [
        char '\t',
        op,
        char '\t',
        pprReg reg,
        ptext (sLit ", "),
        pprRI ri
    ]
    where
        op = hcat [
                ptext (sLit "cmpl"),
                pprSize sz,
                case ri of
                    RIReg _ -> empty
                    RIImm _ -> char 'i'
            ]
pprInstr (BCC cond blockid) = hcat [
        char '\t',
        ptext (sLit "b"),
        pprCond cond,
        char '\t',
        ppr lbl
    ]
    where lbl = mkAsmTempLabel (getUnique blockid)

pprInstr (BCCFAR cond blockid) = vcat [
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

pprInstr (JMP lbl) = hcat [ -- an alias for b that takes a CLabel
        char '\t',
        ptext (sLit "b"),
        char '\t',
        ppr lbl
    ]

pprInstr (MTCTR reg) = hcat [
        char '\t',
        ptext (sLit "mtctr"),
        char '\t',
        pprReg reg
    ]
pprInstr (BCTR _ _) = hcat [
        char '\t',
        ptext (sLit "bctr")
    ]
pprInstr (BL lbl _) = hcat [
        ptext (sLit "\tbl\t"),
        ppr lbl
    ]
pprInstr (BCTRL _) = hcat [
        char '\t',
        ptext (sLit "bctrl")
    ]
pprInstr (ADD reg1 reg2 ri) = pprLogic (sLit "add") reg1 reg2 ri
pprInstr (ADDIS reg1 reg2 imm) = hcat [
        char '\t',
        ptext (sLit "addis"),
        char '\t',
        pprReg reg1,
        ptext (sLit ", "),
        pprReg reg2,
        ptext (sLit ", "),
        pprImm imm
    ]

pprInstr (ADDC reg1 reg2 reg3) = pprLogic (sLit "addc") reg1 reg2 (RIReg reg3)
pprInstr (ADDE reg1 reg2 reg3) = pprLogic (sLit "adde") reg1 reg2 (RIReg reg3)
pprInstr (SUBF reg1 reg2 reg3) = pprLogic (sLit "subf") reg1 reg2 (RIReg reg3)
pprInstr (MULLW reg1 reg2 ri@(RIReg _)) = pprLogic (sLit "mullw") reg1 reg2 ri
pprInstr (MULLW reg1 reg2 ri@(RIImm _)) = pprLogic (sLit "mull") reg1 reg2 ri
pprInstr (DIVW reg1 reg2 reg3) = pprLogic (sLit "divw") reg1 reg2 (RIReg reg3)
pprInstr (DIVWU reg1 reg2 reg3) = pprLogic (sLit "divwu") reg1 reg2 (RIReg reg3)

pprInstr (MULLW_MayOflo reg1 reg2 reg3) = vcat [
         hcat [ ptext (sLit "\tmullwo\t"), pprReg reg1, ptext (sLit ", "),
                                          pprReg reg2, ptext (sLit ", "),
                                          pprReg reg3 ],
         hcat [ ptext (sLit "\tmfxer\t"),  pprReg reg1 ],
         hcat [ ptext (sLit "\trlwinm\t"), pprReg reg1, ptext (sLit ", "),
                                          pprReg reg1, ptext (sLit ", "),
                                          ptext (sLit "2, 31, 31") ]
    ]

            -- for some reason, "andi" doesn't exist.
        -- we'll use "andi." instead.
pprInstr (AND reg1 reg2 (RIImm imm)) = hcat [
        char '\t',
        ptext (sLit "andi."),
        char '\t',
        pprReg reg1,
        ptext (sLit ", "),
        pprReg reg2,
        ptext (sLit ", "),
        pprImm imm
    ]
pprInstr (AND reg1 reg2 ri) = pprLogic (sLit "and") reg1 reg2 ri

pprInstr (OR reg1 reg2 ri) = pprLogic (sLit "or") reg1 reg2 ri
pprInstr (XOR reg1 reg2 ri) = pprLogic (sLit "xor") reg1 reg2 ri

pprInstr (XORIS reg1 reg2 imm) = hcat [
        char '\t',
        ptext (sLit "xoris"),
        char '\t',
        pprReg reg1,
        ptext (sLit ", "),
        pprReg reg2,
        ptext (sLit ", "),
        pprImm imm
    ]

pprInstr (EXTS sz reg1 reg2) = hcat [
        char '\t',
        ptext (sLit "exts"),
        pprSize sz,
        char '\t',
        pprReg reg1,
        ptext (sLit ", "),
        pprReg reg2
    ]

pprInstr (NEG reg1 reg2) = pprUnary (sLit "neg") reg1 reg2
pprInstr (NOT reg1 reg2) = pprUnary (sLit "not") reg1 reg2

pprInstr (SLW reg1 reg2 ri) = pprLogic (sLit "slw") reg1 reg2 (limitShiftRI ri)

pprInstr (SRW reg1 reg2 (RIImm (ImmInt i))) | i > 31 || i < 0 =
    -- Handle the case where we are asked to shift a 32 bit register by
    -- less than zero or more than 31 bits. We convert this into a clear
    -- of the destination register.
    -- Fixes ticket http://ghc.haskell.org/trac/ghc/ticket/5900
    pprInstr (XOR reg1 reg2 (RIReg reg2))
pprInstr (SRW reg1 reg2 ri) = pprLogic (sLit "srw") reg1 reg2 (limitShiftRI ri)

pprInstr (SRAW reg1 reg2 ri) = pprLogic (sLit "sraw") reg1 reg2 (limitShiftRI ri)
pprInstr (RLWINM reg1 reg2 sh mb me) = hcat [
        ptext (sLit "\trlwinm\t"),
        pprReg reg1,
        ptext (sLit ", "),
        pprReg reg2,
        ptext (sLit ", "),
        int sh,
        ptext (sLit ", "),
        int mb,
        ptext (sLit ", "),
        int me
    ]
    
pprInstr (FADD sz reg1 reg2 reg3) = pprBinaryF (sLit "fadd") sz reg1 reg2 reg3
pprInstr (FSUB sz reg1 reg2 reg3) = pprBinaryF (sLit "fsub") sz reg1 reg2 reg3
pprInstr (FMUL sz reg1 reg2 reg3) = pprBinaryF (sLit "fmul") sz reg1 reg2 reg3
pprInstr (FDIV sz reg1 reg2 reg3) = pprBinaryF (sLit "fdiv") sz reg1 reg2 reg3
pprInstr (FNEG reg1 reg2) = pprUnary (sLit "fneg") reg1 reg2

pprInstr (FCMP reg1 reg2) = hcat [
        char '\t',
        ptext (sLit "fcmpu\tcr0, "),
            -- Note: we're using fcmpu, not fcmpo
            -- The difference is with fcmpo, compare with NaN is an invalid operation.
            -- We don't handle invalid fp ops, so we don't care
        pprReg reg1,
        ptext (sLit ", "),
        pprReg reg2
    ]

pprInstr (FCTIWZ reg1 reg2) = pprUnary (sLit "fctiwz") reg1 reg2
pprInstr (FRSP reg1 reg2) = pprUnary (sLit "frsp") reg1 reg2

pprInstr (CRNOR dst src1 src2) = hcat [
        ptext (sLit "\tcrnor\t"),
        int dst,
        ptext (sLit ", "),
        int src1,
        ptext (sLit ", "),
        int src2
    ]

pprInstr (MFCR reg) = hcat [
        char '\t',
        ptext (sLit "mfcr"),
        char '\t',
        pprReg reg
    ]

pprInstr (MFLR reg) = hcat [
        char '\t',
        ptext (sLit "mflr"),
        char '\t',
        pprReg reg
    ]

pprInstr (FETCHPC reg) = vcat [
        ptext (sLit "\tbcl\t20,31,1f"),
        hcat [ ptext (sLit "1:\tmflr\t"), pprReg reg ]
    ]

pprInstr LWSYNC = ptext (sLit "\tlwsync")

-- pprInstr _ = panic "pprInstr (ppc)"


pprLogic :: LitString -> Reg -> Reg -> RI -> SDoc
pprLogic op reg1 reg2 ri = hcat [
        char '\t',
        ptext op,
        case ri of
            RIReg _ -> empty
            RIImm _ -> char 'i',
        char '\t',
        pprReg reg1,
        ptext (sLit ", "),
        pprReg reg2,
        ptext (sLit ", "),
        pprRI ri
    ]


pprUnary :: LitString -> Reg -> Reg -> SDoc
pprUnary op reg1 reg2 = hcat [
        char '\t',
        ptext op,
        char '\t',
        pprReg reg1,
        ptext (sLit ", "),
        pprReg reg2
    ]


pprBinaryF :: LitString -> Size -> Reg -> Reg -> Reg -> SDoc
pprBinaryF op sz reg1 reg2 reg3 = hcat [
        char '\t',
        ptext op,
        pprFSize sz,
        char '\t',
        pprReg reg1,
        ptext (sLit ", "),
        pprReg reg2,
        ptext (sLit ", "),
        pprReg reg3
    ]
    
pprRI :: RI -> SDoc
pprRI (RIReg r) = pprReg r
pprRI (RIImm r) = pprImm r


pprFSize :: Size -> SDoc
pprFSize FF64     = empty
pprFSize FF32     = char 's'
pprFSize _        = panic "PPC.Ppr.pprFSize: no match"

    -- limit immediate argument for shift instruction to range 0..31
limitShiftRI :: RI -> RI
limitShiftRI (RIImm (ImmInt i)) | i > 31 || i < 0 =
  panic $ "PPC.Ppr: Shift by " ++ show i ++ " bits is not allowed."
limitShiftRI x = x

