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
        pprFormat,
        pprImm,
        pprDataItem,
)

where

import PPC.Regs
import PPC.Instr
import PPC.Cond
import PprBase
import Instruction
import Format
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
import DynFlags

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
       sdocWithPlatform $ \platform ->
       case blocks of
         []     -> -- special case for split markers:
           pprLabel lbl
         blocks -> -- special case for code without info table:
           pprSectionHeader Text $$
           (case platformArch platform of
              ArchPPC_64 ELF_V1 -> pprFunctionDescriptor lbl
              ArchPPC_64 ELF_V2 -> pprFunctionPrologue lbl
              _ -> pprLabel lbl) $$ -- blocks guaranteed not null,
                                     -- so label needed
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
             -- See Note [Subsections Via Symbols]
                      text "\t.long "
                  <+> ppr info_lbl
                  <+> char '-'
                  <+> ppr (mkDeadStripPreventer info_lbl)
             else empty)


pprFunctionDescriptor :: CLabel -> SDoc
pprFunctionDescriptor lab = pprGloblDecl lab
                        $$  text ".section \".opd\",\"aw\""
                        $$  text ".align 3"
                        $$  ppr lab <> char ':'
                        $$  text ".quad ."
                        <> ppr lab
                        <> text ",.TOC.@tocbase,0"
                        $$  text ".previous"
                        $$  text ".type "
                        <> ppr lab
                        <> text ", @function"
                        $$  char '.'
                        <> ppr lab
                        <> char ':'

pprFunctionPrologue :: CLabel ->SDoc
pprFunctionPrologue lab =  pprGloblDecl lab
                        $$  text ".type "
                        <> ppr lab
                        <> text ", @function"
                        $$ ppr lab <> char ':'
                        $$ text "0:\taddis\t" <> pprReg toc
                        <> text ",12,.TOC.-0b@ha"
                        $$ text "\taddi\t" <> pprReg toc
                        <> char ',' <> pprReg toc <> text ",.TOC.-0b@l"
                        $$ text "\t.localentry\t" <> ppr lab
                        <> text ",.-" <> ppr lab

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



pprFormat :: Format -> SDoc
pprFormat x
 = ptext (case x of
                II8        -> sLit "b"
                II16        -> sLit "h"
                II32        -> sLit "w"
                II64        -> sLit "d"
                FF32        -> sLit "fs"
                FF64        -> sLit "fd"
                _        -> panic "PPC.Ppr.pprFormat: no match")
                
                
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

pprImm (HIGHERA i)
  = sdocWithPlatform $ \platform ->
    if platformOS platform == OSDarwin
    then panic "PPC.pprImm: highera not implemented on Darwin"
    else pprImm i <> text "@highera"

pprImm (HIGHESTA i)
  = sdocWithPlatform $ \platform ->
    if platformOS platform == OSDarwin
    then panic "PPC.pprImm: highesta not implemented on Darwin"
    else pprImm i <> text "@highesta"


pprAddr :: AddrMode -> SDoc
pprAddr (AddrRegReg r1 r2)
  = pprReg r1 <+> ptext (sLit ", ") <+> pprReg r2

pprAddr (AddrRegImm r1 (ImmInt i)) = hcat [ int i, char '(', pprReg r1, char ')' ]
pprAddr (AddrRegImm r1 (ImmInteger i)) = hcat [ integer i, char '(', pprReg r1, char ')' ]
pprAddr (AddrRegImm r1 imm) = hcat [ pprImm imm, char '(', pprReg r1, char ')' ]


pprSectionHeader :: Section -> SDoc
pprSectionHeader seg =
 sdocWithPlatform $ \platform ->
 let osDarwin = platformOS platform == OSDarwin
     ppc64    = not $ target32Bit platform
 in
 case seg of
  Text              -> text ".text\n\t.align 2"
  Data
   | ppc64          -> text ".data\n.align 3"
   | otherwise      -> text ".data\n.align 2"
  ReadOnlyData
   | osDarwin       -> text ".const\n\t.align 2"
   | ppc64          -> text ".section .rodata\n\t.align 3"
   | otherwise      -> text ".section .rodata\n\t.align 2"
  RelocatableReadOnlyData
   | osDarwin       -> text ".const_data\n\t.align 2"
   | ppc64          -> text ".data\n\t.align 3"
   | otherwise      -> text ".data\n\t.align 2"
  UninitialisedData
   | osDarwin       -> text ".const_data\n\t.align 2"
   | ppc64          -> text ".section .bss\n\t.align 3"
   | otherwise      -> text ".section .bss\n\t.align 2"
  ReadOnlyData16
   | osDarwin       -> text ".const\n\t.align 4"
   | otherwise      -> text ".section .rodata\n\t.align 4"
  OtherSection _ ->
      panic "PprMach.pprSectionHeader: unknown section"


pprDataItem :: CmmLit -> SDoc
pprDataItem lit
  = sdocWithDynFlags $ \dflags ->
    vcat (ppr_item (cmmTypeFormat $ cmmLitType dflags lit) lit dflags)
    where
        imm = litToImm lit
        archPPC_64 dflags = not $ target32Bit $ targetPlatform dflags

        ppr_item II8   _ _ = [ptext (sLit "\t.byte\t") <> pprImm imm]

        ppr_item II32  _ _ = [ptext (sLit "\t.long\t") <> pprImm imm]

        ppr_item II64 _ dflags
           | archPPC_64 dflags = [ptext (sLit "\t.quad\t") <> pprImm imm]


        ppr_item FF32 (CmmFloat r _) _
           = let bs = floatToBytes (fromRational r)
             in  map (\b -> ptext (sLit "\t.byte\t") <> pprImm (ImmInt b)) bs

        ppr_item FF64 (CmmFloat r _) _
           = let bs = doubleToBytes (fromRational r)
             in  map (\b -> ptext (sLit "\t.byte\t") <> pprImm (ImmInt b)) bs

        ppr_item II16 _ _      = [ptext (sLit "\t.short\t") <> pprImm imm]

        ppr_item II64 (CmmInt x _) dflags
           | not(archPPC_64 dflags) =
                [ptext (sLit "\t.long\t")
                    <> int (fromIntegral 
                        (fromIntegral (x `shiftR` 32) :: Word32)),
                 ptext (sLit "\t.long\t")
                    <> int (fromIntegral (fromIntegral x :: Word32))]

        ppr_item _ _ _
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

pprInstr (LD fmt reg addr) = hcat [
        char '\t',
        ptext (sLit "l"),
        ptext (case fmt of
            II8  -> sLit "bz"
            II16 -> sLit "hz"
            II32 -> sLit "wz"
            II64 -> sLit "d"
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
pprInstr (LDFAR fmt reg (AddrRegImm source off)) =
   sdocWithPlatform $ \platform -> vcat [
         pprInstr (ADDIS (tmpReg platform) source (HA off)),
         pprInstr (LD fmt reg (AddrRegImm (tmpReg platform) (LO off)))
    ]

pprInstr (LDFAR _ _ _) =
   panic "PPC.Ppr.pprInstr LDFAR: no match"

pprInstr (LA fmt reg addr) = hcat [
        char '\t',
        ptext (sLit "l"),
        ptext (case fmt of
            II8  -> sLit "ba"
            II16 -> sLit "ha"
            II32 -> sLit "wa"
            II64 -> sLit "d"
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
pprInstr (ST fmt reg addr) = hcat [
        char '\t',
        ptext (sLit "st"),
        pprFormat fmt,
        case addr of AddrRegImm _ _ -> empty
                     AddrRegReg _ _ -> char 'x',
        char '\t',
        pprReg reg,
        ptext (sLit ", "),
        pprAddr addr
    ]
pprInstr (STFAR fmt reg (AddrRegImm source off)) =
   sdocWithPlatform $ \platform -> vcat [
         pprInstr (ADDIS (tmpReg platform) source (HA off)),
         pprInstr (ST fmt reg (AddrRegImm (tmpReg platform) (LO off)))
    ]

pprInstr (STFAR _ _ _) =
   panic "PPC.Ppr.pprInstr STFAR: no match"
pprInstr (STU fmt reg addr) = hcat [
        char '\t',
        ptext (sLit "st"),
        pprFormat fmt,
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
pprInstr (CMP fmt reg ri) = hcat [
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
                pprFormat fmt,
                case ri of
                    RIReg _ -> empty
                    RIImm _ -> char 'i'
            ]
pprInstr (CMPL fmt reg ri) = hcat [
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
                pprFormat fmt,
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
pprInstr (ADDI reg1 reg2 imm) = hcat [
        char '\t',
        ptext (sLit "addi"),
        char '\t',
        pprReg reg1,
        ptext (sLit ", "),
        pprReg reg2,
        ptext (sLit ", "),
        pprImm imm
    ]
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
pprInstr (SUBFC reg1 reg2 reg3) = pprLogic (sLit "subfc") reg1 reg2 (RIReg reg3)
pprInstr (SUBFE reg1 reg2 reg3) = pprLogic (sLit "subfe") reg1 reg2 (RIReg reg3)
pprInstr (MULLD reg1 reg2 ri@(RIReg _)) = pprLogic (sLit "mulld") reg1 reg2 ri
pprInstr (MULLW reg1 reg2 ri@(RIReg _)) = pprLogic (sLit "mullw") reg1 reg2 ri
pprInstr (MULLD reg1 reg2 ri@(RIImm _)) = pprLogic (sLit "mull") reg1 reg2 ri
pprInstr (MULLW reg1 reg2 ri@(RIImm _)) = pprLogic (sLit "mull") reg1 reg2 ri
pprInstr (DIVW reg1 reg2 reg3) = pprLogic (sLit "divw") reg1 reg2 (RIReg reg3)
pprInstr (DIVD reg1 reg2 reg3) = pprLogic (sLit "divd") reg1 reg2 (RIReg reg3)
pprInstr (DIVWU reg1 reg2 reg3) = pprLogic (sLit "divwu") reg1 reg2 (RIReg reg3)
pprInstr (DIVDU reg1 reg2 reg3) = pprLogic (sLit "divdu") reg1 reg2 (RIReg reg3)

pprInstr (MULLW_MayOflo reg1 reg2 reg3) = vcat [
         hcat [ ptext (sLit "\tmullwo\t"), pprReg reg1, ptext (sLit ", "),
                                          pprReg reg2, ptext (sLit ", "),
                                          pprReg reg3 ],
         hcat [ ptext (sLit "\tmfxer\t"),  pprReg reg1 ],
         hcat [ ptext (sLit "\trlwinm\t"), pprReg reg1, ptext (sLit ", "),
                                          pprReg reg1, ptext (sLit ", "),
                                          ptext (sLit "2, 31, 31") ]
    ]
pprInstr (MULLD_MayOflo reg1 reg2 reg3) = vcat [
         hcat [ ptext (sLit "\tmulldo\t"), pprReg reg1, ptext (sLit ", "),
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

pprInstr (ORIS reg1 reg2 imm) = hcat [
        char '\t',
        ptext (sLit "oris"),
        char '\t',
        pprReg reg1,
        ptext (sLit ", "),
        pprReg reg2,
        ptext (sLit ", "),
        pprImm imm
    ]

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

pprInstr (EXTS fmt reg1 reg2) = hcat [
        char '\t',
        ptext (sLit "exts"),
        pprFormat fmt,
        char '\t',
        pprReg reg1,
        ptext (sLit ", "),
        pprReg reg2
    ]

pprInstr (NEG reg1 reg2) = pprUnary (sLit "neg") reg1 reg2
pprInstr (NOT reg1 reg2) = pprUnary (sLit "not") reg1 reg2

pprInstr (SL fmt reg1 reg2 ri) =
         let op = case fmt of
                       II32 -> "slw"
                       II64 -> "sld"
                       _    -> panic "PPC.Ppr.pprInstr: shift illegal size"
         in pprLogic (sLit op) reg1 reg2 (limitShiftRI fmt ri)

pprInstr (SR II32 reg1 reg2 (RIImm (ImmInt i))) | i > 31 || i < 0 =
    -- Handle the case where we are asked to shift a 32 bit register by
    -- less than zero or more than 31 bits. We convert this into a clear
    -- of the destination register.
    -- Fixes ticket http://ghc.haskell.org/trac/ghc/ticket/5900
    pprInstr (XOR reg1 reg2 (RIReg reg2))
pprInstr (SR fmt reg1 reg2 ri) =
         let op = case fmt of
                       II32 -> "srw"
                       II64 -> "srd"
                       _    -> panic "PPC.Ppr.pprInstr: shift illegal size"
         in pprLogic (sLit op) reg1 reg2 (limitShiftRI fmt ri)

pprInstr (SRA fmt reg1 reg2 ri) =
         let op = case fmt of
                       II32 -> "sraw"
                       II64 -> "srad"
                       _    -> panic "PPC.Ppr.pprInstr: shift illegal size"
         in pprLogic (sLit op) reg1 reg2 (limitShiftRI fmt ri)

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
    
pprInstr (FADD fmt reg1 reg2 reg3) = pprBinaryF (sLit "fadd") fmt reg1 reg2 reg3
pprInstr (FSUB fmt reg1 reg2 reg3) = pprBinaryF (sLit "fsub") fmt reg1 reg2 reg3
pprInstr (FMUL fmt reg1 reg2 reg3) = pprBinaryF (sLit "fmul") fmt reg1 reg2 reg3
pprInstr (FDIV fmt reg1 reg2 reg3) = pprBinaryF (sLit "fdiv") fmt reg1 reg2 reg3
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
pprInstr (FCTIDZ reg1 reg2) = pprUnary (sLit "fctidz") reg1 reg2
pprInstr (FCFID reg1 reg2) = pprUnary (sLit "fcfid") reg1 reg2
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

pprInstr (FETCHTOC reg lab) = vcat [
        hcat [ ptext (sLit "0:\taddis\t"), pprReg reg,
               ptext (sLit ",12,.TOC.-0b@ha") ],
        hcat [ ptext (sLit "\taddi\t"), pprReg reg,
               char ',', pprReg reg,
               ptext (sLit ",.TOC.-0b@l") ],
        hcat [ ptext (sLit "\t.localentry\t"),
               ppr lab,
               ptext (sLit ",.-"),
               ppr lab]
    ]

pprInstr LWSYNC = ptext (sLit "\tlwsync")

pprInstr NOP = ptext (sLit "\tnop")

pprInstr (UPDATE_SP fmt amount@(ImmInt offset))
   | fits16Bits offset = vcat [
       pprInstr (LD fmt r0 (AddrRegImm sp (ImmInt 0))),
       pprInstr (STU fmt r0 (AddrRegImm sp amount))
     ]

pprInstr (UPDATE_SP fmt amount)
   = sdocWithPlatform $ \platform ->
       let tmp = tmpReg platform in
         vcat [
           pprInstr (LD fmt r0 (AddrRegImm sp (ImmInt 0))),
           pprInstr (ADDIS tmp sp (HA amount)),
           pprInstr (ADD tmp tmp (RIImm (LO amount))),
           pprInstr (STU fmt r0 (AddrRegReg sp tmp))
         ]

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


pprBinaryF :: LitString -> Format -> Reg -> Reg -> Reg -> SDoc
pprBinaryF op fmt reg1 reg2 reg3 = hcat [
        char '\t',
        ptext op,
        pprFFormat fmt,
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


pprFFormat :: Format -> SDoc
pprFFormat FF64     = empty
pprFFormat FF32     = char 's'
pprFFormat _        = panic "PPC.Ppr.pprFFormat: no match"

    -- limit immediate argument for shift instruction to range 0..63
    -- for 64 bit size and 0..32 otherwise
limitShiftRI :: Format -> RI -> RI
limitShiftRI II64 (RIImm (ImmInt i)) | i > 63 || i < 0 =
  panic $ "PPC.Ppr: Shift by " ++ show i ++ " bits is not allowed."
limitShiftRI II32 (RIImm (ImmInt i)) | i > 31 || i < 0 =
  panic $ "PPC.Ppr: 32 bit: Shift by " ++ show i ++ " bits is not allowed."
limitShiftRI _ x = x

