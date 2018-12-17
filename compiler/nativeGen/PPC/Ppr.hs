-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
module PPC.Ppr (pprNatCmmDecl) where

import GhcPrelude

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
import Hoopl.Collections
import Hoopl.Label

import BlockId
import CLabel

import Unique                ( pprUniqueAlways, getUnique )
import Platform
import FastString
import Outputable
import DynFlags

import Data.Word
import Data.Int
import Data.Bits

-- -----------------------------------------------------------------------------
-- Printing this stuff out

pprNatCmmDecl :: NatCmmDecl CmmStatics Instr -> SDoc
pprNatCmmDecl (CmmData section dats) =
  pprSectionAlign section $$ pprDatas dats

pprNatCmmDecl proc@(CmmProc top_info lbl _ (ListGraph blocks)) =
  case topInfoTable proc of
    Nothing ->
       sdocWithPlatform $ \platform ->
       case blocks of
         []     -> -- special case for split markers:
           pprLabel lbl
         blocks -> -- special case for code without info table:
           pprSectionAlign (Section Text lbl) $$
           (case platformArch platform of
              ArchPPC_64 ELF_V1 -> pprFunctionDescriptor lbl
              ArchPPC_64 ELF_V2 -> pprFunctionPrologue lbl
              _ -> pprLabel lbl) $$ -- blocks guaranteed not null,
                                     -- so label needed
           vcat (map (pprBasicBlock top_info) blocks)

    Just (Statics info_lbl _) ->
      sdocWithPlatform $ \platform ->
      pprSectionAlign (Section Text info_lbl) $$
      (if platformHasSubsectionsViaSymbols platform
          then ppr (mkDeadStripPreventer info_lbl) <> char ':'
          else empty) $$
      vcat (map (pprBasicBlock top_info) blocks) $$
      -- above: Even the first block gets a label, because with branch-chain
      -- elimination, it might be the target of a goto.
      (if platformHasSubsectionsViaSymbols platform
       then
       -- See Note [Subsections Via Symbols] in X86/Ppr.hs
                text "\t.long "
            <+> ppr info_lbl
            <+> char '-'
            <+> ppr (mkDeadStripPreventer info_lbl)
       else empty)

pprFunctionDescriptor :: CLabel -> SDoc
pprFunctionDescriptor lab = pprGloblDecl lab
                        $$  text "\t.section \".opd\", \"aw\""
                        $$  text "\t.align 3"
                        $$  ppr lab <> char ':'
                        $$  text "\t.quad ."
                        <>  ppr lab
                        <>  text ",.TOC.@tocbase,0"
                        $$  text "\t.previous"
                        $$  text "\t.type"
                        <+> ppr lab
                        <>  text ", @function"
                        $$  char '.' <> ppr lab <> char ':'

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

pprBasicBlock :: LabelMap CmmStatics -> NatBasicBlock Instr -> SDoc
pprBasicBlock info_env (BasicBlock blockid instrs)
  = maybe_infotable $$
    pprLabel (blockLbl blockid) $$
    vcat (map pprInstr instrs)
  where
    maybe_infotable = case mapLookup blockid info_env of
       Nothing   -> empty
       Just (Statics info_lbl info) ->
           pprAlignForSection Text $$
           vcat (map pprData info) $$
           pprLabel info_lbl



pprDatas :: CmmStatics -> SDoc
pprDatas (Statics lbl dats) = vcat (pprLabel lbl : map pprData dats)

pprData :: CmmStatic -> SDoc
pprData (CmmString str)
  = text "\t.string" <+> doubleQuotes (pprASCII str)
pprData (CmmUninitialised bytes) = text ".space " <> int bytes
pprData (CmmStaticLit lit)       = pprDataItem lit

pprGloblDecl :: CLabel -> SDoc
pprGloblDecl lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = text ".globl " <> ppr lbl

pprTypeAndSizeDecl :: CLabel -> SDoc
pprTypeAndSizeDecl lbl
  = sdocWithPlatform $ \platform ->
    if platformOS platform == OSLinux && externallyVisibleCLabel lbl
    then text ".type " <>
         ppr lbl <> text ", @object"
    else empty

pprLabel :: CLabel -> SDoc
pprLabel lbl = pprGloblDecl lbl
            $$ pprTypeAndSizeDecl lbl
            $$ (ppr lbl <> char ':')

-- -----------------------------------------------------------------------------
-- pprInstr: print an 'Instr'

instance Outputable Instr where
    ppr instr = pprInstr instr


pprReg :: Reg -> SDoc

pprReg r
  = case r of
      RegReal    (RealRegSingle i) -> ppr_reg_no i
      RegReal    (RealRegPair{})   -> panic "PPC.pprReg: no reg pairs on this arch"
      RegVirtual (VirtualRegI  u)  -> text "%vI_"   <> pprUniqueAlways u
      RegVirtual (VirtualRegHi u)  -> text "%vHi_"  <> pprUniqueAlways u
      RegVirtual (VirtualRegF  u)  -> text "%vF_"   <> pprUniqueAlways u
      RegVirtual (VirtualRegD  u)  -> text "%vD_"   <> pprUniqueAlways u
      RegVirtual (VirtualRegSSE u) -> text "%vSSE_" <> pprUniqueAlways u
  where
    ppr_reg_no :: Int -> SDoc
    ppr_reg_no i
         | i <= 31   = int i      -- GPRs
         | i <= 63   = int (i-32) -- FPRs
         | otherwise = text "very naughty powerpc register"



pprFormat :: Format -> SDoc
pprFormat x
 = ptext (case x of
                II8  -> sLit "b"
                II16 -> sLit "h"
                II32 -> sLit "w"
                II64 -> sLit "d"
                FF32 -> sLit "fs"
                FF64 -> sLit "fd"
                _    -> panic "PPC.Ppr.pprFormat: no match")


pprCond :: Cond -> SDoc
pprCond c
 = ptext (case c of {
                ALWAYS  -> sLit "";
                EQQ     -> sLit "eq";  NE    -> sLit "ne";
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

pprImm (ImmFloat _)  = text "naughty float immediate"
pprImm (ImmDouble _) = text "naughty double immediate"

pprImm (ImmConstantSum a b) = pprImm a <> char '+' <> pprImm b
pprImm (ImmConstantDiff a b) = pprImm a <> char '-'
                   <> lparen <> pprImm b <> rparen

pprImm (LO (ImmInt i))     = pprImm (LO (ImmInteger (toInteger i)))
pprImm (LO (ImmInteger i)) = pprImm (ImmInteger (toInteger lo16))
  where
    lo16 = fromInteger (i .&. 0xffff) :: Int16

pprImm (LO i)
  = pprImm i <> text "@l"

pprImm (HI i)
  = pprImm i <> text "@h"

pprImm (HA (ImmInt i))     = pprImm (HA (ImmInteger (toInteger i)))
pprImm (HA (ImmInteger i)) = pprImm (ImmInteger ha16)
  where
    ha16 = if lo16 >= 0x8000 then hi16+1 else hi16
    hi16 = (i `shiftR` 16)
    lo16 = i .&. 0xffff

pprImm (HA i)
  = pprImm i <> text "@ha"

pprImm (HIGHERA i)
  = pprImm i <> text "@highera"

pprImm (HIGHESTA i)
  = pprImm i <> text "@highesta"


pprAddr :: AddrMode -> SDoc
pprAddr (AddrRegReg r1 r2)
  = pprReg r1 <> char ',' <+> pprReg r2
pprAddr (AddrRegImm r1 (ImmInt i))
  = hcat [ int i, char '(', pprReg r1, char ')' ]
pprAddr (AddrRegImm r1 (ImmInteger i))
  = hcat [ integer i, char '(', pprReg r1, char ')' ]
pprAddr (AddrRegImm r1 imm)
  = hcat [ pprImm imm, char '(', pprReg r1, char ')' ]


pprSectionAlign :: Section -> SDoc
pprSectionAlign sec@(Section seg _) =
 sdocWithPlatform $ \platform ->
   pprSectionHeader platform sec $$
   pprAlignForSection seg

-- | Print appropriate alignment for the given section type.
pprAlignForSection :: SectionType -> SDoc
pprAlignForSection seg =
 sdocWithPlatform $ \platform ->
 let ppc64    = not $ target32Bit platform
 in ptext $ case seg of
       Text              -> sLit ".align 2"
       Data
        | ppc64          -> sLit ".align 3"
        | otherwise      -> sLit ".align 2"
       ReadOnlyData
        | ppc64          -> sLit ".align 3"
        | otherwise      -> sLit ".align 2"
       RelocatableReadOnlyData
        | ppc64          -> sLit ".align 3"
        | otherwise      -> sLit ".align 2"
       UninitialisedData
        | ppc64          -> sLit ".align 3"
        | otherwise      -> sLit ".align 2"
       ReadOnlyData16    -> sLit ".align 4"
       -- TODO: This is copied from the ReadOnlyData case, but it can likely be
       -- made more efficient.
       CString
        | ppc64          -> sLit ".align 3"
        | otherwise      -> sLit ".align 2"
       OtherSection _    -> panic "PprMach.pprSectionAlign: unknown section"

pprDataItem :: CmmLit -> SDoc
pprDataItem lit
  = sdocWithDynFlags $ \dflags ->
    vcat (ppr_item (cmmTypeFormat $ cmmLitType dflags lit) lit dflags)
    where
        imm = litToImm lit
        archPPC_64 dflags = not $ target32Bit $ targetPlatform dflags

        ppr_item II8   _ _ = [text "\t.byte\t" <> pprImm imm]

        ppr_item II32  _ _ = [text "\t.long\t" <> pprImm imm]

        ppr_item II64 _ dflags
           | archPPC_64 dflags = [text "\t.quad\t" <> pprImm imm]


        ppr_item FF32 (CmmFloat r _) _
           = let bs = floatToBytes (fromRational r)
             in  map (\b -> text "\t.byte\t" <> pprImm (ImmInt b)) bs

        ppr_item FF64 (CmmFloat r _) _
           = let bs = doubleToBytes (fromRational r)
             in  map (\b -> text "\t.byte\t" <> pprImm (ImmInt b)) bs

        ppr_item II16 _ _      = [text "\t.short\t" <> pprImm imm]

        ppr_item II64 (CmmInt x _) dflags
           | not(archPPC_64 dflags) =
                [text "\t.long\t"
                    <> int (fromIntegral
                        (fromIntegral (x `shiftR` 32) :: Word32)),
                 text "\t.long\t"
                    <> int (fromIntegral (fromIntegral x :: Word32))]

        ppr_item _ _ _
                = panic "PPC.Ppr.pprDataItem: no match"


pprInstr :: Instr -> SDoc

pprInstr (COMMENT _) = empty -- nuke 'em
{-
pprInstr (COMMENT s) =
     if platformOS platform == OSLinux
     then text "# " <> ftext s
     else text "; " <> ftext s
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
           text "\tSPILL",
        char '\t',
        pprReg reg,
        comma,
        text "SLOT" <> parens (int slot)]

pprInstr (RELOAD slot reg)
   = hcat [
           text "\tRELOAD",
        char '\t',
        text "SLOT" <> parens (int slot),
        comma,
        pprReg reg]
-}

pprInstr (LD fmt reg addr) = hcat [
        char '\t',
        text "l",
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
        text ", ",
        pprAddr addr
    ]

pprInstr (LDFAR fmt reg (AddrRegImm source off)) =
   sdocWithPlatform $ \platform -> vcat [
         pprInstr (ADDIS (tmpReg platform) source (HA off)),
         pprInstr (LD fmt reg (AddrRegImm (tmpReg platform) (LO off)))
    ]
pprInstr (LDFAR _ _ _) =
   panic "PPC.Ppr.pprInstr LDFAR: no match"

pprInstr (LDR fmt reg1 addr) = hcat [
  text "\tl",
  case fmt of
    II32 -> char 'w'
    II64 -> char 'd'
    _    -> panic "PPC.Ppr.Instr LDR: no match",
  text "arx\t",
  pprReg reg1,
  text ", ",
  pprAddr addr
  ]

pprInstr (LA fmt reg addr) = hcat [
        char '\t',
        text "l",
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
        text ", ",
        pprAddr addr
    ]
pprInstr (ST fmt reg addr) = hcat [
        char '\t',
        text "st",
        pprFormat fmt,
        case addr of AddrRegImm _ _ -> empty
                     AddrRegReg _ _ -> char 'x',
        char '\t',
        pprReg reg,
        text ", ",
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
        text "st",
        pprFormat fmt,
        char 'u',
        case addr of AddrRegImm _ _ -> empty
                     AddrRegReg _ _ -> char 'x',
        char '\t',
        pprReg reg,
        text ", ",
        pprAddr addr
    ]
pprInstr (STC fmt reg1 addr) = hcat [
  text "\tst",
  case fmt of
    II32 -> char 'w'
    II64 -> char 'd'
    _    -> panic "PPC.Ppr.Instr STC: no match",
  text "cx.\t",
  pprReg reg1,
  text ", ",
  pprAddr addr
  ]
pprInstr (LIS reg imm) = hcat [
        char '\t',
        text "lis",
        char '\t',
        pprReg reg,
        text ", ",
        pprImm imm
    ]
pprInstr (LI reg imm) = hcat [
        char '\t',
        text "li",
        char '\t',
        pprReg reg,
        text ", ",
        pprImm imm
    ]
pprInstr (MR reg1 reg2)
    | reg1 == reg2 = empty
    | otherwise = hcat [
        char '\t',
        sdocWithPlatform $ \platform ->
        case targetClassOfReg platform reg1 of
            RcInteger -> text "mr"
            _ -> text "fmr",
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2
    ]
pprInstr (CMP fmt reg ri) = hcat [
        char '\t',
        op,
        char '\t',
        pprReg reg,
        text ", ",
        pprRI ri
    ]
    where
        op = hcat [
                text "cmp",
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
        text ", ",
        pprRI ri
    ]
    where
        op = hcat [
                text "cmpl",
                pprFormat fmt,
                case ri of
                    RIReg _ -> empty
                    RIImm _ -> char 'i'
            ]
pprInstr (BCC cond blockid prediction) = hcat [
        char '\t',
        text "b",
        pprCond cond,
        pprPrediction prediction,
        char '\t',
        ppr lbl
    ]
    where lbl = mkLocalBlockLabel (getUnique blockid)
          pprPrediction p = case p of
            Nothing    -> empty
            Just True  -> char '+'
            Just False -> char '-'

pprInstr (BCCFAR cond blockid prediction) = vcat [
        hcat [
            text "\tb",
            pprCond (condNegate cond),
            neg_prediction,
            text "\t$+8"
        ],
        hcat [
            text "\tb\t",
            ppr lbl
        ]
    ]
    where lbl = mkLocalBlockLabel (getUnique blockid)
          neg_prediction = case prediction of
            Nothing    -> empty
            Just True  -> char '-'
            Just False -> char '+'

pprInstr (JMP lbl)
  -- We never jump to ForeignLabels; if we ever do, c.f. handling for "BL"
  | isForeignLabel lbl = panic "PPC.Ppr.pprInstr: JMP to ForeignLabel"
  | otherwise =
    hcat [ -- an alias for b that takes a CLabel
        char '\t',
        text "b",
        char '\t',
        ppr lbl
    ]

pprInstr (MTCTR reg) = hcat [
        char '\t',
        text "mtctr",
        char '\t',
        pprReg reg
    ]
pprInstr (BCTR _ _) = hcat [
        char '\t',
        text "bctr"
    ]
pprInstr (BL lbl _) = do
    sdocWithPlatform $ \platform -> case platformOS platform of
        OSAIX ->
          -- On AIX, "printf" denotes a function-descriptor (for use
          -- by function pointers), whereas the actual entry-code
          -- address is denoted by the dot-prefixed ".printf" label.
          -- Moreover, the PPC NCG only ever emits a BL instruction
          -- for calling C ABI functions. Most of the time these calls
          -- originate from FFI imports and have a 'ForeignLabel',
          -- but when profiling the codegen inserts calls via
          -- 'emitRtsCallGen' which are 'CmmLabel's even though
          -- they'd technically be more like 'ForeignLabel's.
          hcat [
            text "\tbl\t.",
            ppr lbl
          ]
        _ ->
          hcat [
            text "\tbl\t",
            ppr lbl
          ]
pprInstr (BCTRL _) = hcat [
        char '\t',
        text "bctrl"
    ]
pprInstr (ADD reg1 reg2 ri) = pprLogic (sLit "add") reg1 reg2 ri
pprInstr (ADDIS reg1 reg2 imm) = hcat [
        char '\t',
        text "addis",
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        pprImm imm
    ]

pprInstr (ADDO reg1 reg2 reg3) = pprLogic (sLit "addo") reg1 reg2 (RIReg reg3)
pprInstr (ADDC reg1 reg2 reg3) = pprLogic (sLit "addc") reg1 reg2 (RIReg reg3)
pprInstr (ADDE reg1 reg2 reg3) = pprLogic (sLit "adde") reg1 reg2 (RIReg reg3)
pprInstr (ADDZE reg1 reg2) = pprUnary (sLit "addze") reg1 reg2
pprInstr (SUBF reg1 reg2 reg3) = pprLogic (sLit "subf") reg1 reg2 (RIReg reg3)
pprInstr (SUBFO reg1 reg2 reg3) = pprLogic (sLit "subfo") reg1 reg2 (RIReg reg3)
pprInstr (SUBFC reg1 reg2 ri) = hcat [
        char '\t',
        text "subf",
        case ri of
            RIReg _ -> empty
            RIImm _ -> char 'i',
        text "c\t",
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        pprRI ri
    ]
pprInstr (SUBFE reg1 reg2 reg3) = pprLogic (sLit "subfe") reg1 reg2 (RIReg reg3)
pprInstr (MULL fmt reg1 reg2 ri) = pprMul fmt reg1 reg2 ri
pprInstr (MULLO fmt reg1 reg2 reg3) = hcat [
        char '\t',
        text "mull",
        case fmt of
          II32 -> char 'w'
          II64 -> char 'd'
          _    -> panic "PPC: illegal format",
        text "o\t",
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        pprReg reg3
    ]
pprInstr (MFOV fmt reg) = vcat [
        hcat [
            char '\t',
            text "mfxer",
            char '\t',
            pprReg reg
            ],
        hcat [
            char '\t',
            text "extr",
            case fmt of
              II32 -> char 'w'
              II64 -> char 'd'
              _    -> panic "PPC: illegal format",
            text "i\t",
            pprReg reg,
            text ", ",
            pprReg reg,
            text ", 1, ",
            case fmt of
              II32 -> text "1"
              II64 -> text "33"
              _    -> panic "PPC: illegal format"
            ]
        ]

pprInstr (MULHU fmt reg1 reg2 reg3) = hcat [
        char '\t',
        text "mulh",
        case fmt of
          II32 -> char 'w'
          II64 -> char 'd'
          _    -> panic "PPC: illegal format",
        text "u\t",
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        pprReg reg3
    ]

pprInstr (DIV fmt sgn reg1 reg2 reg3) = pprDiv fmt sgn reg1 reg2 reg3

        -- for some reason, "andi" doesn't exist.
        -- we'll use "andi." instead.
pprInstr (AND reg1 reg2 (RIImm imm)) = hcat [
        char '\t',
        text "andi.",
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        pprImm imm
    ]
pprInstr (AND reg1 reg2 ri) = pprLogic (sLit "and") reg1 reg2 ri
pprInstr (ANDC reg1 reg2 reg3) = pprLogic (sLit "andc") reg1 reg2 (RIReg reg3)
pprInstr (NAND reg1 reg2 reg3) = pprLogic (sLit "nand") reg1 reg2 (RIReg reg3)

pprInstr (OR reg1 reg2 ri) = pprLogic (sLit "or") reg1 reg2 ri
pprInstr (XOR reg1 reg2 ri) = pprLogic (sLit "xor") reg1 reg2 ri

pprInstr (ORIS reg1 reg2 imm) = hcat [
        char '\t',
        text "oris",
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        pprImm imm
    ]

pprInstr (XORIS reg1 reg2 imm) = hcat [
        char '\t',
        text "xoris",
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        pprImm imm
    ]

pprInstr (EXTS fmt reg1 reg2) = hcat [
        char '\t',
        text "exts",
        pprFormat fmt,
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2
    ]
pprInstr (CNTLZ fmt reg1 reg2) = hcat [
        char '\t',
        text "cntlz",
        case fmt of
          II32 -> char 'w'
          II64 -> char 'd'
          _    -> panic "PPC: illegal format",
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2
    ]

pprInstr (NEG reg1 reg2) = pprUnary (sLit "neg") reg1 reg2
pprInstr (NOT reg1 reg2) = pprUnary (sLit "not") reg1 reg2

pprInstr (SR II32 reg1 reg2 (RIImm (ImmInt i))) | i < 0  || i > 31 =
    -- Handle the case where we are asked to shift a 32 bit register by
    -- less than zero or more than 31 bits. We convert this into a clear
    -- of the destination register.
    -- Fixes ticket http://ghc.haskell.org/trac/ghc/ticket/5900
    pprInstr (XOR reg1 reg2 (RIReg reg2))

pprInstr (SL II32 reg1 reg2 (RIImm (ImmInt i))) | i < 0  || i > 31 =
    -- As above for SR, but for left shifts.
    -- Fixes ticket http://ghc.haskell.org/trac/ghc/ticket/10870
    pprInstr (XOR reg1 reg2 (RIReg reg2))

pprInstr (SRA II32 reg1 reg2 (RIImm (ImmInt i))) | i > 31 =
    -- PT: I don't know what to do for negative shift amounts:
    -- For now just panic.
    --
    -- For shift amounts greater than 31 set all bit to the
    -- value of the sign bit, this also what sraw does.
    pprInstr (SRA II32 reg1 reg2 (RIImm (ImmInt 31)))

pprInstr (SL fmt reg1 reg2 ri) =
         let op = case fmt of
                       II32 -> "slw"
                       II64 -> "sld"
                       _    -> panic "PPC.Ppr.pprInstr: shift illegal size"
         in pprLogic (sLit op) reg1 reg2 (limitShiftRI fmt ri)

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
        text "\trlwinm\t",
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        int sh,
        text ", ",
        int mb,
        text ", ",
        int me
    ]

pprInstr (CLRLI fmt reg1 reg2 n) = hcat [
        text "\tclrl",
        pprFormat fmt,
        text "i ",
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        int n
    ]
pprInstr (CLRRI fmt reg1 reg2 n) = hcat [
        text "\tclrr",
        pprFormat fmt,
        text "i ",
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        int n
    ]

pprInstr (FADD fmt reg1 reg2 reg3) = pprBinaryF (sLit "fadd") fmt reg1 reg2 reg3
pprInstr (FSUB fmt reg1 reg2 reg3) = pprBinaryF (sLit "fsub") fmt reg1 reg2 reg3
pprInstr (FMUL fmt reg1 reg2 reg3) = pprBinaryF (sLit "fmul") fmt reg1 reg2 reg3
pprInstr (FDIV fmt reg1 reg2 reg3) = pprBinaryF (sLit "fdiv") fmt reg1 reg2 reg3
pprInstr (FABS reg1 reg2) = pprUnary (sLit "fabs") reg1 reg2
pprInstr (FNEG reg1 reg2) = pprUnary (sLit "fneg") reg1 reg2

pprInstr (FCMP reg1 reg2) = hcat [
        char '\t',
        text "fcmpu\t0, ",
            -- Note: we're using fcmpu, not fcmpo
            -- The difference is with fcmpo, compare with NaN is an invalid operation.
            -- We don't handle invalid fp ops, so we don't care.
            -- Morever, we use `fcmpu 0, ...` rather than `fcmpu cr0, ...` for
            -- better portability since some non-GNU assembler (such as
            -- IBM's `as`) tend not to support the symbolic register name cr0.
            -- This matches the syntax that GCC seems to emit for PPC targets.
        pprReg reg1,
        text ", ",
        pprReg reg2
    ]

pprInstr (FCTIWZ reg1 reg2) = pprUnary (sLit "fctiwz") reg1 reg2
pprInstr (FCTIDZ reg1 reg2) = pprUnary (sLit "fctidz") reg1 reg2
pprInstr (FCFID reg1 reg2) = pprUnary (sLit "fcfid") reg1 reg2
pprInstr (FRSP reg1 reg2) = pprUnary (sLit "frsp") reg1 reg2

pprInstr (CRNOR dst src1 src2) = hcat [
        text "\tcrnor\t",
        int dst,
        text ", ",
        int src1,
        text ", ",
        int src2
    ]

pprInstr (MFCR reg) = hcat [
        char '\t',
        text "mfcr",
        char '\t',
        pprReg reg
    ]

pprInstr (MFLR reg) = hcat [
        char '\t',
        text "mflr",
        char '\t',
        pprReg reg
    ]

pprInstr (FETCHPC reg) = vcat [
        text "\tbcl\t20,31,1f",
        hcat [ text "1:\tmflr\t", pprReg reg ]
    ]

pprInstr HWSYNC = text "\tsync"

pprInstr ISYNC  = text "\tisync"

pprInstr LWSYNC = text "\tlwsync"

pprInstr NOP = text "\tnop"


pprLogic :: PtrString -> Reg -> Reg -> RI -> SDoc
pprLogic op reg1 reg2 ri = hcat [
        char '\t',
        ptext op,
        case ri of
            RIReg _ -> empty
            RIImm _ -> char 'i',
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        pprRI ri
    ]


pprMul :: Format -> Reg -> Reg -> RI -> SDoc
pprMul fmt reg1 reg2 ri = hcat [
        char '\t',
        text "mull",
        case ri of
            RIReg _ -> case fmt of
              II32 -> char 'w'
              II64 -> char 'd'
              _    -> panic "PPC: illegal format"
            RIImm _ -> char 'i',
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        pprRI ri
    ]


pprDiv :: Format -> Bool -> Reg -> Reg -> Reg -> SDoc
pprDiv fmt sgn reg1 reg2 reg3 = hcat [
        char '\t',
        text "div",
        case fmt of
          II32 -> char 'w'
          II64 -> char 'd'
          _    -> panic "PPC: illegal format",
        if sgn then empty else char 'u',
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        pprReg reg3
    ]


pprUnary :: PtrString -> Reg -> Reg -> SDoc
pprUnary op reg1 reg2 = hcat [
        char '\t',
        ptext op,
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2
    ]


pprBinaryF :: PtrString -> Format -> Reg -> Reg -> Reg -> SDoc
pprBinaryF op fmt reg1 reg2 reg3 = hcat [
        char '\t',
        ptext op,
        pprFFormat fmt,
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
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
