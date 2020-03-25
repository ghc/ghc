-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
module GHC.CmmToAsm.PPC.Ppr (pprNatCmmDecl) where

import GhcPrelude

import GHC.CmmToAsm.PPC.Regs
import GHC.CmmToAsm.PPC.Instr
import GHC.CmmToAsm.PPC.Cond
import GHC.CmmToAsm.Ppr
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Format
import GHC.Platform.Reg
import GHC.Platform.Reg.Class
import GHC.CmmToAsm.Reg.Target
import GHC.CmmToAsm.Config

import GHC.Cmm hiding (topInfoTable)
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Label

import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.Ppr.Expr () -- For Outputable instances

import GHC.Types.Unique ( pprUniqueAlways, getUnique )
import GHC.Platform
import FastString
import Outputable
import GHC.Driver.Session (targetPlatform)

import Data.Word
import Data.Int
import Data.Bits

-- -----------------------------------------------------------------------------
-- Printing this stuff out

pprNatCmmDecl :: NCGConfig -> NatCmmDecl RawCmmStatics Instr -> SDoc
pprNatCmmDecl config (CmmData section dats) =
  pprSectionAlign config section
  $$ pprDatas (ncgPlatform config) dats

pprNatCmmDecl config proc@(CmmProc top_info lbl _ (ListGraph blocks)) =
  let platform = ncgPlatform config in
  case topInfoTable proc of
    Nothing ->
         -- special case for code without info table:
         pprSectionAlign config (Section Text lbl) $$
         (case platformArch platform of
            ArchPPC_64 ELF_V1 -> pprFunctionDescriptor lbl
            ArchPPC_64 ELF_V2 -> pprFunctionPrologue lbl
            _ -> pprLabel platform lbl) $$ -- blocks guaranteed not null,
                                           -- so label needed
         vcat (map (pprBasicBlock platform top_info) blocks)

    Just (RawCmmStatics info_lbl _) ->
      pprSectionAlign config (Section Text info_lbl) $$
      (if platformHasSubsectionsViaSymbols platform
          then ppr (mkDeadStripPreventer info_lbl) <> char ':'
          else empty) $$
      vcat (map (pprBasicBlock platform top_info) blocks) $$
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

pprBasicBlock :: Platform -> LabelMap RawCmmStatics -> NatBasicBlock Instr -> SDoc
pprBasicBlock platform info_env (BasicBlock blockid instrs)
  = maybe_infotable $$
    pprLabel platform (blockLbl blockid) $$
    vcat (map (pprInstr platform) instrs)
  where
    maybe_infotable = case mapLookup blockid info_env of
       Nothing   -> empty
       Just (RawCmmStatics info_lbl info) ->
           pprAlignForSection platform Text $$
           vcat (map (pprData platform) info) $$
           pprLabel platform info_lbl



pprDatas :: Platform -> RawCmmStatics -> SDoc
-- See note [emit-time elimination of static indirections] in CLabel.
pprDatas _platform (RawCmmStatics alias [CmmStaticLit (CmmLabel lbl), CmmStaticLit ind, _, _])
  | lbl == mkIndStaticInfoLabel
  , let labelInd (CmmLabelOff l _) = Just l
        labelInd (CmmLabel l) = Just l
        labelInd _ = Nothing
  , Just ind' <- labelInd ind
  , alias `mayRedirectTo` ind'
  = pprGloblDecl alias
    $$ text ".equiv" <+> ppr alias <> comma <> ppr (CmmLabel ind')
pprDatas platform (RawCmmStatics lbl dats) = vcat (pprLabel platform lbl : map (pprData platform) dats)

pprData :: Platform -> CmmStatic -> SDoc
pprData platform d = case d of
   CmmString str          -> pprString str
   CmmFileEmbed path      -> pprFileEmbed path
   CmmUninitialised bytes -> text ".space " <> int bytes
   CmmStaticLit lit       -> pprDataItem platform lit

pprGloblDecl :: CLabel -> SDoc
pprGloblDecl lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = text ".globl " <> ppr lbl

pprTypeAndSizeDecl :: Platform -> CLabel -> SDoc
pprTypeAndSizeDecl platform lbl
  = if platformOS platform == OSLinux && externallyVisibleCLabel lbl
    then text ".type " <>
         ppr lbl <> text ", @object"
    else empty

pprLabel :: Platform -> CLabel -> SDoc
pprLabel platform lbl =
   pprGloblDecl lbl
   $$ pprTypeAndSizeDecl platform lbl
   $$ (ppr lbl <> char ':')

-- -----------------------------------------------------------------------------
-- pprInstr: print an 'Instr'

instance Outputable Instr where
    ppr instr = sdocWithDynFlags $ \dflags ->
                  pprInstr (targetPlatform dflags) instr


pprReg :: Reg -> SDoc

pprReg r
  = case r of
      RegReal    (RealRegSingle i) -> ppr_reg_no i
      RegReal    (RealRegPair{})   -> panic "PPC.pprReg: no reg pairs on this arch"
      RegVirtual (VirtualRegI  u)  -> text "%vI_"   <> pprUniqueAlways u
      RegVirtual (VirtualRegHi u)  -> text "%vHi_"  <> pprUniqueAlways u
      RegVirtual (VirtualRegF  u)  -> text "%vF_"   <> pprUniqueAlways u
      RegVirtual (VirtualRegD  u)  -> text "%vD_"   <> pprUniqueAlways u

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
                FF64 -> sLit "fd")


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


pprSectionAlign :: NCGConfig -> Section -> SDoc
pprSectionAlign config sec@(Section seg _) =
   pprSectionHeader config sec $$
   pprAlignForSection (ncgPlatform config) seg

-- | Print appropriate alignment for the given section type.
pprAlignForSection :: Platform -> SectionType -> SDoc
pprAlignForSection platform seg =
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

pprDataItem :: Platform -> CmmLit -> SDoc
pprDataItem platform lit
  = vcat (ppr_item (cmmTypeFormat $ cmmLitType platform lit) lit)
    where
        imm = litToImm lit
        archPPC_64 = not $ target32Bit platform

        ppr_item II8  _ = [text "\t.byte\t" <> pprImm imm]
        ppr_item II16 _ = [text "\t.short\t" <> pprImm imm]
        ppr_item II32 _ = [text "\t.long\t" <> pprImm imm]
        ppr_item II64 _
           | archPPC_64 = [text "\t.quad\t" <> pprImm imm]

        ppr_item II64 (CmmInt x _)
           | not archPPC_64 =
                [text "\t.long\t"
                    <> int (fromIntegral
                        (fromIntegral (x `shiftR` 32) :: Word32)),
                 text "\t.long\t"
                    <> int (fromIntegral (fromIntegral x :: Word32))]


        ppr_item FF32 (CmmFloat r _)
           = let bs = floatToBytes (fromRational r)
             in  map (\b -> text "\t.byte\t" <> pprImm (ImmInt b)) bs

        ppr_item FF64 (CmmFloat r _)
           = let bs = doubleToBytes (fromRational r)
             in  map (\b -> text "\t.byte\t" <> pprImm (ImmInt b)) bs

        ppr_item _ _
                = panic "PPC.Ppr.pprDataItem: no match"


pprInstr :: Platform -> Instr -> SDoc
pprInstr platform instr = case instr of

   COMMENT _
      -> empty -- nuke 'em

   -- COMMENT s
   --    -> if platformOS platform == OSLinux
   --          then text "# " <> ftext s
   --          else text "; " <> ftext s

   DELTA d
      -> pprInstr platform (COMMENT (mkFastString ("\tdelta = " ++ show d)))

   NEWBLOCK _
      -> panic "PprMach.pprInstr: NEWBLOCK"

   LDATA _ _
      -> panic "PprMach.pprInstr: LDATA"

{-
   SPILL reg slot
      -> hcat [
              text "\tSPILL",
           char '\t',
           pprReg reg,
           comma,
           text "SLOT" <> parens (int slot)]

   RELOAD slot reg
      -> hcat [
              text "\tRELOAD",
           char '\t',
           text "SLOT" <> parens (int slot),
           comma,
           pprReg reg]
-}

   LD fmt reg addr
      -> hcat [
           char '\t',
           text "l",
           ptext (case fmt of
               II8  -> sLit "bz"
               II16 -> sLit "hz"
               II32 -> sLit "wz"
               II64 -> sLit "d"
               FF32 -> sLit "fs"
               FF64 -> sLit "fd"
               ),
           case addr of AddrRegImm _ _ -> empty
                        AddrRegReg _ _ -> char 'x',
           char '\t',
           pprReg reg,
           text ", ",
           pprAddr addr
       ]

   LDFAR fmt reg (AddrRegImm source off)
      -> vcat
            [ pprInstr platform (ADDIS (tmpReg platform) source (HA off))
            , pprInstr platform (LD fmt reg (AddrRegImm (tmpReg platform) (LO off)))
            ]

   LDFAR _ _ _
      -> panic "PPC.Ppr.pprInstr LDFAR: no match"

   LDR fmt reg1 addr
      -> hcat [
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

   LA fmt reg addr
      -> hcat [
           char '\t',
           text "l",
           ptext (case fmt of
               II8  -> sLit "ba"
               II16 -> sLit "ha"
               II32 -> sLit "wa"
               II64 -> sLit "d"
               FF32 -> sLit "fs"
               FF64 -> sLit "fd"
               ),
           case addr of AddrRegImm _ _ -> empty
                        AddrRegReg _ _ -> char 'x',
           char '\t',
           pprReg reg,
           text ", ",
           pprAddr addr
           ]

   ST fmt reg addr
      -> hcat [
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

   STFAR fmt reg (AddrRegImm source off)
      -> vcat [ pprInstr platform (ADDIS (tmpReg platform) source (HA off))
              , pprInstr platform (ST fmt reg (AddrRegImm (tmpReg platform) (LO off)))
              ]

   STFAR _ _ _
      -> panic "PPC.Ppr.pprInstr STFAR: no match"

   STU fmt reg addr
      -> hcat [
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

   STC fmt reg1 addr
      -> hcat [
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

   LIS reg imm
      -> hcat [
           char '\t',
           text "lis",
           char '\t',
           pprReg reg,
           text ", ",
           pprImm imm
           ]

   LI reg imm
      -> hcat [
           char '\t',
           text "li",
           char '\t',
           pprReg reg,
           text ", ",
           pprImm imm
           ]

   MR reg1 reg2
    | reg1 == reg2 -> empty
    | otherwise    -> hcat [
        char '\t',
        case targetClassOfReg platform reg1 of
            RcInteger -> text "mr"
            _ -> text "fmr",
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2
        ]

   CMP fmt reg ri
      -> hcat [
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

   CMPL fmt reg ri
      -> hcat [
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

   BCC cond blockid prediction
      -> hcat [
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

   BCCFAR cond blockid prediction
      -> vcat [
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

   JMP lbl _
     -- We never jump to ForeignLabels; if we ever do, c.f. handling for "BL"
     | isForeignLabel lbl -> panic "PPC.Ppr.pprInstr: JMP to ForeignLabel"
     | otherwise ->
       hcat [ -- an alias for b that takes a CLabel
           char '\t',
           text "b",
           char '\t',
           ppr lbl
       ]

   MTCTR reg
      -> hcat [
           char '\t',
           text "mtctr",
           char '\t',
           pprReg reg
        ]

   BCTR _ _ _
      -> hcat [
           char '\t',
           text "bctr"
         ]

   BL lbl _
      -> case platformOS platform of
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

   BCTRL _
      -> hcat [
             char '\t',
             text "bctrl"
         ]

   ADD reg1 reg2 ri
      -> pprLogic (sLit "add") reg1 reg2 ri

   ADDIS reg1 reg2 imm
      -> hcat [
           char '\t',
           text "addis",
           char '\t',
           pprReg reg1,
           text ", ",
           pprReg reg2,
           text ", ",
           pprImm imm
           ]

   ADDO reg1 reg2 reg3
      -> pprLogic (sLit "addo") reg1 reg2 (RIReg reg3)

   ADDC reg1 reg2 reg3
      -> pprLogic (sLit "addc") reg1 reg2 (RIReg reg3)

   ADDE reg1 reg2 reg3
      -> pprLogic (sLit "adde") reg1 reg2 (RIReg reg3)

   ADDZE reg1 reg2
      -> pprUnary (sLit "addze") reg1 reg2

   SUBF reg1 reg2 reg3
      -> pprLogic (sLit "subf") reg1 reg2 (RIReg reg3)

   SUBFO reg1 reg2 reg3
      -> pprLogic (sLit "subfo") reg1 reg2 (RIReg reg3)

   SUBFC reg1 reg2 ri
      -> hcat [
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

   SUBFE reg1 reg2 reg3
      -> pprLogic (sLit "subfe") reg1 reg2 (RIReg reg3)

   MULL fmt reg1 reg2 ri
      -> pprMul fmt reg1 reg2 ri

   MULLO fmt reg1 reg2 reg3
      -> hcat [
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

   MFOV fmt reg
      -> vcat [
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

   MULHU fmt reg1 reg2 reg3
      -> hcat [
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

   DIV fmt sgn reg1 reg2 reg3
      -> pprDiv fmt sgn reg1 reg2 reg3

        -- for some reason, "andi" doesn't exist.
        -- we'll use "andi." instead.
   AND reg1 reg2 (RIImm imm)
      -> hcat [
            char '\t',
            text "andi.",
            char '\t',
            pprReg reg1,
            text ", ",
            pprReg reg2,
            text ", ",
            pprImm imm
        ]

   AND reg1 reg2 ri
      -> pprLogic (sLit "and") reg1 reg2 ri

   ANDC reg1 reg2 reg3
      -> pprLogic (sLit "andc") reg1 reg2 (RIReg reg3)

   NAND reg1 reg2 reg3
      -> pprLogic (sLit "nand") reg1 reg2 (RIReg reg3)

   OR reg1 reg2 ri
      -> pprLogic (sLit "or") reg1 reg2 ri

   XOR reg1 reg2 ri
      -> pprLogic (sLit "xor") reg1 reg2 ri

   ORIS reg1 reg2 imm
      -> hcat [
            char '\t',
            text "oris",
            char '\t',
            pprReg reg1,
            text ", ",
            pprReg reg2,
            text ", ",
            pprImm imm
        ]

   XORIS reg1 reg2 imm
      -> hcat [
            char '\t',
            text "xoris",
            char '\t',
            pprReg reg1,
            text ", ",
            pprReg reg2,
            text ", ",
            pprImm imm
        ]

   EXTS fmt reg1 reg2
      -> hcat [
           char '\t',
           text "exts",
           pprFormat fmt,
           char '\t',
           pprReg reg1,
           text ", ",
           pprReg reg2
         ]

   CNTLZ fmt reg1 reg2
      -> hcat [
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

   NEG reg1 reg2
      -> pprUnary (sLit "neg") reg1 reg2

   NOT reg1 reg2
      -> pprUnary (sLit "not") reg1 reg2

   SR II32 reg1 reg2 (RIImm (ImmInt i))
    -- Handle the case where we are asked to shift a 32 bit register by
    -- less than zero or more than 31 bits. We convert this into a clear
    -- of the destination register.
    -- Fixes ticket https://gitlab.haskell.org/ghc/ghc/issues/5900
      | i < 0  || i > 31 -> pprInstr platform (XOR reg1 reg2 (RIReg reg2))

   SL II32 reg1 reg2 (RIImm (ImmInt i))
    -- As above for SR, but for left shifts.
    -- Fixes ticket https://gitlab.haskell.org/ghc/ghc/issues/10870
      | i < 0  || i > 31 -> pprInstr platform (XOR reg1 reg2 (RIReg reg2))

   SRA II32 reg1 reg2 (RIImm (ImmInt i))
    -- PT: I don't know what to do for negative shift amounts:
    -- For now just panic.
    --
    -- For shift amounts greater than 31 set all bit to the
    -- value of the sign bit, this also what sraw does.
      | i > 31 -> pprInstr platform (SRA II32 reg1 reg2 (RIImm (ImmInt 31)))

   SL fmt reg1 reg2 ri
      -> let op = case fmt of
                       II32 -> "slw"
                       II64 -> "sld"
                       _    -> panic "PPC.Ppr.pprInstr: shift illegal size"
         in pprLogic (sLit op) reg1 reg2 (limitShiftRI fmt ri)

   SR fmt reg1 reg2 ri
      -> let op = case fmt of
                       II32 -> "srw"
                       II64 -> "srd"
                       _    -> panic "PPC.Ppr.pprInstr: shift illegal size"
         in pprLogic (sLit op) reg1 reg2 (limitShiftRI fmt ri)

   SRA fmt reg1 reg2 ri
      -> let op = case fmt of
                       II32 -> "sraw"
                       II64 -> "srad"
                       _    -> panic "PPC.Ppr.pprInstr: shift illegal size"
         in pprLogic (sLit op) reg1 reg2 (limitShiftRI fmt ri)

   RLWINM reg1 reg2 sh mb me
      -> hcat [
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

   CLRLI fmt reg1 reg2 n
      -> hcat [
            text "\tclrl",
            pprFormat fmt,
            text "i ",
            pprReg reg1,
            text ", ",
            pprReg reg2,
            text ", ",
            int n
        ]

   CLRRI fmt reg1 reg2 n
      -> hcat [
            text "\tclrr",
            pprFormat fmt,
            text "i ",
            pprReg reg1,
            text ", ",
            pprReg reg2,
            text ", ",
            int n
        ]

   FADD fmt reg1 reg2 reg3
      -> pprBinaryF (sLit "fadd") fmt reg1 reg2 reg3

   FSUB fmt reg1 reg2 reg3
      -> pprBinaryF (sLit "fsub") fmt reg1 reg2 reg3

   FMUL fmt reg1 reg2 reg3
      -> pprBinaryF (sLit "fmul") fmt reg1 reg2 reg3

   FDIV fmt reg1 reg2 reg3
      -> pprBinaryF (sLit "fdiv") fmt reg1 reg2 reg3

   FABS reg1 reg2
      -> pprUnary (sLit "fabs") reg1 reg2

   FNEG reg1 reg2
      -> pprUnary (sLit "fneg") reg1 reg2

   FCMP reg1 reg2
      -> hcat [
           char '\t',
           text "fcmpu\t0, ",
               -- Note: we're using fcmpu, not fcmpo
               -- The difference is with fcmpo, compare with NaN is an invalid operation.
               -- We don't handle invalid fp ops, so we don't care.
               -- Moreover, we use `fcmpu 0, ...` rather than `fcmpu cr0, ...` for
               -- better portability since some non-GNU assembler (such as
               -- IBM's `as`) tend not to support the symbolic register name cr0.
               -- This matches the syntax that GCC seems to emit for PPC targets.
           pprReg reg1,
           text ", ",
           pprReg reg2
         ]

   FCTIWZ reg1 reg2
      -> pprUnary (sLit "fctiwz") reg1 reg2

   FCTIDZ reg1 reg2
      -> pprUnary (sLit "fctidz") reg1 reg2

   FCFID reg1 reg2
      -> pprUnary (sLit "fcfid") reg1 reg2

   FRSP reg1 reg2
      -> pprUnary (sLit "frsp") reg1 reg2

   CRNOR dst src1 src2
      -> hcat [
           text "\tcrnor\t",
           int dst,
           text ", ",
           int src1,
           text ", ",
           int src2
         ]

   MFCR reg
      -> hcat [
             char '\t',
             text "mfcr",
             char '\t',
             pprReg reg
         ]

   MFLR reg
      -> hcat [
           char '\t',
           text "mflr",
           char '\t',
           pprReg reg
         ]

   FETCHPC reg
      -> vcat [
             text "\tbcl\t20,31,1f",
             hcat [ text "1:\tmflr\t", pprReg reg ]
         ]

   HWSYNC
      -> text "\tsync"

   ISYNC
      -> text "\tisync"

   LWSYNC
      -> text "\tlwsync"

   NOP
      -> text "\tnop"

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
