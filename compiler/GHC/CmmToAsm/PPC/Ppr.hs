{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

module GHC.CmmToAsm.PPC.Ppr
   ( pprNatCmmDecl
   , pprInstr
   )
where

import GHC.Prelude

import GHC.CmmToAsm.PPC.Regs
import GHC.CmmToAsm.PPC.Instr
import GHC.CmmToAsm.PPC.Cond
import GHC.CmmToAsm.Ppr
import GHC.CmmToAsm.Format
import GHC.Platform.Reg
import GHC.Platform.Reg.Class
import GHC.CmmToAsm.Reg.Target
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Utils

import GHC.Cmm hiding (topInfoTable)
import GHC.Cmm.Dataflow.Label.NonDet

import GHC.Cmm.BlockId
import GHC.Cmm.CLabel

import GHC.Types.Unique ( pprUniqueAlways, getUnique )
import GHC.Platform
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.Word
import Data.Int

-- -----------------------------------------------------------------------------
-- Printing this stuff out

pprNatCmmDecl :: IsDoc doc => NCGConfig -> NatCmmDecl RawCmmStatics Instr -> doc
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
            ArchPPC_64 ELF_V1 -> pprFunctionDescriptor platform lbl
            ArchPPC_64 ELF_V2 -> pprFunctionPrologue platform lbl
            _ -> pprLabel platform lbl) $$ -- blocks guaranteed not null,
                                           -- so label needed
         vcat (map (pprBasicBlock config top_info) blocks) $$
         ppWhen (ncgDwarfEnabled config) (line (pprAsmLabel platform (mkAsmTempEndLabel lbl)
                                                <> char ':') $$
                                          line (pprProcEndLabel platform lbl)) $$
         pprSizeDecl platform lbl

    Just (CmmStaticsRaw info_lbl _) ->
      pprSectionAlign config (Section Text info_lbl) $$
      (if platformHasSubsectionsViaSymbols platform
          then line (pprAsmLabel platform (mkDeadStripPreventer info_lbl) <> char ':')
          else empty) $$
      vcat (map (pprBasicBlock config top_info) blocks) $$
      -- above: Even the first block gets a label, because with branch-chain
      -- elimination, it might be the target of a goto.
      (if platformHasSubsectionsViaSymbols platform
       then
       -- See Note [Subsections Via Symbols] in X86/Ppr.hs
                line (text "\t.long "
                      <+> pprAsmLabel platform info_lbl
                      <+> char '-'
                      <+> pprAsmLabel platform (mkDeadStripPreventer info_lbl))
       else empty) $$
      pprSizeDecl platform info_lbl
{-# SPECIALIZE pprNatCmmDecl :: NCGConfig -> NatCmmDecl RawCmmStatics Instr -> SDoc #-}
{-# SPECIALIZE pprNatCmmDecl :: NCGConfig -> NatCmmDecl RawCmmStatics Instr -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Output the ELF .size directive.
pprSizeDecl :: IsDoc doc => Platform -> CLabel -> doc
pprSizeDecl platform lbl
 = if osElfTarget (platformOS platform)
   then line (text "\t.size" <+> prettyLbl <> text ", .-" <> codeLbl)
   else empty
  where
    prettyLbl = pprAsmLabel platform lbl
    codeLbl
      | platformArch platform == ArchPPC_64 ELF_V1 = char '.' <> prettyLbl
      | otherwise                                  = prettyLbl

pprFunctionDescriptor :: IsDoc doc => Platform -> CLabel -> doc
pprFunctionDescriptor platform lab =
  vcat [pprGloblDecl platform lab,
        line (text "\t.section \".opd\", \"aw\""),
        line (text "\t.align 3"),
        line (pprAsmLabel platform lab <> char ':'),
        line (text "\t.quad ."
              <>  pprAsmLabel platform lab
              <>  text ",.TOC.@tocbase,0"),
        line (text "\t.previous"),
        line (text "\t.type"
              <+> pprAsmLabel platform lab
              <>  text ", @function"),
        line (char '.' <> pprAsmLabel platform lab <> char ':')]

pprFunctionPrologue :: IsDoc doc => Platform -> CLabel -> doc
pprFunctionPrologue platform lab =
  vcat [pprGloblDecl platform lab,
        line (text ".type " <> pprAsmLabel platform lab <> text ", @function"),
        line (pprAsmLabel platform lab <> char ':'),
        line (text "0:\taddis\t" <> pprReg toc <> text ",12,.TOC.-0b@ha"),
        line (text "\taddi\t" <> pprReg toc <> char ',' <> pprReg toc <> text ",.TOC.-0b@l"),
        line (text "\t.localentry\t" <> pprAsmLabel platform lab <>
              text ",.-" <> pprAsmLabel platform lab)]

pprProcEndLabel :: IsLine doc => Platform -> CLabel -- ^ Procedure name
                -> doc
pprProcEndLabel platform lbl =
    pprAsmLabel platform (mkAsmTempProcEndLabel lbl) <> char ':'

pprBasicBlock :: IsDoc doc => NCGConfig -> LabelMap RawCmmStatics -> NatBasicBlock Instr
              -> doc
pprBasicBlock config info_env (BasicBlock blockid instrs)
  = maybe_infotable $$
    pprLabel platform asmLbl $$
    vcat (map (pprInstr platform) instrs) $$
    ppWhen (ncgDwarfEnabled config) (
      line (pprAsmLabel platform (mkAsmTempEndLabel asmLbl) <> char ':'
            <> pprProcEndLabel platform asmLbl)
    )
  where
    asmLbl = blockLbl blockid
    platform = ncgPlatform config
    maybe_infotable = case mapLookup blockid info_env of
       Nothing   -> empty
       Just (CmmStaticsRaw info_lbl info) ->
           pprAlignForSection platform Text $$
           vcat (map (pprData platform) info) $$
           pprLabel platform info_lbl



pprDatas :: IsDoc doc => Platform -> RawCmmStatics -> doc
-- See Note [emit-time elimination of static indirections] in "GHC.Cmm.CLabel".
pprDatas platform (CmmStaticsRaw alias [CmmStaticLit (CmmLabel lbl), CmmStaticLit ind, _, _])
  | lbl == mkIndStaticInfoLabel
  , let labelInd (CmmLabelOff l _) = Just l
        labelInd (CmmLabel l) = Just l
        labelInd _ = Nothing
  , Just ind' <- labelInd ind
  , alias `mayRedirectTo` ind'
  = pprGloblDecl platform alias
    $$ line (text ".equiv" <+> pprAsmLabel platform alias <> comma <> pprAsmLabel platform ind')
pprDatas platform (CmmStaticsRaw lbl dats) = vcat (pprLabel platform lbl : map (pprData platform) dats)

pprData :: IsDoc doc => Platform -> CmmStatic -> doc
pprData platform d = case d of
   CmmString str          -> line (pprString str)
   CmmFileEmbed path _    -> line (pprFileEmbed path)
   CmmUninitialised bytes -> line (text ".space " <> int bytes)
   CmmStaticLit lit       -> pprDataItem platform lit

pprGloblDecl :: IsDoc doc => Platform -> CLabel -> doc
pprGloblDecl platform lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = line (text ".globl " <> pprAsmLabel platform lbl)

pprTypeAndSizeDecl :: IsLine doc => Platform -> CLabel -> doc
pprTypeAndSizeDecl platform lbl
  = if platformOS platform == OSLinux && externallyVisibleCLabel lbl
    then text ".type " <>
         pprAsmLabel platform lbl <> text ", @object"
    else empty

pprLabel :: IsDoc doc => Platform -> CLabel -> doc
pprLabel platform lbl =
   pprGloblDecl platform lbl
   $$ line (pprTypeAndSizeDecl platform lbl)
   $$ line (pprAsmLabel platform lbl <> char ':')

-- -----------------------------------------------------------------------------
-- pprInstr: print an 'Instr'

pprReg :: forall doc. IsLine doc => Reg -> doc

pprReg r
  = case r of
      RegReal    (RealRegSingle i) -> ppr_reg_no i
      RegVirtual (VirtualRegI  u)  -> text "%vI_"   <> pprUniqueAlways u
      RegVirtual (VirtualRegHi u)  -> text "%vHi_"  <> pprUniqueAlways u
      RegVirtual (VirtualRegF  u)  -> text "%vF_"   <> pprUniqueAlways u
      RegVirtual (VirtualRegD  u)  -> text "%vD_"   <> pprUniqueAlways u

  where
    ppr_reg_no :: Int -> doc
    ppr_reg_no i
         | i <= 31   = int i      -- GPRs
         | i <= 63   = int (i-32) -- FPRs
         | otherwise = text "very naughty powerpc register"



pprFormat :: IsLine doc => Format -> doc
pprFormat x
 = case x of
                II8  -> text "b"
                II16 -> text "h"
                II32 -> text "w"
                II64 -> text "d"
                FF32 -> text "fs"
                FF64 -> text "fd"


pprCond :: IsLine doc => Cond -> doc
pprCond c
 = case c of {
                ALWAYS  -> text "";
                EQQ     -> text "eq";  NE    -> text "ne";
                LTT     -> text "lt";  GE    -> text "ge";
                GTT     -> text "gt";  LE    -> text "le";
                LU      -> text "lt";  GEU   -> text "ge";
                GU      -> text "gt";  LEU   -> text "le"; }


pprImm :: IsLine doc => Platform -> Imm -> doc
pprImm platform = \case
   ImmInt i       -> int i
   ImmInteger i   -> integer i
   ImmCLbl l      -> pprAsmLabel platform l
   ImmIndex l i   -> pprAsmLabel platform l <> char '+' <> int i
   ImmLit s       -> ftext s
   ImmFloat f     -> float $ fromRational f
   ImmDouble d    -> double $ fromRational d
   ImmConstantSum a b   -> pprImm platform a <> char '+' <> pprImm platform b
   ImmConstantDiff a b  -> pprImm platform a <> char '-' <> lparen <> pprImm platform b <> rparen
   LO (ImmInt i)        -> pprImm platform (LO (ImmInteger (toInteger i)))
   LO (ImmInteger i)    -> pprImm platform (ImmInteger (toInteger lo16))
        where
          lo16 = fromInteger (i .&. 0xffff) :: Int16

   LO i              -> pprImm platform i <> text "@l"
   HI i              -> pprImm platform i <> text "@h"
   HA (ImmInt i)     -> pprImm platform (HA (ImmInteger (toInteger i)))
   HA (ImmInteger i) -> pprImm platform (ImmInteger ha16)
        where
          ha16 = if lo16 >= 0x8000 then hi16+1 else hi16
          hi16 = (i `shiftR` 16)
          lo16 = i .&. 0xffff

   HA i        -> pprImm platform i <> text "@ha"
   HIGHERA i   -> pprImm platform i <> text "@highera"
   HIGHESTA i  -> pprImm platform i <> text "@highesta"


pprAddr :: IsLine doc => Platform -> AddrMode -> doc
pprAddr platform = \case
   AddrRegReg r1 r2             -> pprReg r1 <> char ',' <+> pprReg r2
   AddrRegImm r1 (ImmInt i)     -> hcat [ int i, char '(', pprReg r1, char ')' ]
   AddrRegImm r1 (ImmInteger i) -> hcat [ integer i, char '(', pprReg r1, char ')' ]
   AddrRegImm r1 imm            -> hcat [ pprImm platform imm, char '(', pprReg r1, char ')' ]


pprSectionAlign :: IsDoc doc => NCGConfig -> Section -> doc
pprSectionAlign config sec@(Section seg _) =
   line (pprSectionHeader config sec) $$
   pprAlignForSection (ncgPlatform config) seg

-- | Print appropriate alignment for the given section type.
pprAlignForSection :: IsDoc doc => Platform -> SectionType -> doc
pprAlignForSection platform seg = line $
 let ppc64    = not $ target32Bit platform
 in case seg of
       Text              -> text ".align 2"
       Data
        | ppc64          -> text ".align 3"
        | otherwise      -> text ".align 2"
       ReadOnlyData
        | ppc64          -> text ".align 3"
        | otherwise      -> text ".align 2"
       RelocatableReadOnlyData
        | ppc64          -> text ".align 3"
        | otherwise      -> text ".align 2"
       UninitialisedData
        | ppc64          -> text ".align 3"
        | otherwise      -> text ".align 2"
       -- TODO: This is copied from the ReadOnlyData case, but it can likely be
       -- made more efficient.
       InitArray         -> text ".align 3"
       FiniArray         -> text ".align 3"
       CString
        | ppc64          -> text ".align 3"
        | otherwise      -> text ".align 2"
       OtherSection _    -> panic "PprMach.pprSectionAlign: unknown section"

pprDataItem :: IsDoc doc => Platform -> CmmLit -> doc
pprDataItem platform lit
  = lines_ (ppr_item (cmmTypeFormat $ cmmLitType platform lit) lit)
    where
        imm = litToImm lit
        archPPC_64 = not $ target32Bit platform

        ppr_item II8  _ = [text "\t.byte\t"  <> pprImm platform imm]
        ppr_item II16 _ = [text "\t.short\t" <> pprImm platform imm]
        ppr_item II32 _ = [text "\t.long\t"  <> pprImm platform imm]
        ppr_item II64 _
           | archPPC_64 = [text "\t.quad\t"  <> pprImm platform imm]

        ppr_item II64 (CmmInt x _)
           | not archPPC_64 =
                [text "\t.long\t"
                    <> int (fromIntegral
                        (fromIntegral (x `shiftR` 32) :: Word32)),
                 text "\t.long\t"
                    <> int (fromIntegral (fromIntegral x :: Word32))]


        ppr_item FF32 _ = [text "\t.float\t" <> pprImm platform imm]
        ppr_item FF64 _ = [text "\t.double\t" <> pprImm platform imm]

        ppr_item _ _
                = panic "PPC.Ppr.pprDataItem: no match"


asmComment :: IsLine doc => doc -> doc
asmComment c = whenPprDebug $ text "#" <+> c


pprInstr :: IsDoc doc => Platform -> Instr -> doc
pprInstr platform instr = case instr of

   COMMENT s
      -> line (asmComment (ftext s))

   LOCATION file line' col _name
      -> line (text "\t.loc" <+> int file <+> int line' <+> int col)

   DELTA d
      -> line (asmComment $ text ("\tdelta = " ++ show d))

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
      -> line $ hcat [
           char '\t',
           text "l",
           (case fmt of
               II8  -> text "bz"
               II16 -> text "hz"
               II32 -> text "wz"
               II64 -> text "d"
               FF32 -> text "fs"
               FF64 -> text "fd"
               ),
           case addr of AddrRegImm _ _ -> empty
                        AddrRegReg _ _ -> char 'x',
           char '\t',
           pprReg reg,
           text ", ",
           pprAddr platform addr
       ]

   LDFAR fmt reg (AddrRegImm source off)
      -> vcat
            [ pprInstr platform (ADDIS (tmpReg platform) source (HA off))
            , pprInstr platform (LD fmt reg (AddrRegImm (tmpReg platform) (LO off)))
            ]

   LDFAR _ _ _
      -> panic "PPC.Ppr.pprInstr LDFAR: no match"

   LDR fmt reg1 addr
      -> line $ hcat [
           text "\tl",
           case fmt of
             II32 -> char 'w'
             II64 -> char 'd'
             _    -> panic "PPC.Ppr.Instr LDR: no match",
           text "arx\t",
           pprReg reg1,
           text ", ",
           pprAddr platform addr
           ]

   LA fmt reg addr
      -> line $ hcat [
           char '\t',
           text "l",
           (case fmt of
               II8  -> text "ba"
               II16 -> text "ha"
               II32 -> text "wa"
               II64 -> text "d"
               FF32 -> text "fs"
               FF64 -> text "fd"
               ),
           case addr of AddrRegImm _ _ -> empty
                        AddrRegReg _ _ -> char 'x',
           char '\t',
           pprReg reg,
           text ", ",
           pprAddr platform addr
           ]

   ST fmt reg addr
      -> line $ hcat [
           char '\t',
           text "st",
           pprFormat fmt,
           case addr of AddrRegImm _ _ -> empty
                        AddrRegReg _ _ -> char 'x',
           char '\t',
           pprReg reg,
           text ", ",
           pprAddr platform addr
           ]

   STFAR fmt reg (AddrRegImm source off)
      -> vcat [ pprInstr platform (ADDIS (tmpReg platform) source (HA off))
              , pprInstr platform (ST fmt reg (AddrRegImm (tmpReg platform) (LO off)))
              ]

   STFAR _ _ _
      -> panic "PPC.Ppr.pprInstr STFAR: no match"

   STU fmt reg addr
      -> line $ hcat [
           char '\t',
           text "st",
           pprFormat fmt,
           char 'u',
           case addr of AddrRegImm _ _ -> empty
                        AddrRegReg _ _ -> char 'x',
           char '\t',
           pprReg reg,
           text ", ",
           pprAddr platform addr
           ]

   STC fmt reg1 addr
      -> line $ hcat [
           text "\tst",
           case fmt of
             II32 -> char 'w'
             II64 -> char 'd'
             _    -> panic "PPC.Ppr.Instr STC: no match",
           text "cx.\t",
           pprReg reg1,
           text ", ",
           pprAddr platform addr
           ]

   LIS reg imm
      -> line $ hcat [
           char '\t',
           text "lis",
           char '\t',
           pprReg reg,
           text ", ",
           pprImm platform imm
           ]

   LI reg imm
      -> line $ hcat [
           char '\t',
           text "li",
           char '\t',
           pprReg reg,
           text ", ",
           pprImm platform imm
           ]

   MR reg1 reg2
    | reg1 == reg2 -> empty
    | otherwise    -> line $ hcat [
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
      -> line $ hcat [
           char '\t',
           op,
           char '\t',
           pprReg reg,
           text ", ",
           pprRI platform ri
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
      -> line $ hcat [
           char '\t',
           op,
           char '\t',
           pprReg reg,
           text ", ",
           pprRI platform ri
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
      -> line $ hcat [
           char '\t',
           text "b",
           pprCond cond,
           pprPrediction prediction,
           char '\t',
           pprAsmLabel platform lbl
           ]
         where lbl = mkLocalBlockLabel (getUnique blockid)
               pprPrediction p = case p of
                 Nothing    -> empty
                 Just True  -> char '+'
                 Just False -> char '-'

   BCCFAR cond blockid prediction
      -> lines_ [
           hcat [
               text "\tb",
               pprCond (condNegate cond),
               neg_prediction,
               text "\t$+8"
           ],
           hcat [
               text "\tb\t",
               pprAsmLabel platform lbl
           ]
          ]
          where lbl = mkLocalBlockLabel (getUnique blockid)
                neg_prediction = case prediction of
                  Nothing    -> empty
                  Just True  -> char '-'
                  Just False -> char '+'

   JMP lbl _
     | OSAIX <- platformOS platform ->
       line $ hcat [ -- an alias for b that takes a CLabel
           text "\tb.\t", -- add the ".", cf Note [AIX function descriptors and entry-code addresses]
           pprAsmLabel platform lbl
       ]

     | otherwise ->
       line $ hcat [ -- an alias for b that takes a CLabel
           text "\tb\t",
           pprAsmLabel platform lbl
       ]

   MTCTR reg
      -> line $ hcat [
           char '\t',
           text "mtctr",
           char '\t',
           pprReg reg
        ]

   BCTR _ _ _
      -> line $ hcat [
           char '\t',
           text "bctr"
         ]

   BL lbl _
      -> case platformOS platform of
           OSAIX ->
             -- Note [AIX function descriptors and entry-code addresses]
             -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             -- On AIX, "printf" denotes a function-descriptor (for use
             -- by function pointers), whereas the actual entry-code
             -- address is denoted by the dot-prefixed ".printf" label.
             -- Moreover, the PPC NCG only ever emits a BL instruction
             -- for calling C ABI functions. Most of the time these calls
             -- originate from FFI imports and have a 'ForeignLabel',
             -- but when profiling the codegen inserts calls via
             -- 'emitRtsCallGen' which are 'CmmLabel's even though
             -- they'd technically be more like 'ForeignLabel's.
             line $ hcat [
               text "\tbl\t.",
               pprAsmLabel platform lbl
             ]
           _ ->
             line $ hcat [
               text "\tbl\t",
               pprAsmLabel platform lbl
             ]

   BCTRL _
      -> line $ hcat [
             char '\t',
             text "bctrl"
         ]

   ADD reg1 reg2 ri
      -> pprLogic platform (text "add") reg1 reg2 ri

   ADDIS reg1 reg2 imm
      -> line $ hcat [
           char '\t',
           text "addis",
           char '\t',
           pprReg reg1,
           text ", ",
           pprReg reg2,
           text ", ",
           pprImm platform imm
           ]

   ADDO reg1 reg2 reg3
      -> pprLogic platform (text "addo") reg1 reg2 (RIReg reg3)

   ADDC reg1 reg2 reg3
      -> pprLogic platform (text "addc") reg1 reg2 (RIReg reg3)

   ADDE reg1 reg2 reg3
      -> pprLogic platform (text "adde") reg1 reg2 (RIReg reg3)

   ADDZE reg1 reg2
      -> pprUnary (text "addze") reg1 reg2

   SUBF reg1 reg2 reg3
      -> pprLogic platform (text "subf") reg1 reg2 (RIReg reg3)

   SUBFO reg1 reg2 reg3
      -> pprLogic platform (text "subfo") reg1 reg2 (RIReg reg3)

   SUBFC reg1 reg2 ri
      -> line $ hcat [
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
           pprRI platform ri
           ]

   SUBFE reg1 reg2 reg3
      -> pprLogic platform (text "subfe") reg1 reg2 (RIReg reg3)

   MULL fmt reg1 reg2 ri
      -> pprMul platform fmt reg1 reg2 ri

   MULLO fmt reg1 reg2 reg3
      -> line $ hcat [
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
           line $ hcat [
               char '\t',
               text "mfxer",
               char '\t',
               pprReg reg
               ],
           line $ hcat [
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
      -> line $ hcat [
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
      -> line $ hcat [
            char '\t',
            text "andi.",
            char '\t',
            pprReg reg1,
            text ", ",
            pprReg reg2,
            text ", ",
            pprImm platform imm
        ]

   AND reg1 reg2 ri
      -> pprLogic platform (text "and") reg1 reg2 ri

   ANDC reg1 reg2 reg3
      -> pprLogic platform (text "andc") reg1 reg2 (RIReg reg3)

   NAND reg1 reg2 reg3
      -> pprLogic platform (text "nand") reg1 reg2 (RIReg reg3)

   OR reg1 reg2 ri
      -> pprLogic platform (text "or") reg1 reg2 ri

   XOR reg1 reg2 ri
      -> pprLogic platform (text "xor") reg1 reg2 ri

   ORIS reg1 reg2 imm
      -> line $ hcat [
            char '\t',
            text "oris",
            char '\t',
            pprReg reg1,
            text ", ",
            pprReg reg2,
            text ", ",
            pprImm platform imm
        ]

   XORIS reg1 reg2 imm
      -> line $ hcat [
            char '\t',
            text "xoris",
            char '\t',
            pprReg reg1,
            text ", ",
            pprReg reg2,
            text ", ",
            pprImm platform imm
        ]

   EXTS fmt reg1 reg2
      -> line $ hcat [
           char '\t',
           text "exts",
           pprFormat fmt,
           char '\t',
           pprReg reg1,
           text ", ",
           pprReg reg2
         ]

   CNTLZ fmt reg1 reg2
      -> line $ hcat [
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
      -> pprUnary (text "neg") reg1 reg2

   NOT reg1 reg2
      -> pprUnary (text "not") reg1 reg2

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
                       II32 -> text "slw"
                       II64 -> text "sld"
                       _    -> panic "PPC.Ppr.pprInstr: shift illegal size"
         in pprLogic platform op reg1 reg2 (limitShiftRI fmt ri)

   SR fmt reg1 reg2 ri
      -> let op = case fmt of
                       II32 -> text "srw"
                       II64 -> text "srd"
                       _    -> panic "PPC.Ppr.pprInstr: shift illegal size"
         in pprLogic platform op reg1 reg2 (limitShiftRI fmt ri)

   SRA fmt reg1 reg2 ri
      -> let op = case fmt of
                       II32 -> text "sraw"
                       II64 -> text "srad"
                       _    -> panic "PPC.Ppr.pprInstr: shift illegal size"
         in pprLogic platform op reg1 reg2 (limitShiftRI fmt ri)

   RLWINM reg1 reg2 sh mb me
      -> line $ hcat [
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
      -> line $ hcat [
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
      -> line $ hcat [
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
      -> pprBinaryF (text "fadd") fmt reg1 reg2 reg3

   FSUB fmt reg1 reg2 reg3
      -> pprBinaryF (text "fsub") fmt reg1 reg2 reg3

   FMUL fmt reg1 reg2 reg3
      -> pprBinaryF (text "fmul") fmt reg1 reg2 reg3

   FDIV fmt reg1 reg2 reg3
      -> pprBinaryF (text "fdiv") fmt reg1 reg2 reg3

   FABS reg1 reg2
      -> pprUnary (text "fabs") reg1 reg2

   FNEG reg1 reg2
      -> pprUnary (text "fneg") reg1 reg2

   FMADD signs fmt dst ra rc rb
     -> pprTernaryF (pprFMASign signs) fmt dst ra rc rb

   FCMP reg1 reg2
      -> line $ hcat [
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
      -> pprUnary (text "fctiwz") reg1 reg2

   FCTIDZ reg1 reg2
      -> pprUnary (text "fctidz") reg1 reg2

   FCFID reg1 reg2
      -> pprUnary (text "fcfid") reg1 reg2

   FRSP reg1 reg2
      -> pprUnary (text "frsp") reg1 reg2

   CRNOR dst src1 src2
      -> line $ hcat [
           text "\tcrnor\t",
           int dst,
           text ", ",
           int src1,
           text ", ",
           int src2
         ]

   MFCR reg
      -> line $ hcat [
             char '\t',
             text "mfcr",
             char '\t',
             pprReg reg
         ]

   MFLR reg
      -> line $ hcat [
           char '\t',
           text "mflr",
           char '\t',
           pprReg reg
         ]

   FETCHPC reg
      -> lines_ [
             text "\tbcl\t20,31,1f",
             hcat [ text "1:\tmflr\t", pprReg reg ]
         ]

   HWSYNC
      -> line $ text "\tsync"

   ISYNC
      -> line $ text "\tisync"

   LWSYNC
      -> line $ text "\tlwsync"

   NOP
      -> line $ text "\tnop"

pprLogic :: IsDoc doc => Platform -> Line doc -> Reg -> Reg -> RI -> doc
pprLogic platform op reg1 reg2 ri = line $ hcat [
        char '\t',
        op,
        case ri of
            RIReg _ -> empty
            RIImm _ -> char 'i',
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        pprRI platform ri
    ]


pprMul :: IsDoc doc => Platform -> Format -> Reg -> Reg -> RI -> doc
pprMul platform fmt reg1 reg2 ri = line $ hcat [
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
        pprRI platform ri
    ]


pprDiv :: IsDoc doc => Format -> Bool -> Reg -> Reg -> Reg -> doc
pprDiv fmt sgn reg1 reg2 reg3 = line $ hcat [
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


pprUnary :: IsDoc doc => Line doc -> Reg -> Reg -> doc
pprUnary op reg1 reg2 = line $ hcat [
        char '\t',
        op,
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2
    ]


pprBinaryF :: IsDoc doc => Line doc -> Format -> Reg -> Reg -> Reg -> doc
pprBinaryF op fmt reg1 reg2 reg3 = line $ hcat [
        char '\t',
        op,
        pprFFormat fmt,
        char '\t',
        pprReg reg1,
        text ", ",
        pprReg reg2,
        text ", ",
        pprReg reg3
    ]

pprTernaryF :: IsDoc doc => Line doc -> Format -> Reg -> Reg -> Reg -> Reg -> doc
pprTernaryF op fmt rt ra rc rb = line $ hcat [
        char '\t',
        op,
        pprFFormat fmt,
        char '\t',
        pprReg rt,
        text ", ",
        pprReg ra,
        text ", ",
        pprReg rc,
        text ", ",
        pprReg rb
    ]

pprRI :: IsLine doc => Platform -> RI -> doc
pprRI _        (RIReg r) = pprReg r
pprRI platform (RIImm r) = pprImm platform r


pprFFormat :: IsLine doc => Format -> doc
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
