{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module GHC.CmmToAsm.RISCV64.Ppr where

import GHC.Cmm hiding (topInfoTable)
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Label
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Ppr
import GHC.CmmToAsm.RISCV64.Instr hiding (pprInstr)
import GHC.CmmToAsm.Types
import GHC.Platform
import GHC.Types.Basic
import GHC.Utils.Outputable
import Prelude hiding ((<>))
import GHC.CmmToAsm.Utils
import GHC.Platform.Reg
import GHC.Utils.Panic
import GHC.Types.Unique

pprNatCmmDecl :: IsDoc doc => NCGConfig -> NatCmmDecl RawCmmStatics Instr -> doc
pprNatCmmDecl config (CmmData section dats) =
  pprSectionAlign config section $$ pprDatas config dats
pprNatCmmDecl config proc@(CmmProc top_info lbl _ (ListGraph blocks)) =
  let platform = ncgPlatform config
   in pprProcAlignment config
        $$ case topInfoTable proc of
          Nothing ->
            -- special case for code without info table:
            pprSectionAlign config (Section Text lbl)
              $$
              -- do not
              -- pprProcAlignment config $$
              pprLabel platform lbl
              $$ vcat (map (pprBasicBlock config top_info) blocks) -- blocks guaranteed not null, so label needed
              $$
              -- TODO: Is this call to pprSizeDecl needed? (Doc states this .size is only for source compatibility.)
              pprSizeDecl platform lbl
          Just (CmmStaticsRaw info_lbl _) -> error "TODO: pprNatCmmDecl : "

pprProcAlignment :: IsDoc doc => NCGConfig -> doc
pprProcAlignment config = maybe empty (pprAlign platform . mkAlignment) (ncgProcAlignment config)
  where
    platform = ncgPlatform config

pprAlign :: IsDoc doc => Platform -> Alignment -> doc
pprAlign _platform alignment =
  line $ text "\t.balign " <> int (alignmentBytes alignment)

pprSectionAlign :: IsDoc doc => NCGConfig -> Section -> doc
pprSectionAlign _config (Section (OtherSection _) _) =
  error "AArch64.Ppr.pprSectionAlign: unknown section"
pprSectionAlign config sec@(Section seg _) =
  line (pprSectionHeader config sec)
    $$ pprAlignForSection (ncgPlatform config) seg

-- | Print appropriate alignment for the given section type.
pprAlignForSection :: IsDoc doc => Platform -> SectionType -> doc
pprAlignForSection _platform _seg =
  -- .balign is stable, whereas .align is platform dependent.
  line (text "\t.balign 8") --  always 8

pprLabel :: IsDoc doc => Platform -> CLabel -> doc
pprLabel platform lbl =
  pprGloblDecl platform lbl
    $$ pprTypeDecl platform lbl
    $$ line (pprAsmLabel platform lbl <> char ':')

pprGloblDecl :: IsDoc doc => Platform -> CLabel -> doc
pprGloblDecl platform lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = line (text "\t.globl " <> pprAsmLabel platform lbl)

pprLabelType' :: IsLine doc => Platform -> CLabel -> doc
pprLabelType' platform lbl =
  if isCFunctionLabel lbl || functionOkInfoTable
    then text "@function"
    else text "@object"
  where
    functionOkInfoTable =
      platformTablesNextToCode platform
        && isInfoTableLabel lbl
        && not (isCmmInfoTableLabel lbl)
        && not (isConInfoTableLabel lbl)

-- this is called pprTypeAndSizeDecl in PPC.Ppr
pprTypeDecl :: IsDoc doc => Platform -> CLabel -> doc
pprTypeDecl platform lbl =
  if osElfTarget (platformOS platform) && externallyVisibleCLabel lbl
    then line (text ".type " <> pprAsmLabel platform lbl <> text ", " <> pprLabelType' platform lbl)
    else empty

-- | Output the ELF .size directive.
pprSizeDecl :: IsDoc doc => Platform -> CLabel -> doc
pprSizeDecl platform lbl =
  if osElfTarget (platformOS platform)
    then line (text "\t.size" <+> pprAsmLabel platform lbl <> text ", .-" <> pprAsmLabel platform lbl)
    else empty

pprBasicBlock ::
  IsDoc doc =>
  NCGConfig ->
  LabelMap RawCmmStatics ->
  NatBasicBlock Instr ->
  doc
pprBasicBlock config info_env (BasicBlock blockid instrs) =
  maybe_infotable $
    pprLabel platform asmLbl
      $$ vcat (map (pprInstr platform) instrs)
  where
    asmLbl = blockLbl blockid
    platform = ncgPlatform config
    maybe_infotable c = case mapLookup blockid info_env of
      Nothing -> c
      Just (CmmStaticsRaw info_lbl info) -> error "pprBasicBlock"

pprDatas :: IsDoc doc => NCGConfig -> RawCmmStatics -> doc
-- TODO: Adhere to Note [emit-time elimination of static indirections]
-- See AArch64/Ppr.hs
pprDatas config (CmmStaticsRaw lbl dats)
  = vcat (pprLabel platform lbl : map (pprData config) dats)
   where
      platform = ncgPlatform config

pprData :: IsDoc doc => NCGConfig -> CmmStatic -> doc
pprData _config (CmmString str) = line (pprString str)
pprData _ _ = error $ "TODO: pprData"

pprInstr :: IsDoc doc => Platform -> Instr -> doc
pprInstr platform instr = case instr of
  -- Meta Instructions ---------------------------------------------------------
  -- see Note [dualLine and dualDoc] in GHC.Utils.Outputable
  COMMENT s -> dualDoc (asmComment s) empty
  MULTILINE_COMMENT s -> dualDoc (asmMultilineComment s) empty
  ANN d i -> dualDoc (pprInstr platform i <+> asmDoubleslashComment d) (pprInstr platform i)
  DELTA d -> dualDoc (asmComment $ text "\tdelta = " <> int d) empty
  -- see Note [dualLine and dualDoc] in GHC.Utils.Outputable
  NEWBLOCK _ -> error "pprInstr: NEWBLOCK"
  LDATA _ _ -> error "pprInstr: LDATA"
  PUSH_STACK_FRAME -> error "pprInstr: PUSH_STACK_FRAME"
  POP_STACK_FRAME -> error "pprInstr: POP_STACK_FRAME"
  J label -> line $ pprJ label
  CALL label -> line $ text "\tcall" <+> pprAsmLabel platform label
  JALR reg -> line $ text "\tjalr" <+> text "ra" <> char ',' <+> pprReg reg <> char ',' <+> char '0'
  LI reg immediate -> line $ pprLI reg immediate
  LA reg label -> line $ text "\tla" <+> pprReg reg <> char ',' <+> pprAsmLabel platform label
  MV dst src -> line $ text "\tmv" <+> pprReg dst <> char ',' <+> pprReg src
  FMV_S dst src -> line $ text "\tfmv.s" <+> pprReg dst <> char ',' <+> pprReg src
  FMV_D dst src -> line $ text "\tfmv.d" <+> pprReg dst <> char ',' <+> pprReg src
  FMV_D_X dst src -> line $ text "\tfmv.d.x" <+> pprReg dst <> char ',' <+> pprReg src
  where
    pprLI :: IsLine doc => Reg -> Integer -> doc
    pprLI reg immediate = text "\tli" <+> pprReg reg <> char ',' <+> (text.show) immediate

    pprReg :: IsLine doc => Reg -> doc
    pprReg (RegReal (RealRegSingle regNo)) = (text.regNoToName) regNo
    pprReg virtualReg = (text . showPprUnsafe) virtualReg

    pprJ :: IsLine doc => Target -> doc
    pprJ (TBlock label) = text "\tj" <+> pprBlockId label
    pprJ (TLabel label) = text "\tj" <+> pprAsmLabel platform label
    pprJ (TReg reg) = panic $ "RISCV64 - Ppr.pprJ: Cannot J (jump) to registers. Requested register " ++ show reg

    pprBlockId:: IsLine doc => BlockId -> doc
    pprBlockId blockId = pprAsmLabel platform (mkLocalBlockLabel (getUnique blockId))



-- aarch64 GNU as uses // for comments.
asmComment :: SDoc -> SDoc
asmComment c = whenPprDebug $ text "#" <+> c

asmDoubleslashComment :: SDoc -> SDoc
asmDoubleslashComment c = whenPprDebug $ text "//" <+> c

asmMultilineComment :: SDoc -> SDoc
asmMultilineComment c = whenPprDebug $ text "/*" $+$ c $+$ text "*/"

regNoToName :: RegNo -> String
regNoToName  0 = "zero"
regNoToName  1 = "ra"
regNoToName  2 = "sp"
regNoToName  3 = "gp"
regNoToName  4 = "tp"
regNoToName  5 = "t0"
regNoToName  6 = "t1"
regNoToName  7 = "t2"
regNoToName  8 = "s0"
regNoToName  9 = "s1"
regNoToName 10 = "a0"
regNoToName 11 = "a1"
regNoToName 12 = "a2"
regNoToName 13 = "a3"
regNoToName 14 = "a4"
regNoToName 15 = "a5"
regNoToName 16 = "a6"
regNoToName 17 = "a7"
regNoToName 18 = "s2"
regNoToName 19 = "s3"
regNoToName 20 = "s4"
regNoToName 21 = "s5"
regNoToName 22 = "s6"
regNoToName 23 = "s7"
regNoToName 24 = "s8"
regNoToName 25 = "s9"
regNoToName 26 = "s10"
regNoToName 27 = "s11"
regNoToName 28 = "t3"
regNoToName 29 = "t4"
regNoToName 30 = "t5"
regNoToName 31 = "t6"
regNoToName 32 = "ft0"
regNoToName 33 = "ft1"
regNoToName 34 = "ft2"
regNoToName 35 = "ft3"
regNoToName 36 = "ft4"
regNoToName 37 = "ft5"
regNoToName 38 = "ft6"
regNoToName 39 = "ft7"
regNoToName 40 = "fs0"
regNoToName 41 = "fs1"
regNoToName 42 = "fa0"
regNoToName 43 = "fa1"
regNoToName 44 = "fa2"
regNoToName 45 = "fa3"
regNoToName 46 = "fa4"
regNoToName 47 = "fa5"
regNoToName 48 = "fa6"
regNoToName 49 = "fa7"
regNoToName 50 = "fs2"
regNoToName 51 = "fs3"
regNoToName 52 = "fs4"
regNoToName 53 = "fs5"
regNoToName 54 = "fs6"
regNoToName 55 = "fs7"
regNoToName 56 = "fs8"
regNoToName 57 = "fs9"
regNoToName 58 = "fs10"
regNoToName 59 = "fs11"
regNoToName 60 = "ft8"
regNoToName 61 = "ft9"
regNoToName 62 = "ft10"
regNoToName 63 = "ft11"
regNoToName regNo = panic $ "RISCV64: regToName: Unknown register number " ++ show regNo
