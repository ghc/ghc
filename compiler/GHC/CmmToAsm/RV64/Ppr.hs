{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module GHC.CmmToAsm.RV64.Ppr (pprNatCmmDecl, pprInstr) where

import GHC.Prelude hiding (EQ)

import GHC.CmmToAsm.RV64.Instr
import GHC.CmmToAsm.RV64.Regs
import GHC.CmmToAsm.RV64.Cond
import GHC.CmmToAsm.Ppr
import GHC.CmmToAsm.Format
import GHC.Platform.Reg
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Utils

import GHC.Cmm hiding (topInfoTable)
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Label
import GHC.Types.Basic (Alignment, mkAlignment, alignmentBytes)

import GHC.Cmm.BlockId
import GHC.Cmm.CLabel

import GHC.Types.Unique ( pprUniqueAlways, getUnique )
import GHC.Platform
import GHC.Utils.Outputable

import GHC.Utils.Panic

pprProcAlignment :: IsDoc doc => NCGConfig -> doc
pprProcAlignment config = maybe empty (pprAlign platform . mkAlignment) (ncgProcAlignment config)
   where
      platform = ncgPlatform config

pprNatCmmDecl :: IsDoc doc => NCGConfig -> NatCmmDecl RawCmmStatics Instr -> doc
pprNatCmmDecl config (CmmData section dats) =
  pprSectionAlign config section $$ pprDatas config dats

pprNatCmmDecl config proc@(CmmProc top_info lbl _ (ListGraph blocks)) =
  let platform = ncgPlatform config in
  pprProcAlignment config $$
  case topInfoTable proc of
    Nothing ->
        -- special case for code without info table:
        pprSectionAlign config (Section Text lbl) $$
        -- do not
        -- pprProcAlignment config $$
        pprLabel platform lbl $$ -- blocks guaranteed not null, so label needed
        vcat (map (pprBasicBlock config top_info) blocks) $$
        ppWhen (ncgDwarfEnabled config)
          (line (pprBlockEndLabel platform lbl) $$ line (pprProcEndLabel platform lbl)) $$
        pprSizeDecl platform lbl

    Just (CmmStaticsRaw info_lbl _) ->
      pprSectionAlign config (Section Text info_lbl) $$
      -- pprProcAlignment config $$
      (if platformHasSubsectionsViaSymbols platform
          then line (pprAsmLabel platform (mkDeadStripPreventer info_lbl) <> char ':')
          else empty) $$
      vcat (map (pprBasicBlock config top_info) blocks) $$
      ppWhen (ncgDwarfEnabled config) (line (pprProcEndLabel platform info_lbl)) $$
      -- above: Even the first block gets a label, because with branch-chain
      -- elimination, it might be the target of a goto.
      (if platformHasSubsectionsViaSymbols platform
       then -- See Note [Subsections Via Symbols]
                line
              $ text "\t.long "
            <+> pprAsmLabel platform info_lbl
            <+> char '-'
            <+> pprAsmLabel platform (mkDeadStripPreventer info_lbl)
       else empty) $$
      pprSizeDecl platform info_lbl
{-# SPECIALIZE pprNatCmmDecl :: NCGConfig -> NatCmmDecl RawCmmStatics Instr -> SDoc #-}
{-# SPECIALIZE pprNatCmmDecl :: NCGConfig -> NatCmmDecl RawCmmStatics Instr -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

pprLabel :: IsDoc doc => Platform -> CLabel -> doc
pprLabel platform lbl =
   pprGloblDecl platform lbl
   $$ pprTypeDecl platform lbl
   $$ line (pprAsmLabel platform lbl <> char ':')

pprAlign :: IsDoc doc => Platform -> Alignment -> doc
pprAlign _platform alignment
        = line $ text "\t.balign " <> int (alignmentBytes alignment)

-- | Print appropriate alignment for the given section type.
pprAlignForSection :: IsDoc doc => Platform -> SectionType -> doc
pprAlignForSection _platform _seg
    -- .balign is stable, whereas .align is platform dependent.
    = line (text "\t.balign 8") --  always 8

-- | Print section header and appropriate alignment for that section.
--
-- This one will emit the header:
--
--     .section .text
--     .balign 8
--
pprSectionAlign :: IsDoc doc => NCGConfig -> Section -> doc
pprSectionAlign _config (Section (OtherSection _) _) =
  -- TODO: Valid for RISCV64?
  panic "AArch64.Ppr.pprSectionAlign: unknown section"
pprSectionAlign config sec@(Section seg _) =
    line (pprSectionHeader config sec)
    $$ pprAlignForSection (ncgPlatform config) seg

pprProcEndLabel :: IsLine doc => Platform -> CLabel -- ^ Procedure name
                -> doc
pprProcEndLabel platform lbl =
    pprAsmLabel platform (mkAsmTempProcEndLabel lbl) <> colon

pprBlockEndLabel :: IsLine doc => Platform -> CLabel -- ^ Block name
                 -> doc
pprBlockEndLabel platform lbl =
    pprAsmLabel platform (mkAsmTempEndLabel lbl) <> colon

-- | Output the ELF .size directive.
pprSizeDecl :: IsDoc doc => Platform -> CLabel -> doc
pprSizeDecl platform lbl
 = if osElfTarget (platformOS platform)
   then line (text "\t.size" <+> pprAsmLabel platform lbl <> text ", .-" <> pprAsmLabel platform lbl)
   else empty

pprBasicBlock :: IsDoc doc => NCGConfig -> LabelMap RawCmmStatics -> NatBasicBlock Instr
              -> doc
pprBasicBlock config info_env (BasicBlock blockid instrs)
  = maybe_infotable $
    pprLabel platform asmLbl $$
    vcat (map (pprInstr platform) (id {-detectTrivialDeadlock-} optInstrs)) $$
    ppWhen (ncgDwarfEnabled config) (
      -- Emit both end labels since this may end up being a standalone
      -- top-level block
      line (pprBlockEndLabel platform asmLbl
         <> pprProcEndLabel platform asmLbl)
    )
  where
    -- TODO: Check if we can  filter more instructions here.
    -- TODO: Shouldn't this be a more general check on a higher level?
    -- Filter out identity moves. E.g. mov x18, x18 will be dropped.
    optInstrs = filter f instrs
      where f (MOV o1 o2) | o1 == o2 = False
            f _ = True

    asmLbl = blockLbl blockid
    platform = ncgPlatform config
    maybe_infotable c = case mapLookup blockid info_env of
       Nothing   -> c
       Just (CmmStaticsRaw info_lbl info) ->
          --  pprAlignForSection platform Text $$
           infoTableLoc $$
           vcat (map (pprData config) info) $$
           pprLabel platform info_lbl $$
           c $$
           ppWhen (ncgDwarfEnabled config)
              (line (pprBlockEndLabel platform info_lbl))
    -- Make sure the info table has the right .loc for the block
    -- coming right after it. See Note [Info Offset]
    infoTableLoc = case instrs of
      (l@LOCATION{} : _) -> pprInstr platform l
      _other             -> empty

pprDatas :: IsDoc doc => NCGConfig -> RawCmmStatics -> doc
-- See Note [emit-time elimination of static indirections] in "GHC.Cmm.CLabel".
pprDatas config (CmmStaticsRaw alias [CmmStaticLit (CmmLabel lbl), CmmStaticLit ind, _, _])
  | lbl == mkIndStaticInfoLabel
  , let labelInd (CmmLabelOff l _) = Just l
        labelInd (CmmLabel l) = Just l
        labelInd _ = Nothing
  , Just ind' <- labelInd ind
  , alias `mayRedirectTo` ind'
  = pprGloblDecl (ncgPlatform config) alias
    $$ line (text ".equiv" <+> pprAsmLabel (ncgPlatform config) alias <> comma <> pprAsmLabel (ncgPlatform config) ind')

pprDatas config (CmmStaticsRaw lbl dats)
  = vcat (pprLabel platform lbl : map (pprData config) dats)
   where
      platform = ncgPlatform config

pprData :: IsDoc doc => NCGConfig -> CmmStatic -> doc
pprData _config (CmmString str) = line (pprString str)
pprData _config (CmmFileEmbed path _) = line (pprFileEmbed path)

-- TODO: AFAIK there no Darwin for RISCV, so we may consider to simplify this.
pprData config (CmmUninitialised bytes)
 = line $ let platform = ncgPlatform config
          in if platformOS platform == OSDarwin
                then text ".space " <> int bytes
                else text ".skip "  <> int bytes

pprData config (CmmStaticLit lit) = pprDataItem config lit

pprGloblDecl :: IsDoc doc => Platform -> CLabel -> doc
pprGloblDecl platform lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = line (text "\t.globl " <> pprAsmLabel platform lbl)

-- Note [Always use objects for info tables]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- See discussion in X86.Ppr for why this is necessary.  Essentially we need to
-- ensure that we never pass function symbols when we might want to lookup the
-- info table.  If we did, we could end up with procedure linking tables
-- (PLT)s, and thus the lookup wouldn't point to the function, but into the
-- jump table.
--
-- Fun fact: The LLVMMangler exists to patch this issue su on the LLVM side as
-- well.
pprLabelType' :: IsLine doc => Platform -> CLabel -> doc
pprLabelType' platform lbl =
  if isCFunctionLabel lbl || functionOkInfoTable then
    text "@function"
  else
    text "@object"
  where
    functionOkInfoTable = platformTablesNextToCode platform &&
      isInfoTableLabel lbl && not (isCmmInfoTableLabel lbl) && not (isConInfoTableLabel lbl)

-- this is called pprTypeAndSizeDecl in PPC.Ppr
pprTypeDecl :: IsDoc doc => Platform -> CLabel -> doc
pprTypeDecl platform lbl
    = if osElfTarget (platformOS platform) && externallyVisibleCLabel lbl
      then line (text ".type " <> pprAsmLabel platform lbl <> text ", " <> pprLabelType' platform lbl)
      else empty

pprDataItem :: IsDoc doc => NCGConfig -> CmmLit -> doc
pprDataItem config lit
  = lines_ (ppr_item (cmmTypeFormat $ cmmLitType platform lit) lit)
    where
        platform = ncgPlatform config

        imm = litToImm lit

        ppr_item II8  _ = [text "\t.byte\t"  <> pprImm platform imm]
        ppr_item II16 _ = [text "\t.short\t" <> pprImm platform imm]
        ppr_item II32 _ = [text "\t.long\t"  <> pprImm platform imm]
        ppr_item II64 _ = [text "\t.quad\t"  <> pprImm platform imm]

        ppr_item FF32  (CmmFloat r _)
           = let bs = floatToBytes (fromRational r)
             in  map (\b -> text "\t.byte\t" <> int (fromIntegral b)) bs

        ppr_item FF64 (CmmFloat r _)
           = let bs = doubleToBytes (fromRational r)
             in  map (\b -> text "\t.byte\t" <> int (fromIntegral b)) bs

        ppr_item _ _ = pprPanic "pprDataItem:ppr_item" (text $ show lit)

pprImm :: IsLine doc => Platform -> Imm -> doc
pprImm _ (ImmInt i)     = int i
pprImm _ (ImmInteger i) = integer i
pprImm p (ImmCLbl l)    = pprAsmLabel p l
pprImm p (ImmIndex l i) = pprAsmLabel p l <> char '+' <> int i
pprImm _ (ImmLit s)     = ftext s

-- TODO: See pprIm below for why this is a bad idea!
pprImm _ (ImmFloat f) = float (fromRational f)
pprImm _ (ImmDouble d) = double (fromRational d)

pprImm p (ImmConstantSum a b) = pprImm p a <> char '+' <> pprImm p b
pprImm p (ImmConstantDiff a b) = pprImm p a <> char '-'
                   <> lparen <> pprImm p b <> rparen


-- aarch64 GNU as uses // for comments.
asmComment :: SDoc -> SDoc
asmComment c = text "#" <+> c

asmDoubleslashComment :: SDoc -> SDoc
asmDoubleslashComment c = text "//" <+> c

asmMultilineComment :: SDoc -> SDoc
asmMultilineComment c =  text "/*" $+$ c $+$ text "*/"

pprIm :: IsLine doc => Platform -> Imm -> doc
pprIm platform im = case im of
  ImmInt i     -> int i
  ImmInteger i -> integer i

  -- FIXME: This is AArch64 commentry, not necesarily correct for RISCV!
  -- TODO: This will only work for
  -- The floating point value must be expressible as ±n ÷ 16 × 2^r,
  -- where n and r are integers such that 16 ≤ n ≤ 31 and -3 ≤ r ≤ 4.
  -- and 0 needs to be encoded as wzr/xzr.
  --
  -- Except for 0, we might want to either split it up into enough
  -- ADD operations into an Integer register and then just bit copy it into
  -- the double register? See the toBytes + fromRational above for data items.
  -- This is something the x86 backend does.
  --
  -- We could also just turn them into statics :-/ Which is what the
  -- PowerPC backend does.
  ImmFloat f | f == 0 -> text "zero"
  ImmFloat f -> char '#' <> float (fromRational f)
  ImmDouble d | d == 0 -> text "zero"
  ImmDouble d -> char '#' <> double (fromRational d)
  -- =<lbl> pseudo instruction!
  ImmCLbl l    -> char '=' <> pprAsmLabel platform l
  ImmIndex l o -> text "[=" <> pprAsmLabel platform l <> comma <+> char '#' <> int o <> char ']'
  _            -> panic "AArch64.pprIm"

negOp :: Operand -> Operand
negOp (OpImm (ImmInt i)) = OpImm (ImmInt (negate i))
negOp (OpImm (ImmInteger i)) = OpImm (ImmInteger (negate i))
negOp op = pprPanic "RV64.negOp" (text $ show op)

-- TODO: Is this used in RISCV?!
pprExt :: IsLine doc => ExtMode -> doc
pprExt EUXTB = text "uxtb"
pprExt EUXTH = text "uxth"
pprExt EUXTW = text "uxtw"
pprExt EUXTX = text "uxtx"
pprExt ESXTB = text "sxtb"
pprExt ESXTH = text "sxth"
pprExt ESXTW = text "sxtw"
pprExt ESXTX = text "sxtx"

-- TODO: Is this used in RISCV?!
pprShift :: IsLine doc => ShiftMode -> doc
pprShift SLSL = text "lsl"
pprShift SLSR = text "lsr"
pprShift SASR = text "asr"
pprShift SROR = text "ror"

pprOp :: IsLine doc => Platform -> Operand -> doc
pprOp plat op = case op of
  OpReg w r           -> pprReg w r
  OpRegExt w r x 0 -> pprReg w r <> comma <+> pprExt x
  OpRegExt w r x i -> pprReg w r <> comma <+> pprExt x <> comma <+> char '#' <> int i
  OpRegShift w r s i -> pprReg w r <> comma <+> pprShift s <+> char '#' <> int i
  OpImm im          -> pprIm plat im
  OpImmShift im s i -> pprIm plat im <> comma <+> pprShift s <+> char '#' <> int i
  OpAddr (AddrRegImm r1 im) -> pprImm plat im <> char '(' <> pprReg W64 r1 <> char ')'
  OpAddr (AddrReg r1)       -> text "0(" <+> pprReg W64 r1 <+> char ')'

pprReg :: forall doc. IsLine doc => Width -> Reg -> doc
pprReg w r = case r of
  RegReal    (RealRegSingle i) -> ppr_reg_no w i
  -- virtual regs should not show up, but this is helpful for debugging.
  RegVirtual (VirtualRegI u)   -> text "%vI_" <> pprUniqueAlways u
  RegVirtual (VirtualRegF u)   -> text "%vF_" <> pprUniqueAlways u
  RegVirtual (VirtualRegD u)   -> text "%vD_" <> pprUniqueAlways u
  _                            -> pprPanic "RiscV64.pprReg" (text $ show r)

  where
    -- TODO: Width is only used in error messages, so we could just remove it.
    ppr_reg_no :: Width -> Int -> doc
    -- General Purpose Registers
    ppr_reg_no _ 0 = text "zero"
    ppr_reg_no _ 1 = text "ra"
    ppr_reg_no _ 2 = text "sp"
    ppr_reg_no _ 3 = text "gp"
    ppr_reg_no _ 4 = text "tp"
    ppr_reg_no _ 5 = text "t0"
    ppr_reg_no _ 6 = text "t1"
    ppr_reg_no _ 7 = text "t2"
    ppr_reg_no _ 8 = text "s0"
    ppr_reg_no _ 9 = text "s1"
    ppr_reg_no _ 10 = text "a0"
    ppr_reg_no _ 11 = text "a1"
    ppr_reg_no _ 12 = text "a2"
    ppr_reg_no _ 13 = text "a3"
    ppr_reg_no _ 14 = text "a4"
    ppr_reg_no _ 15 = text "a5"
    ppr_reg_no _ 16 = text "a6"
    ppr_reg_no _ 17 = text "a7"
    ppr_reg_no _ 18 = text "s2"
    ppr_reg_no _ 19 = text "s3"
    ppr_reg_no _ 20 = text "s4"
    ppr_reg_no _ 21 = text "s5"
    ppr_reg_no _ 22 = text "s6"
    ppr_reg_no _ 23 = text "s7"
    ppr_reg_no _ 24 = text "s8"
    ppr_reg_no _ 25 = text "s9"
    ppr_reg_no _ 26 = text "s10"
    ppr_reg_no _ 27 = text "s11"
    ppr_reg_no _ 28 = text "t3"
    ppr_reg_no _ 29 = text "t4"
    ppr_reg_no _ 30 = text "t5"
    ppr_reg_no _ 31 = text "t6"

    -- Floating Point Registers
    ppr_reg_no _ 32 = text "ft0"
    ppr_reg_no _ 33 = text "ft1"
    ppr_reg_no _ 34 = text "ft2"
    ppr_reg_no _ 35 = text "ft3"
    ppr_reg_no _ 36 = text "ft4"
    ppr_reg_no _ 37 = text "ft5"
    ppr_reg_no _ 38 = text "ft6"
    ppr_reg_no _ 39 = text "ft7"
    ppr_reg_no _ 40 = text "fs0"
    ppr_reg_no _ 41 = text "fs1"
    ppr_reg_no _ 42 = text "fa0"
    ppr_reg_no _ 43 = text "fa1"
    ppr_reg_no _ 44 = text "fa2"
    ppr_reg_no _ 45 = text "fa3"
    ppr_reg_no _ 46 = text "fa4"
    ppr_reg_no _ 47 = text "fa5"
    ppr_reg_no _ 48 = text "fa6"
    ppr_reg_no _ 49 = text "fa7"
    ppr_reg_no _ 50 = text "fs2"
    ppr_reg_no _ 51 = text "fs3"
    ppr_reg_no _ 52 = text "fs4"
    ppr_reg_no _ 53 = text "fs5"
    ppr_reg_no _ 54 = text "fs6"
    ppr_reg_no _ 55 = text "fs7"
    ppr_reg_no _ 56 = text "fs8"
    ppr_reg_no _ 57 = text "fs9"
    ppr_reg_no _ 58 = text "fs10"
    ppr_reg_no _ 59 = text "fs11"
    ppr_reg_no _ 60 = text "ft8"
    ppr_reg_no _ 61 = text "ft9"
    ppr_reg_no _ 62 = text "ft10"
    ppr_reg_no _ 63 = text "ft11"

    ppr_reg_no w i
         | i < 0 = pprPanic "Unexpected register number (min is 0)" (ppr w <+> int i)
         | i > 63 = pprPanic "Unexpected register number (max is 63)" (ppr w <+> int i)
         -- no support for widths > W64.
         | otherwise = pprPanic "Unsupported width in register (max is 64)" (ppr w <+> int i)

isIntOp :: Operand -> Bool
isIntOp = not . isFloatOp

isFloatOp :: Operand -> Bool
isFloatOp (OpReg _ (RegReal (RealRegSingle i))) | i > 31 = True
isFloatOp (OpReg _ (RegVirtual (VirtualRegF _))) = True
isFloatOp (OpReg _ (RegVirtual (VirtualRegD _))) = True
isFloatOp _ = False

isSingleOp :: Operand -> Bool
isSingleOp (OpReg W32 _) = True
isSingleOp _ = False

isDoubleOp :: Operand -> Bool
isDoubleOp (OpReg W64 _) = True
isDoubleOp _ = False

isImmOp :: Operand -> Bool
isImmOp (OpImm _) = True
isImmOp _ = False

isImmZero :: Operand -> Bool
isImmZero (OpImm (ImmFloat 0)) = True
isImmZero (OpImm (ImmDouble 0)) = True
isImmZero (OpImm (ImmInt 0)) = True
isImmZero _ = False


isLabel :: Target -> Bool
isLabel (TBlock _) = True
isLabel (TLabel _) = True
isLabel _ = False

getLabel :: IsLine doc => Platform -> Target -> doc
getLabel platform (TBlock bid) = pprBlockId platform bid
getLabel platform (TLabel lbl) = pprAsmLabel platform lbl
getLabel _platform _other = panic "Cannot turn this into a label"

pprBlockId :: IsLine doc => Platform -> BlockId -> doc
pprBlockId platform bid = pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))

pprInstr :: IsDoc doc => Platform -> Instr -> doc
pprInstr platform instr = case instr of
  -- Meta Instructions ---------------------------------------------------------
  -- see Note [dualLine and dualDoc] in GHC.Utils.Outputable
  COMMENT s  -> dualDoc (asmComment s) empty
  MULTILINE_COMMENT s -> dualDoc (asmMultilineComment s) empty
  ANN d i -> dualDoc (pprInstr platform i <+> asmDoubleslashComment d) (pprInstr platform i)

  LOCATION file line' col _name
    -> line (text "\t.loc" <+> int file <+> int line' <+> int col)
  DELTA d   -> dualDoc (asmComment $ text "\tdelta = " <> int d) empty
  NEWBLOCK _ -> panic "PprInstr: NEWBLOCK"
  LDATA _ _  -> panic "pprInstr: LDATA"

  -- Pseudo Instructions -------------------------------------------------------

  PUSH_STACK_FRAME -> lines_ [ text "\taddi sp, sp, -16"
                             , text "\tsd x1, 8(sp)"     -- store RA
                             , text "\tsd x8, 0(sp)"     -- store FP/s0
                             , text "\taddi x8, sp, 16"]

  POP_STACK_FRAME -> lines_  [ text "\tld x8, 0(sp)" -- restore FP/s0
                             , text "\tld x1, 8(sp)" -- restore RA
                             , text "\taddi sp, sp, 16" ]
  -- ===========================================================================
  -- AArch64 Instruction Set
  -- 1. Arithmetic Instructions ------------------------------------------------
  ADD  o1 o2 o3
    | isFloatOp o1 && isFloatOp o2 && isFloatOp o3 -> op3 (text "\tfadd." <> if isSingleOp o1 then text "s" else text "d") o1 o2 o3
    -- This case is used for sign extension: SEXT.W op
    | OpReg W64 _ <- o1 , OpReg W32 _ <- o2, isImmOp o3 -> op3 (text "\taddiw") o1 o2 o3
    | otherwise -> op3 (text "\tadd") o1 o2 o3
  -- CMN  o1 o2    -> op2 (text "\tcmn") o1 o2
  -- CMP  o1 o2
  --   | isFloatOp o1 && isFloatOp o2 -> op2 (text "\tfcmp") o1 o2
  --   | otherwise -> op2 (text "\tcmp") o1 o2
  MUL  o1 o2 o3
    | isFloatOp o1 && isFloatOp o2 && isFloatOp o3 -> op3 (text "\tfmul." <> if isSingleOp o1 then text "s" else text "d") o1 o2 o3
    | otherwise -> op3 (text "\tmul") o1 o2 o3
  SMULH o1 o2 o3 -> op3 (text "\tmulh") o1 o2 o3
  SMULL o1 o2 o3 -> op3 (text "\tsmull") o1 o2 o3
  NEG o1 o2 | isFloatOp o1 && isFloatOp o2 && isSingleOp o2 -> op2 (text "\tfneg.s") o1 o2
  NEG o1 o2 | isFloatOp o1 && isFloatOp o2 && isDoubleOp o2 -> op2 (text "\tfneg.d") o1 o2
  NEG o1 o2 -> op2 (text "\tneg") o1 o2
  DIV o1 o2 o3 | isFloatOp o1 && isFloatOp o2 && isFloatOp o3
  -- TODO: This must (likely) be refined regarding width
    -> op3 (text "\tfdiv." <> if isSingleOp o1 then text "s" else text "d") o1 o2 o3
  DIV o1 o2 o3 -> op3 (text "\tdiv") o1 o2 o3
  REM o1 o2 o3 | isFloatOp o1 && isFloatOp o2 && isFloatOp o3
    -> panic $ "pprInstr - REM not implemented for floats (yet)"
  REM o1 o2 o3 -> op3 (text "\trem") o1 o2 o3
  REMU o1 o2 o3 -> op3 (text "\tremu") o1 o2 o3

  SUB  o1 o2 o3
    | isFloatOp o1 && isFloatOp o2 && isFloatOp o3 -> op3 (text "\tfsub." <> if isSingleOp o1 then text "s" else text "d") o1 o2 o3
    | isImmOp o3 -> op3 (text "\taddi") o1 o2 (negOp o3)
    | otherwise -> op3 (text "\tsub")  o1 o2 o3
  DIVU o1 o2 o3 -> op3 (text "\tdivu") o1 o2 o3

  -- 2. Bit Manipulation Instructions ------------------------------------------
  SBFM o1 o2 o3 o4 -> op4 (text "\tsbfm") o1 o2 o3 o4
  UBFM o1 o2 o3 o4 -> op4 (text "\tubfm") o1 o2 o3 o4
  -- signed and unsigned bitfield extract
  UBFX o1 o2 o3 o4 -> op4 (text "\tubfx") o1 o2 o3 o4

  -- 3. Logical and Move Instructions ------------------------------------------
  AND o1 o2 o3  -> op3 (text "\tand") o1 o2 o3
  OR o1 o2 o3   -> op3 (text "\tor") o1 o2 o3
  -- ANDS o1 o2 o3 -> op3 (text "\tands") o1 o2 o3
  ASR o1 o2 o3 | isImmOp o3 -> op3 (text "\tsrai") o1 o2 o3
  ASR o1 o2 o3  -> op3 (text "\tsra") o1 o2 o3
  BIC o1 o2 o3  -> op3 (text "\tbic") o1 o2 o3
  BICS o1 o2 o3 -> op3 (text "\tbics") o1 o2 o3
  XOR o1 o2 o3  -> op3 (text "\txor") o1 o2 o3
  LSL o1 o2 o3  -> op3 (text "\tsll") o1 o2 o3
  LSR o1 o2 o3  -> op3 (text "\tsrl") o1 o2 o3
  MOV o1 o2
    | isFloatOp o1 && isFloatOp o2 && isDoubleOp o2 -> op2 (text "\tfmv.d") o1 o2 -- fmv.d rd, rs is pseudo op fsgnj.d rd, rs, rs
    | isFloatOp o1 && isFloatOp o2 && isSingleOp o2 -> op2 (text "\tfmv.s") o1 o2 -- fmv.s rd, rs is pseudo op fsgnj.s rd, rs, rs
    | isFloatOp o1 && isImmZero o2 && isDoubleOp o1 -> op2 (text "\tfcvt.d.w") o1 zero
    | isFloatOp o1 && isImmZero o2 && isSingleOp o1 -> op2 (text "\tfcvt.s.w") o1 zero
    | isFloatOp o1 && not (isFloatOp o2) && isSingleOp o1 -> op2 (text "\tfmv.w.x") o1 o2
    | isFloatOp o1 && not (isFloatOp o2) && isDoubleOp o1 -> op2 (text "\tfmv.d.x") o1 o2
    | not (isFloatOp o1) && isFloatOp o2 && isSingleOp o2 -> op2 (text "\tfmv.x.w") o1 o2
    | not (isFloatOp o1) && isFloatOp o2 && isDoubleOp o2 -> op2 (text "\tfmv.x.d") o1 o2
    | (OpImm (ImmInteger i)) <- o2
    , fitsIn12bitImm i
          -> lines_ [ text "\taddi" <+> pprOp platform o1 <> comma <+> pprOp platform x0 <> comma <+> pprOp platform o2 ]
    | (OpImm (ImmInt i)) <- o2
    , fitsIn12bitImm i
          -> lines_ [ text "\taddi" <+> pprOp platform o1 <> comma <+> pprOp platform x0 <> comma <+> pprOp platform o2 ]
    | (OpImm (ImmInteger i)) <- o2
    , fitsIn32bits i
        -> lines_ [ text "\tlui" <+> pprOp platform o1 <> comma <+> text "%hi(" <> pprOp platform o2 <> text ")"
                                             , text "\taddw" <+> pprOp platform o1 <> comma <+> pprOp platform o1 <> comma <+> text "%lo(" <> pprOp platform o2 <> text ")" ]
    | (OpImm (ImmInt i)) <- o2
    , fitsIn32bits i
        -> lines_ [ text "\tlui" <+> pprOp platform o1 <> comma <+> text "%hi(" <> pprOp platform o2 <> text ")"
                                             , text "\taddw" <+> pprOp platform o1 <> comma <+> pprOp platform o1 <> comma <+> text "%lo(" <> pprOp platform o2 <> text ")" ]
    | isImmOp o2
        -- Surrender! Let the assembler figure out the right expressions with pseudo-op LI.
        -> lines_ [ text "\tli" <+> pprOp platform o1 <> comma <+>  pprOp platform o2 ]
    | otherwise                    -> op3 (text "\taddi") o1 o2 (OpImm (ImmInt 0))
  MOVK o1 o2    -> op2 (text "\tmovk") o1 o2
  ORN o1 o2 o3  -> op3 (text "\torn") o1 o2 o3
  ORI o1 o2 o3  -> op3 (text "\tori") o1 o2 o3
  XORI o1 o2 o3 -> op3 (text "\txori") o1 o2 o3
  ROR o1 o2 o3  -> op3 (text "\tror") o1 o2 o3
  TST o1 o2     -> op2 (text "\ttst") o1 o2

  -- 4. Branch Instructions ----------------------------------------------------
  J t             -> pprInstr platform (B t)
  -- TODO: This is odd: (B)ranch and branch and link (BL) do the same: branch and link
  B l | isLabel l -> line $ text "\tjal" <+> pprOp platform x0 <> comma <+> getLabel platform l
  B (TReg r)      -> line $ text "\tjalr" <+> text "x0" <> comma <+> pprReg W64 r <> comma <+> text "0"

  B_FAR bid  -> lines_ [ text "\tla" <+> pprOp platform ip <> comma <+> pprBlockId platform bid
                            , text "\tjalr" <+> text "x0" <> comma <+> pprOp platform ip <> comma <+> text "0" ]

  BL l _ _ | isLabel l-> line $ text "\tcall" <+> getLabel platform l
  BL (TReg r)     _ _ -> line $ text "\tjalr" <+> text "x1" <> comma <+> pprReg W64 r <> comma <+> text "0"

  BCOND c l r t | isLabel t ->
    line $ text "\t" <> pprBcond c <+> pprOp platform l <> comma <+> pprOp platform r <> comma <+> getLabel platform t

  BCOND _ _ _ (TReg _)     -> panic "RV64.ppr: No conditional branching to registers!"

  -- This is the far branches trick: Negate the condition and either do a
  -- register based jump (ignoring the link result in register zero) or just
  -- branch to the end of the block, jumping over the far jump instructions.
  BCOND_FAR c l r b t | isLabel t ->
    lines_ [ text "\t" <> pprBcond (negateCond c) <+> pprOp platform l <> comma <+> pprOp platform r <> comma <+> getLabel platform b <> text "_end"
           , text "\tla" <+> pprOp platform ip <> comma <+> getLabel platform t
           , text "\tjalr" <+> text "x0" <> comma <+> pprOp platform ip <> comma <+> text "0" 
           ]

  BCOND_FAR _ _ _ _ (TReg _)     -> panic "RV64.ppr: No conditional branching to registers!"


  -- 5. Atomic Instructions ----------------------------------------------------
  -- 6. Conditional Instructions -----------------------------------------------
  CSET o l r c  -> case c of
    EQ | isIntOp l && isIntOp r -> lines_ [ subFor l r
                  , text "\tseqz" <+> pprOp platform o <> comma <+> pprOp platform o]
    EQ | isFloatOp l && isFloatOp r -> line $ binOp ("\tfeq." ++ floatOpPrecision platform l r)
    NE | isIntOp l && isIntOp r -> lines_ [ subFor l r
                  , text "\tsnez" <+> pprOp platform o <> comma <+> pprOp platform o]
    --    feq.s   a0,fa0,fa1
    --    xori    a0,a0,1
    NE | isFloatOp l && isFloatOp r -> lines_ [binOp ("\tfeq." ++ floatOpPrecision platform l r)
                                              , text "\txori" <+>  pprOp platform o <> comma <+> pprOp platform o <> comma <+> text "1"]
    SLT -> lines_ [ sltFor l r <+> pprOp platform o <> comma <+> pprOp platform l <> comma <+> pprOp platform r ]
    SLE -> lines_ [ sltFor l r <+> pprOp platform o <> comma <+> pprOp platform r <> comma <+> pprOp platform l
                  , text "\txori" <+>  pprOp platform o <> comma <+> pprOp platform o <> comma <+> text "1" ]
    SGE -> lines_ [ sltFor l r <+> pprOp platform o <> comma <+> pprOp platform l <> comma <+> pprOp platform r
                  , text "\txori" <+>  pprOp platform o <> comma <+> pprOp platform o <> comma <+> text "1" ]
    SGT -> lines_ [ sltFor l r <+> pprOp platform o <> comma <+> pprOp platform r <> comma <+> pprOp platform l ]
    ULT -> lines_ [ sltuFor l r <+> pprOp platform o <> comma <+> pprOp platform l <> comma <+> pprOp platform r ]
    ULE -> lines_ [ sltuFor l r <+> pprOp platform o <> comma <+> pprOp platform r <> comma <+> pprOp platform l
                  , text "\txori" <+>  pprOp platform o <> comma <+> pprOp platform o <> comma <+> text "1" ]
    UGE -> lines_ [ sltuFor l r <+> pprOp platform o <> comma <+> pprOp platform l <> comma <+> pprOp platform r
                  , text "\txori" <+>  pprOp platform o <> comma <+> pprOp platform o <> comma <+> text "1" ]
    UGT -> lines_ [ sltuFor l r <+> pprOp platform o <> comma <+> pprOp platform r <> comma <+> pprOp platform l ]
    OLT | isFloatOp l && isFloatOp r -> line $ binOp ("\tflt." ++ floatOpPrecision platform l r)
    OLE | isFloatOp l && isFloatOp r -> line $ binOp ("\tfle." ++ floatOpPrecision platform l r)
    OGT | isFloatOp l && isFloatOp r -> line $ binOp ("\tfgt." ++ floatOpPrecision platform l r)
    OGE | isFloatOp l && isFloatOp r -> line $ binOp ("\tfge." ++ floatOpPrecision platform l r)
    x  -> pprPanic "RV64.ppr: unhandled CSET conditional" (text (show x) <+> pprOp platform o <> comma <+> pprOp platform r <> comma <+> pprOp platform l)
    where
      subFor l r | (OpImm _) <- r = text "\taddi" <+> pprOp platform o <> comma <+> pprOp platform l <> comma <+> pprOp platform (negOp r)
                 | (OpImm _) <- l = panic "RV64.ppr: Cannot SUB IMM _"
                 | otherwise      = text "\tsub" <+> pprOp platform o <> comma <+> pprOp platform l <> comma <+> pprOp platform r
      sltFor l r | (OpImm _) <- r = text "\tslti"
                 | (OpImm _) <- l = panic "PV64.ppr: Cannot SLT IMM _"
                 | otherwise      = text "\tslt"
      sltuFor l r| (OpImm _) <- r = text "\tsltui"
                 | (OpImm _) <- l = panic "PV64.ppr: Cannot SLTU IMM _"
                 | otherwise      = text "\tsltu"
      binOp :: (IsLine doc) => String -> doc
      binOp op = text op <+> pprOp platform o <> comma <+> pprOp platform l <> comma <+> pprOp platform r

  CBZ o (TBlock bid) -> line $ text "\tbeq x0, " <+> pprOp platform o <> comma <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
  CBZ o (TLabel lbl) -> line $ text "\tbeq x0, " <+> pprOp platform o <> comma <+> pprAsmLabel platform lbl
  CBZ _ (TReg _)     -> panic "AArch64.ppr: No conditional (cbz) branching to registers!"

  CBNZ o (TBlock bid) -> line $ text "\tbne x0, " <+> pprOp platform o <> comma <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
  CBNZ o (TLabel lbl) -> line $ text "\tbne x0, " <+> pprOp platform o <> comma <+> pprAsmLabel platform lbl
  CBNZ _ (TReg _)     -> panic "AArch64.ppr: No conditional (cbnz) branching to registers!"

  -- 7. Load and Store Instructions --------------------------------------------
  -- NOTE: GHC may do whacky things where it only load the lower part of an
  --       address. Not observing the correct size when loading will lead
  --       inevitably to crashes.
  STR II8  o1 o2 -> op2 (text "\tsb") o1 o2
  STR II16 o1 o2 -> op2 (text "\tsh") o1 o2
  STR II32 o1 o2 -> op2 (text "\tsw") o1 o2
  STR II64 o1 o2 -> op2 (text "\tsd") o1 o2
  STR FF32 o1 o2 -> op2 (text "\tfsw") o1 o2
  STR FF64 o1 o2 -> op2 (text "\tfsd") o1 o2

  LDR _f o1 (OpImm (ImmIndex lbl off)) ->
    lines_ [ text "\tla" <+> pprOp platform o1 <> comma <+> pprAsmLabel platform lbl
           , text "\taddi" <+> pprOp platform o1 <> comma <+> pprOp platform o1 <> comma <+> (int off)
           ]

  LDR _f o1 (OpImm (ImmCLbl lbl)) ->
    -- fixing this is _really_ annoyin we need to generate code like:
    -- 1: auipc x16, %pcrel_hi(<lbl>)
    --    addi x16, x16, %pcrel_lo(1b)
    -- I really dislike this (refer back to label 1 syntax from the assembler.)
    --
    -- So we'll go with pseudo ops. la and li it is.
    -- op_adrp o1 (text "%pcrel_hi(" <> pprAsmLabel platform lbl <> text ")") $$
    -- op_add o1 (text "%pcrel_lo(" <> pprAsmLabel platform lbl <> text ")")
    line $ text "\tla" <+> pprOp platform o1 <> comma <+> pprAsmLabel platform lbl

  LDR II8  o1 o2 -> op2 (text "\tlb") o1 o2
  LDR II16 o1 o2 -> op2 (text "\tlh") o1 o2
  LDR II32 o1 o2 -> op2 (text "\tlw") o1 o2
  LDR II64 o1 o2 -> op2 (text "\tld") o1 o2
  LDR FF32 o1 o2 -> op2 (text "\tflw") o1 o2
  LDR FF64 o1 o2 -> op2 (text "\tfld") o1 o2

  LDRU II8  o1 o2 -> op2 (text "\tlbu") o1 o2
  LDRU II16 o1 o2 -> op2 (text "\tlhu") o1 o2
  LDRU II32 o1 o2 -> op2 (text "\tlwu") o1 o2
  -- double words (64bit) cannot be sign extended by definition
  LDRU II64 o1 o2 -> op2 (text "\tld") o1 o2
  LDRU FF32 o1 o2@(OpAddr (AddrReg _)) -> op2 (text "\tflw") o1 o2
  LDRU FF32 o1 o2@(OpAddr (AddrRegImm _ _)) -> op2 (text "\tflw") o1 o2
  LDRU FF64 o1 o2@(OpAddr (AddrReg _)) -> op2 (text "\tfld") o1 o2
  LDRU FF64 o1 o2@(OpAddr (AddrRegImm _ _)) -> op2 (text "\tfld") o1 o2
  LDRU f o1 o2 -> pprPanic "Unsupported unsigned load" ((text.show) f <+> pprOp platform o1 <+> pprOp platform o2)
  -- LDAR _f o1 o2 -> op2 (text "\tldar") o1 o2

  -- STP _f o1 o2 o3 -> op3 (text "\tstp") o1 o2 o3
  -- LDP _f o1 o2 o3 -> op3 (text "\tldp") o1 o2 o3

  -- 8. Synchronization Instructions -------------------------------------------
  DMBSY r w -> line $ text "\tfence" <+> pprDmbType r <> char ',' <+> pprDmbType w

  -- 9. Floating Point Instructions --------------------------------------------
  FCVT o1@(OpReg W32 _) o2@(OpReg W64 _) -> op2 (text "\tfcvt.s.d") o1 o2
  FCVT o1@(OpReg W64 _) o2@(OpReg W32 _) -> op2 (text "\tfcvt.d.s") o1 o2
  FCVT o1 o2 -> pprPanic "RV64.pprInstr - impossible float conversion" $
                  line (pprOp platform o1 <> text "->" <> pprOp platform o2)

  SCVTF o1@(OpReg W32 _) o2@(OpReg W32 _) -> op2 (text "\tfcvt.s.w") o1 o2
  SCVTF o1@(OpReg W32 _) o2@(OpReg W64 _) -> op2 (text "\tfcvt.s.l") o1 o2
  SCVTF o1@(OpReg W64 _) o2@(OpReg W32 _) -> op2 (text "\tfcvt.d.w") o1 o2
  SCVTF o1@(OpReg W64 _) o2@(OpReg W64 _) -> op2 (text "\tfcvt.d.l") o1 o2
  SCVTF o1 o2 -> pprPanic "RV64.pprInstr - impossible integer to float conversion" $
                  line (pprOp platform o1 <> text "->" <> pprOp platform o2)

  FCVTZS o1@(OpReg W32 _) o2@(OpReg W32 _) -> op2 (text "\tfcvt.w.s") o1 o2
  FCVTZS o1@(OpReg W32 _) o2@(OpReg W64 _) -> op2 (text "\tfcvt.w.d") o1 o2
  FCVTZS o1@(OpReg W64 _) o2@(OpReg W32 _) -> op2 (text "\tfcvt.l.s") o1 o2
  FCVTZS o1@(OpReg W64 _) o2@(OpReg W64 _) -> op2 (text "\tfcvt.l.d") o1 o2
  FCVTZS o1 o2 -> pprPanic "RV64.pprInstr - impossible float to integer conversion" $
                  line (pprOp platform o1 <> text "->" <> pprOp platform o2)

  FABS o1 o2 | isSingleOp o2 -> op2 (text "\tfabs.s") o1 o2
  FABS o1 o2 | isDoubleOp o2 -> op2 (text "\tfabs.d") o1 o2
  instr -> panic $ "RV64.pprInstr - Unknown instruction: " ++ instrCon instr
 where op2 op o1 o2        = line $ op <+> pprOp platform o1 <> comma <+> pprOp platform o2
       op3 op o1 o2 o3     = line $ op <+> pprOp platform o1 <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o3
       op4 op o1 o2 o3 o4  = line $ op <+> pprOp platform o1 <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o3 <> comma <+> pprOp platform o4
      --  op_ldr o1 rest      = line $ text "\tld" <+> pprOp platform o1 <> comma <+> rest  <+> text "(" <> pprOp platform o1 <> text ")"
      --  op_adrp o1 rest     = line $ text "\tauipc" <+> pprOp platform o1 <> comma <+> rest
      --  op_add o1 rest      = line $ text "\taddi" <+> pprOp platform o1 <> comma <+> pprOp platform o1 <> comma <+> rest
       pprDmbType DmbRead = text "r"
       pprDmbType DmbWrite = text "w"
       pprDmbType DmbReadWrite = text "rw"

floatOpPrecision :: Platform -> Operand -> Operand -> String
floatOpPrecision _p l r | isFloatOp l && isFloatOp r && isSingleOp l && isSingleOp r = "s" -- single precision
floatOpPrecision _p l r | isFloatOp l && isFloatOp r && isDoubleOp l && isDoubleOp r = "d" -- double precision
floatOpPrecision p l r = pprPanic "Cannot determine floating point precission" (text "op1" <+> pprOp p l <+> text "op2" <+> pprOp p r)

pprBcond :: IsLine doc => Cond -> doc
pprBcond c = text "b" <> pprCond c

pprCond :: IsLine doc => Cond -> doc
pprCond c = case c of
    EQ  -> text "eq"
    NE  -> text "ne"
    SLT -> text "lt"
    SLE -> text "le"
    SGE -> text "ge"
    SGT -> text "gt"
    ULT -> text "ltu"
    ULE -> text "leu"
    UGE -> text "geu"
    UGT -> text "gtu"
    _   -> panic $ "RV64.ppr: unhandled BCOND conditional: " ++ show c
