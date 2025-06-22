{-# LANGUAGE ScopedTypeVariables #-}

module GHC.CmmToAsm.RV64.Ppr (pprNatCmmDecl, pprInstr) where

import GHC.Cmm hiding (topInfoTable)
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow.Label
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Ppr
import GHC.CmmToAsm.RV64.Cond
import GHC.CmmToAsm.RV64.Instr
import GHC.CmmToAsm.RV64.Regs
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Utils
import GHC.Platform
import GHC.Platform.Reg
import GHC.Prelude hiding (EQ)
import GHC.Stack
import GHC.Types.Basic (Alignment, alignmentBytes, mkAlignment)
import GHC.Types.Unique (getUnique, pprUniqueAlways)
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.OrdList

pprNatCmmDecl :: forall doc. (IsDoc doc) => NCGConfig -> NatCmmDecl RawCmmStatics Instr -> doc
pprNatCmmDecl config (CmmData section dats) =
  pprSectionAlign config section $$ pprDatas config dats
pprNatCmmDecl config proc@(CmmProc top_info lbl _ (ListGraph blocks)) =
  let platform = ncgPlatform config

      pprProcAlignment :: doc
      pprProcAlignment = maybe empty (pprAlign . mkAlignment) (ncgProcAlignment config)
   in pprProcAlignment
        $$ case topInfoTable proc of
          Nothing ->
            -- special case for code without info table:
            pprSectionAlign config (Section Text lbl)
              $$
              -- do not
              -- pprProcAlignment config $$
              pprLabel platform lbl
              $$ vcat (map (pprBasicBlock config top_info) blocks) -- blocks guaranteed not null, so label needed
              $$ ppWhen
                (ncgDwarfEnabled config)
                (line (pprBlockEndLabel platform lbl) $$ line (pprProcEndLabel platform lbl))
              $$ pprSizeDecl platform lbl
          Just (CmmStaticsRaw info_lbl _) ->
            pprSectionAlign config (Section Text info_lbl)
              $$
              -- pprProcAlignment config $$
              ( if platformHasSubsectionsViaSymbols platform
                  then line (pprAsmLabel platform (mkDeadStripPreventer info_lbl) <> char ':')
                  else empty
              )
              $$ vcat (map (pprBasicBlock config top_info) blocks)
              $$ ppWhen (ncgDwarfEnabled config) (line (pprProcEndLabel platform info_lbl))
              $$
              -- above: Even the first block gets a label, because with branch-chain
              -- elimination, it might be the target of a goto.
              ( if platformHasSubsectionsViaSymbols platform
                  then -- See Note [Subsections Via Symbols]
                    line
                      $ text "\t.long "
                      <+> pprAsmLabel platform info_lbl
                      <+> char '-'
                      <+> pprAsmLabel platform (mkDeadStripPreventer info_lbl)
                  else empty
              )
              $$ pprSizeDecl platform info_lbl
{-# SPECIALIZE pprNatCmmDecl :: NCGConfig -> NatCmmDecl RawCmmStatics Instr -> SDoc #-}
{-# SPECIALIZE pprNatCmmDecl :: NCGConfig -> NatCmmDecl RawCmmStatics Instr -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

pprLabel :: (IsDoc doc) => Platform -> CLabel -> doc
pprLabel platform lbl =
  pprGloblDecl platform lbl
    $$ pprTypeDecl platform lbl
    $$ line (pprAsmLabel platform lbl <> char ':')

pprAlign :: (IsDoc doc) => Alignment -> doc
pprAlign alignment =
  -- "The .align directive for RISC-V is an alias to .p2align, which aligns to a
  -- power of two, so .align 2 means align to 4 bytes. Because the definition of
  -- the .align directive varies by architecture, it is recommended to use the
  -- unambiguous .p2align or .balign directives instead."
  -- (https://github.com/riscv-non-isa/riscv-asm-manual/blob/main/riscv-asm.md#-align)
  line $ text "\t.balign " <> int (alignmentBytes alignment)

-- | Print appropriate alignment for the given section type.
--
-- Currently, this always aligns to a full machine word (8 byte.) A future
-- improvement could be to really do this per section type (though, it's
-- probably not a big gain.)
pprAlignForSection :: (IsDoc doc) => SectionType -> doc
pprAlignForSection _seg = pprAlign . mkAlignment $ 8

-- | Print section header and appropriate alignment for that section.
--
-- This will e.g. emit a header like:
--
--     .section .text
--     .balign 8
pprSectionAlign :: (IsDoc doc) => NCGConfig -> Section -> doc
pprSectionAlign _config (Section (OtherSection _) _) =
  panic "RV64.Ppr.pprSectionAlign: unknown section"
pprSectionAlign config sec@(Section seg _) =
  line (pprSectionHeader config sec)
    $$ pprAlignForSection seg

pprProcEndLabel ::
  (IsLine doc) =>
  Platform ->
  -- | Procedure name
  CLabel ->
  doc
pprProcEndLabel platform lbl =
  pprAsmLabel platform (mkAsmTempProcEndLabel lbl) <> colon

pprBlockEndLabel ::
  (IsLine doc) =>
  Platform ->
  -- | Block name
  CLabel ->
  doc
pprBlockEndLabel platform lbl =
  pprAsmLabel platform (mkAsmTempEndLabel lbl) <> colon

-- | Output the ELF .size directive (if needed.)
pprSizeDecl :: (IsDoc doc) => Platform -> CLabel -> doc
pprSizeDecl platform lbl
  | osElfTarget (platformOS platform) =
      line $ text "\t.size" <+> asmLbl <> text ", .-" <> asmLbl
  where
    asmLbl = pprAsmLabel platform lbl
pprSizeDecl _ _ = empty

pprBasicBlock ::
  (IsDoc doc) =>
  NCGConfig ->
  LabelMap RawCmmStatics ->
  NatBasicBlock Instr ->
  doc
pprBasicBlock config info_env (BasicBlock blockid instrs) =
  maybe_infotable
    $ pprLabel platform asmLbl
    $$ (vcat . fromOL) (mapOL (pprInstr platform) (id {-detectTrivialDeadlock-} instrs'))
    $$ ppWhen
      (ncgDwarfEnabled config)
      ( -- Emit both end labels since this may end up being a standalone
        -- top-level block
        line
          ( pprBlockEndLabel platform asmLbl
              <> pprProcEndLabel platform asmLbl
          )
      )
  where
    instrs' = injectVectorConfig (toOL optInstrs)
    -- TODO: Check if we can  filter more instructions here.
    -- TODO: Shouldn't this be a more general check on a higher level? And, is this still needed?
    -- Filter out identity moves. E.g. mov x18, x18 will be dropped.
    optInstrs = filter f instrs
      where
        f (MOV o1 o2) | o1 == o2 = False
        f _ = True

    injectVectorConfig :: OrdList Instr -> OrdList Instr
    injectVectorConfig instrs = fst $ foldlOL injectVectorConfig' (nilOL, Nothing) instrs

    -- TODO: Fuse this with optInstrs
    -- TODO: Check config and only run this when vectors are configured
    -- TODO: Check if vectorMinBits is sufficient for the vector config
    injectVectorConfig' :: (OrdList Instr, Maybe Format) -> Instr -> (OrdList Instr, Maybe Format)
    injectVectorConfig' (accInstr, configuredVecFmt) currInstr =
      let configuredVecFmt' Nothing = Nothing
          configuredVecFmt' (Just fmt') = if isJumpishInstr currInstr then Nothing else Just fmt'
       in case (configuredVecFmt, instrVecFormat platform currInstr) of
            (_fmtA, Nothing) ->
              -- no vector instruction
              ( accInstr
                  `appOL` toOL
                    [ -- (MULTILINE_COMMENT (text "No vector instruction" <> colon <+> text (instrCon currInstr) <+> pprInstr platform currInstr)),
                      currInstr
                    ],
                configuredVecFmt' configuredVecFmt
              )
            (Nothing, Just fmtB) ->
              -- vector instruction, but no active config
              ( accInstr
                  `appOL` toOL
                    [ COMMENT (text "No active vector config. Setting" <+> ppr fmtB),
                      (configVec fmtB),
                      currInstr
                    ],
                configuredVecFmt' (Just fmtB)
              )
            (Just fmtA, Just fmtB) ->
              if fmtA == fmtB
                then
                  -- vectors already correctly configured
                  ( accInstr
                      `appOL` toOL
                        [ COMMENT (text "Active vector config. Keeping" <+> ppr fmtB),
                          currInstr
                        ],
                    configuredVecFmt' (Just fmtA)
                  )
                else
                  -- re-configure
                  ( accInstr
                      `appOL` toOL
                        [ (COMMENT (text "Wrong active vector config. Setting" <+> ppr fmtB)),
                          (configVec fmtB),
                          currInstr
                        ],
                    configuredVecFmt' (Just fmtB)
                  )

    configVec :: Format -> Instr
    configVec (VecFormat length fmt) =
      VSETIVLI
        (OpReg II64 zeroReg)
        (fromIntegral length)
        ((formatToWidth . scalarFormatFormat) fmt)
        M1
        TA
        MA
    configVec fmt = pprPanic "Unsupported vector configuration" ((text . show) fmt)

    asmLbl = blockLbl blockid
    platform = ncgPlatform config
    maybe_infotable c = case mapLookup blockid info_env of
      Nothing -> c
      Just (CmmStaticsRaw info_lbl info) ->
        --  pprAlignForSection platform Text $$
        infoTableLoc
          $$ vcat (map (pprData config) info)
          $$ pprLabel platform info_lbl
          $$ c
          $$ ppWhen
            (ncgDwarfEnabled config)
            (line (pprBlockEndLabel platform info_lbl))
    -- Make sure the info table has the right .loc for the block
    -- coming right after it. See Note [Info Offset]
    infoTableLoc = case instrs of
      (l@LOCATION {} : _) -> pprInstr platform l
      _other -> empty

pprDatas :: (IsDoc doc) => NCGConfig -> RawCmmStatics -> doc
-- See Note [emit-time elimination of static indirections] in "GHC.Cmm.CLabel".
pprDatas config (CmmStaticsRaw alias [CmmStaticLit (CmmLabel lbl), CmmStaticLit ind, _, _])
  | lbl == mkIndStaticInfoLabel,
    let labelInd (CmmLabelOff l _) = Just l
        labelInd (CmmLabel l) = Just l
        labelInd _ = Nothing,
    Just ind' <- labelInd ind,
    alias `mayRedirectTo` ind' =
      pprGloblDecl (ncgPlatform config) alias
        $$ line (text ".equiv" <+> pprAsmLabel (ncgPlatform config) alias <> comma <> pprAsmLabel (ncgPlatform config) ind')
pprDatas config (CmmStaticsRaw lbl dats) =
  vcat (pprLabel platform lbl : map (pprData config) dats)
  where
    platform = ncgPlatform config

pprData :: (IsDoc doc) => NCGConfig -> CmmStatic -> doc
pprData _config (CmmString str) = line (pprString str)
pprData _config (CmmFileEmbed path _) = line (pprFileEmbed path)
-- TODO: AFAIK there no Darwin for RISCV, so we may consider to simplify this.
pprData config (CmmUninitialised bytes) =
  line
    $ let platform = ncgPlatform config
       in if platformOS platform == OSDarwin
            then text ".space " <> int bytes
            else text ".skip " <> int bytes
pprData config (CmmStaticLit lit) = pprDataItem config lit

pprGloblDecl :: (IsDoc doc) => Platform -> CLabel -> doc
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
-- Fun fact: The LLVMMangler exists to patch this issue on the LLVM side as
-- well.
pprLabelType' :: (IsLine doc) => Platform -> CLabel -> doc
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
pprTypeDecl :: (IsDoc doc) => Platform -> CLabel -> doc
pprTypeDecl platform lbl =
  if osElfTarget (platformOS platform) && externallyVisibleCLabel lbl
    then line (text ".type " <> pprAsmLabel platform lbl <> text ", " <> pprLabelType' platform lbl)
    else empty

pprDataItem :: (IsDoc doc) => NCGConfig -> CmmLit -> doc
pprDataItem config lit =
  lines_ (ppr_item (cmmTypeFormat $ cmmLitType platform lit) lit)
  where
    platform = ncgPlatform config

    imm = litToImm lit

    ppr_item II8 _ = [text "\t.byte\t" <> pprDataImm platform imm]
    ppr_item II16 _ = [text "\t.half\t" <> pprDataImm platform imm]
    ppr_item II32 _ = [text "\t.long\t" <> pprDataImm platform imm]
    ppr_item II64 _ = [text "\t.quad\t" <> pprDataImm platform imm]
    ppr_item FF32 (CmmFloat r _) =
      let bs = floatToBytes (fromRational r)
       in map (\b -> text "\t.byte\t" <> int (fromIntegral b)) bs
    ppr_item FF64 (CmmFloat r _) =
      let bs = doubleToBytes (fromRational r)
       in map (\b -> text "\t.byte\t" <> int (fromIntegral b)) bs
    ppr_item _ _ = pprPanic "pprDataItem:ppr_item" (text $ show lit)

-- | Pretty print an immediate value in the @data@ section
--
-- This does not include any checks. We rely on the Assembler to check for
-- errors. Use `pprOpImm` for immediates in instructions (operands.)
pprDataImm :: (IsLine doc) => Platform -> Imm -> doc
pprDataImm _ (ImmInt i) = int i
pprDataImm _ (ImmInteger i) = integer i
pprDataImm p (ImmCLbl l) = pprAsmLabel p l
pprDataImm p (ImmIndex l i) = pprAsmLabel p l <> char '+' <> int i
pprDataImm _ (ImmLit s) = ftext s
pprDataImm _ (ImmFloat f) = float (fromRational f)
pprDataImm _ (ImmDouble d) = double (fromRational d)
pprDataImm p (ImmConstantSum a b) = pprDataImm p a <> char '+' <> pprDataImm p b
pprDataImm p (ImmConstantDiff a b) =
  pprDataImm p a
    <> char '-'
    <> lparen
    <> pprDataImm p b
    <> rparen

-- | Comment @c@ with @# c@
asmComment :: SDoc -> SDoc
asmComment c = text "#" <+> c

-- | Commen @c@ with @// c@
asmDoubleslashComment :: SDoc -> SDoc
asmDoubleslashComment c = text "//" <+> c

-- | Comment @c@ with @/* c */@ (multiline comment)
asmMultilineComment :: SDoc -> SDoc
asmMultilineComment c = text "/*" $+$ c $+$ text "*/"

-- | Pretty print an immediate operand of an instruction
--
-- The kinds of immediates we can use here is pretty limited: RISCV doesn't
-- support index expressions (as e.g. Aarch64 does.) Floating points need to
-- fit in range. As we don't need them, forbit them to save us from future
-- troubles.
pprOpImm :: (IsLine doc) => Platform -> Imm -> doc
pprOpImm platform im = case im of
  ImmInt i -> int i
  ImmInteger i -> integer i
  ImmCLbl l -> char '=' <> pprAsmLabel platform l
  _ -> pprPanic "RV64.Ppr.pprOpImm" (text "Unsupported immediate for instruction operands" <> colon <+> (text . show) im)

-- | Negate integer immediate operand
--
-- This function is partial and will panic if the operand is not an integer.
negOp :: Operand -> Operand
negOp (OpImm (ImmInt i)) = OpImm (ImmInt (negate i))
negOp (OpImm (ImmInteger i)) = OpImm (ImmInteger (negate i))
negOp op = pprPanic "RV64.negOp" (text $ show op)

pprOps :: (IsLine doc, HasCallStack) => Platform -> [Operand] -> doc
pprOps platform = hsep . map (pprOp platform)

-- | Pretty print an operand
pprOp :: (IsLine doc, HasCallStack) => Platform -> Operand -> doc
pprOp plat op = case op of
  OpReg fmt r -> pprReg fmt r
  OpImm im -> pprOpImm plat im
  OpAddr (AddrRegImm r1 im) -> pprOpImm plat im <> char '(' <> pprReg II64 r1 <> char ')'
  OpAddr (AddrReg r1) -> text "0(" <+> pprReg II64 r1 <+> char ')'

-- | Pretty print register with calling convention name
--
-- This representation makes it easier to reason about the emitted assembly
-- code.
pprReg :: forall doc. (IsLine doc, HasCallStack) => Format -> Reg -> doc
pprReg fmt r = assertFmtReg fmt r $ case r of
  RegReal (RealRegSingle i) -> ppr_reg_no i
  -- virtual regs should not show up, but this is helpful for debugging.
  RegVirtual (VirtualRegI u) -> text "%vI_" <> pprUniqueAlways u
  RegVirtual (VirtualRegD u) -> text "%vD_" <> pprUniqueAlways u
  _ -> pprPanic "RiscV64.pprReg" (text (show r) <+> ppr fmt)
  where
    ppr_reg_no :: Int -> doc
    -- General Purpose Registers
    ppr_reg_no 0 = text "zero"
    ppr_reg_no 1 = text "ra"
    ppr_reg_no 2 = text "sp"
    ppr_reg_no 3 = text "gp"
    ppr_reg_no 4 = text "tp"
    ppr_reg_no 5 = text "t0"
    ppr_reg_no 6 = text "t1"
    ppr_reg_no 7 = text "t2"
    ppr_reg_no 8 = text "s0"
    ppr_reg_no 9 = text "s1"
    ppr_reg_no 10 = text "a0"
    ppr_reg_no 11 = text "a1"
    ppr_reg_no 12 = text "a2"
    ppr_reg_no 13 = text "a3"
    ppr_reg_no 14 = text "a4"
    ppr_reg_no 15 = text "a5"
    ppr_reg_no 16 = text "a6"
    ppr_reg_no 17 = text "a7"
    ppr_reg_no 18 = text "s2"
    ppr_reg_no 19 = text "s3"
    ppr_reg_no 20 = text "s4"
    ppr_reg_no 21 = text "s5"
    ppr_reg_no 22 = text "s6"
    ppr_reg_no 23 = text "s7"
    ppr_reg_no 24 = text "s8"
    ppr_reg_no 25 = text "s9"
    ppr_reg_no 26 = text "s10"
    ppr_reg_no 27 = text "s11"
    ppr_reg_no 28 = text "t3"
    ppr_reg_no 29 = text "t4"
    ppr_reg_no 30 = text "t5"
    ppr_reg_no 31 = text "t6"
    -- Floating Point Registers
    ppr_reg_no 32 = text "ft0"
    ppr_reg_no 33 = text "ft1"
    ppr_reg_no 34 = text "ft2"
    ppr_reg_no 35 = text "ft3"
    ppr_reg_no 36 = text "ft4"
    ppr_reg_no 37 = text "ft5"
    ppr_reg_no 38 = text "ft6"
    ppr_reg_no 39 = text "ft7"
    ppr_reg_no 40 = text "fs0"
    ppr_reg_no 41 = text "fs1"
    ppr_reg_no 42 = text "fa0"
    ppr_reg_no 43 = text "fa1"
    ppr_reg_no 44 = text "fa2"
    ppr_reg_no 45 = text "fa3"
    ppr_reg_no 46 = text "fa4"
    ppr_reg_no 47 = text "fa5"
    ppr_reg_no 48 = text "fa6"
    ppr_reg_no 49 = text "fa7"
    ppr_reg_no 50 = text "fs2"
    ppr_reg_no 51 = text "fs3"
    ppr_reg_no 52 = text "fs4"
    ppr_reg_no 53 = text "fs5"
    ppr_reg_no 54 = text "fs6"
    ppr_reg_no 55 = text "fs7"
    ppr_reg_no 56 = text "fs8"
    ppr_reg_no 57 = text "fs9"
    ppr_reg_no 58 = text "fs10"
    ppr_reg_no 59 = text "fs11"
    ppr_reg_no 60 = text "ft8"
    ppr_reg_no 61 = text "ft9"
    ppr_reg_no 62 = text "ft10"
    ppr_reg_no 63 = text "ft11"
    ppr_reg_no 64 = text "v0"
    ppr_reg_no 65 = text "v1"
    ppr_reg_no 66 = text "v2"
    ppr_reg_no 67 = text "v3"
    ppr_reg_no 68 = text "v4"
    ppr_reg_no 69 = text "v5"
    ppr_reg_no 70 = text "v6"
    ppr_reg_no 71 = text "v7"
    ppr_reg_no 72 = text "v8"
    ppr_reg_no 73 = text "v9"
    ppr_reg_no 74 = text "v10"
    ppr_reg_no 75 = text "v11"
    ppr_reg_no 76 = text "v12"
    ppr_reg_no 77 = text "v13"
    ppr_reg_no 78 = text "v14"
    ppr_reg_no 79 = text "v15"
    ppr_reg_no 80 = text "v16"
    ppr_reg_no 81 = text "v17"
    ppr_reg_no 82 = text "v18"
    ppr_reg_no 83 = text "v19"
    ppr_reg_no 84 = text "v20"
    ppr_reg_no 85 = text "v21"
    ppr_reg_no 86 = text "v22"
    ppr_reg_no 87 = text "v23"
    ppr_reg_no 88 = text "v24"
    ppr_reg_no 89 = text "v25"
    ppr_reg_no 90 = text "v26"
    ppr_reg_no 91 = text "v27"
    ppr_reg_no 92 = text "v28"
    ppr_reg_no 93 = text "v29"
    ppr_reg_no 94 = text "v30"
    ppr_reg_no 95 = text "v31"
    ppr_reg_no i
      | i < 0 = pprPanic "Unexpected register number (min is 0)" (ppr fmt <+> int i)
      | i > 95 = pprPanic "Unexpected register number (max is 95)" (ppr fmt <+> int i)
      -- no support for widths > W64.
      | otherwise = pprPanic "Unsupported width in register (max is 95)" (ppr fmt <+> int i)

-- | Single precission `Operand` (floating-point)
isSingleOp :: Operand -> Bool
isSingleOp (OpReg FF32 _) = True
isSingleOp _ = False

-- | Double precission `Operand` (floating-point)
isDoubleOp :: Operand -> Bool
isDoubleOp (OpReg FF64 _) = True
isDoubleOp _ = False

-- | `Operand` is an immediate value
isImmOp :: Operand -> Bool
isImmOp (OpImm _) = True
isImmOp _ = False

-- | `Operand` is an immediate @0@ value
isImmZero :: Operand -> Bool
isImmZero (OpImm (ImmFloat 0)) = True
isImmZero (OpImm (ImmDouble 0)) = True
isImmZero (OpImm (ImmInt 0)) = True
isImmZero _ = False

-- | `Target` represents a label
isLabel :: Target -> Bool
isLabel (TBlock _) = True
isLabel _ = False

-- | Get the pretty-printed label from a `Target`
--
-- This function is partial and will panic if the `Target` is not a label.
getLabel :: (IsLine doc) => Platform -> Target -> doc
getLabel platform (TBlock bid) = pprBlockId platform bid
  where
    pprBlockId :: (IsLine doc) => Platform -> BlockId -> doc
    pprBlockId platform bid = pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
getLabel _platform _other = panic "Cannot turn this into a label"

-- | Pretty-print an `Instr`
--
-- This function is partial and will panic if the `Instr` is not supported. This
-- can happen due to invalid operands or unexpected meta instructions.
pprInstr :: (IsDoc doc, HasCallStack) => Platform -> Instr -> doc
pprInstr platform instr = case instr of
  -- see Note [dualLine and dualDoc] in GHC.Utils.Outputable
  COMMENT s -> dualDoc (asmComment s) empty
  MULTILINE_COMMENT s -> dualDoc (asmMultilineComment s) empty
  ANN d i -> dualDoc (pprInstr platform i <+> asmDoubleslashComment d) (pprInstr platform i)
  LOCATION file line' col _name ->
    line (text "\t.loc" <+> int file <+> int line' <+> int col)
  DELTA d -> dualDoc (asmComment $ text "\tdelta = " <> int d) empty
  NEWBLOCK _ -> panic "PprInstr: NEWBLOCK"
  LDATA _ _ -> panic "pprInstr: LDATA"
  PUSH_STACK_FRAME ->
    lines_
      [ text "\taddi sp, sp, -16",
        text "\tsd x1, 8(sp)", -- store RA
        text "\tsd x8, 0(sp)", -- store FP/s0
        text "\taddi x8, sp, 16"
      ]
  POP_STACK_FRAME ->
    lines_
      [ text "\tld x8, 0(sp)", -- restore FP/s0
        text "\tld x1, 8(sp)", -- restore RA
        text "\taddi sp, sp, 16"
      ]
  ADD o1 o2 o3
    | isFloatOp o1 && isFloatOp o2 && isFloatOp o3 -> op3 (text "\tfadd." <> if isSingleOp o1 then text "s" else text "d") o1 o2 o3
    -- This case is used for sign extension: SEXT.W op
    | OpReg II64 _ <- o1, OpReg II32 _ <- o2, isImmOp o3 -> op3 (text "\taddiw") o1 o2 o3
    | otherwise -> op3 (text "\tadd") o1 o2 o3
  MUL o1 o2 o3
    | isFloatOp o1 && isFloatOp o2 && isFloatOp o3 -> op3 (text "\tfmul." <> if isSingleOp o1 then text "s" else text "d") o1 o2 o3
    | otherwise -> op3 (text "\tmul") o1 o2 o3
  MULH o1 o2 o3 -> op3 (text "\tmulh") o1 o2 o3
  NEG o1 o2 | isFloatOp o1 && isFloatOp o2 && isSingleOp o2 -> op2 (text "\tfneg.s") o1 o2
  NEG o1 o2 | isFloatOp o1 && isFloatOp o2 && isDoubleOp o2 -> op2 (text "\tfneg.d") o1 o2
  NEG o1 o2 -> op2 (text "\tneg") o1 o2
  DIV o1 o2 o3
    | isFloatOp o1 && isFloatOp o2 && isFloatOp o3 ->
        -- TODO: This must (likely) be refined regarding width
        op3 (text "\tfdiv." <> if isSingleOp o1 then text "s" else text "d") o1 o2 o3
  DIV o1 o2 o3 -> op3 (text "\tdiv") o1 o2 o3
  REM o1 o2 o3
    | isFloatOp o1 || isFloatOp o2 || isFloatOp o3 ->
        panic "pprInstr - REM not implemented for floats (yet)"
  REM o1 o2 o3 -> op3 (text "\trem") o1 o2 o3
  REMU o1 o2 o3 -> op3 (text "\tremu") o1 o2 o3
  SUB o1 o2 o3
    | isFloatOp o1 && isFloatOp o2 && isFloatOp o3 -> op3 (text "\tfsub." <> if isSingleOp o1 then text "s" else text "d") o1 o2 o3
    | isImmOp o3 -> op3 (text "\taddi") o1 o2 (negOp o3)
    | otherwise -> op3 (text "\tsub") o1 o2 o3
  DIVU o1 o2 o3 -> op3 (text "\tdivu") o1 o2 o3
  AND o1 o2 o3
    | isImmOp o3 -> op3 (text "\tandi") o1 o2 o3
    | otherwise -> op3 (text "\tand") o1 o2 o3
  OR o1 o2 o3 -> op3 (text "\tor") o1 o2 o3
  SRA o1 o2 o3 | isImmOp o3 -> op3 (text "\tsrai") o1 o2 o3
  SRA o1 o2 o3 -> op3 (text "\tsra") o1 o2 o3
  XOR o1 o2 o3 -> op3 (text "\txor") o1 o2 o3
  SLL o1 o2 o3 -> op3 (text "\tsll") o1 o2 o3
  SRL o1 o2 o3 -> op3 (text "\tsrl") o1 o2 o3
  MOV o1 o2
    | isFloatOp o1 && isFloatOp o2 && isDoubleOp o2 -> op2 (text "\tfmv.d") o1 o2 -- fmv.d rd, rs is pseudo op fsgnj.d rd, rs, rs
    | isFloatOp o1 && isFloatOp o2 && isSingleOp o2 -> op2 (text "\tfmv.s") o1 o2 -- fmv.s rd, rs is pseudo op fsgnj.s rd, rs, rs
    | isFloatOp o1 && isImmZero o2 && isDoubleOp o1 -> op2 (text "\tfcvt.d.w") o1 zero
    | isFloatOp o1 && isImmZero o2 && isSingleOp o1 -> op2 (text "\tfcvt.s.w") o1 zero
    | isFloatOp o1 && isImmOp o2 -> pprPanic "Unsupported move of immediate to floating point register" (pprOp platform o1 <+> pprOp platform o2)
    | isFloatRegOp o1 && isIntRegOp o2 && isSingleOp o1 -> op2 (text "\tfmv.w.x") o1 o2
    | isFloatRegOp o1 && isIntRegOp o2 && isDoubleOp o1 -> op2 (text "\tfmv.d.x") o1 o2
    | isIntRegOp o1 && isFloatRegOp o2 && isSingleOp o2 -> op2 (text "\tfmv.x.w") o1 o2
    | isIntRegOp o1 && isFloatRegOp o2 && isDoubleOp o2 -> op2 (text "\tfmv.x.d") o1 o2
    -- TODO: Why does this NOP (reg1 == reg2) happen?
    | isVectorRegOp o1 && isVectorRegOp o2 -> op2 (text "\tvmv.v.v") o1 o2
    | (OpImm (ImmInteger i)) <- o2,
      fitsIn12bitImm i ->
        lines_ [text "\taddi" <+> pprOp platform o1 <> comma <+> pprOp platform x0 <> comma <+> pprOp platform o2]
    | (OpImm (ImmInt i)) <- o2,
      fitsIn12bitImm i ->
        lines_ [text "\taddi" <+> pprOp platform o1 <> comma <+> pprOp platform x0 <> comma <+> pprOp platform o2]
    | (OpImm (ImmInteger i)) <- o2,
      fitsIn32bits i ->
        lines_
          [ text "\tlui" <+> pprOp platform o1 <> comma <+> text "%hi(" <> pprOp platform o2 <> text ")",
            text "\taddw" <+> pprOp platform o1 <> comma <+> pprOp platform o1 <> comma <+> text "%lo(" <> pprOp platform o2 <> text ")"
          ]
    | (OpImm (ImmInt i)) <- o2,
      fitsIn32bits i ->
        lines_
          [ text "\tlui" <+> pprOp platform o1 <> comma <+> text "%hi(" <> pprOp platform o2 <> text ")",
            text "\taddw" <+> pprOp platform o1 <> comma <+> pprOp platform o1 <> comma <+> text "%lo(" <> pprOp platform o2 <> text ")"
          ]
    | isImmOp o2 ->
        -- Surrender! Let the assembler figure out the right expressions with pseudo-op LI.
        lines_ [text "\tli" <+> pprOp platform o1 <> comma <+> pprOp platform o2]
    | otherwise -> op3 (text "\taddi") o1 o2 (OpImm (ImmInt 0))
  ORI o1 o2 o3 -> op3 (text "\tori") o1 o2 o3
  XORI o1 o2 o3 -> op3 (text "\txori") o1 o2 o3
  J o1 -> pprInstr platform (B o1)
  J_TBL _ _ r -> pprInstr platform (B (TReg r))
  B l | isLabel l -> line $ text "\tjal" <+> pprOp platform x0 <> comma <+> getLabel platform l
  B (TReg r) -> line $ text "\tjalr" <+> pprOp platform x0 <> comma <+> pprReg II64 r <> comma <+> text "0"
  BL r _ -> line $ text "\tjalr" <+> text "x1" <> comma <+> pprReg II64 r <> comma <+> text "0"
  BCOND c l r t
    | isLabel t ->
        line $ text "\t" <> pprBcond c <+> pprOp platform l <> comma <+> pprOp platform r <> comma <+> getLabel platform t
  BCOND _ _ _ (TReg _) -> panic "RV64.ppr: No conditional branching to registers!"
  CSET o l r c -> case c of
    EQ
      | isIntRegOp l && isIntOp r ->
          lines_
            [ subFor l r,
              text "\tseqz" <+> pprOp platform o <> comma <+> pprOp platform o
            ]
    EQ | isFloatOp l && isFloatOp r -> line $ binOp ("\tfeq." ++ floatOpPrecision platform l r)
    NE
      | isIntRegOp l && isIntOp r ->
          lines_
            [ subFor l r,
              text "\tsnez" <+> pprOp platform o <> comma <+> pprOp platform o
            ]
    NE
      | isFloatOp l && isFloatOp r ->
          lines_
            [ binOp ("\tfeq." ++ floatOpPrecision platform l r),
              text "\txori" <+> pprOp platform o <> comma <+> pprOp platform o <> comma <+> text "1"
            ]
    SLT -> lines_ [sltFor l r <+> pprOp platform o <> comma <+> pprOp platform l <> comma <+> pprOp platform r]
    SLE ->
      lines_
        [ sltFor l r <+> pprOp platform o <> comma <+> pprOp platform r <> comma <+> pprOp platform l,
          text "\txori" <+> pprOp platform o <> comma <+> pprOp platform o <> comma <+> text "1"
        ]
    SGE ->
      lines_
        [ sltFor l r <+> pprOp platform o <> comma <+> pprOp platform l <> comma <+> pprOp platform r,
          text "\txori" <+> pprOp platform o <> comma <+> pprOp platform o <> comma <+> text "1"
        ]
    SGT -> lines_ [sltFor l r <+> pprOp platform o <> comma <+> pprOp platform r <> comma <+> pprOp platform l]
    ULT -> lines_ [sltuFor l r <+> pprOp platform o <> comma <+> pprOp platform l <> comma <+> pprOp platform r]
    ULE ->
      lines_
        [ sltuFor l r <+> pprOp platform o <> comma <+> pprOp platform r <> comma <+> pprOp platform l,
          text "\txori" <+> pprOp platform o <> comma <+> pprOp platform o <> comma <+> text "1"
        ]
    UGE ->
      lines_
        [ sltuFor l r <+> pprOp platform o <> comma <+> pprOp platform l <> comma <+> pprOp platform r,
          text "\txori" <+> pprOp platform o <> comma <+> pprOp platform o <> comma <+> text "1"
        ]
    UGT -> lines_ [sltuFor l r <+> pprOp platform o <> comma <+> pprOp platform r <> comma <+> pprOp platform l]
    FLT | isFloatOp l && isFloatOp r -> line $ binOp ("\tflt." ++ floatOpPrecision platform l r)
    FLE | isFloatOp l && isFloatOp r -> line $ binOp ("\tfle." ++ floatOpPrecision platform l r)
    FGT | isFloatOp l && isFloatOp r -> line $ binOp ("\tfgt." ++ floatOpPrecision platform l r)
    FGE | isFloatOp l && isFloatOp r -> line $ binOp ("\tfge." ++ floatOpPrecision platform l r)
    x -> pprPanic "RV64.ppr: unhandled CSET conditional" (text (show x) <+> pprOp platform o <> comma <+> pprOp platform l <> comma <+> pprOp platform r)
    where
      subFor l r
        | (OpImm _) <- r = text "\taddi" <+> pprOp platform o <> comma <+> pprOp platform l <> comma <+> pprOp platform (negOp r)
        | (OpImm _) <- l = panic "RV64.ppr: Cannot SUB IMM _"
        | otherwise = text "\tsub" <+> pprOp platform o <> comma <+> pprOp platform l <> comma <+> pprOp platform r
      sltFor l r
        | (OpImm _) <- r = text "\tslti"
        | (OpImm _) <- l = panic "PV64.ppr: Cannot SLT IMM _"
        | otherwise = text "\tslt"
      sltuFor l r
        | (OpImm _) <- r = text "\tsltui"
        | (OpImm _) <- l = panic "PV64.ppr: Cannot SLTU IMM _"
        | otherwise = text "\tsltu"
      binOp :: (IsLine doc) => String -> doc
      binOp op = text op <+> pprOp platform o <> comma <+> pprOp platform l <> comma <+> pprOp platform r
  STR II8 o1 o2 -> op2 (text "\tsb") o1 o2
  STR II16 o1 o2 -> op2 (text "\tsh") o1 o2
  STR II32 o1 o2 -> op2 (text "\tsw") o1 o2
  STR II64 o1 o2 -> op2 (text "\tsd") o1 o2
  STR FF32 o1 o2 -> op2 (text "\tfsw") o1 o2
  STR FF64 o1 o2 -> op2 (text "\tfsd") o1 o2
  STR (VecFormat _ FmtInt8) o1 o2 -> op2 (text "\tvse8.v") o1 o2
  STR (VecFormat _ FmtInt16) o1 o2 -> op2 (text "\tvse16.v") o1 o2
  STR (VecFormat _ FmtInt32) o1 o2 -> op2 (text "\tvse32.v") o1 o2
  STR (VecFormat _ FmtInt64) o1 o2 -> op2 (text "\tvse64.v") o1 o2
  STR (VecFormat _ FmtFloat) o1 o2 -> op2 (text "\tvse32.v") o1 o2
  STR (VecFormat _ FmtDouble) o1 o2 -> op2 (text "\tvse64.v") o1 o2
  LDR _f o1 (OpImm (ImmIndex lbl off)) ->
    lines_
      [ text "\tla" <+> pprOp platform o1 <> comma <+> pprAsmLabel platform lbl,
        text "\taddi" <+> pprOp platform o1 <> comma <+> pprOp platform o1 <> comma <+> int off
      ]
  LDR _f o1 (OpImm (ImmCLbl lbl)) ->
    line $ text "\tla" <+> pprOp platform o1 <> comma <+> pprAsmLabel platform lbl
  LDR II8 o1 o2 -> op2 (text "\tlb") o1 o2
  LDR II16 o1 o2 -> op2 (text "\tlh") o1 o2
  LDR II32 o1 o2 -> op2 (text "\tlw") o1 o2
  LDR II64 o1 o2 -> op2 (text "\tld") o1 o2
  LDR FF32 o1 o2 -> op2 (text "\tflw") o1 o2
  LDR FF64 o1 o2 -> op2 (text "\tfld") o1 o2
  LDR (VecFormat _ FmtInt8) o1 o2 -> op2 (text "\tvle8.v") o1 o2
  LDR (VecFormat _ FmtInt16) o1 o2 -> op2 (text "\tvle16.v") o1 o2
  LDR (VecFormat _ FmtInt32) o1 o2 -> op2 (text "\tvle32.v") o1 o2
  LDR (VecFormat _ FmtInt64) o1 o2 -> op2 (text "\tvle64.v") o1 o2
  LDR (VecFormat _ FmtFloat) o1 o2 -> op2 (text "\tvle32.v") o1 o2
  LDR (VecFormat _ FmtDouble) o1 o2 -> op2 (text "\tvle64.v") o1 o2
  LDRU II8 o1 o2 -> op2 (text "\tlbu") o1 o2
  LDRU II16 o1 o2 -> op2 (text "\tlhu") o1 o2
  LDRU II32 o1 o2 -> op2 (text "\tlwu") o1 o2
  -- double words (64bit) cannot be sign extended by definition
  LDRU II64 o1 o2 -> op2 (text "\tld") o1 o2
  LDRU FF32 o1 o2@(OpAddr (AddrReg _)) -> op2 (text "\tflw") o1 o2
  LDRU FF32 o1 o2@(OpAddr (AddrRegImm _ _)) -> op2 (text "\tflw") o1 o2
  LDRU FF64 o1 o2@(OpAddr (AddrReg _)) -> op2 (text "\tfld") o1 o2
  LDRU FF64 o1 o2@(OpAddr (AddrRegImm _ _)) -> op2 (text "\tfld") o1 o2
  -- vectors
  LDRU (VecFormat _ FmtInt8) o1 o2 -> op2 (text "\tvle8.v") o1 o2
  LDRU (VecFormat _ FmtInt16) o1 o2 -> op2 (text "\tvle16.v") o1 o2
  LDRU (VecFormat _ FmtInt32) o1 o2 -> op2 (text "\tvle32.v") o1 o2
  LDRU (VecFormat _ FmtInt64) o1 o2 -> op2 (text "\tvle64.v") o1 o2
  LDRU (VecFormat _ FmtFloat) o1 o2 -> op2 (text "\tvle32.v") o1 o2
  LDRU (VecFormat _ FmtDouble) o1 o2 -> op2 (text "\tvle64.v") o1 o2
  LDRU f o1 o2 -> pprPanic "Unsupported unsigned load" ((text . show) f <+> pprOp platform o1 <+> pprOp platform o2)
  FENCE r w -> line $ text "\tfence" <+> pprFenceType r <> char ',' <+> pprFenceType w
  FCVT FloatToFloat o1@(OpReg FF32 _) o2@(OpReg FF64 _) -> op2 (text "\tfcvt.s.d") o1 o2
  FCVT FloatToFloat o1@(OpReg FF64 _) o2@(OpReg FF32 _) -> op2 (text "\tfcvt.d.s") o1 o2
  FCVT FloatToFloat o1 o2 ->
    pprPanic "RV64.pprInstr - impossible float to float conversion"
      $ line (pprOp platform o1 <> text "->" <> pprOp platform o2)
  FCVT IntToFloat o1@(OpReg FF32 _) o2@(OpReg II32 _) -> op2 (text "\tfcvt.s.w") o1 o2
  FCVT IntToFloat o1@(OpReg FF32 _) o2@(OpReg II64 _) -> op2 (text "\tfcvt.s.l") o1 o2
  FCVT IntToFloat o1@(OpReg FF64 _) o2@(OpReg II32 _) -> op2 (text "\tfcvt.d.w") o1 o2
  FCVT IntToFloat o1@(OpReg FF64 _) o2@(OpReg II64 _) -> op2 (text "\tfcvt.d.l") o1 o2
  FCVT IntToFloat o1 o2 ->
    pprPanic "RV64.pprInstr - impossible integer to float conversion"
      $ line (pprOp platform o1 <> text "->" <> pprOp platform o2)
  FCVT FloatToInt o1@(OpReg II32 _) o2@(OpReg FF32 _) -> op2 (text "\tfcvt.w.s") o1 o2
  FCVT FloatToInt o1@(OpReg II32 _) o2@(OpReg FF64 _) -> op2 (text "\tfcvt.w.d") o1 o2
  FCVT FloatToInt o1@(OpReg II64 _) o2@(OpReg FF32 _) -> op2 (text "\tfcvt.l.s") o1 o2
  FCVT FloatToInt o1@(OpReg II64 _) o2@(OpReg FF64 _) -> op2 (text "\tfcvt.l.d") o1 o2
  FCVT FloatToInt o1 o2 ->
    pprPanic "RV64.pprInstr - impossible float to integer conversion"
      $ line (pprOp platform o1 <> text "->" <> pprOp platform o2)
  FABS o1 o2 | isSingleOp o2 -> op2 (text "\tfabs.s") o1 o2
  FABS o1 o2 | isDoubleOp o2 -> op2 (text "\tfabs.d") o1 o2
  FMIN o1 o2 o3
    | isSingleOp o1 -> op3 (text "\tfmin.s") o1 o2 o3
    | isDoubleOp o2 -> op3 (text "\tfmin.d") o1 o2 o3
  FMAX o1 o2 o3
    | isSingleOp o1 -> op3 (text "\tfmax.s") o1 o2 o3
    | isDoubleOp o2 -> op3 (text "\tfmax.d") o1 o2 o3
  FMA variant d r1 r2 r3
    | isFloatRegOp d ->
        let fma = case variant of
              FMAdd -> text "\tfmadd" <> dot <> floatPrecission d
              FMSub -> text "\tfmsub" <> dot <> floatPrecission d
              FNMAdd -> text "\tfnmadd" <> dot <> floatPrecission d
              FNMSub -> text "\tfnmsub" <> dot <> floatPrecission d
         in op4 fma d r1 r2 r3
  VFMA variant o1@(OpReg fmt _reg) o2 o3
    | VecFormat _l fmt' <- fmt ->
        let formatString = if (isFloatFormat . scalarFormatFormat) fmt' then text "f" else text ""
            prefix = text "v" <> formatString
            suffix = text "vv"
            fma = case variant of
              FMAdd -> text "madd"
              FMSub -> text "msub" -- TODO: Works only for floats!
              FNMAdd -> text "nmadd" -- TODO: Works only for floats!
              FNMSub -> text "nmsub"
         in op3 (tab <> prefix <> fma <> dot <> suffix) o1 o2 o3
  VFMA _variant o1 _o2 _o3 -> pprPanic "RV64.pprInstr - VFMA can only target registers." (pprOp platform o1)
  VMV o1@(OpReg fmt _reg) o2
    | isFloatOp o1 && isVectorRegOp o2 -> op2 (text "\tvfmv" <> dot <> text "f" <> dot <> text "s") o1 o2
    | isVectorRegOp o1 && isFloatOp o2 -> op2 (text "\tvfmv" <> dot <> opToVInstrSuffix o1 <> dot <> text "f") o1 o2
    | isIntRegOp o1 && isVectorRegOp o2 -> op2 (text "\tvmv" <> dot <> text "x" <> dot <> text "s") o1 o2
    | isVectorRegOp o1 && isIntImmOp o2 -> op2 (text "\tvmv" <> dot <> opToVInstrSuffix o1 <> dot <> text "i") o1 o2
    | isVectorRegOp o1 && isIntRegOp o2 -> op2 (text "\tvmv" <> dot <> opToVInstrSuffix o1 <> dot <> text "x") o1 o2
    | isVectorRegOp o1 && isVectorRegOp o2 -> op2 (text "\tvmv" <> dot <> opToVInstrSuffix o1 <> dot <> text "v") o1 o2
    | True -> pprPanic "RV64.pprInstr - impossible vector move (VMV)" (pprOp platform o1 <+> pprOp platform o2 <+> text "fmt" <> colon <> (text . show) fmt)
  VMV o1 o2 -> pprPanic "RV64.pprInstr - invalid VMV instruction" (text "VMV" <+> pprOp platform o1 <> comma <+> pprOp platform o2)
  VID op | isVectorRegOp op -> op1 (text "\tvid.v") op
  VID op -> pprPanic "RV64.pprInstr - VID can only target registers." (pprOp platform op)
  VMSEQ o1 o2 o3 | allVectorRegOps [o1, o2] && isIntOp o3 && isImmOp o3-> op3 (text "\tvmseq.vi") o1 o2 o3
  VMSEQ o1 o2 o3 | allVectorRegOps [o1, o2] && isIntOp o3 -> op3 (text "\tvmseq.vx") o1 o2 o3
  VMSEQ o1 o2 o3 -> pprPanic "RV64.pprInstr - VMSEQ wrong operands." (pprOps platform [o1, o2, o3])
  VMERGE o1 o2 o3 o4 | allVectorRegOps [o1, o2, o3, o4] -> op4 (text "\tvmerge.vvm") o1 o2 o3 o4
  VMERGE o1 o2 o3 o4 -> pprPanic "RV64.pprInstr - VMERGE wrong operands." (pprOps platform [o1, o2, o3, o4])
  VSLIDEDOWN o1 o2 o3 | allVectorRegOps [o1, o2] && isIntOp o3 -> op3 (text "\tvslidedown.vx") o1 o2 o3
  VSLIDEDOWN o1 o2 o3 -> pprPanic "RV64.pprInstr - VSLIDEDOWN wrong operands." (pprOps platform [o1, o2, o3])
  -- TODO: adjust VSETIVLI to contain only format?
  VSETIVLI (OpReg fmt dst) len width grouping ta ma ->
    line
      $ text "\tvsetivli"
      <+> pprReg fmt dst
      <> comma
      <+> (text . show) len
      <> comma
      <+> pprVWidth width
      <> comma
      <+> pprGrouping grouping
      <> comma
      <+> pprTA ta
      <> comma
      <+> pprMasking ma
  VSETIVLI o1 _ _ _ _ _ -> pprPanic "RV64.pprInstr - VSETIVLI wrong operands." (pprOp platform o1)
  VNEG o1 o2 | allIntVectorRegOps [o1, o2] -> op2 (text "\tvneg.v") o1 o2
  VNEG o1 o2 | allFloatVectorRegOps [o1, o2] -> op2 (text "\tvfneg.v") o1 o2
  VNEG o1 o2 -> pprPanic "RV64.pprInstr - VNEG wrong operands." (pprOps platform [o1, o2])
  VADD o1 o2 o3 | allIntVectorRegOps [o1, o2, o3] -> op3 (text "\tvadd.vv") o1 o2 o3
  VADD o1 o2 o3 | allFloatVectorRegOps [o1, o2, o3] -> op3 (text "\tvfadd.vv") o1 o2 o3
  VADD o1 o2 o3 -> pprPanic "RV64.pprInstr - VADD wrong operands." (pprOps platform [o1, o2, o3])
  VSUB o1 o2 o3 | allIntVectorRegOps [o1, o2, o3] -> op3 (text "\tvsub.vv") o1 o2 o3
  VSUB o1 o2 o3 | allFloatVectorRegOps [o1, o2, o3] -> op3 (text "\tvfsub.vv") o1 o2 o3
  VSUB o1 o2 o3 -> pprPanic "RV64.pprInstr - VSUB wrong operands." (pprOps platform [o1, o2, o3])
  VMUL o1 o2 o3 | allIntVectorRegOps [o1, o2, o3] -> op3 (text "\tvmul.vv") o1 o2 o3
  VMUL o1 o2 o3 | allFloatVectorRegOps [o1, o2, o3] -> op3 (text "\tvfmul.vv") o1 o2 o3
  VMUL o1 o2 o3 -> pprPanic "RV64.pprInstr - VMUL wrong operands." (pprOps platform [o1, o2, o3])
  VQUOT (Just Signed) o1 o2 o3 | allIntVectorRegOps [o1, o2, o3] -> op3 (text "\tvdiv.vv") o1 o2 o3
  VQUOT (Just Unsigned) o1 o2 o3 | allIntVectorRegOps [o1, o2, o3] -> op3 (text "\tvdivu.vv") o1 o2 o3
  VQUOT Nothing o1 o2 o3 | allFloatVectorRegOps [o1, o2, o3] -> op3 (text "\tvfdiv.vv") o1 o2 o3
  VQUOT mbS o1 o2 o3 -> pprPanic ("RV64.pprInstr - VQUOT wrong operands. " ++ show mbS) (pprOps platform [o1, o2, o3])
  VREM Signed o1 o2 o3 | allIntVectorRegOps [o1, o2, o3] -> op3 (text "\tvrem.vv") o1 o2 o3
  VREM Unsigned o1 o2 o3 | allIntVectorRegOps [o1, o2, o3] -> op3 (text "\tvremu.vv") o1 o2 o3
  VREM s o1 o2 o3 -> pprPanic ("RV64.pprInstr - VREM wrong operands. " ++ show s) (pprOps platform [o1, o2, o3])
  VSMIN o1 o2 o3 | allVectorRegOps [o1, o2, o3] -> op3 (text "\tvmin.vv") o1 o2 o3
  VSMIN o1 o2 o3 -> pprPanic "RV64.pprInstr - VSMIN wrong operands." (pprOps platform [o1, o2, o3])
  VSMAX o1 o2 o3 | allVectorRegOps [o1, o2, o3] -> op3 (text "\tvmax.vv") o1 o2 o3
  VSMAX o1 o2 o3 -> pprPanic "RV64.pprInstr - VSMAX wrong operands." (pprOps platform [o1, o2, o3])
  VUMIN o1 o2 o3 | allVectorRegOps [o1, o2, o3] -> op3 (text "\tvminu.vv") o1 o2 o3
  VUMIN o1 o2 o3 -> pprPanic "RV64.pprInstr - VUMIN wrong operands." (pprOps platform [o1, o2, o3])
  VUMAX o1 o2 o3 | allVectorRegOps [o1, o2, o3] -> op3 (text "\tvmaxu.vv") o1 o2 o3
  VUMAX o1 o2 o3 -> pprPanic "RV64.pprInstr - VUMAX wrong operands." (pprOps platform [o1, o2, o3])
  VFMIN o1 o2 o3 | allVectorRegOps [o1, o2, o3] -> op3 (text "\tvfmin.vv") o1 o2 o3
  VFMIN o1 o2 o3 -> pprPanic "RV64.pprInstr - VFMIN wrong operands." (pprOps platform [o1, o2, o3])
  VFMAX o1 o2 o3 | allVectorRegOps [o1, o2, o3] -> op3 (text "\tvfmax.vv") o1 o2 o3
  VFMAX o1 o2 o3 -> pprPanic "RV64.pprInstr - VFMAX wrong operands." (pprOps platform [o1, o2, o3])
  VRGATHER o1 o2 o3 | allVectorRegOps [o1, o2, o3] -> op3 (text "\tvrgather.vv") o1 o2 o3
  VRGATHER o1 o2 o3 -> pprPanic "RV64.pprInstr - VRGATHER wrong operands." (pprOps platform [o1, o2, o3])
  instr -> panic $ "RV64.pprInstr - Unknown instruction: " ++ instrCon instr
  where
    op1 op o1 = line $ op <+> pprOp platform o1
    op2 :: (IsLine (Line t), IsDoc t, HasCallStack) => Line t -> Operand -> Operand -> t
    op2 op o1 o2 = line $ op <+> pprOp platform o1 <> comma <+> pprOp platform o2
    op3 op o1 o2 o3 = line $ op <+> pprOp platform o1 <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o3
    op4 op o1 o2 o3 o4 = line $ op <+> pprOp platform o1 <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o3 <> comma <+> pprOp platform o4
    pprFenceType FenceRead = text "r"
    pprFenceType FenceWrite = text "w"
    pprFenceType FenceReadWrite = text "rw"
    floatPrecission o
      | isSingleOp o = text "s"
      | isDoubleOp o = text "d"
      | otherwise = pprPanic "Impossible floating point precission: " (pprOp platform o)

    pprTA TA = text "ta"
    pprTA TU = text "tu"

    pprVWidth :: (IsLine doc) => Width -> doc
    pprVWidth W8 = text "e8"
    pprVWidth W16 = text "e16"
    pprVWidth W32 = text "e32"
    pprVWidth W64 = text "e64"
    pprVWidth w = panic $ "Unsupported vector element size: " ++ show w

    pprGrouping MF2 = text "mf2"
    pprGrouping MF4 = text "mf4"
    pprGrouping MF8 = text "mf8"
    pprGrouping M1 = text "m1"
    pprGrouping M2 = text "m2"
    pprGrouping M4 = text "m4"
    pprGrouping M8 = text "m8"

    pprMasking MA = text "ma"
    pprMasking MU = text "mu"

    opToVInstrSuffix :: (IsLine doc) => Operand -> doc
    opToVInstrSuffix op | isIntRegOp op = text "x"
    opToVInstrSuffix op | isFloatOp op = text "f"
    opToVInstrSuffix op | isVectorRegOp op = text "v"
    opToVInstrSuffix op = pprPanic "Unsupported operand for vector instruction" (pprOp platform op)

floatOpPrecision :: Platform -> Operand -> Operand -> String
floatOpPrecision _p l r | isFloatOp l && isFloatOp r && isSingleOp l && isSingleOp r = "s" -- single precision
floatOpPrecision _p l r | isFloatOp l && isFloatOp r && isDoubleOp l && isDoubleOp r = "d" -- double precision
floatOpPrecision p l r = pprPanic "Cannot determine floating point precission" (text "op1" <+> pprOp p l <+> text "op2" <+> pprOp p r)

-- | Pretty print a conditional branch
--
-- This function is partial and will panic if the conditional is not supported;
-- i.e. if its floating point related.
pprBcond :: (IsLine doc) => Cond -> doc
pprBcond c = text "b" <> pprCond c
  where
    pprCond :: (IsLine doc) => Cond -> doc
    pprCond c = case c of
      EQ -> text "eq"
      NE -> text "ne"
      SLT -> text "lt"
      SLE -> text "le"
      SGE -> text "ge"
      SGT -> text "gt"
      ULT -> text "ltu"
      ULE -> text "leu"
      UGE -> text "geu"
      UGT -> text "gtu"
      -- BCOND cannot handle floating point comparisons / registers
      _ -> panic $ "RV64.ppr: unhandled BCOND conditional: " ++ show c

-- | Get the `Format` to configure for a vector operation (if any)
--
-- If an `Instr` is is a vector insruction, we have to configure the correct
-- `Format` such that the vector registers are correctly interpreted by the CPU.
instrVecFormat :: Platform -> Instr -> Maybe Format
instrVecFormat platform instr = case instr of
  ANN _doc instr' -> instrVecFormat platform instr'
  STR fmt _o1 _o2 | isVecFormat fmt -> Just fmt
  LDR fmt _o1 _o2 | isVecFormat fmt -> Just fmt
  -- Special-case for loading smaller sized structures into bigger sized
  -- vectors (e.g. vle16. to a FmtInt64)
  LDRU _fmt (OpReg fmt' _r) _o2 | isVecFormat fmt' -> Just fmt'
  LDRU fmt _o1 _o2 | isVecFormat fmt -> Just fmt
  MOV (OpReg fmt _reg) _o2
    | isVecFormat fmt -> checkedJustFmt fmt
  MOV _o1 (OpReg fmt _reg)
    | isVecFormat fmt -> checkedJustFmt fmt
  VFMA _v (OpReg fmt _reg) _o2 _o3
    | isVecFormat fmt -> checkedJustFmt fmt
  VMV (OpReg fmt _reg) _o2
    | isVecFormat fmt -> checkedJustFmt fmt
  VMV _o1 (OpReg fmt _reg)
    | isVecFormat fmt -> checkedJustFmt fmt
  VMV _o1 _o2 -> pprPanic "Did not match" (pprInstr platform instr)
  VID (OpReg fmt _reg)
    | isVecFormat fmt -> checkedJustFmt fmt
  VID _o1 -> pprPanic "Did not match" (pprInstr platform instr)
  VMSEQ (OpReg fmt _reg) _o2 _o3
    | isVecFormat fmt -> checkedJustFmt fmt
  VMSEQ _o1 _o2 _o3 -> pprPanic "Did not match" (pprInstr platform instr)
  VMERGE (OpReg fmt _reg) _o2 _o3 _o4
    | isVecFormat fmt -> checkedJustFmt fmt
  VMERGE _o1 _o2 _o3 _o4 -> pprPanic "Did not match" (pprInstr platform instr)
  VSLIDEDOWN (OpReg fmt _reg) _o2 _o3
    | isVecFormat fmt -> checkedJustFmt fmt
  VSLIDEDOWN _o1 _o2 _o3 -> pprPanic "Did not match" (pprInstr platform instr)
  VNEG (OpReg fmt _reg) _o2
    | isVecFormat fmt -> checkedJustFmt fmt
  VNEG _o1 _o2 -> pprPanic "Did not match" (pprInstr platform instr)
  VADD (OpReg fmt _reg) _o2 _o3
    | isVecFormat fmt -> checkedJustFmt fmt
  VADD _o1 _o2 _o3 -> pprPanic "Did not match" (pprInstr platform instr)
  VSUB (OpReg fmt _reg) _o2 _o3
    | isVecFormat fmt -> checkedJustFmt fmt
  VSUB _o1 _o2 _o3 -> pprPanic "Did not match" (pprInstr platform instr)
  VMUL (OpReg fmt _reg) _o2 _o3
    | isVecFormat fmt -> checkedJustFmt fmt
  VMUL _o1 _o2 _o3 -> pprPanic "Did not match" (pprInstr platform instr)
  VQUOT _mbS (OpReg fmt _reg) _o2 _o3
    | isVecFormat fmt -> checkedJustFmt fmt
  VQUOT _mbS _o1 _o2 _o3 -> pprPanic "Did not match" (pprInstr platform instr)
  VSMIN (OpReg fmt _reg) _o2 _o3
    | isVecFormat fmt -> checkedJustFmt fmt
  VSMIN _o1 _o2 _o3 -> pprPanic "Did not match" (pprInstr platform instr)
  VSMAX (OpReg fmt _reg) _o2 _o3
    | isVecFormat fmt -> checkedJustFmt fmt
  VSMAX _o1 _o2 _o3 -> pprPanic "Did not match" (pprInstr platform instr)
  VUMIN (OpReg fmt _reg) _o2 _o3
    | isVecFormat fmt -> checkedJustFmt fmt
  VUMIN _o1 _o2 _o3 -> pprPanic "Did not match" (pprInstr platform instr)
  VUMAX (OpReg fmt _reg) _o2 _o3
    | isVecFormat fmt -> checkedJustFmt fmt
  VUMAX _o1 _o2 _o3 -> pprPanic "Did not match" (pprInstr platform instr)
  VFMIN (OpReg fmt _reg) _o2 _o3
    | isVecFormat fmt -> checkedJustFmt fmt
  VFMIN _o1 _o2 _o3 -> pprPanic "Did not match" (pprInstr platform instr)
  VFMAX (OpReg fmt _reg) _o2 _o3 -> checkedJustFmt fmt
  VFMAX _o1 _o2 _o3 -> pprPanic "Did not match" (pprInstr platform instr)
  VRGATHER (OpReg fmt _reg) _o2 _o3 -> checkedJustFmt fmt
  VRGATHER _o1 _o2 _o3 -> pprPanic "Did not match" (pprInstr platform instr)
  _ -> Nothing
  where
    checkedJustFmt :: Format -> Maybe Format
    checkedJustFmt fmt | isVecFormat fmt = Just fmt
    checkedJustFmt fmt =
      pprPanic
        ("Vector format expected but got " ++ show fmt)
        (pprInstr platform instr)
