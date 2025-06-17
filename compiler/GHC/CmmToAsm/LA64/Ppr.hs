module GHC.CmmToAsm.LA64.Ppr (pprNatCmmDecl, pprInstr) where

import GHC.Prelude hiding (EQ)

import GHC.CmmToAsm.LA64.Regs
import GHC.CmmToAsm.LA64.Instr
import GHC.CmmToAsm.LA64.Cond
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Ppr
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Utils
import GHC.Cmm hiding (topInfoTable)
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow.Label
import GHC.Platform
import GHC.Platform.Reg
import GHC.Types.Unique ( pprUniqueAlways, getUnique )
import GHC.Utils.Outputable
import GHC.Types.Basic (Alignment, alignmentBytes, mkAlignment)
import GHC.Utils.Panic

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

pprProcEndLabel :: IsLine doc => Platform -> CLabel -- ^ Procedure
                -> doc
pprProcEndLabel platform lbl =
    pprAsmLabel platform (mkAsmTempProcEndLabel lbl) <> colon

pprBlockEndLabel :: IsLine doc => Platform -> CLabel -- ^ Block name
                 -> doc
pprBlockEndLabel platform lbl =
    pprAsmLabel platform (mkAsmTempEndLabel lbl) <> colon

pprLabel :: IsDoc doc => Platform -> CLabel -> doc
pprLabel platform lbl =
   pprGloblDecl platform lbl
   $$ pprTypeDecl platform lbl
   $$ line (pprAsmLabel platform lbl <> char ':')

pprAlign :: (IsDoc doc) => Alignment -> doc
pprAlign alignment =
  -- .balign is stable, whereas .align is platform dependent.
  line $ text "\t.balign " <> int (alignmentBytes alignment)

-- | Print appropriate alignment for the given section type.
--
-- Currently, this always aligns to a full machine word (8 byte.) A future
-- improvement could be to really do this per section type (though, it's
-- probably not a big gain.)
pprAlignForSection :: (IsDoc doc) => SectionType -> doc
pprAlignForSection _seg = pprAlign . mkAlignment $ 8

-- Print section header and appropriate alignment for that section.
-- This will e.g. emit a header like:
--
--     .section .text
--     .balign 8
--
pprSectionAlign :: IsDoc doc => NCGConfig -> Section -> doc
pprSectionAlign _config (Section (OtherSection _) _) =
  panic "LA64.Ppr.pprSectionAlign: unknown section"
pprSectionAlign config sec@(Section seg _) =
    line (pprSectionHeader config sec)
    $$ pprAlignForSection seg

-- | Output the ELF .size directive
pprSizeDecl :: (IsDoc doc) => Platform -> CLabel -> doc
pprSizeDecl platform lbl
  | osElfTarget (platformOS platform) =
      line $ text "\t.size" <+> pprAsmLabel platform lbl <> text ", .-" <> pprAsmLabel platform lbl
pprSizeDecl _ _ = empty

pprBasicBlock ::
  (IsDoc doc) =>
  NCGConfig ->
  LabelMap RawCmmStatics ->
  NatBasicBlock Instr ->
  doc

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

-- Always use objects for info tables
--
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
  if isCFunctionLabel lbl || functionOkInfoTable
    then text "@function"
    else text "@object"
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

        ppr_item II8  _ = [text "\t.byte\t"  <> pprDataImm platform imm]
        ppr_item II16 _ = [text "\t.short\t" <> pprDataImm platform imm]
        ppr_item II32 _ = [text "\t.long\t"  <> pprDataImm platform imm]
        ppr_item II64 _ = [text "\t.quad\t"  <> pprDataImm platform imm]

        ppr_item FF32  (CmmFloat r _)
           = let bs = floatToBytes (fromRational r)
             in  map (\b -> text "\t.byte\t" <> int (fromIntegral b)) bs

        ppr_item FF64 (CmmFloat r _)
           = let bs = doubleToBytes (fromRational r)
             in  map (\b -> text "\t.byte\t" <> int (fromIntegral b)) bs

        ppr_item _ _ = pprPanic "pprDataItem:ppr_item" (text $ show lit)

-- | Pretty print an immediate value in the @data@ section
-- This does not include any checks. We rely on the Assembler to check for
-- errors. Use `pprOpImm` for immediates in instructions (operands.)
pprDataImm :: IsLine doc => Platform -> Imm -> doc
pprDataImm _ (ImmInt i)     = int i
pprDataImm _ (ImmInteger i) = integer i
pprDataImm p (ImmCLbl l)    = pprAsmLabel p l
pprDataImm p (ImmIndex l i) = pprAsmLabel p l <> char '+' <> int i
pprDataImm _ (ImmLit s)     = ftext s
pprDataImm _ (ImmFloat f) = float (fromRational f)
pprDataImm _ (ImmDouble d) = double (fromRational d)

pprDataImm p (ImmConstantSum a b) = pprDataImm p a <> char '+' <> pprDataImm p b
pprDataImm p (ImmConstantDiff a b) = pprDataImm p a <> char '-'
                   <> lparen <> pprDataImm p b <> rparen

asmComment :: SDoc -> SDoc
asmComment c = text "#" <+> c

asmDoubleslashComment :: SDoc -> SDoc
asmDoubleslashComment c = text "//" <+> c

asmMultilineComment :: SDoc -> SDoc
asmMultilineComment c =  text "/*" $+$ c $+$ text "*/"

-- | Pretty print an immediate operand of an instruction
pprOpImm :: (IsLine doc) => Platform -> Imm -> doc
pprOpImm platform imm = case imm of
  ImmInt i -> int i
  ImmInteger i -> integer i
  ImmCLbl l -> char '=' <> pprAsmLabel platform l
  ImmFloat f -> float (fromRational f)
  ImmDouble d -> double (fromRational d)
  _ -> pprPanic "LA64.Ppr.pprOpImm" (text "Unsupported immediate for instruction operands:" <+> (text . show) imm)

negOp :: Operand -> Operand
negOp (OpImm (ImmInt i)) = OpImm (ImmInt (negate i))
negOp (OpImm (ImmInteger i)) = OpImm (ImmInteger (negate i))
negOp op = pprPanic "LA64.negOp" (text $ show op)

pprOp :: IsLine doc => Platform -> Operand -> doc
pprOp plat op = case op of
  OpReg w r                 -> pprReg w r
  OpImm imm                 -> pprOpImm plat imm
  OpAddr (AddrRegReg r1 r2) -> pprReg W64 r1 <> comma <+> pprReg W64 r2
  OpAddr (AddrRegImm r imm) -> pprReg W64 r <> comma <+> pprOpImm plat imm
  OpAddr (AddrReg r)        -> pprReg W64 r <+> text ", 0"

pprReg :: forall doc. IsLine doc => Width -> Reg -> doc
pprReg w r = case r of
  RegReal    (RealRegSingle i) -> ppr_reg_no i
  -- virtual regs should not show up, but this is helpful for debugging.
  RegVirtual (VirtualRegI u)   -> text "%vI_" <> pprUniqueAlways u
  -- RegVirtual (VirtualRegF u)   -> text "%vF_" <> pprUniqueAlways u
  RegVirtual (VirtualRegD u)   -> text "%vD_" <> pprUniqueAlways u
  _                            -> pprPanic "LA64.pprReg" (text (show r) <+> ppr w)

  where
    ppr_reg_no :: Int -> doc
    -- LoongArch's registers must be started from `$[fr]`
    -- General Purpose Registers
    ppr_reg_no 0  = text "$zero"
    ppr_reg_no 1  = text "$ra"
    ppr_reg_no 2  = text "$tp"
    ppr_reg_no 3  = text "$sp"
    ppr_reg_no 4  = text "$a0"
    ppr_reg_no 5  = text "$a1"
    ppr_reg_no 6  = text "$a2"
    ppr_reg_no 7  = text "$a3"
    ppr_reg_no 8  = text "$a4"
    ppr_reg_no 9  = text "$a5"
    ppr_reg_no 10 = text "$a6"
    ppr_reg_no 11 = text "$a7"
    ppr_reg_no 12 = text "$t0"
    ppr_reg_no 13 = text "$t1"
    ppr_reg_no 14 = text "$t2"
    ppr_reg_no 15 = text "$t3"
    ppr_reg_no 16 = text "$t4"
    ppr_reg_no 17 = text "$t5"
    ppr_reg_no 18 = text "$t6"
    ppr_reg_no 19 = text "$t7"
    ppr_reg_no 20 = text "$t8"
    ppr_reg_no 21 = text "$u0"  -- Reserverd
    ppr_reg_no 22 = text "$fp"
    ppr_reg_no 23 = text "$s0"
    ppr_reg_no 24 = text "$s1"
    ppr_reg_no 25 = text "$s2"
    ppr_reg_no 26 = text "$s3"
    ppr_reg_no 27 = text "$s4"
    ppr_reg_no 28 = text "$s5"
    ppr_reg_no 29 = text "$s6"
    ppr_reg_no 30 = text "$s7"
    ppr_reg_no 31 = text "$s8"

    -- Floating Point Registers
    ppr_reg_no 32 = text "$fa0"
    ppr_reg_no 33 = text "$fa1"
    ppr_reg_no 34 = text "$fa2"
    ppr_reg_no 35 = text "$fa3"
    ppr_reg_no 36 = text "$fa4"
    ppr_reg_no 37 = text "$fa5"
    ppr_reg_no 38 = text "$fa6"
    ppr_reg_no 39 = text "$fa7"
    ppr_reg_no 40 = text "$ft0"
    ppr_reg_no 41 = text "$ft1"
    ppr_reg_no 42 = text "$ft2"
    ppr_reg_no 43 = text "$ft3"
    ppr_reg_no 44 = text "$ft4"
    ppr_reg_no 45 = text "$ft5"
    ppr_reg_no 46 = text "$ft6"
    ppr_reg_no 47 = text "$ft7"
    ppr_reg_no 48 = text "$ft8"
    ppr_reg_no 49 = text "$ft9"
    ppr_reg_no 50 = text "$ft10"
    ppr_reg_no 51 = text "$ft11"
    ppr_reg_no 52 = text "$ft12"
    ppr_reg_no 53 = text "$ft13"
    ppr_reg_no 54 = text "$ft14"
    ppr_reg_no 55 = text "$ft15"
    ppr_reg_no 56 = text "$fs0"
    ppr_reg_no 57 = text "$fs1"
    ppr_reg_no 58 = text "$fs2"
    ppr_reg_no 59 = text "$fs3"
    ppr_reg_no 60 = text "$fs4"
    ppr_reg_no 61 = text "$fs5"
    ppr_reg_no 62 = text "$fs6"
    ppr_reg_no 63 = text "$fs7"

    ppr_reg_no i
         | i < 0 = pprPanic "Unexpected register number (min is 0)" (ppr w <+> int i)
         | i > 63 = pprPanic "Unexpected register number (max is 63)" (ppr w <+> int i)
         -- no support for widths > W64.
         | otherwise = pprPanic "Unsupported width in register (max is 64)" (ppr w <+> int i)

-- | Single precission `Operand` (floating-point)
isSingleOp :: Operand -> Bool
isSingleOp (OpReg W32 _) = True
isSingleOp _ = False

-- | Double precission `Operand` (floating-point)
isDoubleOp :: Operand -> Bool
isDoubleOp (OpReg W64 _) = True
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
  LDATA _ _ -> panic "PprInstr: NEWBLOCK"

  -- Pseudo Instructions -------------------------------------------------------

  PUSH_STACK_FRAME -> lines_ [ text "\taddi.d $sp, $sp, -16"
                             , text "\tst.d   $ra, $sp, 8"
                             , text "\tst.d   $fp, $sp, 0"
                             , text "\taddi.d $fp, $sp, 16"
                             ]

  POP_STACK_FRAME -> lines_  [ text "\tld.d   $fp, $sp, 0"
                             , text "\tld.d   $ra, $sp, 8"
                             , text "\taddi.d $sp, $sp, 16"
                             ]


  -- ===========================================================================
  -- LoongArch64 Instruction Set
  -- Basic Integer Instructions ------------------------------------------------
  -- 1. Arithmetic Instructions ------------------------------------------------
    -- ADD.{W/D}, SUB.{W/D}
    -- ADDI.{W/D}, ADDU16I.D
  ADD  o1 o2 o3
    | isFloatOp o2 && isFloatOp o3 && isSingleOp o2 && isSingleOp o3 -> op3 (text "\tfadd.s") o1 o2 o3
    | isFloatOp o2 && isFloatOp o3 && isDoubleOp o2 && isDoubleOp o3 -> op3 (text "\tfadd.d") o1 o2 o3
    | OpReg W32 _ <- o2, OpReg W32 _ <- o3 -> op3 (text "\tadd.w") o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3 -> op3 (text "\tadd.d") o1 o2 o3
    | OpReg W32 _ <- o2, isImmOp o3 -> op3 (text "\taddi.w") o1 o2 o3
    | OpReg W64 _ <- o2, isImmOp o3 -> op3 (text "\taddi.d") o1 o2 o3
    | otherwise -> pprPanic "LA64.ppr: ADD error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
    -- TODO: Not complete.
    -- Here we should add addu16i.d for optimizations of accelerating GOT accession
    -- with ldptr.w/d, stptr.w/d
  SUB  o1 o2 o3
    | isFloatOp o2 && isFloatOp o3 && isSingleOp o2 && isSingleOp o3 -> op3 (text "\tfsub.s") o1 o2 o3
    | isFloatOp o2 && isFloatOp o3 && isDoubleOp o2 && isDoubleOp o3 -> op3 (text "\tfsub.d") o1 o2 o3
    | OpReg W32 _ <- o2, OpReg W32 _ <- o3 -> op3 (text "\tsub.w") o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3 -> op3 (text "\tsub.d") o1 o2 o3
    | OpReg W32 _ <- o2, isImmOp o3 -> op3 (text "\taddi.w") o1 o2 (negOp o3)
    | OpReg W64 _ <- o2, isImmOp o3 -> op3 (text "\taddi.d") o1 o2 (negOp o3)
    | otherwise -> pprPanic "LA64.ppr: SUB error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
    -- ALSL.{W[U]/D}
  ALSL  o1 o2 o3 o4
    | OpReg W32 _ <- o2, OpReg W32 _ <- o3, isImmOp o4 -> op4 (text "\talsl.w") o1 o2 o3 o4
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3, isImmOp o4 -> op4 (text "\talsl.d") o1 o2 o3 o4
    | otherwise -> pprPanic "LA64.ppr: ALSL error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
  ALSLU  o1 o2 o3 o4 -> op4 (text "\talsl.wu") o1 o2 o3 o4
    -- LoongArch-Assembler should implement following pesudo instructions, here we can directly use them.
    -- li.w rd, s32
    -- li.w rd, u32
    -- li.d rd, s64
    -- li.d rd, u64
    --
    -- # Load with one instruction
    -- ori dst, $zero, imm[11:0]
    --
    -- # Load with two instructions
    -- lu12i.w dst, imm[31:12]
    -- ori     dst, dst, imm[11:0]
    --
    -- # Load with four instructions
    -- lu12i.w dst, imm[31:12]
    -- ori     dst, dst, imm[11:0]
    -- lu32i.d dst, imm[51:32]
    -- lu52i.d dst, dst, imm[63:52]

  --  -- LU12I.W, LU32I.D, LU52I.D
  LU12I  o1 o2 -> op2 (text "\tlu12i.w") o1 o2
  LU32I  o1 o2 -> op2 (text "\tlu32i.d") o1 o2
  LU52I  o1 o2 o3 -> op3 (text "\tlu52i.d") o1 o2 o3
    -- SSLT[U]
    -- SSLT[U]I
  SSLT  o1 o2 o3
    | isImmOp o3 -> op3 (text "\tslti") o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3  -> op3 (text "\tslt") o1 o2 o3
    | otherwise -> pprPanic "LA64.ppr: SSLT error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
  SSLTU  o1 o2 o3
    | isImmOp o3 -> op3 (text "\tsltui") o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3 -> op3 (text "\tsltu") o1 o2 o3
    | otherwise -> pprPanic "LA64.ppr: SSLTU error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
    -- PCADDI, PCADDU121, PCADDU18l, PCALAU12I
  PCADDI  o1 o2     -> op2 (text "\tpcaddi") o1 o2
  PCADDU12I  o1 o2  -> op2 (text "\tpcaddu12i") o1 o2
  PCADDU18I  o1 (OpImm (ImmCLbl lbl))  ->
    lines_ [
      text "\tpcaddu18i" <+> pprOp platform o1 <> comma <+> text "%call36(" <+> pprAsmLabel platform lbl <+> text ")"
           ]
  PCALAU12I  o1 o2  -> op2 (text "\tpcalau12i") o1 o2
    -- AND, OR, NOR, XOR, ANDN, ORN
    -- ANDI, ORI, XORI: zero-extention
  AND  o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3 -> op3 (text "\tand") o1 o2 o3
    | OpReg W64 _ <- o2, isImmOp o3 -> op3 (text "\tandi") o1 o2 o3
    | otherwise -> pprPanic "LA64.ppr: AND error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
  OR  o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3 -> op3 (text "\tor") o1 o2 o3
    | OpReg W64 _ <- o2, isImmOp o3 -> op3 (text "\tori") o1 o2 o3
    | otherwise -> pprPanic "LA64.ppr: OR error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
  XOR  o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3 -> op3 (text "\txor") o1 o2 o3
    | OpReg W64 _ <- o2, isImmOp o3 -> op3 (text "\txori") o1 o2 o3
    | otherwise -> pprPanic "LA64.ppr: XOR error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
  NOR  o1 o2 o3   -> op3 (text "\tnor") o1 o2 o3
  ANDN  o1 o2 o3  -> op3 (text "\tandn") o1 o2 o3
  ORN  o1 o2 o3   -> op3 (text "\torn") o1 o2 o3

  -----------------------------------------------------------------------------
  -- Pseudo instructions
  -- NOP, alias for "andi r0, r0, r0"
  NOP -> line $ text "\tnop"
  -- NEG o1 o2, alias for "sub o1, r0, o2"
  NEG o1 o2
    | isFloatOp o2 && isSingleOp o2 -> op2 (text "\tfneg.s") o1 o2
    | isFloatOp o2 && isDoubleOp o2 -> op2 (text "\tfneg.d") o1 o2
    | OpReg W32 _ <- o2 -> op3 (text "\tsub.w" ) o1 zero o2
    | OpReg W64 _ <- o2 -> op3 (text "\tsub.d" ) o1 zero o2
    | otherwise -> pprPanic "LA64.ppr: NEG error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2)
  -- Here we can do more simplitcations.
  -- To be honest, floating point instructions are too scarce, so maybe
  -- we should reimplement some pseudo instructions with others.
  MOV o1 o2
    | isFloatOp o1 && isFloatOp o2 && isSingleOp o1 && isSingleOp o2 -> op2 (text "\tfmov.s") o1 o2
    | isFloatOp o1 && isFloatOp o2 && isDoubleOp o1 && isDoubleOp o2 -> op2 (text "\tfmov.d") o1 o2
    | isFloatOp o1 && isImmZero o2 && isSingleOp o1 -> op2 (text "\tmovgr2fr.w") o1 zero
    | isFloatOp o1 && isImmZero o2 && isDoubleOp o1 -> op2 (text "\tmovgr2fr.d") o1 zero
    | isFloatOp o1 && not (isFloatOp o2) && isSingleOp o1 -> op2 (text "\tmovgr2fr.w") o1 o2
    | isFloatOp o1 && not (isFloatOp o2) && isDoubleOp o1 -> op2 (text "\tmovgr2fr.d") o1 o2
    | not (isFloatOp o1) && isFloatOp o2 && isSingleOp o2 -> op2 (text "\tmovfr2gr.s") o1 o2
    | not (isFloatOp o1) && isFloatOp o2 && isDoubleOp o2 -> op2 (text "\tmovfr2gr.d") o1 o2
    | isImmOp o2, (OpImm (ImmInt i)) <- o2, fitsInNbits 12 (fromIntegral i) ->
      lines_ [text "\taddi.d" <+> pprOp platform o1 <> comma <+> pprOp platform x0 <+> comma <> pprOp platform o2]
    | isImmOp o2, (OpImm (ImmInteger i)) <- o2, fitsInNbits 12 (fromIntegral i) ->
      lines_ [text "\taddi.d" <+> pprOp platform o1 <> comma <+> pprOp platform x0 <+> comma <> pprOp platform o2]
    | OpReg W64 _ <- o2 -> op2 (text "\tmove") o1 o2
    | OpReg _ _ <- o2  ->
      lines_ [
        text "\tbstrpick.d" <+> pprOp platform o1 <> comma <+> pprOp platform o2 <> comma <+> pprOp platform (OpImm (ImmInt ((widthToInt (min (widthFromOpReg o1) (widthFromOpReg o2))) - 1))) <+> text ", 0"
             ]
    -- TODO: Maybe we can do more.
    -- Let the assembler do these concret things.
    | isImmOp o2 ->
      lines_ [text "\tli.d" <+> pprOp platform o1 <> comma <+> pprOp platform o2]
    | otherwise -> pprPanic "LA64.ppr: MOV error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2)

  -- CSET pesudo instrcutions implementation
  CSET cond dst o1 o2 -> case cond of
    -- SEQ dst, rd, rs -> [SUB rd, rd, rs;  sltui dst, rd, 1]
    EQ | isIntOp o1 && isIntOp o2 ->
      lines_ [
              subFor o1 o2,
              text "\tsltui" <+> pprOp platform dst <> comma <+> pprOp platform dst <> comma <+> pprOp platform (OpImm (ImmInt 1))
             ]
    EQ | isFloatOp o1 && isFloatOp o2 && isDoubleOp o1 && isDoubleOp o2 ->
      lines_ [
              text "\tfcmp.seq.d $fcc0," <+> pprOp platform o1 <> comma <+> pprOp platform o2,
              text "\tmovcf2gr" <+> pprOp platform dst <+> text ", $fcc0"
             ]
    EQ | isFloatOp o1 && isFloatOp o2 && isSingleOp o1 && isSingleOp o2 ->
      lines_ [
              text "\tfcmp.seq.s $fcc0," <+> pprOp platform o1 <> comma <+> pprOp platform o2,
              text "\tmovcf2gr" <+> pprOp platform dst <+> text ", $fcc0"
             ]
    -- SNE rd, rs -> [SUB rd, rd, rs;   sltu rd, zero, rs]
    NE | isIntOp o1 && isIntOp o2 ->
      lines_ [
              subFor o1 o2,
              text "\tsltu" <+> pprOp platform dst <> comma <+> text "$r0" <+> comma <+> pprOp platform dst
             ]
    NE | isFloatOp o1 && isFloatOp o2 && isDoubleOp o1 && isDoubleOp o2 ->
      lines_ [
              text "\tfcmp.cune.d $fcc0," <+> pprOp platform o1 <> comma <+> pprOp platform o2,
              text "\tmovcf2gr" <+> pprOp platform dst <+> text ", $fcc0"
             ]
    NE | isFloatOp o1 && isFloatOp o2 && isSingleOp o1 && isSingleOp o2 ->
      lines_ [
              text "\tfcmp.cune.s $fcc0," <+> pprOp platform o1 <> comma <+> pprOp platform o2,
              text "\tmovcf2gr" <+> pprOp platform dst <+> text ", $fcc0"
             ]
    SLT -> lines_ [ sltFor o1 o2 <+> pprOp platform dst <> comma <+> pprOp platform o1 <> comma <+> pprOp platform o2 ]
    -- SLE rd, rs -> [SLT rd, rs;  xori rd, rs, o1]
    SLE ->
      lines_ [
              sltFor o1 o2 <+> pprOp platform dst <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o1,
              text "\txori" <+>  pprOp platform dst <> comma <+> pprOp platform dst <> comma <+> pprOp platform (OpImm (ImmInt 1))
             ]
    -- SGE rd, rs -> [SLT rd, rs;  xori rd, rs, o1]
    SGE ->
      lines_ [
              sltFor o1 o2 <+> pprOp platform dst <> comma <+> pprOp platform o1 <> comma <+> pprOp platform o2,
              text "\txori" <+>  pprOp platform dst <> comma <+> pprOp platform dst <> comma <+> pprOp platform (OpImm (ImmInt 1))
             ]
    -- SGT rd, rs -> [SLT rd, rs]
    SGT -> lines_ [ sltFor o1 o2 <+> pprOp platform dst <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o1 ]

    ULT -> lines_ [ sltuFor o1 o2 <+> pprOp platform dst <> comma <+> pprOp platform o1 <> comma <+> pprOp platform o2 ]
    ULE ->
      lines_ [
              sltuFor o1 o2 <+> pprOp platform dst <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o1,
              text "\txori" <+> pprOp platform dst <> comma <+> pprOp platform dst <> comma <+> pprOp platform (OpImm (ImmInt 1))
             ]
    -- UGE rd, rs -> [SLTU rd, rs;  xori rd, rs, 1]
    UGE ->
      lines_ [
              sltuFor o1 o2 <+> pprOp platform dst <> comma <+> pprOp platform o1 <> comma <+> pprOp platform o2,
              text "\txori" <+>  pprOp platform dst <> comma <+> pprOp platform dst <> comma <+> pprOp platform (OpImm (ImmInt 1))
             ]
    -- SGTU rd, rs -> [SLTU rd, rs]
    UGT -> lines_ [ sltuFor o1 o2 <+> pprOp platform dst <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o1 ]

    -- TODO:
    -- LoongArch's floating point instrcutions don't write the compared result to an interger register, instead of cc.
    -- Fcond dst o1 o2 -> [fcmp.cond.[s/d] fcc0 o1 o2;  movcf2gr dst, fcc0]
    FLT | isFloatOp o1 && isFloatOp o2 && isDoubleOp o1 && isDoubleOp o2 ->
      lines_ [
              text "\tfcmp.slt.d $fcc0," <+> pprOp platform o1 <> comma <+> pprOp platform o2,
              text "\tmovcf2gr" <+> pprOp platform dst <+> text ", $fcc0"
             ]
    FLE | isFloatOp o1 && isFloatOp o2 && isDoubleOp o1 && isDoubleOp o2 ->
      lines_ [
              text "\tfcmp.sle.d $fcc0," <+> pprOp platform o1 <> comma <+> pprOp platform o2,
              text "\tmovcf2gr" <+> pprOp platform dst <+> text ", $fcc0"
             ]
    FGT | isFloatOp o1 && isFloatOp o2 && isDoubleOp o1 && isDoubleOp o2 ->
      lines_ [
              text "\tfcmp.slt.d $fcc0," <+> pprOp platform o2 <> comma <+> pprOp platform o1,
              text "\tmovcf2gr" <+> pprOp platform dst <+> text ", $fcc0"
             ]
    FGE | isFloatOp o1 && isFloatOp o2 && isDoubleOp o1 && isDoubleOp o2 ->
      lines_ [
              text "\tfcmp.sle.d $fcc0," <+> pprOp platform o2 <> comma <+> pprOp platform o1,
              text "\tmovcf2gr" <+> pprOp platform dst <+> text ", $fcc0"
             ]

    FLT | isFloatOp o1 && isFloatOp o2 && isSingleOp o1 && isSingleOp o2 ->
      lines_ [
              text "\tfcmp.slt.s $fcc0," <+> pprOp platform o1 <> comma <+> pprOp platform o2,
              text "\tmovcf2gr" <+> pprOp platform dst <+> text ", $fcc0"
             ]
    FLE | isFloatOp o1 && isFloatOp o2 && isSingleOp o1 && isSingleOp o2 ->
      lines_ [
              text "\tfcmp.sle.s $fcc0," <+> pprOp platform o1 <> comma <+> pprOp platform o2,
              text "\tmovcf2gr" <+> pprOp platform dst <+> text ", $fcc0"
             ]
    FGT | isFloatOp o1 && isFloatOp o2 && isSingleOp o1 && isSingleOp o2 ->
      lines_ [
              text "\tfcmp.slt.s $fcc0," <+> pprOp platform o2 <> comma <+> pprOp platform o1,
              text "\tmovcf2gr" <+> pprOp platform dst <+> text ", $fcc0"
             ]
    FGE | isFloatOp o1 && isFloatOp o2 && isSingleOp o1 && isSingleOp o2 ->
      lines_ [
              text "\tfcmp.sle.s $fcc0," <+> pprOp platform o2 <> comma <+> pprOp platform o1,
              text "\tmovcf2gr" <+> pprOp platform dst <+> text ", $fcc0"
             ]

    _ -> pprPanic "LA64.ppr: CSET error: " (pprCond cond <+> pprOp platform dst <> comma <+> (ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2)

    where
      subFor o1 o2  | (OpReg W64 _) <- dst, (OpImm _) <- o2  =
                        text "\taddi.d" <+> pprOp platform dst <> comma <+> pprOp platform o1 <> comma <+> pprOp platform (negOp o2)
                    | (OpReg W64 _) <- dst, (OpReg W64 _) <- o2 =
                        text "\tsub.d" <+> pprOp platform dst <> comma <+> pprOp platform o1 <> comma <+> pprOp platform o2
                    | otherwise = pprPanic "LA64.ppr: unknown subFor format: " ((ppr (widthFromOpReg dst)) <+> pprOp platform dst <+> (ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2)

      sltFor o1 o2  | (OpReg W64 _) <- dst, (OpImm _) <- o2   = text "\tslti"
                    | (OpReg W64 _) <- dst, (OpReg W64 _) <- o2 = text "\tslt"
                    | otherwise = pprPanic "LA64.ppr: unknown sltFor format: " ((ppr (widthFromOpReg dst)) <+> pprOp platform dst <+> (ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2)

      sltuFor o1 o2 | (OpReg W64 _) <- dst, (OpImm _) <- o2   = text "\tsltui"
                    | (OpReg W64 _) <- dst, (OpReg W64 _) <- o2 = text "\tsltu"
                    | otherwise = pprPanic "LA64.ppr: unknown sltuFor format: " ((ppr (widthFromOpReg dst)) <+> pprOp platform dst <+> (ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2)

    -- MUL.{W/D}, MULH, {W[U]/D[U]}, 'h' means high 32bit.
    -- MULW.D.W[U]
  MUL  o1 o2 o3
    | isFloatOp o1 && isFloatOp o2 && isFloatOp o3 && isSingleOp o1 && isSingleOp o2 && isSingleOp o3 -> op3 (text "\tfmul.s") o1 o2 o3
    | isFloatOp o1 && isFloatOp o2 && isFloatOp o3 && isDoubleOp o1 && isDoubleOp o2 && isDoubleOp o3 -> op3 (text "\tfmul.d") o1 o2 o3
    | OpReg W32 _ <- o2, OpReg W32 _ <- o3 -> op3 (text "\tmul.w") o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3 -> op3 (text "\tmul.d") o1 o2 o3
    | otherwise -> pprPanic "LA64.ppr: MUL error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
  MULW   o1 o2 o3
    | OpReg W32 _ <- o2, OpReg W32 _ <- o3 -> op3 (text "\tmulw.d.w") o1 o2 o3
    | otherwise -> pprPanic "LA64.ppr: MULW error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
  MULWU  o1 o2 o3
    | OpReg W32 _ <- o2, OpReg W32 _ <- o3 -> op3 (text "\tmulw.d.wu") o1 o2 o3
    | otherwise -> pprPanic "LA64.ppr: MULWU error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
  MULH  o1 o2 o3
    | OpReg W32 _ <- o2, OpReg W32 _ <- o3 -> op3 (text "\tmulh.w") o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o2 -> op3 (text "\tmulh.d") o1 o2 o3
    | otherwise -> pprPanic "LA64.ppr: MULH error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
  MULHU  o1 o2 o3
    | OpReg W32 _ <- o2, OpReg W32 _ <- o3 -> op3 (text "\tmulh.wu") o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3 -> op3 (text "\tmulh.du") o1 o2 o3
    | otherwise -> pprPanic "LA64.ppr: MULHU error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
    -- DIV.{W[U]/D[U]}, MOD.{W[U]/D[U]}
  DIV  o1 o2 o3
    | isFloatOp o1 && isFloatOp o2 && isFloatOp o3 && isSingleOp o1 && isSingleOp o2 && isSingleOp o3 -> op3 (text "\tfdiv.s") o1 o2 o3
    | isFloatOp o1 && isFloatOp o2 && isFloatOp o3 && isDoubleOp o1 && isDoubleOp o2 && isDoubleOp o3 -> op3 (text "\tfdiv.d") o1 o2 o3
    | OpReg W32 _ <- o2, OpReg W32 _ <- o3 -> op3 (text "\tdiv.w") o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3 -> op3 (text "\tdiv.d") o1 o2 o3
    | otherwise -> pprPanic "LA64.ppr: DIV error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
  DIVU  o1 o2 o3
    | OpReg W32 _ <- o2, OpReg W32 _ <- o3 -> op3 (text "\tdiv.wu") o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3 -> op3 (text "\tdiv.du") o1 o2 o3
    | otherwise -> pprPanic "LA64.ppr: DIVU error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
  MOD  o1 o2 o3
    | OpReg W32 _ <- o2, OpReg W32 _ <- o3 -> op3 (text "\tmod.w") o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3 -> op3 (text "\tmod.d") o1 o2 o3
    | otherwise -> pprPanic "LA64.ppr: MOD error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
  MODU  o1 o2 o3
    | OpReg W32 _ <- o2, OpReg W32 _ <- o3 -> op3 (text "\tmod.wu") o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3 -> op3 (text "\tmod.du") o1 o2 o3
    | otherwise -> pprPanic "LA64.ppr: MODU error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
  -- 2. Bit-shift Instuctions --------------------------------------------------
    -- SLL.W, SRL.W, SRA.W, ROTR.W
    -- SLL.D, SRL.D, SRA.D, ROTR.D
    -- SLLI.W, SRLI.W, SRAI.W, ROTRI.W
    -- SLLI.D, SRLI.D, SRAI.D, ROTRI.D
  SLL  o1 o2 o3
    | OpReg W32 _ <- o2, OpReg W32 _ <- o3 -> op3 (text "\tsll.w") o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3 -> op3 (text "\tsll.d") o1 o2 o3
    | OpReg W32 _ <- o2, isImmOp o3 ->
        lines_ [text "\tslli.w" <+> pprOp platform o1 <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o3]
    | OpReg W64 _ <- o2, isImmOp o3 ->
        lines_ [text "\tslli.d" <+> pprOp platform o1 <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o3]
    | otherwise -> pprPanic "LA64.ppr: SLL error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
  SRL  o1 o2 o3
    | OpReg W32 _ <- o2, OpReg W32 _ <- o3 -> op3 (text "\tsrl.w") o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3 -> op3 (text "\tsrl.d") o1 o2 o3
    | OpReg W32 _ <- o2, isImmOp o3 ->
        lines_ [text "\tsrli.w" <+> pprOp platform o1 <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o3]
    | OpReg W64 _ <- o2, isImmOp o3 ->
        lines_ [text "\tsrli.d" <+> pprOp platform o1 <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o3]
    | otherwise -> pprPanic "LA64.ppr: SRL error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
  SRA  o1 o2 o3
    | OpReg W32 _ <- o2, OpReg W32 _ <- o3 -> op3 (text "\tsra.w") o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3 -> op3 (text "\tsra.d") o1 o2 o3
    | OpReg W32 _ <- o2, isImmOp o3 ->
        lines_ [text "\tsrai.w" <+> pprOp platform o1 <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o3]
    | OpReg W64 _ <- o2, isImmOp o3 ->
        lines_ [text "\tsrai.d" <+> pprOp platform o1 <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o3]
    | otherwise -> pprPanic "LA64.ppr: SRA error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
  ROTR  o1 o2 o3
    | OpReg W32 _ <- o2, OpReg W32 _ <- o3 -> op3 (text "\trotr.w") o1 o2 o3
    | OpReg W64 _ <- o2, OpReg W64 _ <- o3 -> op3 (text "\trotr.d") o1 o2 o3
    | OpReg W32 _ <- o2, isImmOp o3 ->
        lines_ [text "\trotri.w" <+> pprOp platform o1 <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o3]
    | OpReg W64 _ <- o2, isImmOp o3 ->
        lines_ [text "\trotri.d" <+> pprOp platform o1 <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o3]
    | otherwise -> pprPanic "LA64.ppr: ROTR error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2 <+> (ppr (widthFromOpReg o3)) <+> pprOp platform o3)
  -- 3. Bit-manupulation Instructions ------------------------------------------
    -- EXT.W{B/H}
  EXT o1 o2
    | OpReg W8 _ <- o2  -> op2 (text "\text.w.b") o1 o2
    | OpReg W16 _ <- o2 -> op2 (text "\text.w.h") o1 o2
    | otherwise -> pprPanic "LA64.ppr: EXT error: " ((ppr (widthFromOpReg o1)) <+> pprOp platform o1 <+> (ppr (widthFromOpReg o2)) <+> pprOp platform o2)
    -- CL{O/Z}.{W/D}, CT{O/Z}.{W/D}
  CLO o1 o2
    | OpReg W32 _ <- o2 -> op2 (text "\tclo.w") o1 o2
    | OpReg W64 _ <- o2 -> op2 (text "\tclo.d") o1 o2
    | otherwise -> pprPanic "LA64.ppr: CLO error" (pprOp platform o1 <+> pprOp platform o2)
  CLZ o1 o2
    | OpReg W32 _ <- o2 -> op2 (text "\tclz.w") o1 o2
    | OpReg W64 _ <- o2 -> op2 (text "\tclz.d") o1 o2
    | otherwise -> pprPanic "LA64.ppr: CLZ error" (pprOp platform o1 <+> pprOp platform o2)
  CTO o1 o2
    | OpReg W32 _ <- o2 -> op2 (text "\tcto.w") o1 o2
    | OpReg W64 _ <- o2 -> op2 (text "\tcto.d") o1 o2
    | otherwise -> pprPanic "LA64.ppr: CTO error" (pprOp platform o1 <+> pprOp platform o2)
  CTZ o1 o2
    | OpReg W32 _ <- o2 -> op2 (text "\tctz.w") o1 o2
    | OpReg W64 _ <- o2 -> op2 (text "\tctz.d") o1 o2
    | otherwise -> pprPanic "LA64.ppr: CTZ error" (pprOp platform o1 <+> pprOp platform o2)
    -- BYTEPICK.{W/D} rd, rj, rk, sa2/sa3
  BYTEPICK o1 o2 o3 o4
    | OpReg W32 _ <- o2 -> op4 (text "\tbytepick.w") o1 o2 o3 o4
    | OpReg W64 _ <- o2 -> op4 (text "\tbytepick.d") o1 o2 o3 o4
    | otherwise -> pprPanic "LA64.ppr: BYTEPICK error" (pprOp platform o1 <+> pprOp platform o2 <+> pprOp platform o3 <+> pprOp platform o4)
    -- REVB.{2H/4H/2W/D}
  REVB2H o1 o2 -> op2 (text "\trevb.2h") o1 o2
  REVB4H o1 o2 -> op2 (text "\trevb.4h") o1 o2
  REVB2W o1 o2 -> op2 (text "\trevb.2w") o1 o2
  REVBD  o1 o2 -> op2 (text "\trevb.d") o1 o2
    -- REVH.{2W/D}
  REVH2W o1 o2 -> op2 (text "\trevh.2w") o1 o2
  REVHD o1 o2 -> op2 (text "\trevh.d") o1 o2
    -- BITREV.{4B/8B}
    -- BITREV.{W/D}
  BITREV4B o1 o2 -> op2 (text "\tbitrev.4b") o1 o2
  BITREV8B o1 o2 -> op2 (text "\tbitrev.8b") o1 o2
  BITREVW o1 o2 -> op2 (text "\tbitrev.w") o1 o2
  BITREVD o1 o2 -> op2 (text "\tbitrev.d") o1 o2
    -- BSTRINS.{W/D}
  BSTRINS II64 o1 o2 o3 o4 -> op4 (text "\tbstrins.d") o1 o2 o3 o4
  BSTRINS II32 o1 o2 o3 o4 -> op4 (text "\tbstrins.w") o1 o2 o3 o4
    -- BSTRPICK.{W/D}
  BSTRPICK II64 o1 o2 o3 o4 -> op4 (text "\tbstrpick.d") o1 o2 o3 o4
  BSTRPICK II32 o1 o2 o3 o4 -> op4 (text "\tbstrpick.w") o1 o2 o3 o4
    -- MASKEQZ rd, rj, rk:  if rk == 0 ? rd = 0 : rd = rj
  MASKEQZ o1 o2 o3 -> op3 (text "\tmaskeqz") o1 o2 o3
    -- MASKNEZ:  if rk == 0 ? rd = 0 : rd = rj
  MASKNEZ o1 o2 o3 -> op3 (text "\tmasknez") o1 o2 o3
  -- 4. Branch Instructions ----------------------------------------------------
    -- BEQ, BNE, BLT[U], BGE[U]   rj, rd, off16
    -- BEQZ, BNEZ   rj, off21
    -- B
    -- BL
    -- JIRL
    -- jr rd = jirl $zero, rd, 0: Commonly used for subroutine return.
  J (TReg r) -> line $ text "\tjirl" <+> text "$r0" <> comma <+> pprReg W64 r <> comma <+> text " 0"
  J_TBL _ _ r    -> pprInstr platform (B (TReg r))

  B (TBlock bid) -> line $ text "\tb" <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
  B (TLabel lbl) -> line $ text "\tb" <+> pprAsmLabel platform lbl
  B (TReg r)     -> line $ text "\tjr" <+> pprReg W64 r

  BL (TBlock bid) _ -> line $ text "\tbl" <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
  BL (TLabel lbl) _ -> line $ text "\tbl" <+> pprAsmLabel platform lbl
  BL (TReg r) _    -> line $ text "\tjirl" <+> text "$r1" <> comma <+> pprReg W64 r <> comma <+> text " 0"

  CALL (TBlock bid) _ -> line $ text "\tcall36" <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
  CALL (TLabel lbl) _ -> line $ text "\tcall36" <+> pprAsmLabel platform lbl
  CALL (TReg r) _ -> line $ text "\tjirl" <+> text "$r1" <> comma <+> pprReg W64 r <> comma <+> text " 0"

  CALL36 (TBlock bid) -> line $ text "\tcall36" <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
  CALL36 (TLabel lbl) -> line $ text "\tcall36" <+> pprAsmLabel platform lbl
  CALL36 _ -> panic "LA64.ppr: CALL36: Not to registers!"
  TAIL36 r (TBlock bid) -> line $ text "\ttail36" <+> pprOp platform r <> comma <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
  TAIL36 r (TLabel lbl) -> line $ text "\ttail36" <+> pprOp platform r <> comma <+> pprAsmLabel platform lbl
  TAIL36 _ _ -> panic "LA64.ppr: TAIL36: Not to registers!"

  BCOND1 c j d (TBlock bid) -> case c of
    SLE ->
      line $ text "\tbge" <+> pprOp platform d <> comma <+> pprOp platform j <> comma <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
    SGT ->
      line $ text "\tblt" <+> pprOp platform d <> comma <+> pprOp platform j <> comma <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
    ULE ->
      line $ text "\tbgeu" <+> pprOp platform d <> comma <+> pprOp platform j <> comma <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
    UGT ->
      line $ text "\tbltu" <+> pprOp platform d <> comma <+> pprOp platform j <> comma <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
    _ -> line $ text "\t" <> pprBcond c <+> pprOp platform j <> comma <+> pprOp platform d <> comma <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))

  BCOND1 _ _ _ (TLabel _) -> panic "LA64.ppr: BCOND1: No conditional branching to TLabel!"

  BCOND1 _ _ _ (TReg _) -> panic "LA64.ppr: BCOND1: No conditional branching to registers!"

  -- Reuse t8(IP) register
  BCOND c j d (TBlock bid) -> case c of
    SLE ->
      lines_ [
              text "\tslt $t8, " <+> pprOp platform  d <> comma <+> pprOp platform j,
              text "\tbeqz $t8, " <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
             ]
    SGT ->
      lines_ [
              text "\tslt $t8, " <+> pprOp platform  d <> comma <+> pprOp platform j,
              text "\tbnez $t8, " <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
             ]
    ULE ->
      lines_ [
              text "\tsltu $t8, " <+> pprOp platform  d <> comma <+> pprOp platform j,
              text "\tbeqz $t8, " <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
             ]
    UGT ->
      lines_ [
              text "\tsltu $t8, " <+> pprOp platform  d <> comma <+> pprOp platform j,
              text "\tbnez $t8, " <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
             ]
    EQ ->
      lines_ [
              text "\tsub.d $t8, " <+> pprOp platform  j <> comma <+> pprOp platform d,
              text "\tbeqz $t8, " <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
             ]
    NE ->
      lines_ [
              text "\tsub.d $t8, " <+> pprOp platform  j <> comma <+> pprOp platform d,
              text "\tbnez $t8, " <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
             ]
    SLT ->
      lines_ [
              text "\tslt $t8, " <+> pprOp platform  j <> comma <+> pprOp platform d,
              text "\tbnez $t8, " <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
             ]
    SGE ->
      lines_ [
              text "\tslt $t8, " <+> pprOp platform  j <> comma <+> pprOp platform d,
              text "\tbeqz $t8, " <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
             ]
    ULT ->
      lines_ [
              text "\tsltu $t8, " <+> pprOp platform  j <> comma <+> pprOp platform d,
              text "\tbnez $t8, " <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
             ]
    UGE ->
      lines_ [
              text "\tsltu $t8, " <+> pprOp platform  j <> comma <+> pprOp platform d,
              text "\tbeqz $t8, " <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
             ]
    _ -> panic "LA64.ppr: BCOND: Unsupported cond!"

  BCOND _ _ _ (TLabel _) -> panic "LA64.ppr: BCOND: No conditional branching to TLabel!"

  BCOND _ _ _ (TReg _) -> panic "LA64.ppr: BCOND: No conditional branching to registers!"

  BEQZ j (TBlock bid) ->
    line $ text "\tbeqz" <+> pprOp platform j <> comma <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
  BEQZ j (TLabel lbl) ->
    line $ text "\tbeqz" <+> pprOp platform j <> comma <+> pprAsmLabel platform lbl
  BEQZ _ (TReg _)     -> panic "LA64.ppr: BEQZ: No conditional branching to registers!"

  BNEZ j (TBlock bid) ->
    line $ text "\tbnez" <+> pprOp platform j <> comma <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
  BNEZ j (TLabel lbl) ->
    line $ text "\tbnez" <+> pprOp platform j <> comma <+> pprAsmLabel platform lbl
  BNEZ _ (TReg _)     -> panic "LA64.ppr: BNEZ: No conditional branching to registers!"

  -- 5. Common Memory Access Instructions --------------------------------------
    -- LD.{B[U]/H[U]/W[U]/D}, ST.{B/H/W/D}: AddrRegImm
    -- LD: load, ST: store, x: offset in register, u: load unsigned imm.
    -- LD format dst src: 'src' means final address, not single register or immdiate.
  -- Load symbol's address
  LD _fmt o1 (OpImm (ImmIndex lbl' off)) | Just (_, lbl) <- dynamicLinkerLabelInfo lbl' ->
    lines_ [ text "\tpcalau12i" <+> pprOp platform o1 <> comma <+> text "%got_pc_hi20(" <> pprAsmLabel platform lbl <> text ")"
            , text "\tld.d"   <+> pprOp platform o1 <> comma <+> pprOp platform o1 <> comma <+> text "%got_pc_lo12(" <> pprAsmLabel platform lbl <> text ")"
            , text "\taddi.d"   <+> pprOp platform o1 <> comma <+> pprOp platform o1 <> comma <+> int off
           ]
  LD _fmt o1 (OpImm (ImmIndex lbl off)) | isForeignLabel lbl ->
    lines_ [ text "\tpcalau12i" <+> pprOp platform o1 <> comma <+> text "%got_pc_hi20(" <> pprAsmLabel platform lbl <> text ")"
            , text "\tld.d"   <+> pprOp platform o1 <> comma <+> pprOp platform o1 <> comma <+> text "%got_pc_lo12(" <> pprAsmLabel platform lbl <> text ")"
            , text "\taddi.d"   <+> pprOp platform o1 <> comma <+> pprOp platform o1 <> comma <+> int off
           ]
  LD _fmt o1 (OpImm (ImmIndex lbl off)) ->
    lines_ [ text "\tpcalau12i" <+> pprOp platform o1 <> comma <+> text "%pc_hi20(" <> pprAsmLabel platform lbl <> text ")"
            , text "\taddi.d"    <+> pprOp platform o1 <> comma <+> pprOp platform o1 <> comma <+> text "%pc_lo12(" <> pprAsmLabel platform lbl <> text ")"
            , text "\taddi.d"   <+> pprOp platform o1 <> comma <+> pprOp platform o1 <> comma <+> int off
           ]

  LD _fmt o1 (OpImm (ImmCLbl lbl')) | Just (_, lbl) <- dynamicLinkerLabelInfo lbl' ->
    lines_ [ text "\tpcalau12i" <+> pprOp platform o1 <> comma <+> text "%got_pc_hi20(" <> pprAsmLabel platform lbl <> text ")"
            , text "\tld.d"   <+> pprOp platform o1 <> comma <+> pprOp platform o1 <> comma <+> text "%got_pc_lo12(" <> pprAsmLabel platform lbl <> text ")"
           ]
  LD _fmt o1 (OpImm (ImmCLbl lbl)) | isForeignLabel lbl ->
    lines_ [ text "\tpcalau12i" <+> pprOp platform o1 <> comma <+> text "%got_pc_hi20(" <> pprAsmLabel platform lbl <> text ")"
            , text "\tld.d"   <+> pprOp platform o1 <> comma <+> pprOp platform o1 <> comma <+> text "%got_pc_lo12(" <> pprAsmLabel platform lbl <> text ")"
           ]
  LD _fmt o1 (OpImm (ImmCLbl lbl)) ->
    lines_ [ text "\tpcalau12i" <+> pprOp platform o1 <> comma <+> text "%pc_hi20(" <> pprAsmLabel platform lbl <> text ")"
            , text "\taddi.d"    <+> pprOp platform o1 <> comma <+> pprOp platform o1 <> comma <+> text "%pc_lo12(" <> pprAsmLabel platform lbl <> text ")"
           ]

  LD II8  o1 o2 -> op2 (text "\tld.b") o1 o2
  LD II16 o1 o2 -> op2 (text "\tld.h") o1 o2
  LD II32 o1 o2 -> op2 (text "\tld.w") o1 o2
  LD II64 o1 o2 -> op2 (text "\tld.d") o1 o2
  LD FF32 o1 o2 -> op2 (text "\tfld.s") o1 o2
  LD FF64 o1 o2 -> op2 (text "\tfld.d") o1 o2

  LDU II8  o1 o2 -> op2 (text "\tld.bu") o1 o2
  LDU II16 o1 o2 -> op2 (text "\tld.hu") o1 o2
  LDU II32 o1 o2 -> op2 (text "\tld.wu") o1 o2
  LDU II64 o1 o2 -> op2 (text "\tld.d") o1 o2   -- double words (64bit) cannot be sign extended by definition
  LDU FF32 o1 o2@(OpAddr (AddrReg _))       -> op2 (text "\tfld.s") o1 o2
  LDU FF32 o1 o2@(OpAddr (AddrRegImm _ _))  -> op2 (text "\tfld.s") o1 o2
  LDU FF64 o1 o2@(OpAddr (AddrReg _))       -> op2 (text "\tfld.d") o1 o2
  LDU FF64 o1 o2@(OpAddr (AddrRegImm _ _))  -> op2 (text "\tfld.d") o1 o2
  LDU f o1 o2 -> pprPanic "Unsupported unsigned load" ((text.show) f <+> pprOp platform o1 <+> pprOp platform o2)

  ST II8  o1 o2 -> op2 (text "\tst.b") o1 o2
  ST II16 o1 o2 -> op2 (text "\tst.h") o1 o2
  ST II32 o1 o2 -> op2 (text "\tst.w") o1 o2
  ST II64 o1 o2 -> op2 (text "\tst.d") o1 o2
  ST FF32 o1 o2 -> op2 (text "\tfst.s") o1 o2
  ST FF64 o1 o2 -> op2 (text "\tfst.d") o1 o2

    -- LDPTR.{W/D}, STPTR.{W/D}: AddrRegImm: AddrRegImm
  LDPTR II32 o1 o2 -> op2 (text "\tldptr.w") o1 o2
  LDPTR II64 o1 o2 -> op2 (text "\tldptr.d") o1 o2
  STPTR II32 o1 o2 -> op2 (text "\tstptr.w") o1 o2
  STPTR II64 o1 o2 -> op2 (text "\tstptr.d") o1 o2

    -- LDX.{B[U]/H[U]/W[U]/D}, STX.{B/H/W/D}: AddrRegReg
  LDX II8   o1 o2 -> op2 (text "\tldx.b")  o1 o2
  LDX II16  o1 o2 -> op2 (text "\tldx.h")  o1 o2
  LDX II32  o1 o2 -> op2 (text "\tldx.w")  o1 o2
  LDX II64  o1 o2 -> op2 (text "\tldx.d")  o1 o2
  LDX FF32  o1 o2 -> op2 (text "\tfldx.s") o1 o2
  LDX FF64  o1 o2 -> op2 (text "\tfldx.d") o1 o2
  LDXU II8  o1 o2 -> op2 (text "\tldx.bu") o1 o2
  LDXU II16 o1 o2 -> op2 (text "\tldx.hu") o1 o2
  LDXU II32 o1 o2 -> op2 (text "\tldx.wu") o1 o2
  LDXU II64 o1 o2 -> op2 (text "\tldx.d")  o1 o2
  STX II8   o1 o2 -> op2 (text "\tstx.b")  o1 o2
  STX II16  o1 o2 -> op2 (text "\tstx.h")  o1 o2
  STX II32  o1 o2 -> op2 (text "\tstx.w")  o1 o2
  STX II64  o1 o2 -> op2 (text "\tstx.d")  o1 o2
  STX FF32  o1 o2 -> op2 (text "\tfstx.s") o1 o2
  STX FF64  o1 o2 -> op2 (text "\tfstx.d") o1 o2

  PRELD h o1@(OpAddr (AddrRegImm _ _)) -> op2 (text "\tpreld") h o1
  -- 6. Bound Check Memory Access Instructions ---------------------------------
    -- LD{GT/LE}.{B/H/W/D}, ST{GT/LE}.{B/H/W/D}
  -- 7. Atomic Memory Access Instructions --------------------------------------
    -- AM{SWAP/ADD/AND/OR/XOR/MAX/MIN}[DB].{W/D}, AM{MAX/MIN}[_DB].{WU/DU}
  AMSWAPDB II8 o1 o2 o3 -> op3 (text "\tamswap_db.b") o1 o2 o3
  AMSWAPDB II16 o1 o2 o3 -> op3 (text "\tamswap_db.h") o1 o2 o3
  AMSWAPDB II32 o1 o2 o3 -> op3 (text "\tamswap_db.w") o1 o2 o3
  AMSWAPDB II64 o1 o2 o3 -> op3 (text "\tamswap_db.d") o1 o2 o3
    -- AM.{SWAP/ADD}[_DB].{B/H}
    -- AMCAS[_DB].{B/H/W/D}
    -- LL.{W/D}, SC.{W/D}
    -- SC.Q
    -- LL.ACQ.{W/D}, SC.REL.{W/D}
  -- 8. Barrier Instructions ---------------------------------------------------
    -- DBAR, IBAR
  DBAR h -> line $ text "\tdbar" <+> pprBarrierType h
  IBAR h -> line $ text "\tibar" <+> pprBarrierType h

    -- Floating-point convert precision
  FCVT o1@(OpReg W32 _) o2@(OpReg W64 _) -> op2 (text "\tfcvt.s.d") o1 o2
  FCVT o1@(OpReg W64 _) o2@(OpReg W32 _) -> op2 (text "\tfcvt.d.s") o1 o2
  FCVT o1 o2 -> pprPanic "LA64.pprInstr - impossible float conversion" $
                  line (pprOp platform o1 <> text "->" <> pprOp platform o2)
    -- Signed fixed-point convert to floating-point
    -- For LoongArch, ffint.* instructions's second operand must be float-pointing register,
    -- so we need one more operation.
    -- Also to tfint.*.
  SCVTF o1@(OpReg W32 _) o2@(OpReg W32 _) -> lines_
    [
      text "\tmovgr2fr.w" <+> pprOp platform o1 <> comma <+> pprOp platform o2,
      text "\tffint.s.w" <+> pprOp platform o1 <> comma <+> pprOp platform o1
    ]
  SCVTF o1@(OpReg W32 _) o2@(OpReg W64 _) -> lines_
    [
      text "\tmovgr2fr.d" <+> pprOp platform o1 <> comma <+> pprOp platform o2,
      text "\tffint.s.l" <+> pprOp platform o1 <> comma <+> pprOp platform o1
    ]
  SCVTF o1@(OpReg W64 _) o2@(OpReg W32 _) -> lines_
    [
      text "\tmovgr2fr.w" <+> pprOp platform o1 <> comma <+> pprOp platform o2,
      text "\tffint.d.w" <+> pprOp platform o1 <> comma <+> pprOp platform o1
    ]
  SCVTF o1@(OpReg W64 _) o2@(OpReg W64 _) -> lines_
    [
      text "\tmovgr2fr.d" <+> pprOp platform o1 <> comma <+> pprOp platform o2,
      text "\tffint.d.l" <+> pprOp platform o1 <> comma <+> pprOp platform o1
    ]
  SCVTF o1 o2 -> pprPanic "LA64.pprInstr - impossible integer to float conversion" $
                  line (pprOp platform o1 <> text "->" <> pprOp platform o2)

    -- Floating-point convert to signed integer, rounding toward zero
    -- TODO: FCVTZS will destroy src-floating register if the previous opertion
    -- includes this reg. So I'm just stupidly saving and restoring by adding
    -- an extra register.
  FCVTZS o1@(OpReg W32 _) o2@(OpReg W32 _) o3@(OpReg W32 _) -> lines_
    [
      text "\tfmov.s" <+> pprOp platform o2 <> comma <+> pprOp platform o3,
      text "\tftintrz.w.s" <+> pprOp platform o3 <> comma <+> pprOp platform o3,
      text "\tmovfr2gr.s" <+> pprOp platform o1 <> comma <+> pprOp platform o3,
      text "\tfmov.s" <+> pprOp platform o3 <> comma <+> pprOp platform o2
    ]
  FCVTZS o1@(OpReg W32 _) o2@(OpReg W64 _) o3@(OpReg W64 _) -> lines_
    [
      text "\tfmov.d" <+> pprOp platform o2 <> comma <+> pprOp platform o3,
      text "\tftintrz.w.d" <+> pprOp platform o3 <> comma <+> pprOp platform o3,
      text "\tmovfr2gr.s" <+> pprOp platform o1 <> comma <+> pprOp platform o3,
      text "\tfmov.s" <+> pprOp platform o3 <> comma <+> pprOp platform o2
    ]
  FCVTZS o1@(OpReg W64 _) o2@(OpReg W32 _) o3@(OpReg W32 _) -> lines_
    [
      text "\tfmov.s" <+> pprOp platform o2 <> comma <+> pprOp platform o3,
      text "\tftintrz.l.s" <+> pprOp platform o3 <> comma <+> pprOp platform o3,
      text "\tmovfr2gr.d" <+> pprOp platform o1 <> comma <+> pprOp platform o3,
      text "\tfmov.s" <+> pprOp platform o3 <> comma <+> pprOp platform o2
    ]
  FCVTZS o1@(OpReg W64 _) o2@(OpReg W64 _) o3@(OpReg W64 _) -> lines_
    [
      text "\tfmov.d" <+> pprOp platform o2 <> comma <+> pprOp platform o3,
      text "\tftintrz.l.d" <+> pprOp platform o3 <> comma <+> pprOp platform o3,
      text "\tmovfr2gr.d" <+> pprOp platform o1 <> comma <+> pprOp platform o3,
      text "\tfmov.d" <+> pprOp platform o3 <> comma <+> pprOp platform o2
    ]
  FCVTZS o1 o2 o3 -> pprPanic "LA64.pprInstr - impossible float to integer conversion" $
                   line (pprOp platform o3 <> text "->" <+> pprOp platform o1 <+> text "tmpReg:" <+> pprOp platform o2)

  FMIN o1 o2 o3  -> op3 (text "fmin." <> if isSingleOp o2 then text "s" else text "d") o1 o2 o3
  FMINA o1 o2 o3 -> op3 (text "fmina." <> if isSingleOp o2 then text "s" else text "d") o1 o2 o3
  FMAX o1 o2 o3  -> op3 (text "fmax." <> if isSingleOp o2 then text "s" else text "d") o1 o2 o3
  FMAXA o1 o2 o3 -> op3 (text "fmaxa." <> if isSingleOp o2 then text "s" else text "d") o1 o2 o3
  FABS o1 o2 -> op2 (text "fabs." <> if isSingleOp o2 then text "s" else text "d") o1 o2
  FNEG o1 o2 -> op2 (text "fneg." <> if isSingleOp o2 then text "s" else text "d") o1 o2
  FSQRT o1 o2 -> op2 (text "fsqrt." <> if isSingleOp o2 then text "s" else text "d") o1 o2
  FMA variant d o1 o2 o3 ->
    let fma = case variant of
                FMAdd   -> text "\tfmadd." <+> floatPrecission d
                FMSub   -> text "\tfmsub." <+> floatPrecission d
                FNMAdd  -> text "\tfnmadd." <+> floatPrecission d
                FNMSub  -> text "\tfnmsub." <+> floatPrecission d
    in op4 fma d o1 o2 o3

  instr -> panic $ "LA64.pprInstr - Unknown instruction: " ++ (instrCon instr)
  where op2 op o1 o2        = line $ op <+> pprOp platform o1 <> comma <+> pprOp platform o2
        op3 op o1 o2 o3     = line $ op <+> pprOp platform o1 <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o3
        op4 op o1 o2 o3 o4  = line $ op <+> pprOp platform o1 <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o3 <> comma <+> pprOp platform o4
{-
    Support dbar with different hints.
    On LoongArch uses "dbar 0" (full completion barrier) for everything.
    But the full completion barrier has no performance to tell, so
    Loongson-3A6000 and newer processors have made finer granularity hints
    available:

    Hint 0x700: barrier for "read after read" from the same address.
    Bit4: ordering or completion (0: completion, 1: ordering)
    Bit3: barrier for previous read (0: true, 1: false)
    Bit2: barrier for previous write (0: true, 1: false)
    Bit1: barrier for succeeding read (0: true, 1: false)
    Bit0: barrier for succeeding write (0: true, 1: false)

    DBAR 0b10100: acquire
    DBAR 0b10010: release
    DBAR 0b10000: seqcst
-}
        pprBarrierType Hint0 = text "0x0"
        pprBarrierType HintSeqcst  = text "0x10"
        pprBarrierType HintRelease = text "0x12"
        pprBarrierType HintAcquire = text "0x14"
        pprBarrierType Hint700 = text "0x700"
        floatPrecission o | isSingleOp o = text "s"
                          | isDoubleOp o = text "d"
                          | otherwise  = pprPanic "Impossible floating point precission: " (pprOp platform o)

-- LoongArch64 Conditional Branch Instructions
pprBcond :: IsLine doc => Cond -> doc
pprBcond c = text "b" <> pprCond c

pprCond :: IsLine doc => Cond -> doc
pprCond c = case c of
      EQ -> text "eq"     -- beq  rj, rd, off16
      NE -> text "ne"     -- bne  rj, rd, off16
      SLT -> text "lt"    -- blt  rj, rd, off16
      SGE -> text "ge"    -- bge  rj, rd, off16
      ULT -> text "ltu"   -- bltu rj, rd, off16
      UGE -> text "geu"   -- bgeu rj, rd, off16
      -- Following not real instructions, just mark it.
      SLE    -> text "sle->ge"   -- ble  rj, rd, off16 -> bge  rd, rj, off16
      SGT    -> text "sgt->lt"   -- bgt  rj, rd, off16 -> blt  rd, rj, off16
      ULE    -> text "ule->geu"  -- bleu rj, rd, off16 -> bgeu rd, rj, off16
      UGT    -> text "ugt->ltu"  -- bgtu rj, rd, off16 -> bltu rd, rj, off16
      _ -> panic $ "LA64.ppr: non-implemented branch condition: " ++ show c
