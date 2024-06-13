{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module GHC.CmmToAsm.AArch64.Ppr (pprNatCmmDecl, pprInstr, pprBasicBlock) where

import GHC.Prelude hiding (EQ)

import GHC.CmmToAsm.AArch64.Instr
import GHC.CmmToAsm.AArch64.Regs
import GHC.CmmToAsm.AArch64.Cond
import GHC.CmmToAsm.Ppr
import GHC.CmmToAsm.Format
import GHC.Platform.Reg
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Utils

import GHC.Cmm hiding (topInfoTable)
import GHC.Cmm.Dataflow.Label

import GHC.Cmm.BlockId
import GHC.Cmm.CLabel

import GHC.Types.Unique ( pprUniqueAlways, getUnique )
import GHC.Platform
import GHC.Utils.Outputable

import GHC.Utils.Panic

pprNatCmmDecl :: IsDoc doc => NCGConfig -> NatCmmDecl RawCmmStatics Instr -> doc
pprNatCmmDecl config (CmmData section dats) =
  let platform = ncgPlatform config
  in
  pprSectionAlign config section $$ pprDatas platform dats

pprNatCmmDecl config proc@(CmmProc top_info lbl _ (ListGraph blocks)) =
  let platform = ncgPlatform config
      with_dwarf = ncgDwarfEnabled config
  in
  case topInfoTable proc of
    Nothing ->
        -- special case for code without info table:
        pprSectionAlign config (Section Text lbl) $$
        -- do not
        -- pprProcAlignment config $$
        pprLabel platform lbl $$ -- blocks guaranteed not null, so label needed
        vcat (map (pprBasicBlock platform with_dwarf top_info) blocks) $$
        (if ncgDwarfEnabled config
         then line (pprAsmLabel platform (mkAsmTempEndLabel lbl) <> char ':') else empty) $$
        pprSizeDecl platform lbl

    Just (CmmStaticsRaw info_lbl _) ->
      pprSectionAlign config (Section Text info_lbl) $$
      -- pprProcAlignment config $$
      (if platformHasSubsectionsViaSymbols platform
          then line (pprAsmLabel platform (mkDeadStripPreventer info_lbl) <> char ':')
          else empty) $$
      vcat (map (pprBasicBlock platform with_dwarf top_info) blocks) $$
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
     panic "AArch64.Ppr.pprSectionAlign: unknown section"
pprSectionAlign config sec@(Section seg _) =
    line (pprSectionHeader config sec)
    $$ pprAlignForSection (ncgPlatform config) seg

-- | Output the ELF .size directive.
pprSizeDecl :: IsDoc doc => Platform -> CLabel -> doc
pprSizeDecl platform lbl
 = if osElfTarget (platformOS platform)
   then line (text "\t.size" <+> pprAsmLabel platform lbl <> text ", .-" <> pprAsmLabel platform lbl)
   else empty

pprBasicBlock :: IsDoc doc => Platform -> {- dwarf enabled -} Bool -> LabelMap RawCmmStatics -> NatBasicBlock Instr
              -> doc
pprBasicBlock platform with_dwarf info_env (BasicBlock blockid instrs)
  = maybe_infotable $
    pprLabel platform asmLbl $$
    vcat (map (pprInstr platform) (id {-detectTrivialDeadlock-} optInstrs)) $$
    (if  with_dwarf
      then line (pprAsmLabel platform (mkAsmTempEndLabel asmLbl) <> char ':')
      else empty
    )
  where
    -- Filter out identity moves. E.g. mov x18, x18 will be dropped.
    optInstrs = filter f instrs
      where f (MOV o1 o2) | o1 == o2 = False
            f _ = True

    asmLbl = blockLbl blockid
    maybe_infotable c = case mapLookup blockid info_env of
       Nothing   -> c
       Just (CmmStaticsRaw info_lbl info) ->
          --  pprAlignForSection platform Text $$
           infoTableLoc $$
           vcat (map (pprData platform) info) $$
           pprLabel platform info_lbl $$
           c $$
           (if with_dwarf
             then line (pprAsmLabel platform (mkAsmTempEndLabel info_lbl) <> char ':')
             else empty)
    -- Make sure the info table has the right .loc for the block
    -- coming right after it. See Note [Info Offset]
    infoTableLoc = case instrs of
      (l@LOCATION{} : _) -> pprInstr platform l
      _other             -> empty

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

pprDatas platform (CmmStaticsRaw lbl dats)
  = vcat (pprLabel platform lbl : map (pprData platform) dats)

pprData :: IsDoc doc => Platform -> CmmStatic -> doc
pprData _platform (CmmString str) = line (pprString str)
pprData _platform (CmmFileEmbed path _) = line (pprFileEmbed path)

pprData platform (CmmUninitialised bytes)
 = line $ if platformOS platform == OSDarwin
                then text ".space " <> int bytes
                else text ".skip "  <> int bytes

pprData platform (CmmStaticLit lit) = pprDataItem platform lit

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

pprDataItem :: IsDoc doc => Platform -> CmmLit -> doc
pprDataItem platform lit
  = lines_ (ppr_item (cmmTypeFormat $ cmmLitType platform lit) lit)
    where
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
pprImm _ (ImmFloat f)
  | f == 0 = text "wzr"
  | otherwise = float (fromRational f)
pprImm _ (ImmDouble d)
  | d == 0 = text "xzr"
  | otherwise = double (fromRational d)

pprImm p (ImmConstantSum a b) = pprImm p a <> char '+' <> pprImm p b
pprImm p (ImmConstantDiff a b) = pprImm p a <> char '-'
                   <> lparen <> pprImm p b <> rparen


-- aarch64 GNU as uses // for comments.
asmComment :: SDoc -> SDoc
asmComment c = whenPprDebug $ text "#" <+> c

asmDoubleslashComment :: SDoc -> SDoc
asmDoubleslashComment c = whenPprDebug $ text "//" <+> c

asmMultilineComment :: SDoc -> SDoc
asmMultilineComment c = whenPprDebug $ text "/*" $+$ c $+$ text "*/"

pprIm :: IsLine doc => Platform -> Imm -> doc
pprIm platform im = case im of
  ImmInt i     -> char '#' <> int i
  ImmInteger i -> char '#' <> integer i

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
  ImmFloat f | f == 0 -> text "wzr"
  ImmFloat f -> char '#' <> float (fromRational f)
  ImmDouble d | d == 0 -> text "xzr"
  ImmDouble d -> char '#' <> double (fromRational d)
  -- =<lbl> pseudo instruction!
  ImmCLbl l    -> char '=' <> pprAsmLabel platform l
  ImmIndex l o -> text "[=" <> pprAsmLabel platform l <> comma <+> char '#' <> int o <> char ']'
  _            -> panic "AArch64.pprIm"

pprExt :: IsLine doc => ExtMode -> doc
pprExt EUXTB = text "uxtb"
pprExt EUXTH = text "uxth"
pprExt EUXTW = text "uxtw"
pprExt EUXTX = text "uxtx"
pprExt ESXTB = text "sxtb"
pprExt ESXTH = text "sxth"
pprExt ESXTW = text "sxtw"
pprExt ESXTX = text "sxtx"

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
  -- TODO: Address computation always use registers as 64bit -- is this correct?
  OpAddr (AddrRegReg r1 r2) -> char '[' <+> pprReg W64 r1 <> comma <+> pprReg W64 r2 <+> char ']'
  OpAddr (AddrRegImm r1 im) -> char '[' <+> pprReg W64 r1 <> comma <+> pprImm plat im <+> char ']'
  OpAddr (AddrReg r1)       -> char '[' <+> pprReg W64 r1 <+> char ']'

pprReg :: forall doc. IsLine doc => Width -> Reg -> doc
pprReg w r = case r of
  RegReal    (RealRegSingle i) -> ppr_reg_no w i
  -- virtual regs should not show up, but this is helpful for debugging.
  RegVirtual (VirtualRegI u)   -> text "%vI_" <> pprUniqueAlways u
  RegVirtual (VirtualRegF u)   -> text "%vF_" <> pprUniqueAlways u
  RegVirtual (VirtualRegD u)   -> text "%vD_" <> pprUniqueAlways u
  _                            -> pprPanic "AArch64.pprReg" (text $ show r)

  where
    ppr_reg_no :: Width -> Int -> doc
    ppr_reg_no w 31
         | w == W64 = text "sp"
         | w == W32 = text "wsp"

    ppr_reg_no w i
         | i < 0, w == W32 = text "wzr"
         | i < 0, w == W64 = text "xzr"
         | i < 0 = pprPanic "Invalid Zero Reg" (ppr w <+> int i)
         -- General Purpose Registers
         | i <= 31, w == W8  = text "w" <> int i      -- there are no byte or half
         | i <= 31, w == W16 = text "w" <> int i      -- words... word will do.
         | i <= 31, w == W32 = text "w" <> int i
         | i <= 31, w == W64 = text "x" <> int i
         | i <= 31 = pprPanic "Invalid Reg" (ppr w <+> int i)
         -- Floating Point Registers
         | i <= 63, w == W8  = text "b" <> int (i-32)
         | i <= 63, w == W16 = text "h" <> int (i-32)
         | i <= 63, w == W32 = text "s" <> int (i-32)
         | i <= 63, w == W64 = text "d" <> int (i-32)
         -- no support for 'q'uad in GHC's NCG yet.
         | otherwise = text "very naughty powerpc register"

isFloatOp :: Operand -> Bool
isFloatOp (OpReg _ (RegReal (RealRegSingle i))) | i > 31 = True
isFloatOp (OpReg _ (RegVirtual (VirtualRegF _))) = True
isFloatOp (OpReg _ (RegVirtual (VirtualRegD _))) = True
isFloatOp _ = False

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
               -- see Note [dualLine and dualDoc] in GHC.Utils.Outputable
  NEWBLOCK blockid -> -- This is invalid assembly. But NEWBLOCK should never be contained
                      -- in the final instruction stream. But we still want to be able to
                      -- print it for debugging purposes.
                      line (text "BLOCK " <> pprAsmLabel platform (blockLbl blockid))

  -- Pseudo Instructions -------------------------------------------------------

  PUSH_STACK_FRAME -> lines_ [text "\tstp x29, x30, [sp, #-16]!",
                              text "\tmov x29, sp"]

  POP_STACK_FRAME -> line $ text "\tldp x29, x30, [sp], #16"
  -- ===========================================================================
  -- AArch64 Instruction Set
  -- 1. Arithmetic Instructions ------------------------------------------------
  ADD  o1 o2 o3
    | isFloatOp o1 && isFloatOp o2 && isFloatOp o3 -> op3 (text "\tfadd") o1 o2 o3
    | otherwise -> op3 (text "\tadd") o1 o2 o3
  CMP  o1 o2
    | isFloatOp o1 && isFloatOp o2 -> op2 (text "\tfcmp") o1 o2
    | otherwise -> op2 (text "\tcmp") o1 o2
  CMN  o1 o2       -> op2 (text "\tcmn") o1 o2
  MSUB o1 o2 o3 o4 -> op4 (text "\tmsub") o1 o2 o3 o4
  MUL  o1 o2 o3
    | isFloatOp o1 && isFloatOp o2 && isFloatOp o3 -> op3 (text "\tfmul") o1 o2 o3
    | otherwise -> op3 (text "\tmul") o1 o2 o3
  SMULH o1 o2 o3 -> op3 (text "\tsmulh") o1 o2 o3
  SMULL o1 o2 o3 -> op3 (text "\tsmull") o1 o2 o3
  UMULH o1 o2 o3 -> op3 (text "\tumulh") o1 o2 o3
  UMULL o1 o2 o3 -> op3 (text "\tumull") o1 o2 o3
  NEG  o1 o2
    | isFloatOp o1 && isFloatOp o2 -> op2 (text "\tfneg") o1 o2
    | otherwise -> op2 (text "\tneg") o1 o2
  SDIV o1 o2 o3 | isFloatOp o1 && isFloatOp o2 && isFloatOp o3
    -> op3 (text "\tfdiv") o1 o2 o3
  SDIV o1 o2 o3 -> op3 (text "\tsdiv") o1 o2 o3

  SUB  o1 o2 o3
    | isFloatOp o1 && isFloatOp o2 && isFloatOp o3 -> op3 (text "\tfsub") o1 o2 o3
    | otherwise -> op3 (text "\tsub")  o1 o2 o3
  UDIV o1 o2 o3 -> op3 (text "\tudiv") o1 o2 o3

  -- 2. Bit Manipulation Instructions ------------------------------------------
  SBFM o1 o2 o3 o4 -> op4 (text "\tsbfm") o1 o2 o3 o4
  UBFM o1 o2 o3 o4 -> op4 (text "\tubfm") o1 o2 o3 o4
  CLZ  o1 o2       -> op2 (text "\tclz")  o1 o2
  RBIT o1 o2       -> op2 (text "\trbit")  o1 o2
  REV  (OpReg W8  (RegReal (RealRegSingle i))) _ | i < 32 ->
    {- swapping a single byte is a no-op -} empty
  REV  o1@(OpReg W16 (RegReal (RealRegSingle i))) o2 | i < 32 ->
                      op2 (text "\trev16") o1 o2
  REV  o1 o2       -> op2 (text "\trev")   o1 o2
  -- signed and unsigned bitfield extract
  SBFX o1 o2 o3 o4 -> op4 (text "\tsbfx") o1 o2 o3 o4
  UBFX o1 o2 o3 o4 -> op4 (text "\tubfx") o1 o2 o3 o4
  SXTB o1 o2       -> op2 (text "\tsxtb") o1 o2
  UXTB o1 o2       -> op2 (text "\tuxtb") o1 o2
  SXTH o1 o2       -> op2 (text "\tsxth") o1 o2
  UXTH o1 o2       -> op2 (text "\tuxth") o1 o2

  -- 3. Logical and Move Instructions ------------------------------------------
  AND o1 o2 o3  -> op3 (text "\tand") o1 o2 o3
  ASR o1 o2 o3  -> op3 (text "\tasr") o1 o2 o3
  EOR o1 o2 o3  -> op3 (text "\teor") o1 o2 o3
  LSL o1 o2 o3  -> op3 (text "\tlsl") o1 o2 o3
  LSR o1 o2 o3  -> op3 (text "\tlsr") o1 o2 o3
  MOV o1 o2
    | isFloatOp o1 || isFloatOp o2 -> op2 (text "\tfmov") o1 o2
    | otherwise                    -> op2 (text "\tmov") o1 o2
  MOVK o1 o2    -> op2 (text "\tmovk") o1 o2
  MOVZ o1 o2    -> op2 (text "\tmovz") o1 o2
  MVN o1 o2     -> op2 (text "\tmvn") o1 o2
  ORR o1 o2 o3  -> op3 (text "\torr") o1 o2 o3

  -- 4. Branch Instructions ----------------------------------------------------
  J t            -> pprInstr platform (B t)
  B (TBlock bid) -> line $ text "\tb" <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
  B (TLabel lbl) -> line $ text "\tb" <+> pprAsmLabel platform lbl
  B (TReg r)     -> line $ text "\tbr" <+> pprReg W64 r

  BL (TBlock bid) _ _ -> line $ text "\tbl" <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
  BL (TLabel lbl) _ _ -> line $ text "\tbl" <+> pprAsmLabel platform lbl
  BL (TReg r)     _ _ -> line $ text "\tblr" <+> pprReg W64 r

  BCOND c (TBlock bid) -> line $ text "\t" <> pprBcond c <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
  BCOND c (TLabel lbl) -> line $ text "\t" <> pprBcond c <+> pprAsmLabel platform lbl
  BCOND _ (TReg _)     -> panic "AArch64.ppr: No conditional branching to registers!"

  -- 5. Atomic Instructions ----------------------------------------------------
  -- 6. Conditional Instructions -----------------------------------------------
  CSET o c  -> line $ text "\tcset" <+> pprOp platform o <> comma <+> pprCond c

  CBZ o (TBlock bid) -> line $ text "\tcbz" <+> pprOp platform o <> comma <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
  CBZ o (TLabel lbl) -> line $ text "\tcbz" <+> pprOp platform o <> comma <+> pprAsmLabel platform lbl
  CBZ _ (TReg _)     -> panic "AArch64.ppr: No conditional (cbz) branching to registers!"

  CBNZ o (TBlock bid) -> line $ text "\tcbnz" <+> pprOp platform o <> comma <+> pprAsmLabel platform (mkLocalBlockLabel (getUnique bid))
  CBNZ o (TLabel lbl) -> line $ text "\tcbnz" <+> pprOp platform o <> comma <+> pprAsmLabel platform lbl
  CBNZ _ (TReg _)     -> panic "AArch64.ppr: No conditional (cbnz) branching to registers!"

  -- 7. Load and Store Instructions --------------------------------------------
  -- NOTE: GHC may do whacky things where it only load the lower part of an
  --       address. Not observing the correct size when loading will lead
  --       inevitably to crashes.
  STR _f o1@(OpReg W8 (RegReal (RealRegSingle i))) o2 | i < 32 ->
    op2 (text "\tstrb") o1 o2
  STR _f o1@(OpReg W16 (RegReal (RealRegSingle i))) o2 | i < 32 ->
    op2 (text "\tstrh") o1 o2
  STR _f o1 o2 -> op2 (text "\tstr") o1 o2
  STLR _f o1 o2 -> op2 (text "\tstlr") o1 o2

#if defined(darwin_HOST_OS)
  LDR _f o1 (OpImm (ImmIndex lbl' off)) | Just (_info, lbl) <- dynamicLinkerLabelInfo lbl' ->
    op_adrp o1 (pprAsmLabel platform lbl <> text "@gotpage") $$
    op_ldr o1 (pprAsmLabel platform lbl <> text "@gotpageoff") $$
    op_add o1 (char '#' <> int off) -- TODO: check that off is in 12bits.

  LDR _f o1 (OpImm (ImmIndex lbl off)) | isForeignLabel lbl ->
    op_adrp o1 (pprAsmLabel platform lbl <> text "@gotpage") $$
    op_ldr o1 (pprAsmLabel platform lbl <> text "@gotpageoff") $$
    op_add o1 (char '#' <> int off) -- TODO: check that off is in 12bits.

  LDR _f o1 (OpImm (ImmIndex lbl off)) ->
    op_adrp o1 (pprAsmLabel platform lbl <> text "@page") $$
    op_add o1 (pprAsmLabel platform lbl <> text "@pageoff") $$
    op_add o1 (char '#' <> int off) -- TODO: check that off is in 12bits.

  LDR _f o1 (OpImm (ImmCLbl lbl')) | Just (_info, lbl) <- dynamicLinkerLabelInfo lbl' ->
    op_adrp o1 (pprAsmLabel platform lbl <> text "@gotpage") $$
    op_ldr o1 (pprAsmLabel platform lbl <> text "@gotpageoff")

  LDR _f o1 (OpImm (ImmCLbl lbl)) | isForeignLabel lbl ->
    op_adrp o1 (pprAsmLabel platform lbl <> text "@gotpage") $$
    op_ldr o1 (pprAsmLabel platform lbl <> text "@gotpageoff")

  LDR _f o1 (OpImm (ImmCLbl lbl)) ->
    op_adrp o1 (pprAsmLabel platform lbl <> text "@page") $$
    op_add o1 (pprAsmLabel platform lbl <> text "@pageoff")

#else
  LDR _f o1 (OpImm (ImmIndex lbl' off)) | Just (_info, lbl) <- dynamicLinkerLabelInfo lbl' ->
    op_adrp o1 (text ":got:" <> pprAsmLabel platform lbl) $$
    op_ldr o1 (text ":got_lo12:" <> pprAsmLabel platform lbl) $$
    op_add o1 (char '#' <> int off) -- TODO: check that off is in 12bits.

  LDR _f o1 (OpImm (ImmIndex lbl off)) | isForeignLabel lbl ->
    op_adrp o1 (text ":got:" <> pprAsmLabel platform lbl) $$
    op_ldr o1 (text ":got_lo12:" <> pprAsmLabel platform lbl) $$
    op_add o1 (char '#' <> int off) -- TODO: check that off is in 12bits.

  LDR _f o1 (OpImm (ImmIndex lbl off)) ->
    op_adrp o1 (pprAsmLabel platform lbl) $$
    op_add o1 (text ":lo12:" <> pprAsmLabel platform lbl) $$
    op_add o1 (char '#' <> int off) -- TODO: check that off is in 12bits.

  LDR _f o1 (OpImm (ImmCLbl lbl')) | Just (_info, lbl) <- dynamicLinkerLabelInfo lbl' ->
    op_adrp o1 (text ":got:" <> pprAsmLabel platform lbl) $$
    op_ldr o1 (text ":got_lo12:" <> pprAsmLabel platform lbl)

  LDR _f o1 (OpImm (ImmCLbl lbl)) | isForeignLabel lbl ->
    op_adrp o1 (text ":got:" <> pprAsmLabel platform lbl) $$
    op_ldr o1 (text ":got_lo12:" <> pprAsmLabel platform lbl)

  LDR _f o1 (OpImm (ImmCLbl lbl)) ->
    op_adrp o1 (pprAsmLabel platform lbl) $$
    op_add o1 (text ":lo12:" <> pprAsmLabel platform lbl)

#endif

  LDR _f o1@(OpReg W8 (RegReal (RealRegSingle i))) o2 | i < 32 ->
    op2 (text "\tldrb") o1 o2
  LDR _f o1@(OpReg W16 (RegReal (RealRegSingle i))) o2 | i < 32 ->
    op2 (text "\tldrh") o1 o2
  LDR _f o1 o2 -> op2 (text "\tldr") o1 o2
  LDAR _f o1 o2 -> op2 (text "\tldar") o1 o2

  -- 8. Synchronization Instructions -------------------------------------------
  DMBISH -> line $ text "\tdmb ish"

  -- 9. Floating Point Instructions --------------------------------------------
  FMOV o1 o2 -> op2 (text "\tfmov") o1 o2
  FCVT o1 o2 -> op2 (text "\tfcvt") o1 o2
  SCVTF o1 o2 -> op2 (text "\tscvtf") o1 o2
  FCVTZS o1 o2 -> op2 (text "\tfcvtzs") o1 o2
  FABS o1 o2 -> op2 (text "\tfabs") o1 o2
  FMA variant d r1 r2 r3 ->
    let fma = case variant of
                FMAdd  -> text "\tfmadd"
                FMSub  -> text "\tfmsub"
                FNMAdd -> text "\tfnmadd"
                FNMSub -> text "\tfnmsub"
    in op4 fma d r1 r2 r3
 where op2 op o1 o2        = line $ op <+> pprOp platform o1 <> comma <+> pprOp platform o2
       op3 op o1 o2 o3     = line $ op <+> pprOp platform o1 <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o3
       op4 op o1 o2 o3 o4  = line $ op <+> pprOp platform o1 <> comma <+> pprOp platform o2 <> comma <+> pprOp platform o3 <> comma <+> pprOp platform o4
       op_ldr o1 rest      = line $ text "\tldr" <+> pprOp platform o1 <> comma <+> text "[" <> pprOp platform o1 <> comma <+> rest <> text "]"
       op_adrp o1 rest     = line $ text "\tadrp" <+> pprOp platform o1 <> comma <+> rest
       op_add o1 rest      = line $ text "\tadd" <+> pprOp platform o1 <> comma <+> pprOp platform o1 <> comma <+> rest

pprBcond :: IsLine doc => Cond -> doc
pprBcond c = text "b." <> pprCond c

pprCond :: IsLine doc => Cond -> doc
pprCond c = case c of
  ALWAYS -> text "al" -- Always
  EQ     -> text "eq" -- Equal
  NE     -> text "ne" -- Not Equal

  SLT    -> text "lt" -- Signed less than                  ; Less than, or unordered
  SLE    -> text "le" -- Signed less than or equal         ; Less than or equal, or unordered
  SGE    -> text "ge" -- Signed greater than or equal      ; Greater than or equal
  SGT    -> text "gt" -- Signed greater than               ; Greater than

  ULT    -> text "lo" -- Carry clear/ unsigned lower       ; less than
  ULE    -> text "ls" -- Unsigned lower or same            ; Less than or equal
  UGE    -> text "hs" -- Carry set/unsigned higher or same ; Greater than or equal, or unordered
  UGT    -> text "hi" -- Unsigned higher                   ; Greater than, or unordered

  -- NEVER  -> text "nv" -- Never
  VS     -> text "vs" -- Overflow                          ; Unordered (at least one NaN operand)
  VC     -> text "vc" -- No overflow                       ; Not unordered

  -- Ordered variants.  Respecting NaN.
  OLT    -> text "mi"
  OLE    -> text "ls"
  OGE    -> text "ge"
  OGT    -> text "gt"

  -- Unordered
  UOLT   -> text "lt"
  UOLE   -> text "le"
  UOGE   -> text "pl"
  UOGT   -> text "hi"
