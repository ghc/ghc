module GHC.CmmToAsm.AArch64.Ppr (pprNatCmmDecl) where

import GHC.Prelude hiding (EQ)

import GHC.CmmToAsm.AArch64.Instr
import GHC.CmmToAsm.AArch64.Regs
import GHC.CmmToAsm.AArch64.Cond
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
import GHC.Types.Basic (Alignment, mkAlignment, alignmentBytes)

import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.Ppr.Expr () -- For Outputable instances

import GHC.Types.Unique ( pprUniqueAlways, getUnique )
import GHC.Platform
import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Driver.Session (targetPlatform)

pprProcAlignment :: NCGConfig -> SDoc
pprProcAlignment config = maybe empty (pprAlign platform . mkAlignment) (ncgProcAlignment config)
   where
      platform = ncgPlatform config

pprNatCmmDecl :: NCGConfig -> NatCmmDecl RawCmmStatics Instr -> SDoc
pprNatCmmDecl config (CmmData section dats) =
  pprSectionAlign config section $$ pprDatas config dats

pprNatCmmDecl config proc@(CmmProc top_info lbl _ (ListGraph blocks)) =
  let platform = ncgPlatform config in
  pprProcAlignment config $$
  case topInfoTable proc of
    Nothing ->
        -- special case for code without info table:
        pprSectionAlign config (Section Text lbl) $$
        pprProcAlignment config $$
        pprLabel platform lbl $$ -- blocks guaranteed not null, so label needed
        vcat (map (pprBasicBlock config top_info) blocks) $$
        (if ncgDebugLevel config > 0
         then ppr (mkAsmTempEndLabel lbl) <> char ':' else empty) $$
        pprSizeDecl platform lbl

    Just (CmmStaticsRaw info_lbl _) ->
      pprSectionAlign config (Section Text info_lbl) $$
      pprProcAlignment config $$
      (if platformHasSubsectionsViaSymbols platform
          then ppr (mkDeadStripPreventer info_lbl) <> char ':'
          else empty) $$
      vcat (map (pprBasicBlock config top_info) blocks) $$
      -- above: Even the first block gets a label, because with branch-chain
      -- elimination, it might be the target of a goto.
      (if platformHasSubsectionsViaSymbols platform
       then -- See Note [Subsections Via Symbols]
                text "\t.long "
            <+> ppr info_lbl
            <+> char '-'
            <+> ppr (mkDeadStripPreventer info_lbl)
       else empty) $$
      pprSizeDecl platform info_lbl

pprNatCmmDecl _ _ = undefined

pprLabel :: Platform -> CLabel -> SDoc
pprLabel platform lbl =
   pprGloblDecl lbl
   $$ pprTypeDecl platform lbl
   $$ (ppr lbl <> char ':')

pprAlign :: Platform -> Alignment -> SDoc
pprAlign platform alignment
        = text ".align " <> int (alignmentOn platform)
  where
        bytes = alignmentBytes alignment
        alignmentOn platform = if platformOS platform == OSDarwin
                               then log2 bytes
                               else      bytes

        log2 :: Int -> Int  -- cache the common ones
        log2 1 = 0
        log2 2 = 1
        log2 4 = 2
        log2 8 = 3
        log2 n = 1 + log2 (n `quot` 2)

-- | Print appropriate alignment for the given section type.
pprAlignForSection :: Platform -> SectionType -> SDoc
pprAlignForSection _platform _seg
    -- .balign is stable, whereas .align is platform dependent.
    = text ".balign 8" --  always 8

instance Outputable Instr where
    ppr instr = sdocWithDynFlags $ \dflags ->
                       pprInstr (targetPlatform dflags) instr

-- | Print section header and appropriate alignment for that section.
pprSectionAlign :: NCGConfig -> Section -> SDoc
pprSectionAlign _config (Section (OtherSection _) _) =
     panic "AArch64.Ppr.pprSectionAlign: unknown section"
pprSectionAlign config sec@(Section seg _) =
    pprSectionHeader config sec $$
    pprAlignForSection (ncgPlatform config) seg

-- | Output the ELF .size directive.
pprSizeDecl :: Platform -> CLabel -> SDoc
pprSizeDecl platform lbl
 = if osElfTarget (platformOS platform)
   then text "\t.size" <+> ppr lbl <> ptext (sLit ", .-") <> ppr lbl
   else empty

pprBasicBlock :: NCGConfig -> LabelMap RawCmmStatics -> NatBasicBlock Instr
              -> SDoc
pprBasicBlock config info_env (BasicBlock blockid instrs)
  = maybe_infotable $
    pprLabel platform asmLbl $$
    vcat (map (pprInstr platform) instrs) $$
    (if  ncgDebugLevel config > 0
      then ppr (mkAsmTempEndLabel asmLbl) <> char ':'
      else empty
    )
  where
    asmLbl = blockLbl blockid
    platform = ncgPlatform config
    maybe_infotable c = case mapLookup blockid info_env of
       Nothing   -> c
       Just (CmmStaticsRaw info_lbl info) ->
           pprAlignForSection platform Text $$
           infoTableLoc $$
           vcat (map (pprData config) info) $$
           pprLabel platform info_lbl $$
           c $$
           (if ncgDebugLevel config > 0
             then ppr (mkAsmTempEndLabel info_lbl) <> char ':'
             else empty)
    -- Make sure the info table has the right .loc for the block
    -- coming right after it. See [Note: Info Offset]
    infoTableLoc = case instrs of
      (l@LOCATION{} : _) -> pprInstr platform l
      _other             -> empty

pprDatas :: NCGConfig -> RawCmmStatics -> SDoc
-- See note [emit-time elimination of static indirections] in "GHC.Cmm.CLabel".
pprDatas _config (CmmStaticsRaw alias [CmmStaticLit (CmmLabel lbl), CmmStaticLit ind, _, _])
  | lbl == mkIndStaticInfoLabel
  , let labelInd (CmmLabelOff l _) = Just l
        labelInd (CmmLabel l) = Just l
        labelInd _ = Nothing
  , Just ind' <- labelInd ind
  , alias `mayRedirectTo` ind'
  = pprGloblDecl alias
    $$ text ".equiv" <+> ppr alias <> comma <> ppr (CmmLabel ind')

pprDatas config (CmmStaticsRaw lbl dats)
  = vcat (pprLabel platform lbl : map (pprData config) dats)
   where
      platform = ncgPlatform config

pprData :: NCGConfig -> CmmStatic -> SDoc
pprData _config (CmmString str) = pprString str
pprData _config (CmmFileEmbed path) = pprFileEmbed path

pprData config (CmmUninitialised bytes)
 = let platform = ncgPlatform config
   in if platformOS platform == OSDarwin
         then text ".space " <> int bytes
         else text ".skip "  <> int bytes

pprData config (CmmStaticLit lit) = pprDataItem config lit

pprGloblDecl :: CLabel -> SDoc
pprGloblDecl lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = text ".globl " <> ppr lbl

-- See discussion in X86.Ppr
-- for why this is necessary.  Essentially we need to ensure that we never
-- pass function symbols when we migth want to lookup the info table.  If we
-- did, we could end up with procedure linking tables (PLT)s, and thus the
-- lookup wouldn't pooint to the function, but into the jump table.
pprLabelType' :: Platform -> CLabel -> SDoc
pprLabelType' platform lbl =
  if isCFunctionLabel lbl || functionOkInfoTable then
    text "@function"
  else
    text "@object"
  where
    functionOkInfoTable = platformTablesNextToCode platform &&
      isInfoTableLabel lbl && not (isConInfoTableLabel lbl)

-- this is called pprTypeAndSizeDecl in PPC.Ppr
pprTypeDecl :: Platform -> CLabel -> SDoc
pprTypeDecl platform lbl
    = if osElfTarget (platformOS platform) && externallyVisibleCLabel lbl
      then text ".type " <> ppr lbl <> ptext (sLit  ", ") <> pprLabelType' platform lbl
      else empty

pprDataItem :: NCGConfig -> CmmLit -> SDoc
pprDataItem config lit
  = vcat (ppr_item (cmmTypeFormat $ cmmLitType platform lit) lit)
    where
        platform = ncgPlatform config

        imm = litToImm lit

        ppr_item II8  _ = [text "\t.byte\t"  <> pprImm imm]
        ppr_item II16 _ = [text "\t.short\t" <> pprImm imm]
        ppr_item II32 _ = [text "\t.long\t"  <> pprImm imm]
        ppr_item II64 _ = [text "\t.quad\t"  <> pprImm imm]

        ppr_item FF32  (CmmFloat r _)
           = let bs = floatToBytes (fromRational r)
             in  map (\b -> text "\t.byte\t" <> pprImm (ImmInt b)) bs

        ppr_item FF64 (CmmFloat r _)
           = let bs = doubleToBytes (fromRational r)
             in  map (\b -> text "\t.byte\t" <> pprImm (ImmInt b)) bs

pprImm :: Imm -> SDoc

pprImm (ImmInt i)     = int i
pprImm (ImmInteger i) = integer i
pprImm (ImmCLbl l)    = ppr l
pprImm (ImmIndex l i) = ppr l <> char '+' <> int i
pprImm (ImmLit s)     = s

-- XXX: See pprIm below for why this is a bad idea!
pprImm (ImmFloat f)
  | f == 0 = text "wzr"
  | otherwise = float (fromRational f)
pprImm (ImmDouble d)
  | d == 0 = text "xzr"
  | otherwise = double (fromRational d)

pprImm (ImmConstantSum a b) = pprImm a <> char '+' <> pprImm b
pprImm (ImmConstantDiff a b) = pprImm a <> char '-'
                   <> lparen <> pprImm b <> rparen


-- aarch64 GNU as uses // for comments.
asmComment :: SDoc -> SDoc
asmComment c = whenPprDebug $ text "#" <+> c

asmMultilineComment :: SDoc -> SDoc
asmMultilineComment c = whenPprDebug $ text "/*" $+$ c $+$ text "*/"

pprIm :: Imm -> SDoc
pprIm im = case im of
  ImmInt i     -> char '#' <> int i
  ImmInteger i -> char '#' <> integer i

  -- XXX: This will only work for
  -- The floating point value must be expressable as ±n ÷ 16 × 2^r,
  -- where n and r are integers such that 16 ≤ n ≤ 31 and −3 ≤ r ≤ 4.
  -- and 0 needs to be encoded as wzr/xzr.
  --
  -- Except for 0, we might want to either split it up into enough
  -- ADD operations into an Integer register and then just bit copy it into
  -- the double register? See the toBytes + fromRational above for data items.
  -- This is something the x86 backend does.
  --
  -- We could also just turn them into statics :-/ Which is what the
  -- PowerPC backend odes.
  ImmFloat f | f == 0 -> text "wzr"
  ImmFloat f -> char '#' <> float (fromRational f)
  ImmDouble d | d == 0 -> text "xzr"
  ImmDouble d -> char '#' <> double (fromRational d)
  -- =<lbl> pseudo instruction!
  ImmCLbl l    -> char '=' <> ppr l
  ImmIndex l o -> text "[=" <> ppr l <> comma <+> char '#' <> int o <> char ']'
  _            -> panic "AArch64.pprIm"

pprExt :: ExtMode -> SDoc
pprExt EUXTB = text "uxtb"
pprExt EUXTH = text "uxth"
pprExt EUXTW = text "uxtw"
pprExt EUXTX = text "uxtx"
pprExt ESXTB = text "sxtb"
pprExt ESXTH = text "sxth"
pprExt ESXTW = text "sxtw"
pprExt ESXTX = text "sxtx"

pprShift :: ShiftMode -> SDoc
pprShift SLSL = text "lsl"
pprShift SLSR = text "lsr"
pprShift SASR = text "asr"
pprShift SROR = text "ror"

pprOp :: Operand -> SDoc
pprOp op = case op of
  OpReg w r           -> pprReg w r
  OpRegExt w r x 0 -> pprReg w r <> comma <+> pprExt x
  OpRegExt w r x i -> pprReg w r <> comma <+> pprExt x <> comma <+> char '#' <> int i
  OpRegShift w r s i -> pprReg w r <> comma <+> pprShift s <> comma <+> char '#' <> int i
  OpImm im          -> pprIm im
  OpImmShift im s i -> pprIm im <> comma <+> pprShift s <+> char '#' <> int i
  -- XXX: Address compuation always use registers as 64bit -- is this correct?
  OpAddr (AddrRegReg r1 r2) -> char '[' <+> pprReg W64 r1 <> comma <+> pprReg W64 r2 <+> char ']'
  OpAddr (AddrRegImm r1 im) -> char '[' <+> pprReg W64 r1 <> comma <+> pprImm im <+> char ']'
  OpAddr (AddrReg r1)       -> char '[' <+> pprReg W64 r1 <+> char ']'
  OpAddr _          -> panic "AArch64.pprOp: no amode"

pprReg :: Width -> Reg -> SDoc
pprReg w r = case r of
  RegReal    (RealRegSingle i) -> ppr_reg_no w i
  RegReal    (RealRegPair{})   -> panic "AArch64.pprReg: no reg pairs on this arch!"
  -- virtual regs should not show up, but this is helpful for debugging.
  RegVirtual (VirtualRegI u)   -> text "%vI_" <> pprUniqueAlways u
  RegVirtual (VirtualRegF u)   -> text "%vF_" <> pprUniqueAlways u
  RegVirtual (VirtualRegD u)   -> text "%vD_" <> pprUniqueAlways u

  where
    ppr_reg_no :: Width -> Int -> SDoc
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

pprInstr :: Platform -> Instr -> SDoc
pprInstr platform instr = case instr of
  -- Meta Instructions ---------------------------------------------------------
  COMMENT s  -> asmComment s
  MULTILINE_COMMENT s -> asmMultilineComment s
  ANN d i -> pprInstr platform i <+> asmComment d
  LOCATION file line col _name
    -> text "\t.loc" <+> ppr file <+> ppr line <+> ppr col
  DELTA d    -> asmComment $ text ("\tdelta = " ++ show d)
  NEWBLOCK _ -> panic "PprInstr: NEWBLOCK"
  LDATA _ _  -> panic "pprInstr: LDATA"

  -- Pseudo Instructions -------------------------------------------------------

  PUSH_STACK_FRAME -> text "\tstp x29, x30, [sp, #-16]!"
                   $$ text "\tmov x29, sp"

  POP_STACK_FRAME -> text "\tldp x29, x30, [sp], #16"
  -- ===========================================================================
  -- AArch64 Instruction Set
  -- 1. Arithmetic Instructions ------------------------------------------------
  ADD  o1 o2 o3
    | isFloatOp o1 && isFloatOp o2 && isFloatOp o3 -> text "\tfadd"  <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
    | otherwise -> text "\tadd"  <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
  CMN  o1 o2    -> text "\tcmn"  <+> pprOp o1 <> comma <+> pprOp o2
  CMP  o1 o2
    | isFloatOp o1 && isFloatOp o2 -> text "\tfcmp"  <+> pprOp o1 <> comma <+> pprOp o2
    | otherwise -> text "\tcmp" <+> pprOp o1 <> comma <+> pprOp o2
  MSUB o1 o2 o3 o4 -> text "\tmsub" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3 <> comma <+> pprOp o4
  MUL  o1 o2 o3
    | isFloatOp o1 && isFloatOp o2 && isFloatOp o3 -> text "\tfmul"  <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
    | otherwise -> text "\tmul"  <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
  NEG  o1 o2
    | isFloatOp o1 && isFloatOp o2 -> text "\tfneg"  <+> pprOp o1 <> comma <+> pprOp o2
    | otherwise -> text "\tneg"  <+> pprOp o1 <> comma <+> pprOp o2
  SDIV o1 o2 o3 | isFloatOp o1 && isFloatOp o2 && isFloatOp o3
    -> text "\tfdiv" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
  SDIV o1 o2 o3 -> text "\tsdiv" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3

  SUB  o1 o2 o3
    | isFloatOp o1 && isFloatOp o2 && isFloatOp o3 -> text "\tfsub"  <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
    | otherwise -> text "\tsub"  <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
  UDIV o1 o2 o3 -> text "\tudiv" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3

  -- 2. Bit Manipulation Instructions ------------------------------------------
  SBFM o1 o2 o3 o4 -> text "\tsbfm" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3 <> comma <+> pprOp o4
  UBFM o1 o2 o3 o4 -> text "\tubfm" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3 <> comma <+> pprOp o4
  -- 3. Logical and Move Instructions ------------------------------------------
  AND o1 o2 o3  -> text "\tand" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
  ANDS o1 o2 o3 -> text "\tands" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
  ASR o1 o2 o3  -> text "\tasr" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
  BIC o1 o2 o3  -> text "\tbic" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
  BICS o1 o2 o3 -> text "\tbics" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
  EON o1 o2 o3  -> text "\teon" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
  EOR o1 o2 o3  -> text "\teor" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
  LSL o1 o2 o3  -> text "\tlsl" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
  LSR o1 o2 o3  -> text "\tlsr" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
  MOV o1 o2
    | isFloatOp o1 || isFloatOp o2 -> text "\tfmov" <+> pprOp o1 <> comma <+> pprOp o2
    | otherwise -> text "\tmov" <+> pprOp o1 <> comma <+> pprOp o2
  MOVK o1 o2    -> text "\tmovk" <+> pprOp o1 <> comma <+> pprOp o2
  MVN o1 o2     -> text "\tmvn" <+> pprOp o1 <> comma <+> pprOp o2
  ORN o1 o2 o3  -> text "\torn" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
  ORR o1 o2 o3  -> text "\torr" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
  ROR o1 o2 o3  -> text "\tror" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
  TST o1 o2     -> text "\ttst" <+> pprOp o1 <> comma <+> pprOp o2

  -- 4. Branch Instructions ----------------------------------------------------
  B (TBlock bid) -> text "\tb" <+> ppr (mkLocalBlockLabel (getUnique bid))
  B (TLabel lbl) -> text "\tb" <+> ppr lbl
  B (TReg r)     -> text "\tbr" <+> pprReg W64 r

  BL (TBlock bid) -> text "\tbl" <+> ppr (mkLocalBlockLabel (getUnique bid))
  BL (TLabel lbl) -> text "\tbl" <+> ppr lbl
  BL (TReg r)     -> text "\tblr" <+> pprReg W64 r

  BCOND c (TBlock bid) -> text "\t" <> pprBcond c <+> ppr (mkLocalBlockLabel (getUnique bid))
  BCOND c (TLabel lbl) -> text "\t" <> pprBcond c <+> ppr lbl
  BCOND c (TReg r)     -> panic "AArch64.ppr: No conditional branching to registers!"

  -- 5. Atomic Instructions ----------------------------------------------------
  -- 6. Conditional Instructions -----------------------------------------------
  CSET o c  -> text "\tcset" <+> pprOp o <> comma <+> pprCond c

  -- 7. Load and Store Instructions --------------------------------------------
  -- NOTE: GHC may do whacky things where it only load the lower part of an
  --       address. Not observing the correct size when loading will lead
  --       inevitably to crashes.
  STR f o1@(OpReg W8 (RegReal (RealRegSingle i))) o2 | i < 32 ->
    text "\tstrb" <+> pprOp o1 <> comma <+> pprOp o2
  STR f o1@(OpReg W16 (RegReal (RealRegSingle i))) o2 | i < 32 ->
    text "\tstrh" <+> pprOp o1 <> comma <+> pprOp o2
  STR f o1 o2 -> text "\tstr" <+> pprOp o1 <> comma <+> pprOp o2

  LDR f o1 (OpImm (ImmIndex lbl off)) ->
    text "\tadrp " <+> pprOp o1 <> comma <+> ppr lbl $$
    text "\tadd" <+> pprOp o1 <> comma <+> pprOp o1 <> comma <+> text ":lo12:" <> ppr lbl $$
    text "\tadd" <+> pprOp o1 <> comma <+> pprOp o1 <> comma <+> char '#' <> int off -- XXX: check that off is in 12bits.

  LDR f o1 (OpImm (ImmCLbl lbl)) ->
    text "\tadrp" <+> pprOp o1 <> comma <+> ppr lbl $$
    text "\tadd" <+> pprOp o1 <> comma <+> pprOp o1 <> comma <+> text ":lo12:" <> ppr lbl

  LDR f o1@(OpReg W8 (RegReal (RealRegSingle i))) o2 | i < 32 ->
    text "\tldrsb" <+> pprOp o1 <> comma <+> pprOp o2
  LDR f o1@(OpReg W16 (RegReal (RealRegSingle i))) o2 | i < 32 ->
    text "\tldrsh" <+> pprOp o1 <> comma <+> pprOp o2
  LDR f o1 o2 -> text "\tldr" <+> pprOp o1 <> comma <+> pprOp o2

  STP f o1 o2 o3 -> text "\tstp" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3
  LDP f o1 o2 o3 -> text "\tldp" <+> pprOp o1 <> comma <+> pprOp o2 <> comma <+> pprOp o3

  -- 8. Synchronization Instructions -------------------------------------------
  DMBSY -> text "\tdmb sy"
  -- 8. Synchronization Instructions -------------------------------------------
  FCVT o1 o2 -> text "\tfcvt" <+> pprOp o1 <> comma <+> pprOp o2
  SCVTF o1 o2 -> text "\tscvtf" <+> pprOp o1 <> comma <+> pprOp o2
  FCVTZS o1 o2 -> text "\tfcvtzs" <+> pprOp o1 <> comma <+> pprOp o2

pprBcond :: Cond -> SDoc
pprBcond c = text "b." <> pprCond c

pprCond :: Cond -> SDoc
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

  NEVER  -> text "ne" -- Never
  VS     -> text "vs" -- Overflow                          ; Unordered (at least one NaN operand)
  VC     -> text "vc" -- No overflow                       ; Not unordered

  -- Orderd variants.  Respecting NaN.
  OLT    -> text "mi"
  OLE    -> text "ls"
  OGE    -> text "ge"
  OGT    -> text "gt"