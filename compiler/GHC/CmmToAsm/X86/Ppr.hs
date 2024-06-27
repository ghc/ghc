
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

module GHC.CmmToAsm.X86.Ppr (
        pprNatCmmDecl,
        pprInstr,
)

where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Reg

import GHC.CmmToAsm.X86.Regs
import GHC.CmmToAsm.X86.Instr
import GHC.CmmToAsm.X86.Cond
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Utils
import GHC.CmmToAsm.Ppr

import GHC.Cmm              hiding (topInfoTable)
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.DebugBlock (pprUnwindTable)

import GHC.Types.Basic (Alignment, mkAlignment, alignmentBytes)
import GHC.Types.Unique ( pprUniqueAlways )

import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.Word

-- Note [Subsections Via Symbols]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- If we are using the .subsections_via_symbols directive
-- (available on recent versions of Darwin),
-- we have to make sure that there is some kind of reference
-- from the entry code to a label on the _top_ of the info table,
-- so that the linker will not think it is unreferenced and dead-strip
-- it. That's why the label is called a DeadStripPreventer (_dsp).
--
-- The LLVM code gen already creates `iTableSuf` symbols, where
-- the X86 would generate the DeadStripPreventer (_dsp) symbol.
-- Therefore all that is left for llvm code gen, is to ensure
-- that all the `iTableSuf` symbols are marked as used.
-- As of this writing the documentation regarding the
-- .subsections_via_symbols and -dead_strip can be found at
-- <https://developer.apple.com/library/mac/documentation/DeveloperTools/Reference/Assembler/040-Assembler_Directives/asm_directives.html#//apple_ref/doc/uid/TP30000823-TPXREF101>

pprProcAlignment :: IsDoc doc => NCGConfig -> doc
pprProcAlignment config = maybe empty (pprAlign platform . mkAlignment) (ncgProcAlignment config)
   where
      platform = ncgPlatform config

pprNatCmmDecl :: IsDoc doc => NCGConfig -> NatCmmDecl (Alignment, RawCmmStatics) Instr -> doc
pprNatCmmDecl config (CmmData section dats) =
  pprSectionAlign config section $$ pprDatas config dats

pprNatCmmDecl config proc@(CmmProc top_info entry_lbl _ (ListGraph blocks)) =
  let platform = ncgPlatform config
      top_info_table = topInfoTable proc
      -- we need a label to delimit the proc code (e.g. in debug builds). When
      -- we have an info table, we reuse the info table label. Otherwise we make
      -- a fresh "entry" label from the label of the entry block. We can't reuse
      -- the entry block label as-is, otherwise we get redundant labels:
      -- delimiters for the entry block and for the whole proc are the same (see
      -- #22792).
      proc_lbl = case top_info_table of
        Just (CmmStaticsRaw info_lbl _) -> info_lbl
        Nothing                         -> toProcDelimiterLbl entry_lbl

      -- handle subsections_via_symbols when enabled and when we have an
      -- info-table to link to. See Note [Subsections Via Symbols]
      (sub_via_sym_label,sub_via_sym_offset)
        | platformHasSubsectionsViaSymbols platform
        , Just (CmmStaticsRaw info_lbl _) <- top_info_table
        , info_dsp_lbl <- pprAsmLabel platform (mkDeadStripPreventer info_lbl)
        = ( line (info_dsp_lbl <> colon)
          , line $ text "\t.long " <+> pprAsmLabel platform info_lbl <+> char '-' <+> info_dsp_lbl
          )
        | otherwise = (empty,empty)

  in vcat
    [ -- section directive. Requires proc_lbl when split-section is enabled to
      -- use as a subsection name.
      pprSectionAlign config (Section Text proc_lbl)

      -- section alignment. Note that when there is an info table, we align the
      -- info table and not the entry code!
    , pprProcAlignment config

      -- Special label when ncgExposeInternalSymbols is enabled. See Note
      -- [Internal proc labels] in GHC.Cmm.Label
    , pprExposedInternalProcLabel config entry_lbl

      -- Subsections-via-symbols label. See Note [Subsections Via Symbols]
    , sub_via_sym_label

      -- We need to print a label indicating the beginning of the entry code:
      -- 1. Without tables-next-to-code, we just print it here
      -- 2. With tables-next-to-code, the proc_lbl is the info-table label and it
      -- will be printed in pprBasicBlock after the info-table itself.
    , case top_info_table of
        Nothing -> pprLabel platform proc_lbl
        Just _  -> empty

      -- Proc's basic blocks
    , vcat (map (pprBasicBlock config top_info) blocks)
      -- Note that even the first block gets a label, because with branch-chain
      -- elimination, it might be the target of a goto.

      -- Print the proc end label when debugging is enabled
    , ppWhen (ncgDwarfEnabled config) $ line (pprProcEndLabel platform proc_lbl)

      -- Subsections-via-symbols offset. See Note [Subsections Via Symbols]
    , sub_via_sym_offset

      -- ELF .size directive (size of the entry code function)
    , pprSizeDecl platform proc_lbl
    ]
{-# SPECIALIZE pprNatCmmDecl :: NCGConfig -> NatCmmDecl (Alignment, RawCmmStatics) Instr -> SDoc #-}
{-# SPECIALIZE pprNatCmmDecl :: NCGConfig -> NatCmmDecl (Alignment, RawCmmStatics) Instr -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Output an internal proc label. See Note [Internal proc labels] in CLabel.
pprExposedInternalProcLabel :: IsDoc doc => NCGConfig -> CLabel -> doc
pprExposedInternalProcLabel config lbl
  | ncgExposeInternalSymbols config
  , Just lbl' <- ppInternalProcLabel (ncgThisModule config) lbl
  = line (lbl' <> colon)
  | otherwise
  = empty

pprProcEndLabel :: IsLine doc => Platform -> CLabel -- ^ Procedure name
                -> doc
pprProcEndLabel platform lbl = pprAsmLabel platform (mkAsmTempProcEndLabel lbl) <> colon

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

pprBasicBlock :: IsDoc doc => NCGConfig -> LabelMap RawCmmStatics -> NatBasicBlock Instr -> doc
pprBasicBlock config info_env (BasicBlock blockid instrs)
  = maybe_infotable $
    pprLabel platform block_label $$
    vcat (map (pprInstr platform) instrs) $$
    ppWhen (ncgDwarfEnabled config) (
      -- Emit both end labels since this may end up being a standalone
      -- top-level block
      line (pprBlockEndLabel platform block_label) $$
      line (pprProcEndLabel platform block_label)
    )
  where
    block_label = blockLbl blockid
    platform = ncgPlatform config
    maybe_infotable c = case mapLookup blockid info_env of
       Nothing -> c
       Just (CmmStaticsRaw infoLbl info) ->
           pprAlignForSection platform Text $$
           infoTableLoc $$
           vcat (map (pprData config) info) $$
           pprLabel platform infoLbl $$
           c $$
           ppWhen (ncgDwarfEnabled config) (line (pprBlockEndLabel platform infoLbl))

    -- Make sure the info table has the right .loc for the block
    -- coming right after it. See Note [Info Offset]
    infoTableLoc = case instrs of
      (l@LOCATION{} : _) -> pprInstr platform l
      _other             -> empty


pprDatas :: IsDoc doc => NCGConfig -> (Alignment, RawCmmStatics) -> doc
-- See Note [emit-time elimination of static indirections] in "GHC.Cmm.CLabel".
pprDatas config (_, CmmStaticsRaw alias [CmmStaticLit (CmmLabel lbl), CmmStaticLit ind, _, _])
  | lbl == mkIndStaticInfoLabel
  , let labelInd (CmmLabelOff l _) = Just l
        labelInd (CmmLabel l) = Just l
        labelInd _ = Nothing
  , Just ind' <- labelInd ind
  , alias `mayRedirectTo` ind'
  = pprGloblDecl (ncgPlatform config) alias
    $$ line (text ".equiv" <+> pprAsmLabel (ncgPlatform config) alias <> comma <> pprAsmLabel (ncgPlatform config) ind')

pprDatas config (align, (CmmStaticsRaw lbl dats))
 = vcat (pprAlign platform align : pprLabel platform lbl : map (pprData config) dats)
   where
      platform = ncgPlatform config

pprData :: IsDoc doc => NCGConfig -> CmmStatic -> doc
pprData _config (CmmString str) = line (pprString str)
pprData _config (CmmFileEmbed path _) = line (pprFileEmbed path)

pprData config (CmmUninitialised bytes)
 = line
 $ let platform = ncgPlatform config
   in if platformOS platform == OSDarwin
         then text ".space " <> int bytes
         else text ".skip "  <> int bytes

pprData config (CmmStaticLit lit) = pprDataItem config lit

pprGloblDecl :: IsDoc doc => Platform -> CLabel -> doc
pprGloblDecl platform lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = line (text ".globl " <> pprAsmLabel platform lbl)

pprLabelType' :: IsLine doc => Platform -> CLabel -> doc
pprLabelType' platform lbl =
  if isCFunctionLabel lbl || functionOkInfoTable then
    text "@function"
  else
    text "@object"
  where
    {-
    NOTE: This is a bit hacky.

    With the `tablesNextToCode` info tables look like this:
    ```
      <info table data>
    label_info:
      <info table code>
    ```
    So actually info table label points exactly to the code and we can mark
    the label as @function. (This is required to make perf and potentially other
    tools to work on Haskell binaries).
    This usually works well but it can cause issues with a linker.
    A linker uses different algorithms for the relocation depending on
    the symbol type.For some reason, a linker will generate JUMP_SLOT relocation
    when constructor info table is referenced from a data section.
    This only happens with static constructor call so
    we mark _con_info symbols as `@object` to avoid the issue with relocations.

    @SimonMarlow hack explanation:
    "The reasoning goes like this:

    * The danger when we mark a symbol as `@function` is that the linker will
      redirect it to point to the PLT and use a `JUMP_SLOT` relocation when
      the symbol refers to something outside the current shared object.
      A PLT / JUMP_SLOT reference only works for symbols that we jump to, not
      for symbols representing data,, nor for info table symbol references which
      we expect to point directly to the info table.
    * GHC generates code that might refer to any info table symbol from the text
      segment, but that's OK, because those will be explicit GOT references
      generated by the code generator.
    * When we refer to info tables from the data segment, it's either
      * a FUN_STATIC/THUNK_STATIC local to this module
      * a `con_info` that could be from anywhere

    So, the only info table symbols that we might refer to from the data segment
    of another shared object are `con_info` symbols, so those are the ones we
    need to exclude from getting the @function treatment.
    "

    A good place to check for more
    https://gitlab.haskell.org/ghc/ghc/wikis/commentary/position-independent-code

    Another possible hack is to create an extra local function symbol for
    every code-like thing to give the needed information for to the tools
    but mess up with the relocation. https://phabricator.haskell.org/D4730
    -}
    functionOkInfoTable = platformTablesNextToCode platform &&
      isInfoTableLabel lbl && not (isCmmInfoTableLabel lbl) && not (isConInfoTableLabel lbl)


pprTypeDecl :: IsDoc doc => Platform -> CLabel -> doc
pprTypeDecl platform lbl
    = if osElfTarget (platformOS platform) && externallyVisibleCLabel lbl
      then line (text ".type " <> pprAsmLabel platform lbl <> text ", " <> pprLabelType' platform lbl)
      else empty

pprLabel :: IsDoc doc => Platform -> CLabel -> doc
pprLabel platform lbl =
   pprGloblDecl platform lbl
   $$ pprTypeDecl platform lbl
   $$ line (pprAsmLabel platform lbl <> colon)

pprAlign :: IsDoc doc => Platform -> Alignment -> doc
pprAlign platform alignment
        = line $ text ".align " <> int (alignmentOn platform)
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

pprReg :: forall doc. IsLine doc => Platform -> Format -> Reg -> doc
pprReg platform f r
  = case r of
      RegReal    (RealRegSingle i) ->
          if target32Bit platform then ppr32_reg_no f i
                                  else ppr64_reg_no f i
      RegVirtual (VirtualRegI    u) -> text "%vI_"   <> pprUniqueAlways u
      RegVirtual (VirtualRegHi   u) -> text "%vHi_"  <> pprUniqueAlways u
      RegVirtual (VirtualRegD    u) -> text "%vD_"   <> pprUniqueAlways u
      RegVirtual (VirtualRegV128 u) -> text "%vV128_" <> pprUniqueAlways u

  where
    ppr32_reg_no :: Format -> Int -> doc
    ppr32_reg_no II8   = ppr32_reg_byte
    ppr32_reg_no II16  = ppr32_reg_word
    ppr32_reg_no fmt   = ppr32_reg_long fmt

    ppr32_reg_byte i =
      case i of {
         0 -> text "%al";     1 -> text "%bl";
         2 -> text "%cl";     3 -> text "%dl";
        _  -> text "very naughty I386 byte register: " <> int i
      }

    ppr32_reg_word i =
      case i of {
         0 -> text "%ax";     1 -> text "%bx";
         2 -> text "%cx";     3 -> text "%dx";
         4 -> text "%si";     5 -> text "%di";
         6 -> text "%bp";     7 -> text "%sp";
        _  -> text "very naughty I386 word register"
      }

    ppr32_reg_long fmt i =
      case i of {
         0 -> text "%eax";    1 -> text "%ebx";
         2 -> text "%ecx";    3 -> text "%edx";
         4 -> text "%esi";    5 -> text "%edi";
         6 -> text "%ebp";    7 -> text "%esp";
         _  -> ppr_reg_float fmt i
      }

    ppr64_reg_no :: Format -> Int -> doc
    ppr64_reg_no II8   = ppr64_reg_byte
    ppr64_reg_no II16  = ppr64_reg_word
    ppr64_reg_no II32  = ppr64_reg_long
    ppr64_reg_no fmt   = ppr64_reg_quad fmt

    ppr64_reg_byte i =
      case i of {
         0 -> text "%al";      1 -> text "%bl";
         2 -> text "%cl";      3 -> text "%dl";
         4 -> text "%sil";     5 -> text "%dil"; -- new 8-bit regs!
         6 -> text "%bpl";     7 -> text "%spl";
         8 -> text "%r8b";     9 -> text "%r9b";
        10 -> text "%r10b";   11 -> text "%r11b";
        12 -> text "%r12b";   13 -> text "%r13b";
        14 -> text "%r14b";   15 -> text "%r15b";
        _  -> text "very naughty x86_64 byte register: " <> int i
      }

    ppr64_reg_word i =
      case i of {
         0 -> text "%ax";      1 -> text "%bx";
         2 -> text "%cx";      3 -> text "%dx";
         4 -> text "%si";      5 -> text "%di";
         6 -> text "%bp";      7 -> text "%sp";
         8 -> text "%r8w";     9 -> text "%r9w";
        10 -> text "%r10w";   11 -> text "%r11w";
        12 -> text "%r12w";   13 -> text "%r13w";
        14 -> text "%r14w";   15 -> text "%r15w";
        _  -> text "very naughty x86_64 word register"
      }

    ppr64_reg_long i =
      case i of {
         0 -> text "%eax";    1  -> text "%ebx";
         2 -> text "%ecx";    3  -> text "%edx";
         4 -> text "%esi";    5  -> text "%edi";
         6 -> text "%ebp";    7  -> text "%esp";
         8 -> text "%r8d";    9  -> text "%r9d";
        10 -> text "%r10d";   11 -> text "%r11d";
        12 -> text "%r12d";   13 -> text "%r13d";
        14 -> text "%r14d";   15 -> text "%r15d";
        _  -> text "very naughty x86_64 register"
      }

    ppr64_reg_quad fmt i =
      case i of {
         0 -> text "%rax";     1 -> text "%rbx";
         2 -> text "%rcx";     3 -> text "%rdx";
         4 -> text "%rsi";     5 -> text "%rdi";
         6 -> text "%rbp";     7 -> text "%rsp";
         8 -> text "%r8";      9 -> text "%r9";
        10 -> text "%r10";    11 -> text "%r11";
        12 -> text "%r12";    13 -> text "%r13";
        14 -> text "%r14";    15 -> text "%r15";
        _  -> ppr_reg_float fmt i
      }

ppr_reg_float :: IsLine doc => Format -> Int -> doc
ppr_reg_float fmt i
  | W256 <- size
  = case i of
        16 -> text "%ymm0" ;   17 -> text "%ymm1"
        18 -> text "%ymm2" ;   19 -> text "%ymm3"
        20 -> text "%ymm4" ;   21 -> text "%ymm5"
        22 -> text "%ymm6" ;   23 -> text "%ymm7"
        24 -> text "%ymm8" ;   25 -> text "%ymm9"
        26 -> text "%ymm10";   27 -> text "%ymm11"
        28 -> text "%ymm12";   29 -> text "%ymm13"
        30 -> text "%ymm14";   31 -> text "%ymm15"
        _  -> text "very naughty x86 register"
  | W512 <- size
  = case i of
        16 -> text "%zmm0" ;   17 -> text "%zmm1"
        18 -> text "%zmm2" ;   19 -> text "%zmm3"
        20 -> text "%zmm4" ;   21 -> text "%zmm5"
        22 -> text "%zmm6" ;   23 -> text "%zmm7"
        24 -> text "%zmm8" ;   25 -> text "%zmm9"
        26 -> text "%zmm10";   27 -> text "%zmm11"
        28 -> text "%zmm12";   29 -> text "%zmm13"
        30 -> text "%zmm14";   31 -> text "%zmm15"
        _  -> text "very naughty x86 register"
  | otherwise
  = case i of
        16 -> text "%xmm0" ;   17 -> text "%xmm1"
        18 -> text "%xmm2" ;   19 -> text "%xmm3"
        20 -> text "%xmm4" ;   21 -> text "%xmm5"
        22 -> text "%xmm6" ;   23 -> text "%xmm7"
        24 -> text "%xmm8" ;   25 -> text "%xmm9"
        26 -> text "%xmm10";   27 -> text "%xmm11"
        28 -> text "%xmm12";   29 -> text "%xmm13"
        30 -> text "%xmm14";   31 -> text "%xmm15"
        _  -> text "very naughty x86 register"
  where size = formatToWidth fmt

pprFormat :: IsLine doc => Format -> doc
pprFormat x = case x of
  II8   -> text "b"
  II16  -> text "w"
  II32  -> text "l"
  II64  -> text "q"
  FF32  -> text "ss"      -- "scalar single-precision float" (SSE2)
  FF64  -> text "sd"      -- "scalar double-precision float" (SSE2)
  VecFormat _ FmtFloat  -> text "ps"
  VecFormat _ FmtDouble -> text "pd"
  -- TODO: this is shady because it only works for certain instructions
  VecFormat _ FmtInt8   -> text "b"
  VecFormat _ FmtInt16  -> text "w"
  VecFormat _ FmtInt32  -> text "l"
  VecFormat _ FmtInt64  -> text "q"

pprFormat_x87 :: IsLine doc => Format -> doc
pprFormat_x87 x = case x of
  FF32  -> text "s"
  FF64  -> text "l"
  _     -> panic "X86.Ppr.pprFormat_x87"


pprCond :: IsLine doc => Cond -> doc
pprCond c = case c of {
  GEU     -> text "ae";   LU   -> text "b";
  EQQ     -> text "e";    GTT  -> text "g";
  GE      -> text "ge";   GU   -> text "a";
  LTT     -> text "l";    LE   -> text "le";
  LEU     -> text "be";   NE   -> text "ne";
  NEG     -> text "s";    POS  -> text "ns";
  CARRY   -> text "c";   OFLO  -> text "o";
  PARITY  -> text "p";   NOTPARITY -> text "np";
  ALWAYS  -> text "mp"}


pprImm :: IsLine doc => Platform -> Imm -> doc
pprImm platform = \case
   ImmInt i            -> int i
   ImmInteger i        -> integer i
   ImmCLbl l           -> pprAsmLabel platform l
   ImmIndex l i        -> pprAsmLabel platform l <> char '+' <> int i
   ImmLit s            -> ftext s
   ImmFloat f          -> float $ fromRational f
   ImmDouble d         -> double $ fromRational d
   ImmConstantSum a b  -> pprImm platform a <> char '+' <> pprImm platform b
   ImmConstantDiff a b -> pprImm platform a <> char '-' <> lparen <> pprImm platform b <> rparen



pprAddr :: IsLine doc => Platform -> AddrMode -> doc
pprAddr platform (ImmAddr imm off)
  = let pp_imm = pprImm platform imm
    in
    if (off == 0) then
        pp_imm
    else if (off < 0) then
        pp_imm <> int off
    else
        pp_imm <> char '+' <> int off

pprAddr platform (AddrBaseIndex base index displacement)
  = let
        pp_disp  = ppr_disp displacement
        pp_off p = pp_disp <> char '(' <> p <> char ')'
        pp_reg r = pprReg platform (archWordFormat (target32Bit platform)) r
    in
    case (base, index) of
      (EABaseNone,  EAIndexNone) -> pp_disp
      (EABaseReg b, EAIndexNone) -> pp_off (pp_reg b)
      (EABaseRip,   EAIndexNone) -> pp_off (text "%rip")
      (EABaseNone,  EAIndex r i) -> pp_off (comma <> pp_reg r <> comma <> int i)
      (EABaseReg b, EAIndex r i) -> pp_off (pp_reg b <> comma <> pp_reg r
                                       <> comma <> int i)
      _                         -> panic "X86.Ppr.pprAddr: no match"

  where
    ppr_disp (ImmInt 0) = empty
    ppr_disp imm        = pprImm platform imm

-- | Print section header and appropriate alignment for that section.
pprSectionAlign :: IsDoc doc => NCGConfig -> Section -> doc
pprSectionAlign _config (Section (OtherSection _) _) =
     panic "X86.Ppr.pprSectionAlign: unknown section"
pprSectionAlign config sec@(Section seg _) =
    line (pprSectionHeader config sec) $$
    pprAlignForSection (ncgPlatform config) seg

-- | Print appropriate alignment for the given section type.
pprAlignForSection :: IsDoc doc => Platform -> SectionType -> doc
pprAlignForSection platform seg = line $
    text ".align " <>
    case platformOS platform of
      -- Darwin: alignments are given as shifts.
      OSDarwin
       | target32Bit platform ->
          case seg of
           CString           -> int 1
           _                 -> int 2
       | otherwise ->
          case seg of
           CString           -> int 1
           _                 -> int 3
      -- Other: alignments are given as bytes.
      _
       | target32Bit platform ->
          case seg of
           Text              -> text "4,0x90"
           CString           -> int 1
           _                 -> int 4
       | otherwise ->
          case seg of
           CString           -> int 1
           _                 -> int 8

pprDataItem :: forall doc. IsDoc doc => NCGConfig -> CmmLit -> doc
pprDataItem config lit =
  let (itemFmt, items) = itemFormatAndItems (cmmTypeFormat $ cmmLitType platform lit)
  in line $ itemFmt <> hsep (punctuate comma (items lit))
    where
        platform = ncgPlatform config

        pprLitImm, pprII64AsII32x2 :: CmmLit -> [Line doc]
        pprLitImm = (:[]) . pprImm platform . litToImm
        pprII64AsII32x2 (CmmInt x _)
          = [ int (fromIntegral (fromIntegral x :: Word32))
            , int (fromIntegral (fromIntegral (x `shiftR` 32) :: Word32)) ]
        pprII64AsII32x2 x
          = pprPanic "X86 pprDataItem II64" (ppr x)

        itemFormatAndItems :: Format -> (Line doc, CmmLit -> [Line doc])
        itemFormatAndItems = \case
          II8  -> ( text "\t.byte\t", pprLitImm )
          II16 -> ( text "\t.word\t", pprLitImm )
          II32 -> ( text "\t.long\t", pprLitImm )
          II64 ->
            case platformOS platform of
              OSDarwin
                | target32Bit platform
                -> ( text "\t.long\t", pprII64AsII32x2 )
              _ -> ( text "\t.quad\t", pprLitImm )
          FF32 -> ( text "\t.float\t", pprLitImm )
          FF64 -> ( text "\t.double\t", pprLitImm )
          VecFormat _ sFmt ->
            let (fmtTxt, pprElt) = itemFormatAndItems (scalarFormatFormat sFmt)
            in (fmtTxt, \ case { CmmVec elts -> pprElt =<< elts
                               ; x -> pprPanic "X86 pprDataItem VecFormat" (ppr x)
                               })

asmComment :: IsLine doc => doc -> doc
asmComment c = whenPprDebug $ text "# " <> c

pprInstr :: forall doc. IsDoc doc => Platform -> Instr -> doc
pprInstr platform i = case i of
   COMMENT s
      -> line (asmComment (ftext s))

   LOCATION file line' col _name
      -> line (text "\t.loc " <> int file <+> int line' <+> int col)

   DELTA d
      -> line (asmComment $ text ("\tdelta = " ++ show d))

   NEWBLOCK _
      -> panic "pprInstr: NEWBLOCK"

   UNWIND lbl d
      -> line (asmComment (text "\tunwind = " <> pprUnwindTable platform d))
         $$ line (pprAsmLabel platform lbl <> colon)

   LDATA _ _
      -> panic "pprInstr: LDATA"

{-
   SPILL reg slot
      -> hcat [
           text "\tSPILL",
           char ' ',
           pprUserReg reg,
           comma,
           text "SLOT" <> parens (int slot)]

   RELOAD slot reg
      -> hcat [
        text "\tRELOAD",
        char ' ',
        text "SLOT" <> parens (int slot),
        comma,
        pprUserReg reg]
-}

   -- Replace 'mov $0x0,%reg' by 'xor %reg,%reg', which is smaller and cheaper.
   -- The code generator catches most of these already, but not all.
   MOV format (OpImm (ImmInt 0)) dst@(OpReg _)
     -> pprInstr platform (XOR format' dst dst)
        where format' = case format of
                II64 -> II32          -- 32-bit version is equivalent, and smaller
                _    -> format

   MOV fmt src dst
     -> pprFormatOpOp (text "mov") fmt' src dst
       where
          fmt' = case fmt of
            VecFormat _l sFmt -> scalarFormatFormat sFmt
            _ -> fmt

   CMOV cc format src dst
     -> pprCondOpReg (text "cmov") format cc src dst

   MOVD format src dst
     -> pprMovdOpOp (text "mov") format src dst

   MOVZxL II32 src dst
      -> pprFormatOpOp (text "mov") II32 src dst
        -- 32-to-64 bit zero extension on x86_64 is accomplished by a simple
        -- movl.  But we represent it as a MOVZxL instruction, because
        -- the reg alloc would tend to throw away a plain reg-to-reg
        -- move, and we still want it to do that.

   MOVZxL formats src dst
      -> pprFormatOpOpCoerce (text "movz") formats II32 src dst
        -- zero-extension only needs to extend to 32 bits: on x86_64,
        -- the remaining zero-extension to 64 bits is automatic, and the 32-bit
        -- instruction is shorter.

   MOVSxL formats src dst
      -> pprFormatOpOpCoerce (text "movs") formats (archWordFormat (target32Bit platform)) src dst

   -- here we do some patching, since the physical registers are only set late
   -- in the code generation.
   LEA format (OpAddr (AddrBaseIndex (EABaseReg reg1) (EAIndex reg2 1) (ImmInt 0))) dst@(OpReg reg3)
      | reg1 == reg3
      -> pprFormatOpOp (text "add") format (OpReg reg2) dst

   LEA format (OpAddr (AddrBaseIndex (EABaseReg reg1) (EAIndex reg2 1) (ImmInt 0))) dst@(OpReg reg3)
      | reg2 == reg3
      -> pprFormatOpOp (text "add") format (OpReg reg1) dst

   LEA format (OpAddr (AddrBaseIndex (EABaseReg reg1) EAIndexNone displ)) dst@(OpReg reg3)
      | reg1 == reg3
      -> pprInstr platform (ADD format (OpImm displ) dst)

   LEA format src dst
      -> pprFormatOpOp (text "lea") format src dst

   ADD format (OpImm (ImmInt (-1))) dst
      -> pprFormatOp (text "dec") format dst

   ADD format (OpImm (ImmInt 1)) dst
      -> pprFormatOp (text "inc") format dst

   ADD format src dst
      -> pprFormatOpOp (text "add") format src dst

   ADC format src dst
      -> pprFormatOpOp (text "adc") format src dst

   SUB format src dst
      -> pprFormatOpOp (text "sub") format src dst

   SBB format src dst
      -> pprFormatOpOp (text "sbb") format src dst

   IMUL format op1 op2
      -> pprFormatOpOp (text "imul") format op1 op2

   ADD_CC format src dst
      -> pprFormatOpOp (text "add") format src dst

   SUB_CC format src dst
      -> pprFormatOpOp (text "sub") format src dst

   -- Use a 32-bit instruction when possible as it saves a byte.
   -- Notably, extracting the tag bits of a pointer has this form.
   -- TODO: we could save a byte in a subsequent CMP instruction too,
   -- but need something like a peephole pass for this
   AND II64 src@(OpImm (ImmInteger mask)) dst
      | 0 <= mask && mask < 0xffffffff
      -> pprInstr platform (AND II32 src dst)

   AND FF32 src dst
      -> pprOpOp (text "andps") FF32 src dst

   AND FF64 src dst
      -> pprOpOp (text "andpd") FF64 src dst

   AND format src dst
      -> pprFormatOpOp (text "and") format src dst

   OR  format src dst
      -> pprFormatOpOp (text "or")  format src dst

   XOR FF32 src dst
      -> pprOpOp (text "xorps") FF32 src dst

   XOR FF64 src dst
      ->  pprOpOp (text "xorpd") FF64 src dst

   XOR format@(VecFormat _ sfmt) src dst | isIntScalarFormat sfmt
       -> pprOpOp (text "pxor") format src dst

   XOR format src dst
      -> pprFormatOpOp (text "xor") format src dst

   POPCNT format src dst
      -> pprOpOp (text "popcnt") format src (OpReg dst)

   LZCNT format src dst
      ->  pprOpOp (text "lzcnt") format src (OpReg dst)

   TZCNT format src dst
      -> pprOpOp (text "tzcnt") format src (OpReg dst)

   BSF format src dst
      -> pprOpOp (text "bsf") format src (OpReg dst)

   BSR format src dst
      -> pprOpOp (text "bsr") format src (OpReg dst)

   PDEP format src mask dst
      -> pprFormatOpOpReg (text "pdep") format src mask dst

   PEXT format src mask dst
      -> pprFormatOpOpReg (text "pext") format src mask dst

   PREFETCH NTA format src
      -> pprFormatOp_ (text "prefetchnta") format src

   PREFETCH Lvl0 format src
      -> pprFormatOp_ (text "prefetcht0") format src

   PREFETCH Lvl1 format src
      -> pprFormatOp_ (text "prefetcht1") format src

   PREFETCH Lvl2 format src
      -> pprFormatOp_ (text "prefetcht2") format src

   NOT format op
      -> pprFormatOp (text "not") format op

   BSWAP format op
      -> pprFormatOp (text "bswap") format (OpReg op)

   NEGI format op
      -> pprFormatOp (text "neg") format op

   SHL format src dst
      -> pprShift (text "shl") format src dst

   SAR format src dst
      -> pprShift (text "sar") format src dst

   SHR format src dst
      -> pprShift (text "shr") format src dst

   SHLD format src dst1 dst2
      -> pprShift2 (text "shld") format src dst1 dst2

   SHRD format src dst1 dst2
      -> pprShift2 (text "shrd") format src dst1 dst2

   BT format imm src
      -> pprFormatImmOp (text "bt") format imm src

   CMP format src dst
     | isFloatFormat format -> pprFormatOpOp (text "ucomi") format src dst -- SSE2
     | otherwise            -> pprFormatOpOp (text "cmp")   format src dst

   TEST format src dst
      -> pprFormatOpOp (text "test") format' src dst
         where
        -- Match instructions like 'test $0x3,%esi' or 'test $0x7,%rbx'.
        -- We can replace them by equivalent, but smaller instructions
        -- by reducing the size of the immediate operand as far as possible.
        -- (We could handle masks larger than a single byte too,
        -- but it would complicate the code considerably
        -- and tag checks are by far the most common case.)
        -- The mask must have the high bit clear for this smaller encoding
        -- to be completely equivalent to the original; in particular so
        -- that the signed comparison condition bits are the same as they
        -- would be if doing a full word comparison. See #13425.
          format' = case (src,dst) of
           (OpImm (ImmInteger mask), OpReg dstReg)
             | 0 <= mask && mask < 128 -> minSizeOfReg platform dstReg
           _ -> format
          minSizeOfReg platform (RegReal (RealRegSingle i))
            | target32Bit platform && i <= 3        = II8  -- al, bl, cl, dl
            | target32Bit platform && i <= 7        = II16 -- si, di, bp, sp
            | not (target32Bit platform) && i <= 15 = II8  -- al .. r15b
          minSizeOfReg _ _ = format                 -- other

   PUSH format op
      -> pprFormatOp (text "push") format op

   POP format op
      -> pprFormatOp (text "pop") format op

-- both unused (SDM):
-- PUSHA -> text "\tpushal"
-- POPA  -> text "\tpopal"

   NOP
      -> line $ text "\tnop"

   CLTD II8
      -> line $ text "\tcbtw"

   CLTD II16
      -> line $ text "\tcwtd"

   CLTD II32
      -> line $ text "\tcltd"

   CLTD II64
      -> line $ text "\tcqto"

   CLTD x
      -> panic $ "pprInstr: CLTD " ++ show x

   SETCC cond op
      -> pprCondInstr (text "set") cond (pprOperand platform II8 op)

   XCHG format src val
      -> pprFormatOpReg (text "xchg") format src val

   JXX cond blockid
      -> pprCondInstr (text "j") cond (pprAsmLabel platform lab)
         where lab = blockLbl blockid

   JXX_GBL cond imm
      -> pprCondInstr (text "j") cond (pprImm platform imm)

   JMP (OpImm imm) _
      -> line $ text "\tjmp " <> pprImm platform imm

   JMP op _
      -> line $ text "\tjmp *" <> pprOperand platform (archWordFormat (target32Bit platform)) op

   JMP_TBL op _ _ _
      -> pprInstr platform (JMP op [])

   CALL (Left imm) _
      -> line $ text "\tcall " <> pprImm platform imm

   CALL (Right reg) _
      -> line $ text "\tcall *" <> pprReg platform (archWordFormat (target32Bit platform)) reg

   IDIV fmt op
      -> pprFormatOp (text "idiv") fmt op

   DIV fmt op
      -> pprFormatOp (text "div")  fmt op

   IMUL2 fmt op
      -> pprFormatOp (text "imul") fmt op

   -- x86_64 only
   MUL format op1 op2
      -> pprFormatOpOp (text "mul") format op1 op2

   MUL2 format op
      -> pprFormatOp (text "mul") format op

   FDIV format op1 op2
      -> pprFormatOpOp (text "div") format op1 op2

   FMA3 format var perm op1 op2 op3
      -> let mnemo = case var of
               FMAdd  -> text "vfmadd"
               FMSub  -> text "vfmsub"
               FNMAdd -> text "vfnmadd"
               FNMSub -> text "vfnmsub"
         in pprFormatOpRegReg (mnemo <> pprFMAPermutation perm) format op1 op2 op3

   SQRT format op1 op2
      -> pprFormatOpReg (text "sqrt") format op1 op2

   CVTSS2SD from to
      -> pprRegReg (text "cvtss2sd") from to

   CVTSD2SS from to
      -> pprRegReg (text "cvtsd2ss") from to

   CVTTSS2SIQ fmt from to
      -> pprFormatFormatOpReg (text "cvttss2si") FF32 fmt from to

   CVTTSD2SIQ fmt from to
      -> pprFormatFormatOpReg (text "cvttsd2si") FF64 fmt from to

   CVTSI2SS fmt from to
      -> pprFormatOpReg (text "cvtsi2ss") fmt from to

   CVTSI2SD fmt from to
      -> pprFormatOpReg (text "cvtsi2sd") fmt from to

       -- FETCHGOT for PIC on ELF platforms
   FETCHGOT reg
      -> lines_ [ text "\tcall 1f",
                  hcat [ text "1:\tpopl\t", pprReg platform II32 reg ],
                  hcat [ text "\taddl\t$_GLOBAL_OFFSET_TABLE_+(.-1b), ",
                         pprReg platform II32 reg ]
                ]

    -- FETCHPC for PIC on Darwin/x86
    -- get the instruction pointer into a register
    -- (Terminology note: the IP is called Program Counter on PPC,
    --  and it's a good thing to use the same name on both platforms)
   FETCHPC reg
      -> lines_ [ text "\tcall 1f",
                  hcat [ text "1:\tpopl\t", pprReg platform II32 reg ]
                ]

   -- the
   -- GST fmt src addr ==> FLD dst ; FSTPsz addr
   g@(X87Store fmt  addr)
      -> pprX87 g (hcat [gtab, text "fstp", pprFormat_x87 fmt, gsp, pprAddr platform addr])

   -- Atomics
   LOCK i
      -> line (text "\tlock") $$ pprInstr platform i

   MFENCE
      -> line $ text "\tmfence"

   XADD format src dst
      -> pprFormatOpOp (text "xadd") format src dst

   CMPXCHG format src dst
      -> pprFormatOpOp (text "cmpxchg") format src dst

   -- Vector Instructions
   VADD format s1 s2 dst
     -> pprFormatOpRegReg (text "vadd") format s1 s2 dst
   VSUB format s1 s2 dst
     -> pprFormatOpRegReg (text "vsub") format s1 s2 dst
   VMUL format s1 s2 dst
     -> pprFormatOpRegReg (text "vmul") format s1 s2 dst
   VDIV format s1 s2 dst
     -> pprFormatOpRegReg (text "vdiv") format s1 s2 dst
   VBROADCAST format from to
     -> pprBroadcast (text "vbroadcast") format from to
   VMOVU format from to
     -> pprFormatOpOp (text "vmovu") format from to
   MOVU format from to
     -> pprFormatOpOp (text "movu") format from to
   MOVL format from to
     -> pprFormatOpOp (text "movl") format from to
   MOVH format from to
     -> pprFormatOpOp (text "movh") format from to

   MOVDQU  format from to
     -> pprOpOp (text "movdqu") format from to
   VMOVDQU format from to
     -> pprOpOp vmovdqu_op format from to
     where
      vmovdqu_op = case format of
        VecFormat 8  FmtInt64 -> text "vmovdqu64"
        VecFormat 16 FmtInt32 -> text "vmovdqu32"
        VecFormat 32 FmtInt16 -> text "vmovdqu32" -- NB: not using vmovdqu16/8, as they
        VecFormat 64 FmtInt8  -> text "vmovdqu32" -- require the additional AVX512BW extension
        _ -> text "vmovdqu"

   VPXOR format s1 s2 dst
     -> pprXor (text "vpxor") format s1 s2 dst
   VEXTRACT format offset from to
     -> pprFormatImmRegOp (text "vextract") format offset from to
   INSERTPS format offset addr dst
     -> pprInsert (text "insertps") format offset addr dst

   SHUF format offset src dst
     -> pprShuf (text "shuf" <> pprFormat format) format offset src dst
   VSHUF format offset src1 src2 dst
     -> pprVShuf (text "vshuf" <> pprFormat format) format offset src1 src2 dst
   PSHUFD format offset src dst
     -> pprShuf (text "pshufd") format offset src dst
   VPSHUFD format offset src dst
     -> pprShuf (text "vpshufd") format offset src dst

   PSLLDQ format offset dst
     -> pprDoubleShift (text "pslldq") format offset dst
   PSRLDQ format offset dst
     -> pprDoubleShift (text "psrldq") format offset dst

   MOVHLPS format from to
     -> pprOpReg (text "movhlps") format (OpReg from) to
   PUNPCKLQDQ format from to
     -> pprOpReg (text "punpcklqdq") format from to

  where
   gtab :: Line doc
   gtab  = char '\t'

   gsp :: Line doc
   gsp   = char ' '



   pprX87 :: Instr -> Line doc -> doc
   pprX87 fake actual
      = line (char '#' <> pprX87Instr fake) $$ line actual

   pprX87Instr :: Instr -> Line doc
   pprX87Instr (X87Store fmt dst) = pprFormatAddr (text "gst") fmt dst
   pprX87Instr _ = panic "X86.Ppr.pprX87Instr: no match"

   pprDollImm :: Imm -> Line doc
   pprDollImm i = text "$" <> pprImm platform i


   pprOperand :: Platform -> Format -> Operand -> Line doc
   pprOperand platform f op = case op of
      OpReg r   -> pprReg platform f r
      OpImm i   -> pprDollImm i
      OpAddr ea -> pprAddr platform ea


   pprMnemonic_  :: Line doc -> Line doc
   pprMnemonic_ name =
      char '\t' <> name <> space


   pprMnemonic  :: Line doc -> Format -> Line doc
   pprMnemonic name format =
      char '\t' <> name <> pprFormat format <> space


   pprGenMnemonic  :: Line doc -> Format -> Line doc
   pprGenMnemonic name _ =
      char '\t' <> name <> text "" <> space

   pprBroadcastMnemonic  :: Line doc -> Format -> Line doc
   pprBroadcastMnemonic name format =
      char '\t' <> name <> pprBroadcastFormat format <> space

   pprBroadcastFormat :: Format -> Line doc
   pprBroadcastFormat (VecFormat _ f)
     = case f of
         FmtFloat  -> text "ss"
         FmtDouble -> text "sd"
         FmtInt8   -> text "b"
         FmtInt16  -> text "w"
         FmtInt32  -> text "d"
         FmtInt64  -> text "q"
   pprBroadcastFormat _ = panic "Scalar Format invading vector operation"

   pprFormatImmOp :: Line doc -> Format -> Imm -> Operand -> doc
   pprFormatImmOp name format imm op1
     = line $ hcat [
           pprMnemonic name format,
           char '$',
           pprImm platform imm,
           comma,
           pprOperand platform format op1
       ]

   pprFormatOp_ :: Line doc -> Format -> Operand -> doc
   pprFormatOp_ name format op1
     = line $ hcat [
           pprMnemonic_ name ,
           pprOperand platform format op1
       ]

   pprFormatOp :: Line doc -> Format -> Operand -> doc
   pprFormatOp name format op1
     = line $ hcat [
           pprMnemonic name format,
           pprOperand platform format op1
       ]


   pprFormatOpOp :: Line doc -> Format -> Operand -> Operand -> doc
   pprFormatOpOp name format op1 op2
     = line $ hcat [
           pprMnemonic name format,
           pprOperand platform format op1,
           comma,
           pprOperand platform format op2
       ]

   pprMovdOpOp :: Line doc -> Format -> Operand -> Operand -> doc
   pprMovdOpOp name format op1 op2
     = let instr = case format of
             -- bitcasts to/from a general purpose register to a floating point
             -- register require II32 or II64.
             II32 -> text "d"
             II64 -> text "q"
             FF32 -> text "d"
             FF64 -> text "q"
             _    -> panic "X86.Ppr.pprMovdOpOp: improper format for movd/movq."
       in line $ hcat [
           char '\t' <> name <> instr <> space,
           pprOperand platform format op1,
           comma,
           pprOperand platform (movdOutFormat format) op2
           ]

   pprFormatImmRegOp :: Line doc -> Format -> Imm -> Reg -> Operand -> doc
   pprFormatImmRegOp name format off reg1 op2
     = line $ hcat [
           pprMnemonic name format,
           pprDollImm off,
           comma,
           pprReg platform format reg1,
           comma,
           pprOperand platform format op2
       ]

   pprFormatOpRegReg :: Line doc -> Format -> Operand -> Reg -> Reg -> doc
   pprFormatOpRegReg name format op1 reg2 reg3
     = line $ hcat [
           pprMnemonic name format,
           pprOperand platform format op1,
           comma,
           pprReg platform format reg2,
           comma,
           pprReg platform format reg3
       ]

   pprFMAPermutation :: FMAPermutation -> Line doc
   pprFMAPermutation FMA132 = text "132"
   pprFMAPermutation FMA213 = text "213"
   pprFMAPermutation FMA231 = text "231"

   pprOpOp :: Line doc -> Format -> Operand -> Operand -> doc
   pprOpOp name format op1 op2
     = line $ hcat [
           pprMnemonic_ name,
           pprOperand platform format op1,
           comma,
           pprOperand platform format op2
       ]

   pprRegReg :: Line doc -> Reg -> Reg -> doc
   pprRegReg name reg1 reg2
     = line $ hcat [
           pprMnemonic_ name,
           pprReg platform (archWordFormat (target32Bit platform)) reg1,
           comma,
           pprReg platform (archWordFormat (target32Bit platform)) reg2
       ]

   pprOpReg :: Line doc -> Format -> Operand -> Reg -> doc
   pprOpReg name format op reg
     = line $ hcat [
           pprMnemonic_ name,
           pprOperand platform format op,
           comma,
           pprReg platform (archWordFormat (target32Bit platform)) reg
       ]


   pprFormatOpReg :: Line doc -> Format -> Operand -> Reg -> doc
   pprFormatOpReg name format op1 reg2
     = line $ hcat [
           pprMnemonic name format,
           pprOperand platform format op1,
           comma,
           pprReg platform (archWordFormat (target32Bit platform)) reg2
       ]

   pprCondOpReg :: Line doc -> Format -> Cond -> Operand -> Reg -> doc
   pprCondOpReg name format cond op1 reg2
     = line $ hcat [
           char '\t',
           name,
           pprCond cond,
           space,
           pprOperand platform format op1,
           comma,
           pprReg platform format reg2
       ]

   pprFormatFormatOpReg :: Line doc -> Format -> Format -> Operand -> Reg -> doc
   pprFormatFormatOpReg name format1 format2 op1 reg2
     = line $ hcat [
           pprMnemonic name format2,
           pprOperand platform format1 op1,
           comma,
           pprReg platform format2 reg2
       ]

   pprFormatOpOpReg :: Line doc -> Format -> Operand -> Operand -> Reg -> doc
   pprFormatOpOpReg name format op1 op2 reg3
     = line $ hcat [
           pprMnemonic name format,
           pprOperand platform format op1,
           comma,
           pprOperand platform format op2,
           comma,
           pprReg platform format reg3
       ]



   pprFormatAddr :: Line doc -> Format -> AddrMode -> Line doc
   pprFormatAddr name format  op
     = hcat [
           pprMnemonic name format,
           comma,
           pprAddr platform op
       ]

   pprShift :: Line doc -> Format -> Operand -> Operand -> doc
   pprShift name format src dest
     = line $ hcat [
           pprMnemonic name format,
           pprOperand platform II8 src,  -- src is 8-bit sized
           comma,
           pprOperand platform format dest
       ]

   pprShift2 :: Line doc -> Format -> Operand -> Operand -> Operand -> doc
   pprShift2 name format src dest1 dest2
     = line $ hcat [
           pprMnemonic name format,
           pprOperand platform II8 src,  -- src is 8-bit sized
           comma,
           pprOperand platform format dest1,
           comma,
           pprOperand platform format dest2
       ]


   pprFormatOpOpCoerce :: Line doc -> Format -> Format -> Operand -> Operand -> doc
   pprFormatOpOpCoerce name format1 format2 op1 op2
     = line $ hcat [ char '\t', name, pprFormat format1, pprFormat format2, space,
           pprOperand platform format1 op1,
           comma,
           pprOperand platform format2 op2
       ]


   pprCondInstr :: Line doc -> Cond -> Line doc -> doc
   pprCondInstr name cond arg
     = line $ hcat [ char '\t', name, pprCond cond, space, arg]

   -- Custom pretty printers
   -- These instructions currently don't follow a uniform suffix pattern
   -- in their names, so we have custom pretty printers for them.
   pprBroadcast :: Line doc -> Format -> Operand -> Reg -> doc
   pprBroadcast name fmt@(VecFormat _ sFmt) op dst
     = line $ hcat [
           pprBroadcastMnemonic name fmt,
           pprOperand platform (scalarFormatFormat sFmt) op,
           comma,
           pprReg platform fmt dst
       ]
   pprBroadcast _ fmt _ _ =
     pprPanic "pprBroadcast: expected vector format" (ppr fmt)

   pprXor :: Line doc -> Format -> Reg -> Reg -> Reg -> doc
   pprXor name format reg1 reg2 reg3
     = line $ hcat [
           pprGenMnemonic name format,
           pprReg platform format reg1,
           comma,
           pprReg platform format reg2,
           comma,
           pprReg platform format reg3
       ]

   pprInsert :: Line doc -> Format -> Imm -> Operand -> Reg -> doc
   pprInsert name format off src dst
     = line $ hcat [
           pprGenMnemonic name format,
           pprDollImm off,
           comma,
           pprOperand platform format src,
           comma,
           pprReg platform format dst
       ]

   pprShuf :: Line doc -> Format -> Imm -> Operand -> Reg -> doc
   pprShuf name format imm1 op2 reg3
     = line $ hcat [
           pprGenMnemonic name format,
           pprDollImm imm1,
           comma,
           pprOperand platform format op2,
           comma,
           pprReg platform format reg3
       ]

   pprVShuf :: Line doc -> Format -> Imm -> Operand -> Reg -> Reg -> doc
   pprVShuf name format imm1 op2 reg3 reg4
     = line $ hcat [
           pprGenMnemonic name format,
           pprDollImm imm1,
           comma,
           pprOperand platform format op2,
           comma,
           pprReg platform format reg3,
           comma,
           pprReg platform format reg4
       ]

   pprDoubleShift :: Line doc -> Format -> Operand -> Reg -> doc
   pprDoubleShift name format off reg
     = line $ hcat [
           pprGenMnemonic name format,
           pprOperand platform format off,
           comma,
           pprReg platform format reg
       ]
