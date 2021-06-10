
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

module GHC.CmmToAsm.X86.Ppr (
        pprNatCmmDecl,
        pprData,
        pprInstr,
        pprFormat,
        pprImm,
        pprDataItem,
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
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel

import GHC.Types.Basic (Alignment, mkAlignment, alignmentBytes)
import GHC.Types.Unique ( pprUniqueAlways )

import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.Word

-- -----------------------------------------------------------------------------
-- Printing this stuff out
--
--
-- Note [Subsections Via Symbols]
--
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

pprProcAlignment :: NCGConfig -> SDoc
pprProcAlignment config = maybe empty (pprAlign platform . mkAlignment) (ncgProcAlignment config)
   where
      platform = ncgPlatform config

pprNatCmmDecl :: NCGConfig -> NatCmmDecl (Alignment, RawCmmStatics) Instr -> SDoc
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
        pprProcLabel config lbl $$
        pprLabel platform lbl $$ -- blocks guaranteed not null, so label needed
        vcat (map (pprBasicBlock config top_info) blocks) $$
        ppWhen (ncgDwarfEnabled config) (pprBlockEndLabel platform lbl $$ pprProcEndLabel platform lbl) $$
        pprSizeDecl platform lbl

    Just (CmmStaticsRaw info_lbl _) ->
      pprSectionAlign config (Section Text info_lbl) $$
      pprProcAlignment config $$
      pprProcLabel config lbl $$
      (if platformHasSubsectionsViaSymbols platform
          then pdoc platform (mkDeadStripPreventer info_lbl) <> colon
          else empty) $$
      vcat (map (pprBasicBlock config top_info) blocks) $$
      ppWhen (ncgDwarfEnabled config) (pprProcEndLabel platform info_lbl) $$
      -- above: Even the first block gets a label, because with branch-chain
      -- elimination, it might be the target of a goto.
      (if platformHasSubsectionsViaSymbols platform
       then -- See Note [Subsections Via Symbols]
                text "\t.long "
            <+> pdoc platform info_lbl
            <+> char '-'
            <+> pdoc platform (mkDeadStripPreventer info_lbl)
       else empty) $$
      pprSizeDecl platform info_lbl

-- | Output an internal proc label. See Note [Internal proc labels] in CLabel.
pprProcLabel :: NCGConfig -> CLabel -> SDoc
pprProcLabel config lbl
  | ncgExposeInternalSymbols config
  , Just lbl' <- ppInternalProcLabel (ncgThisModule config) lbl
  = lbl' <> colon
  | otherwise
  = empty

pprProcEndLabel :: Platform -> CLabel -- ^ Procedure name
                -> SDoc
pprProcEndLabel platform lbl =
    pdoc platform (mkAsmTempProcEndLabel lbl) <> colon

pprBlockEndLabel :: Platform -> CLabel -- ^ Block name
                 -> SDoc
pprBlockEndLabel platform lbl =
    pdoc platform (mkAsmTempEndLabel lbl) <> colon

-- | Output the ELF .size directive.
pprSizeDecl :: Platform -> CLabel -> SDoc
pprSizeDecl platform lbl
 = if osElfTarget (platformOS platform)
   then text "\t.size" <+> pdoc platform lbl <> text ", .-" <> pdoc platform lbl
   else empty

pprBasicBlock :: NCGConfig -> LabelMap RawCmmStatics -> NatBasicBlock Instr -> SDoc
pprBasicBlock config info_env (BasicBlock blockid instrs)
  = maybe_infotable $
    pprLabel platform asmLbl $$
    vcat (map (pprInstr platform) instrs) $$
    ppWhen (ncgDwarfEnabled config) (
      -- Emit both end labels since this may end up being a standalone
      -- top-level block
      pprBlockEndLabel platform asmLbl
      <> pprProcEndLabel platform asmLbl
    )
  where
    asmLbl = blockLbl blockid
    platform = ncgPlatform config
    maybe_infotable c = case mapLookup blockid info_env of
       Nothing -> c
       Just (CmmStaticsRaw infoLbl info) ->
           pprAlignForSection platform Text $$
           infoTableLoc $$
           vcat (map (pprData config) info) $$
           pprLabel platform infoLbl $$
           c $$
           ppWhen (ncgDwarfEnabled config) (pdoc platform (mkAsmTempEndLabel infoLbl) <> colon)

    -- Make sure the info table has the right .loc for the block
    -- coming right after it. See [Note: Info Offset]
    infoTableLoc = case instrs of
      (l@LOCATION{} : _) -> pprInstr platform l
      _other             -> empty


pprDatas :: NCGConfig -> (Alignment, RawCmmStatics) -> SDoc
-- See note [emit-time elimination of static indirections] in "GHC.Cmm.CLabel".
pprDatas config (_, CmmStaticsRaw alias [CmmStaticLit (CmmLabel lbl), CmmStaticLit ind, _, _])
  | lbl == mkIndStaticInfoLabel
  , let labelInd (CmmLabelOff l _) = Just l
        labelInd (CmmLabel l) = Just l
        labelInd _ = Nothing
  , Just ind' <- labelInd ind
  , alias `mayRedirectTo` ind'
  = pprGloblDecl (ncgPlatform config) alias
    $$ text ".equiv" <+> pdoc (ncgPlatform config) alias <> comma <> pdoc (ncgPlatform config) (CmmLabel ind')

pprDatas config (align, (CmmStaticsRaw lbl dats))
 = vcat (pprAlign platform align : pprLabel platform lbl : map (pprData config) dats)
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

pprGloblDecl :: Platform -> CLabel -> SDoc
pprGloblDecl platform lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = text ".globl " <> pdoc platform lbl

pprLabelType' :: Platform -> CLabel -> SDoc
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
      isInfoTableLabel lbl && not (isConInfoTableLabel lbl)


pprTypeDecl :: Platform -> CLabel -> SDoc
pprTypeDecl platform lbl
    = if osElfTarget (platformOS platform) && externallyVisibleCLabel lbl
      then text ".type " <> pdoc platform lbl <> text ", " <> pprLabelType' platform lbl
      else empty

pprLabel :: Platform -> CLabel -> SDoc
pprLabel platform lbl =
   pprGloblDecl platform lbl
   $$ pprTypeDecl platform lbl
   $$ (pdoc platform lbl <> colon)

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

pprReg :: Platform -> Format -> Reg -> SDoc
pprReg platform f r
  = case r of
      RegReal    (RealRegSingle i) ->
          if target32Bit platform then ppr32_reg_no f i
                                  else ppr64_reg_no f i
      RegReal    (RealRegPair _ _) -> panic "X86.Ppr: no reg pairs on this arch"
      RegVirtual (VirtualRegI  u)  -> text "%vI_"   <> pprUniqueAlways u
      RegVirtual (VirtualRegHi u)  -> text "%vHi_"  <> pprUniqueAlways u
      RegVirtual (VirtualRegF  u)  -> text "%vF_"   <> pprUniqueAlways u
      RegVirtual (VirtualRegD  u)  -> text "%vD_"   <> pprUniqueAlways u

  where
    ppr32_reg_no :: Format -> Int -> SDoc
    ppr32_reg_no II8   = ppr32_reg_byte
    ppr32_reg_no II16  = ppr32_reg_word
    ppr32_reg_no _     = ppr32_reg_long

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

    ppr32_reg_long i =
      case i of {
         0 -> text "%eax";    1 -> text "%ebx";
         2 -> text "%ecx";    3 -> text "%edx";
         4 -> text "%esi";    5 -> text "%edi";
         6 -> text "%ebp";    7 -> text "%esp";
         _  -> ppr_reg_float i
      }

    ppr64_reg_no :: Format -> Int -> SDoc
    ppr64_reg_no II8   = ppr64_reg_byte
    ppr64_reg_no II16  = ppr64_reg_word
    ppr64_reg_no II32  = ppr64_reg_long
    ppr64_reg_no _     = ppr64_reg_quad

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

    ppr64_reg_quad i =
      case i of {
         0 -> text "%rax";     1 -> text "%rbx";
         2 -> text "%rcx";     3 -> text "%rdx";
         4 -> text "%rsi";     5 -> text "%rdi";
         6 -> text "%rbp";     7 -> text "%rsp";
         8 -> text "%r8";      9 -> text "%r9";
        10 -> text "%r10";    11 -> text "%r11";
        12 -> text "%r12";    13 -> text "%r13";
        14 -> text "%r14";    15 -> text "%r15";
        _  -> ppr_reg_float i
      }

ppr_reg_float :: Int -> SDoc
ppr_reg_float i = case i of
        16 -> text "%xmm0" ;   17 -> text "%xmm1"
        18 -> text "%xmm2" ;   19 -> text "%xmm3"
        20 -> text "%xmm4" ;   21 -> text "%xmm5"
        22 -> text "%xmm6" ;   23 -> text "%xmm7"
        24 -> text "%xmm8" ;   25 -> text "%xmm9"
        26 -> text "%xmm10";   27 -> text "%xmm11"
        28 -> text "%xmm12";   29 -> text "%xmm13"
        30 -> text "%xmm14";   31 -> text "%xmm15"
        _  -> text "very naughty x86 register"

pprFormat :: Format -> SDoc
pprFormat x = case x of
  II8   -> text "b"
  II16  -> text "w"
  II32  -> text "l"
  II64  -> text "q"
  FF32  -> text "ss"      -- "scalar single-precision float" (SSE2)
  FF64  -> text "sd"      -- "scalar double-precision float" (SSE2)

pprFormat_x87 :: Format -> SDoc
pprFormat_x87 x = case x of
  FF32  -> text "s"
  FF64  -> text "l"
  _     -> panic "X86.Ppr.pprFormat_x87"


pprCond :: Cond -> SDoc
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


pprImm :: Platform -> Imm -> SDoc
pprImm platform = \case
   ImmInt i            -> int i
   ImmInteger i        -> integer i
   ImmCLbl l           -> pdoc platform l
   ImmIndex l i        -> pdoc platform l <> char '+' <> int i
   ImmLit s            -> s
   ImmFloat f          -> float $ fromRational f
   ImmDouble d         -> double $ fromRational d
   ImmConstantSum a b  -> pprImm platform a <> char '+' <> pprImm platform b
   ImmConstantDiff a b -> pprImm platform a <> char '-' <> lparen <> pprImm platform b <> rparen



pprAddr :: Platform -> AddrMode -> SDoc
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
pprSectionAlign :: NCGConfig -> Section -> SDoc
pprSectionAlign _config (Section (OtherSection _) _) =
     panic "X86.Ppr.pprSectionAlign: unknown section"
pprSectionAlign config sec@(Section seg _) =
    pprSectionHeader config sec $$
    pprAlignForSection (ncgPlatform config) seg

-- | Print appropriate alignment for the given section type.
pprAlignForSection :: Platform -> SectionType -> SDoc
pprAlignForSection platform seg =
    text ".align " <>
    case platformOS platform of
      -- Darwin: alignments are given as shifts.
      OSDarwin
       | target32Bit platform ->
          case seg of
           ReadOnlyData16    -> int 4
           CString           -> int 1
           _                 -> int 2
       | otherwise ->
          case seg of
           ReadOnlyData16    -> int 4
           CString           -> int 1
           _                 -> int 3
      -- Other: alignments are given as bytes.
      _
       | target32Bit platform ->
          case seg of
           Text              -> text "4,0x90"
           ReadOnlyData16    -> int 16
           CString           -> int 1
           _                 -> int 4
       | otherwise ->
          case seg of
           ReadOnlyData16    -> int 16
           CString           -> int 1
           _                 -> int 8

pprDataItem :: NCGConfig -> CmmLit -> SDoc
pprDataItem config lit
  = vcat (ppr_item (cmmTypeFormat $ cmmLitType platform lit) lit)
    where
        platform = ncgPlatform config
        imm = litToImm lit

        -- These seem to be common:
        ppr_item II8   _ = [text "\t.byte\t" <> pprImm platform imm]
        ppr_item II16  _ = [text "\t.word\t" <> pprImm platform imm]
        ppr_item II32  _ = [text "\t.long\t" <> pprImm platform imm]

        ppr_item FF32 _ = [text "\t.float\t" <> pprImm platform imm]
        ppr_item FF64 _ = [text "\t.double\t" <> pprImm platform imm]

        ppr_item II64 _
            = case platformOS platform of
              OSDarwin
               | target32Bit platform ->
                  case lit of
                  CmmInt x _ ->
                      [text "\t.long\t"
                          <> int (fromIntegral (fromIntegral x :: Word32)),
                       text "\t.long\t"
                          <> int (fromIntegral
                              (fromIntegral (x `shiftR` 32) :: Word32))]
                  _ -> panic "X86.Ppr.ppr_item: no match for II64"
               | otherwise ->
                  [text "\t.quad\t" <> pprImm platform imm]

              _ -> [text "\t.quad\t" <> pprImm platform imm]


asmComment :: SDoc -> SDoc
asmComment c = whenPprDebug $ text "# " <> c

pprInstr :: Platform -> Instr -> SDoc
pprInstr platform i = case i of
   COMMENT s
      -> asmComment (ftext s)

   LOCATION file line col _name
      -> text "\t.loc " <> ppr file <+> ppr line <+> ppr col

   DELTA d
      -> asmComment $ text ("\tdelta = " ++ show d)

   NEWBLOCK _
      -> panic "pprInstr: NEWBLOCK"

   UNWIND lbl d
      -> asmComment (text "\tunwind = " <> pdoc platform d)
         $$ pdoc platform lbl <> colon

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

   MOV format src dst
     -> pprFormatOpOp (text "mov") format src dst

   CMOV cc format src dst
     -> pprCondOpReg (text "cmov") format cc src dst

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
      -> text "\tnop"

   CLTD II8
      -> text "\tcbtw"

   CLTD II16
      -> text "\tcwtd"

   CLTD II32
      -> text "\tcltd"

   CLTD II64
      -> text "\tcqto"

   CLTD x
      -> panic $ "pprInstr: CLTD " ++ show x

   SETCC cond op
      -> pprCondInstr (text "set") cond (pprOperand platform II8 op)

   XCHG format src val
      -> pprFormatOpReg (text "xchg") format src val

   JXX cond blockid
      -> pprCondInstr (text "j") cond (pdoc platform lab)
         where lab = blockLbl blockid

   JXX_GBL cond imm
      -> pprCondInstr (text "j") cond (pprImm platform imm)

   JMP (OpImm imm) _
      -> text "\tjmp " <> pprImm platform imm

   JMP op _
      -> text "\tjmp *" <> pprOperand platform (archWordFormat (target32Bit platform)) op

   JMP_TBL op _ _ _
      -> pprInstr platform (JMP op [])

   CALL (Left imm) _
      -> text "\tcall " <> pprImm platform imm

   CALL (Right reg) _
      -> text "\tcall *" <> pprReg platform (archWordFormat (target32Bit platform)) reg

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
      -> vcat [ text "\tcall 1f",
                hcat [ text "1:\tpopl\t", pprReg platform II32 reg ],
                hcat [ text "\taddl\t$_GLOBAL_OFFSET_TABLE_+(.-1b), ",
                       pprReg platform II32 reg ]
              ]

    -- FETCHPC for PIC on Darwin/x86
    -- get the instruction pointer into a register
    -- (Terminology note: the IP is called Program Counter on PPC,
    --  and it's a good thing to use the same name on both platforms)
   FETCHPC reg
      -> vcat [ text "\tcall 1f",
                hcat [ text "1:\tpopl\t", pprReg platform II32 reg ]
              ]

   -- the
   -- GST fmt src addr ==> FLD dst ; FSTPsz addr
   g@(X87Store fmt  addr)
      -> pprX87 g (hcat [gtab, text "fstp", pprFormat_x87 fmt, gsp, pprAddr platform addr])

   -- Atomics
   LOCK i
      -> text "\tlock" $$ pprInstr platform i

   MFENCE
      -> text "\tmfence"

   XADD format src dst
      -> pprFormatOpOp (text "xadd") format src dst

   CMPXCHG format src dst
      -> pprFormatOpOp (text "cmpxchg") format src dst


  where
   gtab :: SDoc
   gtab  = char '\t'

   gsp :: SDoc
   gsp   = char ' '



   pprX87 :: Instr -> SDoc -> SDoc
   pprX87 fake actual
      = (char '#' <> pprX87Instr fake) $$ actual

   pprX87Instr :: Instr -> SDoc
   pprX87Instr (X87Store fmt dst) = pprFormatAddr (text "gst") fmt dst
   pprX87Instr _ = panic "X86.Ppr.pprX87Instr: no match"

   pprDollImm :: Imm -> SDoc
   pprDollImm i = text "$" <> pprImm platform i


   pprOperand :: Platform -> Format -> Operand -> SDoc
   pprOperand platform f op = case op of
      OpReg r   -> pprReg platform f r
      OpImm i   -> pprDollImm i
      OpAddr ea -> pprAddr platform ea


   pprMnemonic_  :: SDoc -> SDoc
   pprMnemonic_ name =
      char '\t' <> name <> space


   pprMnemonic  :: SDoc -> Format -> SDoc
   pprMnemonic name format =
      char '\t' <> name <> pprFormat format <> space


   pprFormatImmOp :: SDoc -> Format -> Imm -> Operand -> SDoc
   pprFormatImmOp name format imm op1
     = hcat [
           pprMnemonic name format,
           char '$',
           pprImm platform imm,
           comma,
           pprOperand platform format op1
       ]


   pprFormatOp_ :: SDoc -> Format -> Operand -> SDoc
   pprFormatOp_ name format op1
     = hcat [
           pprMnemonic_ name ,
           pprOperand platform format op1
       ]

   pprFormatOp :: SDoc -> Format -> Operand -> SDoc
   pprFormatOp name format op1
     = hcat [
           pprMnemonic name format,
           pprOperand platform format op1
       ]


   pprFormatOpOp :: SDoc -> Format -> Operand -> Operand -> SDoc
   pprFormatOpOp name format op1 op2
     = hcat [
           pprMnemonic name format,
           pprOperand platform format op1,
           comma,
           pprOperand platform format op2
       ]


   pprOpOp :: SDoc -> Format -> Operand -> Operand -> SDoc
   pprOpOp name format op1 op2
     = hcat [
           pprMnemonic_ name,
           pprOperand platform format op1,
           comma,
           pprOperand platform format op2
       ]

   pprRegReg :: SDoc -> Reg -> Reg -> SDoc
   pprRegReg name reg1 reg2
     = hcat [
           pprMnemonic_ name,
           pprReg platform (archWordFormat (target32Bit platform)) reg1,
           comma,
           pprReg platform (archWordFormat (target32Bit platform)) reg2
       ]


   pprFormatOpReg :: SDoc -> Format -> Operand -> Reg -> SDoc
   pprFormatOpReg name format op1 reg2
     = hcat [
           pprMnemonic name format,
           pprOperand platform format op1,
           comma,
           pprReg platform (archWordFormat (target32Bit platform)) reg2
       ]

   pprCondOpReg :: SDoc -> Format -> Cond -> Operand -> Reg -> SDoc
   pprCondOpReg name format cond op1 reg2
     = hcat [
           char '\t',
           name,
           pprCond cond,
           space,
           pprOperand platform format op1,
           comma,
           pprReg platform format reg2
       ]

   pprFormatFormatOpReg :: SDoc -> Format -> Format -> Operand -> Reg -> SDoc
   pprFormatFormatOpReg name format1 format2 op1 reg2
     = hcat [
           pprMnemonic name format2,
           pprOperand platform format1 op1,
           comma,
           pprReg platform format2 reg2
       ]

   pprFormatOpOpReg :: SDoc -> Format -> Operand -> Operand -> Reg -> SDoc
   pprFormatOpOpReg name format op1 op2 reg3
     = hcat [
           pprMnemonic name format,
           pprOperand platform format op1,
           comma,
           pprOperand platform format op2,
           comma,
           pprReg platform format reg3
       ]



   pprFormatAddr :: SDoc -> Format -> AddrMode -> SDoc
   pprFormatAddr name format  op
     = hcat [
           pprMnemonic name format,
           comma,
           pprAddr platform op
       ]

   pprShift :: SDoc -> Format -> Operand -> Operand -> SDoc
   pprShift name format src dest
     = hcat [
           pprMnemonic name format,
           pprOperand platform II8 src,  -- src is 8-bit sized
           comma,
           pprOperand platform format dest
       ]


   pprFormatOpOpCoerce :: SDoc -> Format -> Format -> Operand -> Operand -> SDoc
   pprFormatOpOpCoerce name format1 format2 op1 op2
     = hcat [ char '\t', name, pprFormat format1, pprFormat format2, space,
           pprOperand platform format1 op1,
           comma,
           pprOperand platform format2 op2
       ]


   pprCondInstr :: SDoc -> Cond -> SDoc -> SDoc
   pprCondInstr name cond arg
     = hcat [ char '\t', name, pprCond cond, space, arg]
