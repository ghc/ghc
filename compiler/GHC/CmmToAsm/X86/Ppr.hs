{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
module GHC.CmmToAsm.X86.Ppr (
        pprNatCmmDecl,
        pprData,
        pprInstr,
        pprFormat,
        pprImm,
        pprDataItem,
)

where

#include "HsVersions.h"

import GhcPrelude

import GHC.CmmToAsm.X86.Regs
import GHC.CmmToAsm.X86.Instr
import GHC.CmmToAsm.X86.Cond
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Format
import GHC.Platform.Reg
import GHC.CmmToAsm.Ppr


import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Label
import GHC.Types.Basic (Alignment, mkAlignment, alignmentBytes)
import GHC.Driver.Session
import GHC.Cmm              hiding (topInfoTable)
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Types.Unique ( pprUniqueAlways )
import GHC.Platform
import FastString
import Outputable

import Data.Word
import Data.Bits

-- -----------------------------------------------------------------------------
-- Printing this stuff out
--
--
-- Note [Subsections Via Symbols]
--
-- If we are using the .subsections_via_symbols directive
-- (available on recent versions of Darwin),
-- we have to make sure that there is some kind of reference
-- from the entry code to a label on the _top_ of of the info table,
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
        pprLabel platform lbl $$ -- blocks guaranteed not null, so label needed
        vcat (map (pprBasicBlock config top_info) blocks) $$
        (if ncgDebugLevel config > 0
         then ppr (mkAsmTempEndLabel lbl) <> char ':' else empty) $$
        pprSizeDecl platform lbl

    Just (RawCmmStatics info_lbl _) ->
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

-- | Output the ELF .size directive.
pprSizeDecl :: Platform -> CLabel -> SDoc
pprSizeDecl platform lbl
 = if osElfTarget (platformOS platform)
   then text "\t.size" <+> ppr lbl <> ptext (sLit ", .-") <> ppr lbl
   else empty

pprBasicBlock :: NCGConfig -> LabelMap RawCmmStatics -> NatBasicBlock Instr -> SDoc
pprBasicBlock config info_env (BasicBlock blockid instrs)
  = maybe_infotable $
    pprLabel platform asmLbl $$
    vcat (map (pprInstr platform) instrs) $$
    (if ncgDebugLevel config > 0
      then ppr (mkAsmTempEndLabel asmLbl) <> char ':'
      else empty
    )
  where
    asmLbl = blockLbl blockid
    platform = ncgPlatform config
    maybe_infotable c = case mapLookup blockid info_env of
       Nothing -> c
       Just (RawCmmStatics infoLbl info) ->
           pprAlignForSection platform Text $$
           infoTableLoc $$
           vcat (map (pprData config) info) $$
           pprLabel platform infoLbl $$
           c $$
           (if ncgDebugLevel config > 0
               then ppr (mkAsmTempEndLabel infoLbl) <> char ':'
               else empty
           )
    -- Make sure the info table has the right .loc for the block
    -- coming right after it. See [Note: Info Offset]
    infoTableLoc = case instrs of
      (l@LOCATION{} : _) -> pprInstr platform l
      _other             -> empty


pprDatas :: NCGConfig -> (Alignment, RawCmmStatics) -> SDoc
-- See note [emit-time elimination of static indirections] in CLabel.
pprDatas _config (_, RawCmmStatics alias [CmmStaticLit (CmmLabel lbl), CmmStaticLit ind, _, _])
  | lbl == mkIndStaticInfoLabel
  , let labelInd (CmmLabelOff l _) = Just l
        labelInd (CmmLabel l) = Just l
        labelInd _ = Nothing
  , Just ind' <- labelInd ind
  , alias `mayRedirectTo` ind'
  = pprGloblDecl alias
    $$ text ".equiv" <+> ppr alias <> comma <> ppr (CmmLabel ind')

pprDatas config (align, (RawCmmStatics lbl dats))
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

pprGloblDecl :: CLabel -> SDoc
pprGloblDecl lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = text ".globl " <> ppr lbl

pprLabelType' :: DynFlags -> CLabel -> SDoc
pprLabelType' dflags lbl =
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
    functionOkInfoTable = tablesNextToCode dflags &&
      isInfoTableLabel lbl && not (isConInfoTableLabel lbl)


pprTypeDecl :: Platform -> CLabel -> SDoc
pprTypeDecl platform lbl
    = if osElfTarget (platformOS platform) && externallyVisibleCLabel lbl
      then
        sdocWithDynFlags $ \df ->
          text ".type " <> ppr lbl <> ptext (sLit  ", ") <> pprLabelType' df lbl
      else empty

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

instance Outputable Instr where
    ppr instr = sdocWithDynFlags $ \dflags ->
                  pprInstr (targetPlatform dflags) instr


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

    ppr32_reg_byte i = ptext
      (case i of {
         0 -> sLit "%al";     1 -> sLit "%bl";
         2 -> sLit "%cl";     3 -> sLit "%dl";
        _  -> sLit $ "very naughty I386 byte register: " ++ show i
      })

    ppr32_reg_word i = ptext
      (case i of {
         0 -> sLit "%ax";     1 -> sLit "%bx";
         2 -> sLit "%cx";     3 -> sLit "%dx";
         4 -> sLit "%si";     5 -> sLit "%di";
         6 -> sLit "%bp";     7 -> sLit "%sp";
        _  -> sLit "very naughty I386 word register"
      })

    ppr32_reg_long i = ptext
      (case i of {
         0 -> sLit "%eax";    1 -> sLit "%ebx";
         2 -> sLit "%ecx";    3 -> sLit "%edx";
         4 -> sLit "%esi";    5 -> sLit "%edi";
         6 -> sLit "%ebp";    7 -> sLit "%esp";
         _  -> ppr_reg_float i
      })

    ppr64_reg_no :: Format -> Int -> SDoc
    ppr64_reg_no II8   = ppr64_reg_byte
    ppr64_reg_no II16  = ppr64_reg_word
    ppr64_reg_no II32  = ppr64_reg_long
    ppr64_reg_no _     = ppr64_reg_quad

    ppr64_reg_byte i = ptext
      (case i of {
         0 -> sLit "%al";     1 -> sLit "%bl";
         2 -> sLit "%cl";     3 -> sLit "%dl";
         4 -> sLit "%sil";    5 -> sLit "%dil"; -- new 8-bit regs!
         6 -> sLit "%bpl";    7 -> sLit "%spl";
         8 -> sLit "%r8b";    9  -> sLit "%r9b";
        10 -> sLit "%r10b";   11 -> sLit "%r11b";
        12 -> sLit "%r12b";   13 -> sLit "%r13b";
        14 -> sLit "%r14b";   15 -> sLit "%r15b";
        _  -> sLit $ "very naughty x86_64 byte register: " ++ show i
      })

    ppr64_reg_word i = ptext
      (case i of {
         0 -> sLit "%ax";     1 -> sLit "%bx";
         2 -> sLit "%cx";     3 -> sLit "%dx";
         4 -> sLit "%si";     5 -> sLit "%di";
         6 -> sLit "%bp";     7 -> sLit "%sp";
         8 -> sLit "%r8w";    9  -> sLit "%r9w";
        10 -> sLit "%r10w";   11 -> sLit "%r11w";
        12 -> sLit "%r12w";   13 -> sLit "%r13w";
        14 -> sLit "%r14w";   15 -> sLit "%r15w";
        _  -> sLit "very naughty x86_64 word register"
      })

    ppr64_reg_long i = ptext
      (case i of {
         0 -> sLit "%eax";    1  -> sLit "%ebx";
         2 -> sLit "%ecx";    3  -> sLit "%edx";
         4 -> sLit "%esi";    5  -> sLit "%edi";
         6 -> sLit "%ebp";    7  -> sLit "%esp";
         8 -> sLit "%r8d";    9  -> sLit "%r9d";
        10 -> sLit "%r10d";   11 -> sLit "%r11d";
        12 -> sLit "%r12d";   13 -> sLit "%r13d";
        14 -> sLit "%r14d";   15 -> sLit "%r15d";
        _  -> sLit "very naughty x86_64 register"
      })

    ppr64_reg_quad i = ptext
      (case i of {
         0 -> sLit "%rax";      1 -> sLit "%rbx";
         2 -> sLit "%rcx";      3 -> sLit "%rdx";
         4 -> sLit "%rsi";      5 -> sLit "%rdi";
         6 -> sLit "%rbp";      7 -> sLit "%rsp";
         8 -> sLit "%r8";       9 -> sLit "%r9";
        10 -> sLit "%r10";    11 -> sLit "%r11";
        12 -> sLit "%r12";    13 -> sLit "%r13";
        14 -> sLit "%r14";    15 -> sLit "%r15";
        _  -> ppr_reg_float i
      })

ppr_reg_float :: Int -> PtrString
ppr_reg_float i = case i of
        16 -> sLit "%xmm0" ;   17 -> sLit "%xmm1"
        18 -> sLit "%xmm2" ;   19 -> sLit "%xmm3"
        20 -> sLit "%xmm4" ;   21 -> sLit "%xmm5"
        22 -> sLit "%xmm6" ;   23 -> sLit "%xmm7"
        24 -> sLit "%xmm8" ;   25 -> sLit "%xmm9"
        26 -> sLit "%xmm10";   27 -> sLit "%xmm11"
        28 -> sLit "%xmm12";   29 -> sLit "%xmm13"
        30 -> sLit "%xmm14";   31 -> sLit "%xmm15"
        _  -> sLit "very naughty x86 register"

pprFormat :: Format -> SDoc
pprFormat x
 = ptext (case x of
                II8   -> sLit "b"
                II16  -> sLit "w"
                II32  -> sLit "l"
                II64  -> sLit "q"
                FF32  -> sLit "ss"      -- "scalar single-precision float" (SSE2)
                FF64  -> sLit "sd"      -- "scalar double-precision float" (SSE2)
                )

pprFormat_x87 :: Format -> SDoc
pprFormat_x87 x
  = ptext $ case x of
                FF32  -> sLit "s"
                FF64  -> sLit "l"
                _     -> panic "X86.Ppr.pprFormat_x87"


pprCond :: Cond -> SDoc
pprCond c
 = ptext (case c of {
                GEU     -> sLit "ae";   LU    -> sLit "b";
                EQQ     -> sLit "e";    GTT   -> sLit "g";
                GE      -> sLit "ge";   GU    -> sLit "a";
                LTT     -> sLit "l";    LE    -> sLit "le";
                LEU     -> sLit "be";   NE    -> sLit "ne";
                NEG     -> sLit "s";    POS   -> sLit "ns";
                CARRY   -> sLit "c";   OFLO  -> sLit "o";
                PARITY  -> sLit "p";   NOTPARITY -> sLit "np";
                ALWAYS  -> sLit "mp"})


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



pprAddr :: Platform -> AddrMode -> SDoc
pprAddr _platform (ImmAddr imm off)
  = let pp_imm = pprImm imm
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
    ppr_disp imm        = pprImm imm

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
        ppr_item II8   _ = [text "\t.byte\t" <> pprImm imm]
        ppr_item II16  _ = [text "\t.word\t" <> pprImm imm]
        ppr_item II32  _ = [text "\t.long\t" <> pprImm imm]

        ppr_item FF32  (CmmFloat r _)
           = let bs = floatToBytes (fromRational r)
             in  map (\b -> text "\t.byte\t" <> pprImm (ImmInt b)) bs

        ppr_item FF64 (CmmFloat r _)
           = let bs = doubleToBytes (fromRational r)
             in  map (\b -> text "\t.byte\t" <> pprImm (ImmInt b)) bs

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
                  [text "\t.quad\t" <> pprImm imm]
              _
               | target32Bit platform ->
                  [text "\t.quad\t" <> pprImm imm]
               | otherwise ->
                  -- x86_64: binutils can't handle the R_X86_64_PC64
                  -- relocation type, which means we can't do
                  -- pc-relative 64-bit addresses. Fortunately we're
                  -- assuming the small memory model, in which all such
                  -- offsets will fit into 32 bits, so we have to stick
                  -- to 32-bit offset fields and modify the RTS
                  -- appropriately
                  --
                  -- See Note [x86-64-relative] in includes/rts/storage/InfoTables.h
                  --
                  case lit of
                  -- A relative relocation:
                  CmmLabelDiffOff _ _ _ _ ->
                      [text "\t.long\t" <> pprImm imm,
                       text "\t.long\t0"]
                  _ ->
                      [text "\t.quad\t" <> pprImm imm]

        ppr_item _ _
                = panic "X86.Ppr.ppr_item: no match"


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
      -> asmComment (text "\tunwind = " <> ppr d)
         $$ ppr lbl <> colon

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
     -> pprFormatOpOp (sLit "mov") format src dst

   CMOV cc format src dst
     -> pprCondOpReg (sLit "cmov") format cc src dst

   MOVZxL II32 src dst
      -> pprFormatOpOp (sLit "mov") II32 src dst
        -- 32-to-64 bit zero extension on x86_64 is accomplished by a simple
        -- movl.  But we represent it as a MOVZxL instruction, because
        -- the reg alloc would tend to throw away a plain reg-to-reg
        -- move, and we still want it to do that.

   MOVZxL formats src dst
      -> pprFormatOpOpCoerce (sLit "movz") formats II32 src dst
        -- zero-extension only needs to extend to 32 bits: on x86_64,
        -- the remaining zero-extension to 64 bits is automatic, and the 32-bit
        -- instruction is shorter.

   MOVSxL formats src dst
      -> pprFormatOpOpCoerce (sLit "movs") formats (archWordFormat (target32Bit platform)) src dst

   -- here we do some patching, since the physical registers are only set late
   -- in the code generation.
   LEA format (OpAddr (AddrBaseIndex (EABaseReg reg1) (EAIndex reg2 1) (ImmInt 0))) dst@(OpReg reg3)
      | reg1 == reg3
      -> pprFormatOpOp (sLit "add") format (OpReg reg2) dst

   LEA format (OpAddr (AddrBaseIndex (EABaseReg reg1) (EAIndex reg2 1) (ImmInt 0))) dst@(OpReg reg3)
      | reg2 == reg3
      -> pprFormatOpOp (sLit "add") format (OpReg reg1) dst

   LEA format (OpAddr (AddrBaseIndex (EABaseReg reg1) EAIndexNone displ)) dst@(OpReg reg3)
      | reg1 == reg3
      -> pprInstr platform (ADD format (OpImm displ) dst)

   LEA format src dst
      -> pprFormatOpOp (sLit "lea") format src dst

   ADD format (OpImm (ImmInt (-1))) dst
      -> pprFormatOp (sLit "dec") format dst

   ADD format (OpImm (ImmInt 1)) dst
      -> pprFormatOp (sLit "inc") format dst

   ADD format src dst
      -> pprFormatOpOp (sLit "add") format src dst

   ADC format src dst
      -> pprFormatOpOp (sLit "adc") format src dst

   SUB format src dst
      -> pprFormatOpOp (sLit "sub") format src dst

   SBB format src dst
      -> pprFormatOpOp (sLit "sbb") format src dst

   IMUL format op1 op2
      -> pprFormatOpOp (sLit "imul") format op1 op2

   ADD_CC format src dst
      -> pprFormatOpOp (sLit "add") format src dst

   SUB_CC format src dst
      -> pprFormatOpOp (sLit "sub") format src dst

   -- Use a 32-bit instruction when possible as it saves a byte.
   -- Notably, extracting the tag bits of a pointer has this form.
   -- TODO: we could save a byte in a subsequent CMP instruction too,
   -- but need something like a peephole pass for this
   AND II64 src@(OpImm (ImmInteger mask)) dst
      | 0 <= mask && mask < 0xffffffff
      -> pprInstr platform (AND II32 src dst)

   AND FF32 src dst
      -> pprOpOp (sLit "andps") FF32 src dst

   AND FF64 src dst
      -> pprOpOp (sLit "andpd") FF64 src dst

   AND format src dst
      -> pprFormatOpOp (sLit "and") format src dst

   OR  format src dst
      -> pprFormatOpOp (sLit "or")  format src dst

   XOR FF32 src dst
      -> pprOpOp (sLit "xorps") FF32 src dst

   XOR FF64 src dst
      ->  pprOpOp (sLit "xorpd") FF64 src dst

   XOR format src dst
      -> pprFormatOpOp (sLit "xor") format src dst

   POPCNT format src dst
      -> pprOpOp (sLit "popcnt") format src (OpReg dst)

   LZCNT format src dst
      ->  pprOpOp (sLit "lzcnt") format src (OpReg dst)

   TZCNT format src dst
      -> pprOpOp (sLit "tzcnt") format src (OpReg dst)

   BSF format src dst
      -> pprOpOp (sLit "bsf") format src (OpReg dst)

   BSR format src dst
      -> pprOpOp (sLit "bsr") format src (OpReg dst)

   PDEP format src mask dst
      -> pprFormatOpOpReg (sLit "pdep") format src mask dst

   PEXT format src mask dst
      -> pprFormatOpOpReg (sLit "pext") format src mask dst

   PREFETCH NTA format src
      -> pprFormatOp_ (sLit "prefetchnta") format src

   PREFETCH Lvl0 format src
      -> pprFormatOp_ (sLit "prefetcht0") format src

   PREFETCH Lvl1 format src
      -> pprFormatOp_ (sLit "prefetcht1") format src

   PREFETCH Lvl2 format src
      -> pprFormatOp_ (sLit "prefetcht2") format src

   NOT format op
      -> pprFormatOp (sLit "not") format op

   BSWAP format op
      -> pprFormatOp (sLit "bswap") format (OpReg op)

   NEGI format op
      -> pprFormatOp (sLit "neg") format op

   SHL format src dst
      -> pprShift (sLit "shl") format src dst

   SAR format src dst
      -> pprShift (sLit "sar") format src dst

   SHR format src dst
      -> pprShift (sLit "shr") format src dst

   BT format imm src
      -> pprFormatImmOp (sLit "bt") format imm src

   CMP format src dst
     | isFloatFormat format -> pprFormatOpOp (sLit "ucomi") format src dst -- SSE2
     | otherwise            -> pprFormatOpOp (sLit "cmp")   format src dst

   TEST format src dst
      -> pprFormatOpOp (sLit "test") format' src dst
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
      -> pprFormatOp (sLit "push") format op

   POP format op
      -> pprFormatOp (sLit "pop") format op

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
      -> pprCondInstr (sLit "set") cond (pprOperand platform II8 op)

   JXX cond blockid
      -> pprCondInstr (sLit "j") cond (ppr lab)
         where lab = blockLbl blockid

   JXX_GBL cond imm
      -> pprCondInstr (sLit "j") cond (pprImm imm)

   JMP (OpImm imm) _
      -> text "\tjmp " <> pprImm imm

   JMP op _
      -> text "\tjmp *" <> pprOperand platform (archWordFormat (target32Bit platform)) op

   JMP_TBL op _ _ _
      -> pprInstr platform (JMP op [])

   CALL (Left imm) _
      -> text "\tcall " <> pprImm imm

   CALL (Right reg) _
      -> text "\tcall *" <> pprReg platform (archWordFormat (target32Bit platform)) reg

   IDIV fmt op
      -> pprFormatOp (sLit "idiv") fmt op

   DIV fmt op
      -> pprFormatOp (sLit "div")  fmt op

   IMUL2 fmt op
      -> pprFormatOp (sLit "imul") fmt op

   -- x86_64 only
   MUL format op1 op2
      -> pprFormatOpOp (sLit "mul") format op1 op2

   MUL2 format op
      -> pprFormatOp (sLit "mul") format op

   FDIV format op1 op2
      -> pprFormatOpOp (sLit "div") format op1 op2

   SQRT format op1 op2
      -> pprFormatOpReg (sLit "sqrt") format op1 op2

   CVTSS2SD from to
      -> pprRegReg (sLit "cvtss2sd") from to

   CVTSD2SS from to
      -> pprRegReg (sLit "cvtsd2ss") from to

   CVTTSS2SIQ fmt from to
      -> pprFormatFormatOpReg (sLit "cvttss2si") FF32 fmt from to

   CVTTSD2SIQ fmt from to
      -> pprFormatFormatOpReg (sLit "cvttsd2si") FF64 fmt from to

   CVTSI2SS fmt from to
      -> pprFormatOpReg (sLit "cvtsi2ss") fmt from to

   CVTSI2SD fmt from to
      -> pprFormatOpReg (sLit "cvtsi2sd") fmt from to

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
      -> pprFormatOpOp (sLit "xadd") format src dst

   CMPXCHG format src dst
      -> pprFormatOpOp (sLit "cmpxchg") format src dst


  where
   gtab :: SDoc
   gtab  = char '\t'

   gsp :: SDoc
   gsp   = char ' '



   pprX87 :: Instr -> SDoc -> SDoc
   pprX87 fake actual
      = (char '#' <> pprX87Instr fake) $$ actual

   pprX87Instr :: Instr -> SDoc
   pprX87Instr (X87Store fmt dst) = pprFormatAddr (sLit "gst") fmt dst
   pprX87Instr _ = panic "X86.Ppr.pprX87Instr: no match"

   pprDollImm :: Imm -> SDoc
   pprDollImm i = text "$" <> pprImm i


   pprOperand :: Platform -> Format -> Operand -> SDoc
   pprOperand platform f op = case op of
      OpReg r   -> pprReg platform f r
      OpImm i   -> pprDollImm i
      OpAddr ea -> pprAddr platform ea


   pprMnemonic_  :: PtrString -> SDoc
   pprMnemonic_ name =
      char '\t' <> ptext name <> space


   pprMnemonic  :: PtrString -> Format -> SDoc
   pprMnemonic name format =
      char '\t' <> ptext name <> pprFormat format <> space


   pprFormatImmOp :: PtrString -> Format -> Imm -> Operand -> SDoc
   pprFormatImmOp name format imm op1
     = hcat [
           pprMnemonic name format,
           char '$',
           pprImm imm,
           comma,
           pprOperand platform format op1
       ]


   pprFormatOp_ :: PtrString -> Format -> Operand -> SDoc
   pprFormatOp_ name format op1
     = hcat [
           pprMnemonic_ name ,
           pprOperand platform format op1
       ]

   pprFormatOp :: PtrString -> Format -> Operand -> SDoc
   pprFormatOp name format op1
     = hcat [
           pprMnemonic name format,
           pprOperand platform format op1
       ]


   pprFormatOpOp :: PtrString -> Format -> Operand -> Operand -> SDoc
   pprFormatOpOp name format op1 op2
     = hcat [
           pprMnemonic name format,
           pprOperand platform format op1,
           comma,
           pprOperand platform format op2
       ]


   pprOpOp :: PtrString -> Format -> Operand -> Operand -> SDoc
   pprOpOp name format op1 op2
     = hcat [
           pprMnemonic_ name,
           pprOperand platform format op1,
           comma,
           pprOperand platform format op2
       ]

   pprRegReg :: PtrString -> Reg -> Reg -> SDoc
   pprRegReg name reg1 reg2
     = hcat [
           pprMnemonic_ name,
           pprReg platform (archWordFormat (target32Bit platform)) reg1,
           comma,
           pprReg platform (archWordFormat (target32Bit platform)) reg2
       ]


   pprFormatOpReg :: PtrString -> Format -> Operand -> Reg -> SDoc
   pprFormatOpReg name format op1 reg2
     = hcat [
           pprMnemonic name format,
           pprOperand platform format op1,
           comma,
           pprReg platform (archWordFormat (target32Bit platform)) reg2
       ]

   pprCondOpReg :: PtrString -> Format -> Cond -> Operand -> Reg -> SDoc
   pprCondOpReg name format cond op1 reg2
     = hcat [
           char '\t',
           ptext name,
           pprCond cond,
           space,
           pprOperand platform format op1,
           comma,
           pprReg platform format reg2
       ]

   pprFormatFormatOpReg :: PtrString -> Format -> Format -> Operand -> Reg -> SDoc
   pprFormatFormatOpReg name format1 format2 op1 reg2
     = hcat [
           pprMnemonic name format2,
           pprOperand platform format1 op1,
           comma,
           pprReg platform format2 reg2
       ]

   pprFormatOpOpReg :: PtrString -> Format -> Operand -> Operand -> Reg -> SDoc
   pprFormatOpOpReg name format op1 op2 reg3
     = hcat [
           pprMnemonic name format,
           pprOperand platform format op1,
           comma,
           pprOperand platform format op2,
           comma,
           pprReg platform format reg3
       ]



   pprFormatAddr :: PtrString -> Format -> AddrMode -> SDoc
   pprFormatAddr name format  op
     = hcat [
           pprMnemonic name format,
           comma,
           pprAddr platform op
       ]

   pprShift :: PtrString -> Format -> Operand -> Operand -> SDoc
   pprShift name format src dest
     = hcat [
           pprMnemonic name format,
           pprOperand platform II8 src,  -- src is 8-bit sized
           comma,
           pprOperand platform format dest
       ]


   pprFormatOpOpCoerce :: PtrString -> Format -> Format -> Operand -> Operand -> SDoc
   pprFormatOpOpCoerce name format1 format2 op1 op2
     = hcat [ char '\t', ptext name, pprFormat format1, pprFormat format2, space,
           pprOperand platform format1 op1,
           comma,
           pprOperand platform format2 op2
       ]


   pprCondInstr :: PtrString -> Cond -> SDoc -> SDoc
   pprCondInstr name cond arg
     = hcat [ char '\t', ptext name, pprCond cond, space, arg]
