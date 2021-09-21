{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}


-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
module GHC.CmmToAsm.SPARC.Ppr (
        pprNatCmmDecl,
        pprBasicBlock,
        pprData,
        pprInstr,
        pprFormat,
        pprImm,
        pprDataItem
)

where

import GHC.Prelude

import Data.Word
import qualified Data.Array.Unsafe as U ( castSTUArray )
import Data.Array.ST

import Control.Monad.ST

import GHC.CmmToAsm.SPARC.Regs
import GHC.CmmToAsm.SPARC.Instr
import GHC.CmmToAsm.SPARC.Cond
import GHC.CmmToAsm.SPARC.Imm
import GHC.CmmToAsm.SPARC.AddrMode
import GHC.CmmToAsm.SPARC.Base
import GHC.Platform.Reg
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Ppr
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Utils

import GHC.Cmm hiding (topInfoTable)
import GHC.Cmm.Ppr() -- For Outputable instances
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Dataflow.Collections

import GHC.Types.Unique ( pprUniqueAlways )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Platform

-- -----------------------------------------------------------------------------
-- Printing this stuff out

pprNatCmmDecl :: NCGConfig -> NatCmmDecl RawCmmStatics Instr -> SDoc
pprNatCmmDecl config (CmmData section dats) =
  pprSectionAlign config section
  $$ pprDatas (ncgPlatform config) dats

pprNatCmmDecl config proc@(CmmProc top_info lbl _ (ListGraph blocks)) =
  let platform = ncgPlatform config in
  case topInfoTable proc of
    Nothing ->
        -- special case for code without info table:
        pprSectionAlign config (Section Text lbl) $$
        pprLabel platform lbl $$ -- blocks guaranteed not null, so label needed
        vcat (map (pprBasicBlock platform top_info) blocks)

    Just (CmmStaticsRaw info_lbl _) ->
      (if platformHasSubsectionsViaSymbols platform
          then pprSectionAlign config dspSection $$
               pdoc platform (mkDeadStripPreventer info_lbl) <> char ':'
          else empty) $$
      vcat (map (pprBasicBlock platform top_info) blocks) $$
      -- above: Even the first block gets a label, because with branch-chain
      -- elimination, it might be the target of a goto.
      (if platformHasSubsectionsViaSymbols platform
       then
       -- See Note [Subsections Via Symbols] in X86/Ppr.hs
                text "\t.long "
            <+> pdoc platform info_lbl
            <+> char '-'
            <+> pdoc platform (mkDeadStripPreventer info_lbl)
       else empty)

dspSection :: Section
dspSection = Section Text $
    panic "subsections-via-symbols doesn't combine with split-sections"

pprBasicBlock :: Platform -> LabelMap RawCmmStatics -> NatBasicBlock Instr -> SDoc
pprBasicBlock platform info_env (BasicBlock blockid instrs)
  = maybe_infotable $$
    pprLabel platform (blockLbl blockid) $$
    vcat (map (pprInstr platform) instrs)
  where
    maybe_infotable = case mapLookup blockid info_env of
       Nothing   -> empty
       Just (CmmStaticsRaw info_lbl info) ->
           pprAlignForSection Text $$
           vcat (map (pprData platform) info) $$
           pprLabel platform info_lbl


pprDatas :: Platform -> RawCmmStatics -> SDoc
-- See note [emit-time elimination of static indirections] in "GHC.Cmm.CLabel".
pprDatas platform (CmmStaticsRaw alias [CmmStaticLit (CmmLabel lbl), CmmStaticLit ind, _, _])
  | lbl == mkIndStaticInfoLabel
  , let labelInd (CmmLabelOff l _) = Just l
        labelInd (CmmLabel l) = Just l
        labelInd _ = Nothing
  , Just ind' <- labelInd ind
  , alias `mayRedirectTo` ind'
  = pprGloblDecl platform alias
    $$ text ".equiv" <+> pdoc platform alias <> comma <> pdoc platform (CmmLabel ind')
pprDatas platform (CmmStaticsRaw lbl dats) = vcat (pprLabel platform lbl : map (pprData platform) dats)

pprData :: Platform -> CmmStatic -> SDoc
pprData platform d = case d of
   CmmString str          -> pprString str
   CmmFileEmbed path      -> pprFileEmbed path
   CmmUninitialised bytes -> text ".skip " <> int bytes
   CmmStaticLit lit       -> pprDataItem platform lit

pprGloblDecl :: Platform -> CLabel -> SDoc
pprGloblDecl platform lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = text ".global " <> pdoc platform lbl

pprTypeAndSizeDecl :: Platform -> CLabel -> SDoc
pprTypeAndSizeDecl platform lbl
    = if platformOS platform == OSLinux && externallyVisibleCLabel lbl
      then text ".type " <> pdoc platform lbl <> text ", @object"
      else empty

pprLabel :: Platform -> CLabel -> SDoc
pprLabel platform lbl =
   pprGloblDecl platform lbl
   $$ pprTypeAndSizeDecl platform lbl
   $$ (pdoc platform lbl <> char ':')

-- -----------------------------------------------------------------------------
-- pprInstr: print an 'Instr'

instance OutputableP Platform Instr where
    pdoc = pprInstr


-- | Pretty print a register.
pprReg :: Reg -> SDoc
pprReg reg
 = case reg of
        RegVirtual vr
         -> case vr of
                VirtualRegI   u -> text "%vI_"   <> pprUniqueAlways u
                VirtualRegHi  u -> text "%vHi_"  <> pprUniqueAlways u
                VirtualRegF   u -> text "%vF_"   <> pprUniqueAlways u
                VirtualRegD   u -> text "%vD_"   <> pprUniqueAlways u


        RegReal rr
         -> case rr of
                RealRegSingle r1
                 -> pprReg_ofRegNo r1

                RealRegPair r1 r2
                 -> text "(" <> pprReg_ofRegNo r1
                 <> vbar     <> pprReg_ofRegNo r2
                 <> text ")"



-- | Pretty print a register name, based on this register number.
--   The definition has been unfolded so we get a jump-table in the
--   object code. This function is called quite a lot when emitting
--   the asm file..
--
pprReg_ofRegNo :: Int -> SDoc
pprReg_ofRegNo i
 = case i of {
         0 -> text "%g0";   1 -> text "%g1";
         2 -> text "%g2";   3 -> text "%g3";
         4 -> text "%g4";   5 -> text "%g5";
         6 -> text "%g6";   7 -> text "%g7";
         8 -> text "%o0";   9 -> text "%o1";
        10 -> text "%o2";  11 -> text "%o3";
        12 -> text "%o4";  13 -> text "%o5";
        14 -> text "%o6";  15 -> text "%o7";
        16 -> text "%l0";  17 -> text "%l1";
        18 -> text "%l2";  19 -> text "%l3";
        20 -> text "%l4";  21 -> text "%l5";
        22 -> text "%l6";  23 -> text "%l7";
        24 -> text "%i0";  25 -> text "%i1";
        26 -> text "%i2";  27 -> text "%i3";
        28 -> text "%i4";  29 -> text "%i5";
        30 -> text "%i6";  31 -> text "%i7";
        32 -> text "%f0";  33 -> text "%f1";
        34 -> text "%f2";  35 -> text "%f3";
        36 -> text "%f4";  37 -> text "%f5";
        38 -> text "%f6";  39 -> text "%f7";
        40 -> text "%f8";  41 -> text "%f9";
        42 -> text "%f10"; 43 -> text "%f11";
        44 -> text "%f12"; 45 -> text "%f13";
        46 -> text "%f14"; 47 -> text "%f15";
        48 -> text "%f16"; 49 -> text "%f17";
        50 -> text "%f18"; 51 -> text "%f19";
        52 -> text "%f20"; 53 -> text "%f21";
        54 -> text "%f22"; 55 -> text "%f23";
        56 -> text "%f24"; 57 -> text "%f25";
        58 -> text "%f26"; 59 -> text "%f27";
        60 -> text "%f28"; 61 -> text "%f29";
        62 -> text "%f30"; 63 -> text "%f31";
        _  -> text "very naughty sparc register" }


-- | Pretty print a format for an instruction suffix.
pprFormat :: Format -> SDoc
pprFormat x
 = case x of
        II8     -> text "ub"
        II16    -> text "uh"
        II32    -> text ""
        II64    -> text "d"
        FF32    -> text ""
        FF64    -> text "d"


-- | Pretty print a format for an instruction suffix.
--      eg LD is 32bit on sparc, but LDD is 64 bit.
pprStFormat :: Format -> SDoc
pprStFormat x
 = case x of
        II8   -> text "b"
        II16  -> text "h"
        II32  -> text ""
        II64  -> text "x"
        FF32  -> text ""
        FF64  -> text "d"



-- | Pretty print a condition code.
pprCond :: Cond -> SDoc
pprCond c
 = case c of
        ALWAYS  -> text ""
        NEVER   -> text "n"
        GEU     -> text "geu"
        LU      -> text "lu"
        EQQ     -> text "e"
        GTT     -> text "g"
        GE      -> text "ge"
        GU      -> text "gu"
        LTT     -> text "l"
        LE      -> text "le"
        LEU     -> text "leu"
        NE      -> text "ne"
        NEG     -> text "neg"
        POS     -> text "pos"
        VC      -> text "vc"
        VS      -> text "vs"


-- | Pretty print an address mode.
pprAddr :: Platform -> AddrMode -> SDoc
pprAddr platform am
 = case am of
        AddrRegReg r1 (RegReal (RealRegSingle 0))
         -> pprReg r1

        AddrRegReg r1 r2
         -> hcat [ pprReg r1, char '+', pprReg r2 ]

        AddrRegImm r1 (ImmInt i)
         | i == 0               -> pprReg r1
         | not (fits13Bits i)   -> largeOffsetError i
         | otherwise            -> hcat [ pprReg r1, pp_sign, int i ]
         where
                pp_sign = if i > 0 then char '+' else empty

        AddrRegImm r1 (ImmInteger i)
         | i == 0               -> pprReg r1
         | not (fits13Bits i)   -> largeOffsetError i
         | otherwise            -> hcat [ pprReg r1, pp_sign, integer i ]
         where
                pp_sign = if i > 0 then char '+' else empty

        AddrRegImm r1 imm
         -> hcat [ pprReg r1, char '+', pprImm platform imm ]


-- | Pretty print an immediate value.
pprImm :: Platform -> Imm -> SDoc
pprImm platform imm
 = case imm of
        ImmInt i        -> int i
        ImmInteger i    -> integer i
        ImmCLbl l       -> pdoc platform l
        ImmIndex l i    -> pdoc platform l <> char '+' <> int i
        ImmLit s        -> s

        ImmConstantSum a b
         -> pprImm platform a <> char '+' <> pprImm platform b

        ImmConstantDiff a b
         -> pprImm platform a <> char '-' <> lparen <> pprImm platform b <> rparen

        LO i
         -> hcat [ text "%lo(", pprImm platform i, rparen ]

        HI i
         -> hcat [ text "%hi(", pprImm platform i, rparen ]

        -- these should have been converted to bytes and placed
        --      in the data section.
        ImmFloat _      -> text "naughty float immediate"
        ImmDouble _     -> text "naughty double immediate"


-- | Pretty print a section \/ segment header.
--      On SPARC all the data sections must be at least 8 byte aligned
--      incase we store doubles in them.
--
pprSectionAlign :: NCGConfig -> Section -> SDoc
pprSectionAlign config sec@(Section seg _) =
    pprSectionHeader config sec $$
    pprAlignForSection seg

-- | Print appropriate alignment for the given section type.
pprAlignForSection :: SectionType -> SDoc
pprAlignForSection seg =
    case seg of
      Text                    -> text ".align 4"
      Data                    -> text ".align 8"
      ReadOnlyData            -> text ".align 8"
      RelocatableReadOnlyData -> text ".align 8"
      UninitialisedData       -> text ".align 8"
      ReadOnlyData16          -> text ".align 16"
      -- TODO: This is copied from the ReadOnlyData case, but it can likely be
      -- made more efficient.
      CString                 -> text ".align 8"
      OtherSection _          -> panic "PprMach.pprSectionHeader: unknown section"

-- | Pretty print a data item.
pprDataItem :: Platform -> CmmLit -> SDoc
pprDataItem platform lit
  = vcat (ppr_item (cmmTypeFormat $ cmmLitType platform lit) lit)
    where
        imm = litToImm lit

        ppr_item II8   _        = [text "\t.byte\t" <> pprImm platform imm]
        ppr_item II32  _        = [text "\t.long\t" <> pprImm platform imm]

        ppr_item FF32  (CmmFloat r _)
         = let bs = floatToBytes (fromRational r)
           in  map (\b -> text "\t.byte\t" <> pprImm platform (ImmInt b)) bs

        ppr_item FF64 (CmmFloat r _)
         = let bs = doubleToBytes (fromRational r)
           in  map (\b -> text "\t.byte\t" <> pprImm platform (ImmInt b)) bs

        ppr_item II16  _        = [text "\t.short\t" <> pprImm platform imm]
        ppr_item II64  _        = [text "\t.quad\t"  <> pprImm platform imm]
        ppr_item _ _            = panic "SPARC.Ppr.pprDataItem: no match"

floatToBytes :: Float -> [Int]
floatToBytes f
   = runST (do
        arr <- newArray_ ((0::Int),3)
        writeArray arr 0 f
        arr <- castFloatToWord8Array arr
        i0 <- readArray arr 0
        i1 <- readArray arr 1
        i2 <- readArray arr 2
        i3 <- readArray arr 3
        return (map fromIntegral [i0,i1,i2,i3])
     )

castFloatToWord8Array :: STUArray s Int Float -> ST s (STUArray s Int Word8)
castFloatToWord8Array = U.castSTUArray


asmComment :: SDoc -> SDoc
asmComment c = whenPprDebug $ text "#" <+> c


-- | Pretty print an instruction.
pprInstr :: Platform -> Instr -> SDoc
pprInstr platform = \case
   COMMENT s -> asmComment s
   DELTA d   -> asmComment $ text ("\tdelta = " ++ show d)

   -- Newblocks and LData should have been slurped out before producing the .s file.
   NEWBLOCK _ -> panic "X86.Ppr.pprInstr: NEWBLOCK"
   LDATA _ _  -> panic "PprMach.pprInstr: LDATA"

   -- 64 bit FP loads are expanded into individual instructions in CodeGen.Expand
   LD FF64 _ reg
        | RegReal (RealRegSingle{})     <- reg
        -> panic "SPARC.Ppr: not emitting potentially misaligned LD FF64 instr"

   LD format addr reg
        -> hcat [
               text "\tld",
               pprFormat format,
               char '\t',
               lbrack,
               pprAddr platform addr,
               pp_rbracket_comma,
               pprReg reg
            ]

   -- 64 bit FP stores are expanded into individual instructions in CodeGen.Expand
   ST FF64 reg _
        | RegReal (RealRegSingle{}) <- reg
        -> panic "SPARC.Ppr: not emitting potentially misaligned ST FF64 instr"

   -- no distinction is made between signed and unsigned bytes on stores for the
   -- Sparc opcodes (at least I cannot see any, and gas is nagging me --SOF),
   -- so we call a special-purpose pprFormat for ST..
   ST format reg addr
        -> hcat [
               text "\tst",
               pprStFormat format,
               char '\t',
               pprReg reg,
               pp_comma_lbracket,
               pprAddr platform addr,
               rbrack
            ]


   ADD x cc reg1 ri reg2
        | not x && not cc && riZero ri
        -> hcat [ text "\tmov\t", pprReg reg1, comma, pprReg reg2 ]

        | otherwise
        -> pprRegRIReg platform (if x then text "addx" else text "add") cc reg1 ri reg2


   SUB x cc reg1 ri reg2
        | not x && cc && reg2 == g0
        -> hcat [ text "\tcmp\t", pprReg reg1, comma, pprRI platform ri ]

        | not x && not cc && riZero ri
        -> hcat [ text "\tmov\t", pprReg reg1, comma, pprReg reg2 ]

        | otherwise
        -> pprRegRIReg platform (if x then text "subx" else text "sub") cc reg1 ri reg2

   AND  b reg1 ri reg2 -> pprRegRIReg platform (text "and")  b reg1 ri reg2

   ANDN b reg1 ri reg2 -> pprRegRIReg platform (text "andn") b reg1 ri reg2

   OR b reg1 ri reg2
        | not b && reg1 == g0
        -> let doit = hcat [ text "\tmov\t", pprRI platform ri, comma, pprReg reg2 ]
           in  case ri of
                   RIReg rrr | rrr == reg2 -> empty
                   _                       -> doit

        | otherwise
        -> pprRegRIReg platform (text "or") b reg1 ri reg2

   ORN b reg1 ri reg2 -> pprRegRIReg platform (text "orn") b reg1 ri reg2

   XOR  b reg1 ri reg2 -> pprRegRIReg platform (text "xor")  b reg1 ri reg2
   XNOR b reg1 ri reg2 -> pprRegRIReg platform (text "xnor") b reg1 ri reg2

   SLL reg1 ri reg2 -> pprRegRIReg platform (text "sll") False reg1 ri reg2
   SRL reg1 ri reg2 -> pprRegRIReg platform (text "srl") False reg1 ri reg2
   SRA reg1 ri reg2 -> pprRegRIReg platform (text "sra") False reg1 ri reg2

   RDY rd -> text "\trd\t%y," <> pprReg rd
   WRY reg1 reg2
        -> text "\twr\t"
                <> pprReg reg1
                <> char ','
                <> pprReg reg2
                <> char ','
                <> text "%y"

   SMUL b reg1 ri reg2 -> pprRegRIReg platform (text "smul")  b reg1 ri reg2
   UMUL b reg1 ri reg2 -> pprRegRIReg platform (text "umul")  b reg1 ri reg2
   SDIV b reg1 ri reg2 -> pprRegRIReg platform (text "sdiv")  b reg1 ri reg2
   UDIV b reg1 ri reg2 -> pprRegRIReg platform (text "udiv")  b reg1 ri reg2

   SETHI imm reg
      -> hcat [
            text "\tsethi\t",
            pprImm platform imm,
            comma,
            pprReg reg
         ]

   NOP -> text "\tnop"

   FABS format reg1 reg2
        -> pprFormatRegReg (text "fabs") format reg1 reg2

   FADD format reg1 reg2 reg3
        -> pprFormatRegRegReg (text "fadd") format reg1 reg2 reg3

   FCMP e format reg1 reg2
        -> pprFormatRegReg (if e then text "fcmpe" else text "fcmp")
                           format reg1 reg2

   FDIV format reg1 reg2 reg3
        -> pprFormatRegRegReg (text "fdiv") format reg1 reg2 reg3

   FMOV format reg1 reg2
        -> pprFormatRegReg (text "fmov") format reg1 reg2

   FMUL format reg1 reg2 reg3
        -> pprFormatRegRegReg (text "fmul") format reg1 reg2 reg3

   FNEG format reg1 reg2
        -> pprFormatRegReg (text "fneg") format reg1 reg2

   FSQRT format reg1 reg2
        -> pprFormatRegReg (text "fsqrt") format reg1 reg2

   FSUB format reg1 reg2 reg3
        -> pprFormatRegRegReg (text "fsub") format reg1 reg2 reg3

   FxTOy format1 format2 reg1 reg2
      -> hcat [
            text "\tf",
            (case format1 of
                II32  -> text "ito"
                FF32  -> text "sto"
                FF64  -> text "dto"
                _     -> panic "SPARC.Ppr.pprInstr.FxToY: no match"),
            (case format2 of
                II32  -> text "i\t"
                II64  -> text "x\t"
                FF32  -> text "s\t"
                FF64  -> text "d\t"
                _     -> panic "SPARC.Ppr.pprInstr.FxToY: no match"),
            pprReg reg1, comma, pprReg reg2
         ]


   BI cond b blockid
      -> hcat [
            text "\tb", pprCond cond,
            if b then pp_comma_a else empty,
            char '\t',
            pdoc platform (blockLbl blockid)
         ]

   BF cond b blockid
      -> hcat [
            text "\tfb", pprCond cond,
            if b then pp_comma_a else empty,
            char '\t',
            pdoc platform (blockLbl blockid)
         ]

   JMP addr -> text "\tjmp\t" <> pprAddr platform addr
   JMP_TBL op _ _ -> pprInstr platform (JMP op)

   CALL (Left imm) n _
      -> hcat [ text "\tcall\t", pprImm platform imm, comma, int n ]

   CALL (Right reg) n _
      -> hcat [ text "\tcall\t", pprReg reg, comma, int n ]


-- | Pretty print a RI
pprRI :: Platform -> RI -> SDoc
pprRI platform = \case
   RIReg r -> pprReg r
   RIImm r -> pprImm platform r


-- | Pretty print a two reg instruction.
pprFormatRegReg :: SDoc -> Format -> Reg -> Reg -> SDoc
pprFormatRegReg name format reg1 reg2
  = hcat [
        char '\t',
        name,
        (case format of
            FF32 -> text "s\t"
            FF64 -> text "d\t"
            _    -> panic "SPARC.Ppr.pprFormatRegReg: no match"),

        pprReg reg1,
        comma,
        pprReg reg2
    ]


-- | Pretty print a three reg instruction.
pprFormatRegRegReg :: SDoc -> Format -> Reg -> Reg -> Reg -> SDoc
pprFormatRegRegReg name format reg1 reg2 reg3
  = hcat [
        char '\t',
        name,
        (case format of
            FF32  -> text "s\t"
            FF64  -> text "d\t"
            _    -> panic "SPARC.Ppr.pprFormatRegReg: no match"),
        pprReg reg1,
        comma,
        pprReg reg2,
        comma,
        pprReg reg3
    ]


-- | Pretty print an instruction of two regs and a ri.
pprRegRIReg :: Platform -> SDoc -> Bool -> Reg -> RI -> Reg -> SDoc
pprRegRIReg platform name b reg1 ri reg2
  = hcat [
        char '\t',
        name,
        if b then text "cc\t" else char '\t',
        pprReg reg1,
        comma,
        pprRI platform ri,
        comma,
        pprReg reg2
    ]

{-
pprRIReg :: SDoc -> Bool -> RI -> Reg -> SDoc
pprRIReg name b ri reg1
  = hcat [
        char '\t',
        name,
        if b then text "cc\t" else char '\t',
        pprRI ri,
        comma,
        pprReg reg1
    ]
-}

{-
pp_ld_lbracket :: SDoc
pp_ld_lbracket    = text "\tld\t["
-}

pp_rbracket_comma :: SDoc
pp_rbracket_comma = text "],"


pp_comma_lbracket :: SDoc
pp_comma_lbracket = text ",["


pp_comma_a :: SDoc
pp_comma_a        = text ",a"
