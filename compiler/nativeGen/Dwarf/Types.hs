module Dwarf.Types
  ( -- * Dwarf information
    DwarfInfo(..)
  , pprDwarfInfo
  , pprAbbrevDecls
    -- * Dwarf address range table
  , DwarfARange(..)
  , pprDwarfARanges
    -- * Dwarf frame
  , DwarfFrame(..), DwarfFrameProc(..), DwarfFrameBlock(..)
  , pprDwarfFrame
    -- * Utilities
  , pprByte
  , pprHalf
  , pprData4'
  , pprDwWord
  , pprWord
  , pprLEBWord
  , pprLEBInt
  , wordAlign
  , sectionOffset
  )
  where

import GhcPrelude

import Debug
import CLabel
import CmmExpr         ( GlobalReg(..) )
import Encoding
import FastString
import Outputable
import Platform
import Unique
import Reg
import SrcLoc
import Util

import Dwarf.Constants

import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad (zipWithM, join)
import Data.Bits
import qualified Data.Map as Map
import Data.Word
import Data.Char

import CodeGen.Platform

-- | Individual dwarf records. Each one will be encoded as an entry in
-- the @.debug_info@ section.
data DwarfInfo
  = DwarfCompileUnit { dwChildren :: [DwarfInfo]
                     , dwName :: String
                     , dwProducer :: String
                     , dwCompDir :: String
                     , dwLowLabel :: CLabel
                     , dwHighLabel :: CLabel
                     , dwLineLabel :: PtrString }
  | DwarfSubprogram { dwChildren :: [DwarfInfo]
                    , dwName :: String
                    , dwLabel :: CLabel
                    , dwParent :: Maybe CLabel
                      -- ^ label of DIE belonging to the parent tick
                    }
  | DwarfBlock { dwChildren :: [DwarfInfo]
               , dwLabel :: CLabel
               , dwMarker :: Maybe CLabel
               }
  | DwarfSrcNote { dwSrcSpan :: RealSrcSpan
                 }

-- | Abbreviation codes used for encoding above records in the
-- @.debug_info@ section.
data DwarfAbbrev
  = DwAbbrNull          -- ^ Pseudo, used for marking the end of lists
  | DwAbbrCompileUnit
  | DwAbbrSubprogram
  | DwAbbrSubprogramWithParent
  | DwAbbrBlockWithoutCode
  | DwAbbrBlock
  | DwAbbrGhcSrcNote
  deriving (Eq, Enum)

-- | Generate assembly for the given abbreviation code
pprAbbrev :: DwarfAbbrev -> SDoc
pprAbbrev = pprLEBWord . fromIntegral . fromEnum

-- | Abbreviation declaration. This explains the binary encoding we
-- use for representing 'DwarfInfo'. Be aware that this must be updated
-- along with 'pprDwarfInfo'.
pprAbbrevDecls :: Bool -> SDoc
pprAbbrevDecls haveDebugLine =
  let mkAbbrev abbr tag chld flds =
        let fld (tag, form) = pprLEBWord tag $$ pprLEBWord form
        in pprAbbrev abbr $$ pprLEBWord tag $$ pprByte chld $$
           vcat (map fld flds) $$ pprByte 0 $$ pprByte 0
      -- These are shared between DwAbbrSubprogram and
      -- DwAbbrSubprogramWithParent
      subprogramAttrs =
           [ (dW_AT_name, dW_FORM_string)
           , (dW_AT_MIPS_linkage_name, dW_FORM_string)
           , (dW_AT_external, dW_FORM_flag)
           , (dW_AT_low_pc, dW_FORM_addr)
           , (dW_AT_high_pc, dW_FORM_addr)
           , (dW_AT_frame_base, dW_FORM_block1)
           ]
  in dwarfAbbrevSection $$
     ptext dwarfAbbrevLabel <> colon $$
     mkAbbrev DwAbbrCompileUnit dW_TAG_compile_unit dW_CHILDREN_yes
       ([(dW_AT_name,     dW_FORM_string)
       , (dW_AT_producer, dW_FORM_string)
       , (dW_AT_language, dW_FORM_data4)
       , (dW_AT_comp_dir, dW_FORM_string)
       , (dW_AT_use_UTF8, dW_FORM_flag_present)  -- not represented in body
       , (dW_AT_low_pc,   dW_FORM_addr)
       , (dW_AT_high_pc,  dW_FORM_addr)
       ] ++
       (if haveDebugLine
        then [ (dW_AT_stmt_list, dW_FORM_data4) ]
        else [])) $$
     mkAbbrev DwAbbrSubprogram dW_TAG_subprogram dW_CHILDREN_yes
       subprogramAttrs $$
     mkAbbrev DwAbbrSubprogramWithParent dW_TAG_subprogram dW_CHILDREN_yes
       (subprogramAttrs ++ [(dW_AT_ghc_tick_parent, dW_FORM_ref_addr)]) $$
     mkAbbrev DwAbbrBlockWithoutCode dW_TAG_lexical_block dW_CHILDREN_yes
       [ (dW_AT_name, dW_FORM_string)
       ] $$
     mkAbbrev DwAbbrBlock dW_TAG_lexical_block dW_CHILDREN_yes
       [ (dW_AT_name, dW_FORM_string)
       , (dW_AT_low_pc, dW_FORM_addr)
       , (dW_AT_high_pc, dW_FORM_addr)
       ] $$
     mkAbbrev DwAbbrGhcSrcNote dW_TAG_ghc_src_note dW_CHILDREN_no
       [ (dW_AT_ghc_span_file, dW_FORM_string)
       , (dW_AT_ghc_span_start_line, dW_FORM_data4)
       , (dW_AT_ghc_span_start_col, dW_FORM_data2)
       , (dW_AT_ghc_span_end_line, dW_FORM_data4)
       , (dW_AT_ghc_span_end_col, dW_FORM_data2)
       ] $$
     pprByte 0

-- | Generate assembly for DWARF data
pprDwarfInfo :: Bool -> DwarfInfo -> SDoc
pprDwarfInfo haveSrc d
  = case d of
      DwarfCompileUnit {}  -> hasChildren
      DwarfSubprogram {}   -> hasChildren
      DwarfBlock {}        -> hasChildren
      DwarfSrcNote {}      -> noChildren
  where
    hasChildren =
        pprDwarfInfoOpen haveSrc d $$
        vcat (map (pprDwarfInfo haveSrc) (dwChildren d)) $$
        pprDwarfInfoClose
    noChildren = pprDwarfInfoOpen haveSrc d

-- | Prints assembler data corresponding to DWARF info records. Note
-- that the binary format of this is parameterized in @abbrevDecls@ and
-- has to be kept in synch.
pprDwarfInfoOpen :: Bool -> DwarfInfo -> SDoc
pprDwarfInfoOpen haveSrc (DwarfCompileUnit _ name producer compDir lowLabel
                                           highLabel lineLbl) =
  pprAbbrev DwAbbrCompileUnit
  $$ pprString name
  $$ pprString producer
  $$ pprData4 dW_LANG_Haskell
  $$ pprString compDir
  $$ pprWord (ppr lowLabel)
  $$ pprWord (ppr highLabel)
  $$ if haveSrc
     then sectionOffset (ptext lineLbl) (ptext dwarfLineLabel)
     else empty
pprDwarfInfoOpen _ (DwarfSubprogram _ name label
                                    parent) = sdocWithDynFlags $ \df ->
  ppr (mkAsmTempDieLabel label) <> colon
  $$ pprAbbrev abbrev
  $$ pprString name
  $$ pprString (renderWithStyle df (ppr label) (mkCodeStyle CStyle))
  $$ pprFlag (externallyVisibleCLabel label)
  $$ pprWord (ppr label)
  $$ pprWord (ppr $ mkAsmTempEndLabel label)
  $$ pprByte 1
  $$ pprByte dW_OP_call_frame_cfa
  $$ parentValue
  where
    abbrev = case parent of Nothing -> DwAbbrSubprogram
                            Just _  -> DwAbbrSubprogramWithParent
    parentValue = maybe empty pprParentDie parent
    pprParentDie sym = sectionOffset (ppr sym) (ptext dwarfInfoLabel)
pprDwarfInfoOpen _ (DwarfBlock _ label Nothing) = sdocWithDynFlags $ \df ->
  ppr (mkAsmTempDieLabel label) <> colon
  $$ pprAbbrev DwAbbrBlockWithoutCode
  $$ pprString (renderWithStyle df (ppr label) (mkCodeStyle CStyle))
pprDwarfInfoOpen _ (DwarfBlock _ label (Just marker)) = sdocWithDynFlags $ \df ->
  ppr (mkAsmTempDieLabel label) <> colon
  $$ pprAbbrev DwAbbrBlock
  $$ pprString (renderWithStyle df (ppr label) (mkCodeStyle CStyle))
  $$ pprWord (ppr marker)
  $$ pprWord (ppr $ mkAsmTempEndLabel marker)
pprDwarfInfoOpen _ (DwarfSrcNote ss) =
  pprAbbrev DwAbbrGhcSrcNote
  $$ pprString' (ftext $ srcSpanFile ss)
  $$ pprData4 (fromIntegral $ srcSpanStartLine ss)
  $$ pprHalf (fromIntegral $ srcSpanStartCol ss)
  $$ pprData4 (fromIntegral $ srcSpanEndLine ss)
  $$ pprHalf (fromIntegral $ srcSpanEndCol ss)

-- | Close a DWARF info record with children
pprDwarfInfoClose :: SDoc
pprDwarfInfoClose = pprAbbrev DwAbbrNull

-- | A DWARF address range. This is used by the debugger to quickly locate
-- which compilation unit a given address belongs to. This type assumes
-- a non-segmented address-space.
data DwarfARange
  = DwarfARange
    { dwArngStartLabel :: CLabel
    , dwArngEndLabel   :: CLabel
    }

-- | Print assembler directives corresponding to a DWARF @.debug_aranges@
-- address table entry.
pprDwarfARanges :: [DwarfARange] -> Unique -> SDoc
pprDwarfARanges arngs unitU = sdocWithPlatform $ \plat ->
  let wordSize = platformWordSize plat
      paddingSize = 4 :: Int
      -- header is 12 bytes long.
      -- entry is 8 bytes (32-bit platform) or 16 bytes (64-bit platform).
      -- pad such that first entry begins at multiple of entry size.
      pad n = vcat $ replicate n $ pprByte 0
      -- Fix for #17428
      initialLength = 8 + paddingSize + (1 + length arngs) * 2 * wordSize
  in pprDwWord (ppr initialLength)
     $$ pprHalf 2
     $$ sectionOffset (ppr $ mkAsmTempLabel $ unitU)
                      (ptext dwarfInfoLabel)
     $$ pprByte (fromIntegral wordSize)
     $$ pprByte 0
     $$ pad paddingSize
     -- body
     $$ vcat (map pprDwarfARange arngs)
     -- terminus
     $$ pprWord (char '0')
     $$ pprWord (char '0')

pprDwarfARange :: DwarfARange -> SDoc
pprDwarfARange arng = pprWord (ppr $ dwArngStartLabel arng) $$ pprWord length
  where
    length = ppr (dwArngEndLabel arng)
             <> char '-' <> ppr (dwArngStartLabel arng)

-- | Information about unwind instructions for a procedure. This
-- corresponds to a "Common Information Entry" (CIE) in DWARF.
data DwarfFrame
  = DwarfFrame
    { dwCieLabel :: CLabel
    , dwCieInit  :: UnwindTable
    , dwCieProcs :: [DwarfFrameProc]
    }

-- | Unwind instructions for an individual procedure. Corresponds to a
-- "Frame Description Entry" (FDE) in DWARF.
data DwarfFrameProc
  = DwarfFrameProc
    { dwFdeProc    :: CLabel
    , dwFdeHasInfo :: Bool
    , dwFdeBlocks  :: [DwarfFrameBlock]
      -- ^ List of blocks. Order must match asm!
    }

-- | Unwind instructions for a block. Will become part of the
-- containing FDE.
data DwarfFrameBlock
  = DwarfFrameBlock
    { dwFdeBlkHasInfo :: Bool
    , dwFdeUnwind     :: [UnwindPoint]
      -- ^ these unwind points must occur in the same order as they occur
      -- in the block
    }

instance Outputable DwarfFrameBlock where
  ppr (DwarfFrameBlock hasInfo unwinds) = braces $ ppr hasInfo <+> ppr unwinds

-- | Header for the @.debug_frame@ section. Here we emit the "Common
-- Information Entry" record that etablishes general call frame
-- parameters and the default stack layout.
pprDwarfFrame :: DwarfFrame -> SDoc
pprDwarfFrame DwarfFrame{dwCieLabel=cieLabel,dwCieInit=cieInit,dwCieProcs=procs}
  = sdocWithPlatform $ \plat ->
    let cieStartLabel= mkAsmTempDerivedLabel cieLabel (fsLit "_start")
        cieEndLabel = mkAsmTempEndLabel cieLabel
        length      = ppr cieEndLabel <> char '-' <> ppr cieStartLabel
        spReg       = dwarfGlobalRegNo plat Sp
        retReg      = dwarfReturnRegNo plat
        wordSize    = platformWordSize plat
        pprInit :: (GlobalReg, Maybe UnwindExpr) -> SDoc
        pprInit (g, uw) = pprSetUnwind plat g (Nothing, uw)

        -- Preserve C stack pointer: This necessary to override that default
        -- unwinding behavior of setting $sp = CFA.
        preserveSp = case platformArch plat of
          ArchX86    -> pprByte dW_CFA_same_value $$ pprLEBWord 4
          ArchX86_64 -> pprByte dW_CFA_same_value $$ pprLEBWord 7
          _          -> empty
    in vcat [ ppr cieLabel <> colon
            , pprData4' length -- Length of CIE
            , ppr cieStartLabel <> colon
            , pprData4' (text "-1")
                               -- Common Information Entry marker (-1 = 0xf..f)
            , pprByte 3        -- CIE version (we require DWARF 3)
            , pprByte 0        -- Augmentation (none)
            , pprByte 1        -- Code offset multiplicator
            , pprByte (128-fromIntegral wordSize)
                               -- Data offset multiplicator
                               -- (stacks grow down => "-w" in signed LEB128)
            , pprByte retReg   -- virtual register holding return address
            ] $$
       -- Initial unwind table
       vcat (map pprInit $ Map.toList cieInit) $$
       vcat [ -- RET = *CFA
              pprByte (dW_CFA_offset+retReg)
            , pprByte 0

              -- Preserve C stack pointer
            , preserveSp

              -- Sp' = CFA
              -- (we need to set this manually as our (STG) Sp register is
              -- often not the architecture's default stack register)
            , pprByte dW_CFA_val_offset
            , pprLEBWord (fromIntegral spReg)
            , pprLEBWord 0
            ] $$
       wordAlign $$
       ppr cieEndLabel <> colon $$
       -- Procedure unwind tables
       vcat (map (pprFrameProc cieLabel cieInit) procs)

-- | Writes a "Frame Description Entry" for a procedure. This consists
-- mainly of referencing the CIE and writing state machine
-- instructions to describe how the frame base (CFA) changes.
pprFrameProc :: CLabel -> UnwindTable -> DwarfFrameProc -> SDoc
pprFrameProc frameLbl initUw (DwarfFrameProc procLbl hasInfo blocks)
  = let fdeLabel    = mkAsmTempDerivedLabel procLbl (fsLit "_fde")
        fdeEndLabel = mkAsmTempDerivedLabel procLbl (fsLit "_fde_end")
        procEnd     = mkAsmTempEndLabel procLbl
        ifInfo str  = if hasInfo then text str else empty
                      -- see [Note: Info Offset]
    in vcat [ whenPprDebug $ text "# Unwinding for" <+> ppr procLbl <> colon
            , pprData4' (ppr fdeEndLabel <> char '-' <> ppr fdeLabel)
            , ppr fdeLabel <> colon
            , pprData4' (ppr frameLbl <> char '-' <>
                         ptext dwarfFrameLabel)    -- Reference to CIE
            , pprWord (ppr procLbl <> ifInfo "-1") -- Code pointer
            , pprWord (ppr procEnd <> char '-' <>
                       ppr procLbl <> ifInfo "+1") -- Block byte length
            ] $$
       vcat (S.evalState (mapM pprFrameBlock blocks) initUw) $$
       wordAlign $$
       ppr fdeEndLabel <> colon

-- | Generates unwind information for a block. We only generate
-- instructions where unwind information actually changes. This small
-- optimisations saves a lot of space, as subsequent blocks often have
-- the same unwind information.
pprFrameBlock :: DwarfFrameBlock -> S.State UnwindTable SDoc
pprFrameBlock (DwarfFrameBlock hasInfo uws0) =
    vcat <$> zipWithM pprFrameDecl (True : repeat False) uws0
  where
    pprFrameDecl :: Bool -> UnwindPoint -> S.State UnwindTable SDoc
    pprFrameDecl firstDecl (UnwindPoint lbl uws) = S.state $ \oldUws ->
        let -- Did a register's unwind expression change?
            isChanged :: GlobalReg -> Maybe UnwindExpr
                      -> Maybe (Maybe UnwindExpr, Maybe UnwindExpr)
            isChanged g new
                -- the value didn't change
              | Just new == old = Nothing
                -- the value was and still is undefined
              | Nothing <- old
              , Nothing <- new  = Nothing
                -- the value changed
              | otherwise       = Just (join old, new)
              where
                old = Map.lookup g oldUws

            changed = Map.toList $ Map.mapMaybeWithKey isChanged uws

        in if oldUws == uws
             then (empty, oldUws)
             else let -- see [Note: Info Offset]
                      needsOffset = firstDecl && hasInfo
                      lblDoc = ppr lbl <>
                               if needsOffset then text "-1" else empty
                      doc = sdocWithPlatform $ \plat ->
                           pprByte dW_CFA_set_loc $$ pprWord lblDoc $$
                           vcat (map (uncurry $ pprSetUnwind plat) changed)
                  in (doc, uws)

-- Note [Info Offset]
--
-- GDB was pretty much written with C-like programs in mind, and as a
-- result they assume that once you have a return address, it is a
-- good idea to look at (PC-1) to unwind further - as that's where the
-- "call" instruction is supposed to be.
--
-- Now on one hand, code generated by GHC looks nothing like what GDB
-- expects, and in fact going up from a return pointer is guaranteed
-- to land us inside an info table! On the other hand, that actually
-- gives us some wiggle room, as we expect IP to never *actually* end
-- up inside the info table, so we can "cheat" by putting whatever GDB
-- expects to see there. This is probably pretty safe, as GDB cannot
-- assume (PC-1) to be a valid code pointer in the first place - and I
-- have seen no code trying to correct this.
--
-- Note that this will not prevent GDB from failing to look-up the
-- correct function name for the frame, as that uses the symbol table,
-- which we can not manipulate as easily.
--
-- There's a GDB patch to address this at [1]. At the moment of writing
-- it's not merged, so I recommend building GDB with the patch if you
-- care about unwinding. The hack above doesn't cover every case.
--
-- [1] https://sourceware.org/ml/gdb-patches/2018-02/msg00055.html

-- | Get DWARF register ID for a given GlobalReg
dwarfGlobalRegNo :: Platform -> GlobalReg -> Word8
dwarfGlobalRegNo p UnwindReturnReg = dwarfReturnRegNo p
dwarfGlobalRegNo p reg = maybe 0 (dwarfRegNo p . RegReal) $ globalRegMaybe p reg

-- | Generate code for setting the unwind information for a register,
-- optimized using its known old value in the table. Note that "Sp" is
-- special: We see it as synonym for the CFA.
pprSetUnwind :: Platform
             -> GlobalReg
                -- ^ the register to produce an unwinding table entry for
             -> (Maybe UnwindExpr, Maybe UnwindExpr)
                -- ^ the old and new values of the register
             -> SDoc
pprSetUnwind plat g  (_, Nothing)
  = pprUndefUnwind plat g
pprSetUnwind _    Sp (Just (UwReg s _), Just (UwReg s' o')) | s == s'
  = if o' >= 0
    then pprByte dW_CFA_def_cfa_offset $$ pprLEBWord (fromIntegral o')
    else pprByte dW_CFA_def_cfa_offset_sf $$ pprLEBInt o'
pprSetUnwind plat Sp (_, Just (UwReg s' o'))
  = if o' >= 0
    then pprByte dW_CFA_def_cfa $$
         pprLEBRegNo plat s' $$
         pprLEBWord (fromIntegral o')
    else pprByte dW_CFA_def_cfa_sf $$
         pprLEBRegNo plat s' $$
         pprLEBInt o'
pprSetUnwind _    Sp (_, Just uw)
  = pprByte dW_CFA_def_cfa_expression $$ pprUnwindExpr False uw
pprSetUnwind plat g  (_, Just (UwDeref (UwReg Sp o)))
  | o < 0 && ((-o) `mod` platformWordSize plat) == 0 -- expected case
  = pprByte (dW_CFA_offset + dwarfGlobalRegNo plat g) $$
    pprLEBWord (fromIntegral ((-o) `div` platformWordSize plat))
  | otherwise
  = pprByte dW_CFA_offset_extended_sf $$
    pprLEBRegNo plat g $$
    pprLEBInt o
pprSetUnwind plat g  (_, Just (UwDeref uw))
  = pprByte dW_CFA_expression $$
    pprLEBRegNo plat g $$
    pprUnwindExpr True uw
pprSetUnwind plat g  (_, Just (UwReg g' 0))
  | g == g'
  = pprByte dW_CFA_same_value $$
    pprLEBRegNo plat g
pprSetUnwind plat g  (_, Just uw)
  = pprByte dW_CFA_val_expression $$
    pprLEBRegNo plat g $$
    pprUnwindExpr True uw

-- | Print the register number of the given 'GlobalReg' as an unsigned LEB128
-- encoded number.
pprLEBRegNo :: Platform -> GlobalReg -> SDoc
pprLEBRegNo plat = pprLEBWord . fromIntegral . dwarfGlobalRegNo plat

-- | Generates a DWARF expression for the given unwind expression. If
-- @spIsCFA@ is true, we see @Sp@ as the frame base CFA where it gets
-- mentioned.
pprUnwindExpr :: Bool -> UnwindExpr -> SDoc
pprUnwindExpr spIsCFA expr
  = sdocWithPlatform $ \plat ->
    let pprE (UwConst i)
          | i >= 0 && i < 32 = pprByte (dW_OP_lit0 + fromIntegral i)
          | otherwise        = pprByte dW_OP_consts $$ pprLEBInt i -- lazy...
        pprE (UwReg Sp i) | spIsCFA
                             = if i == 0
                               then pprByte dW_OP_call_frame_cfa
                               else pprE (UwPlus (UwReg Sp 0) (UwConst i))
        pprE (UwReg g i)      = pprByte (dW_OP_breg0+dwarfGlobalRegNo plat g) $$
                               pprLEBInt i
        pprE (UwDeref u)      = pprE u $$ pprByte dW_OP_deref
        pprE (UwLabel l)      = pprByte dW_OP_addr $$ pprWord (ppr l)
        pprE (UwPlus u1 u2)   = pprE u1 $$ pprE u2 $$ pprByte dW_OP_plus
        pprE (UwMinus u1 u2)  = pprE u1 $$ pprE u2 $$ pprByte dW_OP_minus
        pprE (UwTimes u1 u2)  = pprE u1 $$ pprE u2 $$ pprByte dW_OP_mul
    in text "\t.uleb128 2f-1f" $$ -- DW_FORM_block length
       -- computed as the difference of the following local labels 2: and 1:
       text "1:" $$
       pprE expr $$
       text "2:"

-- | Generate code for re-setting the unwind information for a
-- register to @undefined@
pprUndefUnwind :: Platform -> GlobalReg -> SDoc
pprUndefUnwind plat g  = pprByte dW_CFA_undefined $$
                         pprLEBRegNo plat g


-- | Align assembly at (machine) word boundary
wordAlign :: SDoc
wordAlign = sdocWithPlatform $ \plat ->
  text "\t.align " <> case platformOS plat of
    OSDarwin -> case platformWordSize plat of
      8      -> text "3"
      4      -> text "2"
      _other -> error "wordAlign: Unsupported word size!"
    _other   -> ppr (platformWordSize plat)

-- | Assembly for a single byte of constant DWARF data
pprByte :: Word8 -> SDoc
pprByte x = text "\t.byte " <> ppr (fromIntegral x :: Word)

-- | Assembly for a two-byte constant integer
pprHalf :: Word16 -> SDoc
pprHalf x = text "\t.short" <+> ppr (fromIntegral x :: Word)

-- | Assembly for a constant DWARF flag
pprFlag :: Bool -> SDoc
pprFlag f = pprByte (if f then 0xff else 0x00)

-- | Assembly for 4 bytes of dynamic DWARF data
pprData4' :: SDoc -> SDoc
pprData4' x = text "\t.long " <> x

-- | Assembly for 4 bytes of constant DWARF data
pprData4 :: Word -> SDoc
pprData4 = pprData4' . ppr

-- | Assembly for a DWARF word of dynamic data. This means 32 bit, as
-- we are generating 32 bit DWARF.
pprDwWord :: SDoc -> SDoc
pprDwWord = pprData4'

-- | Assembly for a machine word of dynamic data. Depends on the
-- architecture we are currently generating code for.
pprWord :: SDoc -> SDoc
pprWord s = (<> s) . sdocWithPlatform $ \plat ->
  case platformWordSize plat of
    4 -> text "\t.long "
    8 -> text "\t.quad "
    n -> panic $ "pprWord: Unsupported target platform word length " ++
                 show n ++ "!"

-- | Prints a number in "little endian base 128" format. The idea is
-- to optimize for small numbers by stopping once all further bytes
-- would be 0. The highest bit in every byte signals whether there
-- are further bytes to read.
pprLEBWord :: Word -> SDoc
pprLEBWord x | x < 128   = pprByte (fromIntegral x)
             | otherwise = pprByte (fromIntegral $ 128 .|. (x .&. 127)) $$
                           pprLEBWord (x `shiftR` 7)

-- | Same as @pprLEBWord@, but for a signed number
pprLEBInt :: Int -> SDoc
pprLEBInt x | x >= -64 && x < 64
                        = pprByte (fromIntegral (x .&. 127))
            | otherwise = pprByte (fromIntegral $ 128 .|. (x .&. 127)) $$
                          pprLEBInt (x `shiftR` 7)

-- | Generates a dynamic null-terminated string. If required the
-- caller needs to make sure that the string is escaped properly.
pprString' :: SDoc -> SDoc
pprString' str = text "\t.asciz \"" <> str <> char '"'

-- | Generate a string constant. We take care to escape the string.
pprString :: String -> SDoc
pprString str
  = pprString' $ hcat $ map escapeChar $
    if str `lengthIs` utf8EncodedLength str
    then str
    else map (chr . fromIntegral) $ bytesFS $ mkFastString str

-- | Escape a single non-unicode character
escapeChar :: Char -> SDoc
escapeChar '\\' = text "\\\\"
escapeChar '\"' = text "\\\""
escapeChar '\n' = text "\\n"
escapeChar c
  | isAscii c && isPrint c && c /= '?' -- prevents trigraph warnings
  = char c
  | otherwise
  = char '\\' <> char (intToDigit (ch `div` 64)) <>
                 char (intToDigit ((ch `div` 8) `mod` 8)) <>
                 char (intToDigit (ch `mod` 8))
  where ch = ord c

-- | Generate an offset into another section. This is tricky because
-- this is handled differently depending on platform: Mac Os expects
-- us to calculate the offset using assembler arithmetic. Linux expects
-- us to just reference the target directly, and will figure out on
-- their own that we actually need an offset. Finally, Windows has
-- a special directive to refer to relative offsets. Fun.
sectionOffset :: SDoc -> SDoc -> SDoc
sectionOffset target section = sdocWithPlatform $ \plat ->
  case platformOS plat of
    OSDarwin  -> pprDwWord (target <> char '-' <> section)
    OSMinGW32 -> text "\t.secrel32 " <> target
    _other    -> pprDwWord target
