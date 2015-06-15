module Dwarf.Types
  ( -- * Dwarf information
    DwarfInfo(..)
  , pprDwarfInfo
  , pprAbbrevDecls
    -- * Dwarf frame
  , DwarfFrame(..), DwarfFrameProc(..), DwarfFrameBlock(..)
  , pprDwarfFrame
    -- * Utilities
  , pprByte
  , pprData4'
  , pprDwWord
  , pprWord
  , pprLEBWord
  , pprLEBInt
  , wordAlign
  , sectionOffset
  )
  where

import Debug
import CLabel
import CmmExpr         ( GlobalReg(..) )
import Encoding
import FastString
import Outputable
import Platform
import Reg

import Dwarf.Constants

import Data.Bits
import Data.List ( mapAccumL )
import qualified Data.Map as Map
import Data.Word
import Data.Char

import CodeGen.Platform

-- | Individual dwarf records. Each one will be encoded as an entry in
-- the .debug_info section.
data DwarfInfo
  = DwarfCompileUnit { dwChildren :: [DwarfInfo]
                     , dwName :: String
                     , dwProducer :: String
                     , dwCompDir :: String
                     , dwLineLabel :: LitString }
  | DwarfSubprogram { dwChildren :: [DwarfInfo]
                    , dwName :: String
                    , dwLabel :: CLabel }
  | DwarfBlock { dwChildren :: [DwarfInfo]
               , dwLabel :: CLabel
               , dwMarker :: CLabel }

-- | Abbreviation codes used for encoding above records in the
-- .debug_info section.
data DwarfAbbrev
  = DwAbbrNull          -- ^ Pseudo, used for marking the end of lists
  | DwAbbrCompileUnit
  | DwAbbrSubprogram
  | DwAbbrBlock
  deriving (Eq, Enum)

-- | Generate assembly for the given abbreviation code
pprAbbrev :: DwarfAbbrev -> SDoc
pprAbbrev = pprLEBWord . fromIntegral . fromEnum

-- | Abbreviation declaration. This explains the binary encoding we
-- use for representing @DwarfInfo@.
pprAbbrevDecls :: Bool -> SDoc
pprAbbrevDecls haveDebugLine =
  let mkAbbrev abbr tag chld flds =
        let fld (tag, form) = pprLEBWord tag $$ pprLEBWord form
        in pprAbbrev abbr $$ pprLEBWord tag $$ pprByte chld $$
           vcat (map fld flds) $$ pprByte 0 $$ pprByte 0
  in dwarfAbbrevSection $$
     ptext dwarfAbbrevLabel <> colon $$
     mkAbbrev DwAbbrCompileUnit dW_TAG_compile_unit dW_CHILDREN_yes
       ([ (dW_AT_name, dW_FORM_string)
       , (dW_AT_producer, dW_FORM_string)
       , (dW_AT_language, dW_FORM_data4)
       , (dW_AT_comp_dir, dW_FORM_string)
       , (dW_AT_use_UTF8, dW_FORM_flag)
       ] ++
       (if haveDebugLine
        then [ (dW_AT_stmt_list, dW_FORM_data4) ]
        else [])) $$
     mkAbbrev DwAbbrSubprogram dW_TAG_subprogram dW_CHILDREN_yes
       [ (dW_AT_name, dW_FORM_string)
       , (dW_AT_MIPS_linkage_name, dW_FORM_string)
       , (dW_AT_external, dW_FORM_flag)
       , (dW_AT_low_pc, dW_FORM_addr)
       , (dW_AT_high_pc, dW_FORM_addr)
       , (dW_AT_frame_base, dW_FORM_block1)
       ] $$
     mkAbbrev DwAbbrBlock dW_TAG_lexical_block dW_CHILDREN_yes
       [ (dW_AT_name, dW_FORM_string)
       , (dW_AT_low_pc, dW_FORM_addr)
       , (dW_AT_high_pc, dW_FORM_addr)
       ] $$
     pprByte 0

-- | Generate assembly for DWARF data
pprDwarfInfo :: Bool -> DwarfInfo -> SDoc
pprDwarfInfo haveSrc d
  = pprDwarfInfoOpen haveSrc d $$
    vcat (map (pprDwarfInfo haveSrc) (dwChildren d)) $$
    pprDwarfInfoClose

-- | Prints assembler data corresponding to DWARF info records. Note
-- that the binary format of this is paramterized in @abbrevDecls@ and
-- has to be kept in synch.
pprDwarfInfoOpen :: Bool -> DwarfInfo -> SDoc
pprDwarfInfoOpen haveSrc (DwarfCompileUnit _ name producer compDir lineLbl) =
  pprAbbrev DwAbbrCompileUnit
  $$ pprString name
  $$ pprString producer
  $$ pprData4 dW_LANG_Haskell
  $$ pprString compDir
  $$ pprFlag True -- use UTF8
  $$ if haveSrc
     then sectionOffset lineLbl dwarfLineLabel
     else empty
pprDwarfInfoOpen _ (DwarfSubprogram _ name label) = sdocWithDynFlags $ \df ->
  pprAbbrev DwAbbrSubprogram
  $$ pprString name
  $$ pprString (renderWithStyle df (ppr label) (mkCodeStyle CStyle))
  $$ pprFlag (externallyVisibleCLabel label)
  $$ pprWord (ppr label)
  $$ pprWord (ppr $ mkAsmTempEndLabel label)
  $$ pprByte 1
  $$ pprByte dW_OP_call_frame_cfa
pprDwarfInfoOpen _ (DwarfBlock _ label marker) = sdocWithDynFlags $ \df ->
  pprAbbrev DwAbbrBlock
  $$ pprString (renderWithStyle df (ppr label) (mkCodeStyle CStyle))
  $$ pprWord (ppr marker)
  $$ pprWord (ppr $ mkAsmTempEndLabel marker)

-- | Close a DWARF info record with children
pprDwarfInfoClose :: SDoc
pprDwarfInfoClose = pprAbbrev DwAbbrNull

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
    { dwFdeBlock      :: CLabel
    , dwFdeBlkHasInfo :: Bool
    , dwFdeUnwind     :: UnwindTable
    }

-- | Header for the .debug_frame section. Here we emit the "Common
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
        pprInit (g, uw) = pprSetUnwind plat g (Nothing, uw)
    in vcat [ ppr cieLabel <> colon
            , pprData4' length -- Length of CIE
            , ppr cieStartLabel <> colon
            , pprData4' (ptext (sLit "-1"))
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

              -- Sp' = CFA
              -- (we need to set this manually as our Sp register is
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
    in vcat [ pprData4' (ppr fdeEndLabel <> char '-' <> ppr fdeLabel)
            , ppr fdeLabel <> colon
            , pprData4' (ppr frameLbl <> char '-' <>
                         ptext dwarfFrameLabel)    -- Reference to CIE
            , pprWord (ppr procLbl <> ifInfo "-1") -- Code pointer
            , pprWord (ppr procEnd <> char '-' <>
                       ppr procLbl <> ifInfo "+1") -- Block byte length
            ] $$
       vcat (snd $ mapAccumL pprFrameBlock initUw blocks) $$
       wordAlign $$
       ppr fdeEndLabel <> colon

-- | Generates unwind information for a block. We only generate
-- instructions where unwind information actually changes. This small
-- optimisations saves a lot of space, as subsequent blocks often have
-- the same unwind information.
pprFrameBlock :: UnwindTable -> DwarfFrameBlock -> (UnwindTable, SDoc)
pprFrameBlock oldUws (DwarfFrameBlock blockLbl hasInfo uws)
  | uws == oldUws
  = (oldUws, empty)
  | otherwise
  = (,) uws $ sdocWithPlatform $ \plat ->
    let lbl = ppr blockLbl <> if hasInfo then text "-1" else empty
              -- see [Note: Info Offset]
        isChanged g v | old == Just v  = Nothing
                      | otherwise      = Just (old, v)
                      where old = Map.lookup g oldUws
        changed = Map.toList $ Map.mapMaybeWithKey isChanged uws
        died    = Map.toList $ Map.difference oldUws uws
    in pprByte dW_CFA_set_loc $$ pprWord lbl $$
       vcat (map (uncurry $ pprSetUnwind plat) changed) $$
       vcat (map (pprUndefUnwind plat . fst) died)

-- [Note: Info Offset]
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

-- | Get DWARF register ID for a given GlobalReg
dwarfGlobalRegNo :: Platform -> GlobalReg -> Word8
dwarfGlobalRegNo p = maybe 0 (dwarfRegNo p . RegReal) . globalRegMaybe p

-- | Generate code for setting the unwind information for a register,
-- optimized using its known old value in the table. Note that "Sp" is
-- special: We see it as synonym for the CFA.
pprSetUnwind :: Platform -> GlobalReg -> (Maybe UnwindExpr, UnwindExpr) -> SDoc
pprSetUnwind _    Sp (Just (UwReg s _), UwReg s' o') | s == s'
  = if o' >= 0
    then pprByte dW_CFA_def_cfa_offset $$ pprLEBWord (fromIntegral o')
    else pprByte dW_CFA_def_cfa_offset_sf $$ pprLEBInt o'
pprSetUnwind plat Sp (_, UwReg s' o')
  = if o' >= 0
    then pprByte dW_CFA_def_cfa $$
         pprLEBWord (fromIntegral $ dwarfGlobalRegNo plat s') $$
         pprLEBWord (fromIntegral o')
    else pprByte dW_CFA_def_cfa_sf $$
         pprLEBWord (fromIntegral $ dwarfGlobalRegNo plat s') $$
         pprLEBInt o'
pprSetUnwind _    Sp (_, uw)
  = pprByte dW_CFA_def_cfa_expression $$ pprUnwindExpr False uw
pprSetUnwind plat g  (_, UwDeref (UwReg Sp o))
  | o < 0 && ((-o) `mod` platformWordSize plat) == 0 -- expected case
  = pprByte (dW_CFA_offset + dwarfGlobalRegNo plat g) $$
    pprLEBWord (fromIntegral ((-o) `div` platformWordSize plat))
  | otherwise
  = pprByte dW_CFA_offset_extended_sf $$
    pprLEBWord (fromIntegral (dwarfGlobalRegNo plat g)) $$
    pprLEBInt o
pprSetUnwind plat g  (_, UwDeref uw)
  = pprByte dW_CFA_expression $$
    pprLEBWord (fromIntegral (dwarfGlobalRegNo plat g)) $$
    pprUnwindExpr True uw
pprSetUnwind plat g  (_, uw)
  = pprByte dW_CFA_val_expression $$
    pprLEBWord (fromIntegral (dwarfGlobalRegNo plat g)) $$
    pprUnwindExpr True uw

-- | Generates a DWARF expression for the given unwind expression. If
-- @spIsCFA@ is true, we see @Sp@ as the frame base CFA where it gets
-- mentioned.
pprUnwindExpr :: Bool -> UnwindExpr -> SDoc
pprUnwindExpr spIsCFA expr
  = sdocWithPlatform $ \plat ->
    let ppr (UwConst i)
          | i >= 0 && i < 32 = pprByte (dW_OP_lit0 + fromIntegral i)
          | otherwise        = pprByte dW_OP_consts $$ pprLEBInt i -- lazy...
        ppr (UwReg Sp i) | spIsCFA
                             = if i == 0
                               then pprByte dW_OP_call_frame_cfa
                               else ppr (UwPlus (UwReg Sp 0) (UwConst i))
        ppr (UwReg g i)      = pprByte (dW_OP_breg0+dwarfGlobalRegNo plat g) $$
                               pprLEBInt i
        ppr (UwDeref u)      = ppr u $$ pprByte dW_OP_deref
        ppr (UwPlus u1 u2)   = ppr u1 $$ ppr u2 $$ pprByte dW_OP_plus
        ppr (UwMinus u1 u2)  = ppr u1 $$ ppr u2 $$ pprByte dW_OP_minus
        ppr (UwTimes u1 u2)  = ppr u1 $$ ppr u2 $$ pprByte dW_OP_mul
    in ptext (sLit "\t.byte 1f-.-1") $$
       ppr expr $$
       ptext (sLit "1:")

-- | Generate code for re-setting the unwind information for a
-- register to "undefined"
pprUndefUnwind :: Platform -> GlobalReg -> SDoc
pprUndefUnwind _    Sp = panic "pprUndefUnwind Sp" -- should never happen
pprUndefUnwind plat g  = pprByte dW_CFA_undefined $$
                         pprLEBWord (fromIntegral $ dwarfGlobalRegNo plat g)


-- | Align assembly at (machine) word boundary
wordAlign :: SDoc
wordAlign = sdocWithPlatform $ \plat ->
  ptext (sLit "\t.align ") <> case platformOS plat of
    OSDarwin -> case platformWordSize plat of
      8      -> text "3"
      4      -> text "2"
      _other -> error "wordAlign: Unsupported word size!"
    _other   -> ppr (platformWordSize plat)

-- | Assembly for a single byte of constant DWARF data
pprByte :: Word8 -> SDoc
pprByte x = ptext (sLit "\t.byte ") <> ppr (fromIntegral x :: Word)

-- | Assembly for a constant DWARF flag
pprFlag :: Bool -> SDoc
pprFlag f = pprByte (if f then 0xff else 0x00)

-- | Assembly for 4 bytes of dynamic DWARF data
pprData4' :: SDoc -> SDoc
pprData4' x = ptext (sLit "\t.long ") <> x

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
    4 -> ptext (sLit "\t.long ")
    8 -> ptext (sLit "\t.quad ")
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
pprString' str = ptext (sLit "\t.asciz \"") <> str <> char '"'

-- | Generate a string constant. We take care to escape the string.
pprString :: String -> SDoc
pprString str
  = pprString' $ hcat $ map escapeChar $
    if utf8EncodedLength str == length str
    then str
    else map (chr . fromIntegral) $ bytesFS $ mkFastString str

-- | Escape a single non-unicode character
escapeChar :: Char -> SDoc
escapeChar '\\' = ptext (sLit "\\\\")
escapeChar '\"' = ptext (sLit "\\\"")
escapeChar '\n' = ptext (sLit "\\n")
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
sectionOffset :: LitString -> LitString -> SDoc
sectionOffset target section = sdocWithPlatform $ \plat ->
  case platformOS plat of
    OSDarwin  -> pprDwWord (ptext target <> char '-' <> ptext section)
    OSMinGW32 -> text "\t.secrel32 " <> ptext target
    _other    -> pprDwWord (ptext target)
