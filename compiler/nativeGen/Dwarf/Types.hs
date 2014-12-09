module Dwarf.Types
  ( DwarfInfo(..)
  , pprDwarfInfo
  , pprAbbrevDecls
  , pprByte
  , pprWord
  , pprDwWord
  , pprLEBWord
  , pprLEBInt
  )
  where

import CLabel
import FastString
import Outputable
import Platform

import Dwarf.Constants

import Data.Bits
import Data.Word
import Data.Char

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
       ] $$
     mkAbbrev DwAbbrBlock dW_TAG_lexical_block dW_CHILDREN_yes
       [ (dW_AT_name, dW_FORM_string)
       , (dW_AT_low_pc, dW_FORM_addr)
       , (dW_AT_high_pc, dW_FORM_addr)
       ]
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
  $$ if haveSrc
     then pprData4' (ptext lineLbl <> char '-' <> ptext dwarfLineLabel)
     else empty
pprDwarfInfoOpen _ (DwarfSubprogram _ name label) = sdocWithDynFlags $ \df ->
  pprAbbrev DwAbbrSubprogram
  $$ pprString name
  $$ pprString (renderWithStyle df (ppr label) (mkCodeStyle CStyle))
  $$ pprFlag (externallyVisibleCLabel label)
  $$ pprWord (ppr label)
  $$ pprWord (ppr $ mkAsmTempEndLabel label)
pprDwarfInfoOpen _ (DwarfBlock _ label marker) = sdocWithDynFlags $ \df ->
  pprAbbrev DwAbbrBlock
  $$ pprString (renderWithStyle df (ppr label) (mkCodeStyle CStyle))
  $$ pprWord (ppr marker)
  $$ pprWord (ppr $ mkAsmTempEndLabel marker)

-- | Close a DWARF info record with children
pprDwarfInfoClose :: SDoc
pprDwarfInfoClose = pprAbbrev DwAbbrNull

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
pprString = pprString' . hcat . map escape
  where escape '\\' = ptext (sLit "\\\\")
        escape '\"' = ptext (sLit "\\\"")
        escape '\n' = ptext (sLit "\\n")
        escape c    | isAscii c && isPrint c && c /= '?'
                      -- escaping '?' prevents trigraph warnings
                    = char c
                    | otherwise
                    = let ch = ord c
                      in char '\\' <>
                         char (intToDigit (ch `div` 64)) <>
                         char (intToDigit ((ch `div` 8) `mod` 8)) <>
                         char (intToDigit (ch `mod` 8))
