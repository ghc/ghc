-- | Constants describing the DWARF format. Most of this simply
-- mirrors /usr/include/dwarf.h.

module Dwarf.Constants where

import FastString
import Platform
import Outputable

import Data.Word

-- | Language ID used for Haskell.
dW_LANG_Haskell :: Word
dW_LANG_Haskell = 0x18
  -- Thanks to Nathan Howell for getting us our very own language ID!

-- | Dwarf tags
dW_TAG_compile_unit, dW_TAG_subroutine_type,
  dW_TAG_file_type, dW_TAG_subprogram, dW_TAG_lexical_block,
  dW_TAG_base_type, dW_TAG_structure_type, dW_TAG_pointer_type,
  dW_TAG_array_type, dW_TAG_subrange_type, dW_TAG_typedef,
  dW_TAG_variable, dW_TAG_arg_variable, dW_TAG_auto_variable :: Word
dW_TAG_array_type      = 1
dW_TAG_lexical_block   = 11
dW_TAG_pointer_type    = 15
dW_TAG_compile_unit    = 17
dW_TAG_structure_type  = 19
dW_TAG_typedef         = 22
dW_TAG_subroutine_type = 32
dW_TAG_subrange_type   = 33
dW_TAG_base_type       = 36
dW_TAG_file_type       = 41
dW_TAG_subprogram      = 46
dW_TAG_variable        = 52
dW_TAG_auto_variable   = 256
dW_TAG_arg_variable    = 257

-- | Dwarf attributes
dW_AT_name, dW_AT_stmt_list, dW_AT_low_pc, dW_AT_high_pc, dW_AT_language,
  dW_AT_comp_dir, dW_AT_producer, dW_AT_external, dW_AT_frame_base,
  dW_AT_MIPS_linkage_name :: Word
dW_AT_name              = 0x03
dW_AT_stmt_list         = 0x10
dW_AT_low_pc            = 0x11
dW_AT_high_pc           = 0x12
dW_AT_language          = 0x13
dW_AT_comp_dir          = 0x1b
dW_AT_producer          = 0x25
dW_AT_external          = 0x3f
dW_AT_frame_base        = 0x40
dW_AT_MIPS_linkage_name = 0x2007

-- | Abbrev declaration
dW_CHILDREN_no, dW_CHILDREN_yes :: Word8
dW_CHILDREN_no  = 0
dW_CHILDREN_yes = 1

dW_FORM_addr, dW_FORM_data4, dW_FORM_string, dW_FORM_flag,
  dW_FORM_block1, dW_FORM_ref4 :: Word
dW_FORM_addr   = 0x01
dW_FORM_data4  = 0x06
dW_FORM_string = 0x08
dW_FORM_flag   = 0x0c
dW_FORM_block1 = 0x0a
dW_FORM_ref4   = 0x13

-- | Dwarf native types
dW_ATE_address, dW_ATE_boolean, dW_ATE_float, dW_ATE_signed,
  dW_ATE_signed_char, dW_ATE_unsigned, dW_ATE_unsigned_char :: Word
dW_ATE_address       = 1
dW_ATE_boolean       = 2
dW_ATE_float         = 4
dW_ATE_signed        = 5
dW_ATE_signed_char   = 6
dW_ATE_unsigned      = 7
dW_ATE_unsigned_char = 8

-- | Call frame information
dW_CFA_set_loc, dW_CFA_undefined, dW_CFA_same_value,
  dW_CFA_def_cfa, dW_CFA_def_cfa_offset, dW_CFA_def_cfa_expression,
  dW_CFA_expression, dW_CFA_offset_extended_sf, dW_CFA_def_cfa_offset_sf,
  dW_CFA_def_cfa_sf, dW_CFA_val_offset, dW_CFA_val_expression,
  dW_CFA_offset :: Word8
dW_CFA_set_loc            = 0x01
dW_CFA_undefined          = 0x07
dW_CFA_same_value         = 0x08
dW_CFA_def_cfa            = 0x0c
dW_CFA_def_cfa_offset     = 0x0e
dW_CFA_def_cfa_expression = 0x0f
dW_CFA_expression         = 0x10
dW_CFA_offset_extended_sf = 0x11
dW_CFA_def_cfa_sf         = 0x12
dW_CFA_def_cfa_offset_sf  = 0x13
dW_CFA_val_offset         = 0x14
dW_CFA_val_expression     = 0x16
dW_CFA_offset             = 0x80

-- | Operations
dW_OP_deref, dW_OP_consts,
  dW_OP_minus, dW_OP_mul, dW_OP_plus,
  dW_OP_lit0, dW_OP_breg0, dW_OP_call_frame_cfa :: Word8
dW_OP_deref          = 0x06
dW_OP_consts         = 0x11
dW_OP_minus          = 0x1c
dW_OP_mul            = 0x1e
dW_OP_plus           = 0x22
dW_OP_lit0           = 0x30
dW_OP_breg0          = 0x70
dW_OP_call_frame_cfa = 0x9c

-- | Dwarf section declarations
dwarfInfoSection, dwarfAbbrevSection, dwarfLineSection,
  dwarfFrameSection, dwarfGhcSection :: SDoc
dwarfInfoSection   = dwarfSection "info"
dwarfAbbrevSection = dwarfSection "abbrev"
dwarfLineSection   = dwarfSection "line"
dwarfFrameSection  = dwarfSection "frame"
dwarfGhcSection    = dwarfSection "ghc"

dwarfSection :: String -> SDoc
dwarfSection name = sdocWithPlatform $ \plat ->
  case platformOS plat of
    OSDarwin -> ftext $ mkFastString $
                  ".section __DWARF,__debug_" ++ name ++ ",regular,debug"
    _other   -> ftext $ mkFastString $
                  ".section .debug_" ++ name ++ ",\"\",@progbits"

-- | Dwarf section labels
dwarfInfoLabel, dwarfAbbrevLabel, dwarfLineLabel :: LitString
dwarfInfoLabel   = sLit ".Lsection_info"
dwarfAbbrevLabel = sLit ".Lsection_abbrev"
dwarfLineLabel   = sLit ".Lsection_line"
