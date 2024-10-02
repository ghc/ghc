{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module GHC.CmmToAsm.Dwarf.Types
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

import GHC.Prelude

import GHC.Cmm.DebugBlock
import GHC.Cmm.CLabel
import GHC.Cmm.Expr
import GHC.Utils.Encoding
import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Platform
import GHC.Types.Unique
import GHC.Platform.Reg
import GHC.Types.SrcLoc
import GHC.Utils.Misc

import GHC.CmmToAsm.Dwarf.Constants

import qualified Data.ByteString as BS
import qualified GHC.Utils.Monad.State.Strict as S
import Control.Monad (zipWithM, join)
import qualified Data.Map as Map
import Data.Word
import Data.Char

import GHC.Platform.Regs

-- | Individual dwarf records. Each one will be encoded as an entry in
-- the @.debug_info@ section.
data DwarfInfo
  = DwarfCompileUnit { dwChildren :: [DwarfInfo]
                     , dwName :: String
                     , dwProducer :: String
                     , dwCompDir :: String
                     , dwLowLabel :: CLabel
                     , dwHighLabel :: CLabel }
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
pprAbbrev :: IsDoc doc => DwarfAbbrev -> doc
pprAbbrev = pprLEBWord . fromIntegral . fromEnum

-- | Abbreviation declaration. This explains the binary encoding we
-- use for representing 'DwarfInfo'. Be aware that this must be updated
-- along with 'pprDwarfInfo'.
pprAbbrevDecls :: IsDoc doc => Platform -> Bool -> doc
pprAbbrevDecls platform haveDebugLine =
  let mkAbbrev abbr tag chld flds =
        let fld (tag, form) = pprLEBWord tag $$ pprLEBWord form
        in pprAbbrev abbr $$ pprLEBWord tag $$ pprByte chld $$
           vcat (map fld flds) $$ pprByte 0 $$ pprByte 0
      -- These are shared between DwAbbrSubprogram and
      -- DwAbbrSubprogramWithParent
      subprogramAttrs =
           [ (dW_AT_name, dW_FORM_string)
           , (dW_AT_linkage_name, dW_FORM_string)
           , (dW_AT_external, dW_FORM_flag)
           , (dW_AT_low_pc, dW_FORM_addr)
           , (dW_AT_high_pc, dW_FORM_addr)
           , (dW_AT_frame_base, dW_FORM_block1)
           ]
  in dwarfAbbrevSection platform $$
     line (dwarfAbbrevLabel <> colon) $$
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
{-# SPECIALIZE pprAbbrevDecls :: Platform -> Bool -> SDoc #-}
{-# SPECIALIZE pprAbbrevDecls :: Platform -> Bool -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Generate assembly for DWARF data
pprDwarfInfo :: IsDoc doc => Platform -> Bool -> DwarfInfo -> doc
pprDwarfInfo platform haveSrc d
  = case d of
      DwarfCompileUnit {dwChildren = kids} -> hasChildren kids
      DwarfSubprogram  {dwChildren = kids} -> hasChildren kids
      DwarfBlock       {dwChildren = kids} -> hasChildren kids
      DwarfSrcNote {}                      -> noChildren
  where
    hasChildren kids =
        pprDwarfInfoOpen platform haveSrc d $$
        vcat (map (pprDwarfInfo platform haveSrc) kids) $$
        pprDwarfInfoClose
    noChildren = pprDwarfInfoOpen platform haveSrc d
{-# SPECIALIZE pprDwarfInfo :: Platform -> Bool -> DwarfInfo -> SDoc #-}
{-# SPECIALIZE pprDwarfInfo :: Platform -> Bool -> DwarfInfo -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Print a CLabel name in a ".stringz \"LABEL\""
pprLabelString :: IsDoc doc => Platform -> CLabel -> doc
pprLabelString platform label =
   pprString'                  -- we don't need to escape the string as labels don't contain exotic characters
    $ pprCLabel platform label -- pretty-print as C label (foreign labels may be printed differently in Asm)

-- | Prints assembler data corresponding to DWARF info records. Note
-- that the binary format of this is parameterized in @abbrevDecls@ and
-- has to be kept in synch.
pprDwarfInfoOpen :: IsDoc doc => Platform -> Bool -> DwarfInfo -> doc
pprDwarfInfoOpen platform haveSrc (DwarfCompileUnit _ name producer compDir lowLabel
                                           highLabel) =
  pprAbbrev DwAbbrCompileUnit
  $$ pprString name
  $$ pprString producer
  $$ pprData4 dW_LANG_Haskell
  $$ pprString compDir
     -- Offset due to Note [Info Offset]
  $$ pprWord platform (pprAsmLabel platform lowLabel <> text "-1")
  $$ pprWord platform (pprAsmLabel platform highLabel)
  $$ if haveSrc
     then sectionOffset platform dwarfLineLabel dwarfLineLabel
     else empty
pprDwarfInfoOpen platform _ (DwarfSubprogram _ name label parent) =
  line (pprAsmLabel platform (mkAsmTempDieLabel label) <> colon)
  $$ pprAbbrev abbrev
  $$ pprString name
  $$ pprLabelString platform label
  $$ pprFlag (externallyVisibleCLabel label)
     -- Offset due to Note [Info Offset]
  $$ pprWord platform (pprAsmLabel platform label <> text "-1")
  $$ pprWord platform (pprAsmLabel platform $ mkAsmTempProcEndLabel label)
  $$ pprByte 1
  $$ pprByte dW_OP_call_frame_cfa
  $$ parentValue
  where
    abbrev = case parent of Nothing -> DwAbbrSubprogram
                            Just _  -> DwAbbrSubprogramWithParent
    parentValue = maybe empty pprParentDie parent
    pprParentDie sym = sectionOffset platform (pprAsmLabel platform sym) dwarfInfoLabel
pprDwarfInfoOpen platform _ (DwarfBlock _ label Nothing) =
  line (pprAsmLabel platform (mkAsmTempDieLabel label) <> colon)
  $$ pprAbbrev DwAbbrBlockWithoutCode
  $$ pprLabelString platform label
pprDwarfInfoOpen platform _ (DwarfBlock _ label (Just marker)) =
  line (pprAsmLabel platform (mkAsmTempDieLabel label) <> colon)
  $$ pprAbbrev DwAbbrBlock
  $$ pprLabelString platform label
  $$ pprWord platform (pprAsmLabel platform marker)
  $$ pprWord platform (pprAsmLabel platform $ mkAsmTempEndLabel marker)
pprDwarfInfoOpen _ _ (DwarfSrcNote ss) =
  pprAbbrev DwAbbrGhcSrcNote
  $$ pprString' (ftext $ srcSpanFile ss)
  $$ pprData4 (fromIntegral $ srcSpanStartLine ss)
  $$ pprHalf (fromIntegral $ srcSpanStartCol ss)
  $$ pprData4 (fromIntegral $ srcSpanEndLine ss)
  $$ pprHalf (fromIntegral $ srcSpanEndCol ss)

-- | Close a DWARF info record with children
pprDwarfInfoClose :: IsDoc doc => doc
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
pprDwarfARanges :: IsDoc doc => Platform -> [DwarfARange] -> Unique -> doc
pprDwarfARanges platform arngs unitU =
  let wordSize = platformWordSizeInBytes platform
      paddingSize = 4 :: Int
      -- header is 12 bytes long.
      -- entry is 8 bytes (32-bit platform) or 16 bytes (64-bit platform).
      -- pad such that first entry begins at multiple of entry size.
      pad n = vcat $ replicate n $ pprByte 0
      -- Fix for #17428
      initialLength = 8 + paddingSize + (1 + length arngs) * 2 * wordSize
  in pprDwWord (int initialLength)
     $$ pprHalf 2
     $$ sectionOffset platform (pprAsmLabel platform $ mkAsmTempLabel $ unitU) dwarfInfoLabel
     $$ pprByte (fromIntegral wordSize)
     $$ pprByte 0
     $$ pad paddingSize
     -- body
     $$ vcat (map (pprDwarfARange platform) arngs)
     -- terminus
     $$ pprWord platform (char '0')
     $$ pprWord platform (char '0')
{-# SPECIALIZE pprDwarfARanges :: Platform -> [DwarfARange] -> Unique -> SDoc #-}
{-# SPECIALIZE pprDwarfARanges :: Platform -> [DwarfARange] -> Unique -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

pprDwarfARange :: IsDoc doc => Platform -> DwarfARange -> doc
pprDwarfARange platform arng =
    -- Offset due to Note [Info Offset].
    pprWord platform (pprAsmLabel platform (dwArngStartLabel arng) <> text "-1")
    $$ pprWord platform length
  where
    length = pprAsmLabel platform (dwArngEndLabel arng)
             <> char '-' <> pprAsmLabel platform (dwArngStartLabel arng)

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

instance OutputableP Platform DwarfFrameBlock where
  pdoc env (DwarfFrameBlock hasInfo unwinds) = braces $ ppr hasInfo <+> pdoc env unwinds

-- | Header for the @.debug_frame@ section. Here we emit the "Common
-- Information Entry" record that establishes general call frame
-- parameters and the default stack layout.
pprDwarfFrame :: forall doc. IsDoc doc => Platform -> DwarfFrame -> doc
pprDwarfFrame platform DwarfFrame{dwCieLabel=cieLabel,dwCieInit=cieInit,dwCieProcs=procs}
  = let cieStartLabel= mkAsmTempDerivedLabel cieLabel (fsLit "_start")
        cieEndLabel = mkAsmTempEndLabel cieLabel
        length      = pprAsmLabel platform cieEndLabel <> char '-' <> pprAsmLabel platform cieStartLabel
        spReg       = dwarfGlobalRegNo platform Sp
        retReg      = dwarfReturnRegNo platform
        wordSize    = platformWordSizeInBytes platform
        pprInit :: (GlobalReg, Maybe UnwindExpr) -> doc
        pprInit (g, uw) = pprSetUnwind platform g (Nothing, uw)

        -- Preserve C stack pointer: This necessary to override that default
        -- unwinding behavior of setting $sp = CFA.
        preserveSp = case platformArch platform of
          ArchX86    -> pprByte dW_CFA_same_value $$ pprLEBWord 4
          ArchX86_64 -> pprByte dW_CFA_same_value $$ pprLEBWord 7
          _          -> empty
    in vcat [ line (pprAsmLabel platform cieLabel <> colon)
            , pprData4' length -- Length of CIE
            , line (pprAsmLabel platform cieStartLabel <> colon)
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
       wordAlign platform $$
       line (pprAsmLabel platform cieEndLabel <> colon) $$
       -- Procedure unwind tables
       vcat (map (pprFrameProc platform cieLabel cieInit) procs)
{-# SPECIALIZE pprDwarfFrame :: Platform -> DwarfFrame -> SDoc #-}
{-# SPECIALIZE pprDwarfFrame :: Platform -> DwarfFrame -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Writes a "Frame Description Entry" for a procedure. This consists
-- mainly of referencing the CIE and writing state machine
-- instructions to describe how the frame base (CFA) changes.
pprFrameProc :: IsDoc doc => Platform -> CLabel -> UnwindTable -> DwarfFrameProc -> doc
pprFrameProc platform frameLbl initUw (DwarfFrameProc procLbl hasInfo blocks)
  = let fdeLabel    = mkAsmTempDerivedLabel procLbl (fsLit "_fde")
        fdeEndLabel = mkAsmTempDerivedLabel procLbl (fsLit "_fde_end")
        procEnd     = mkAsmTempProcEndLabel procLbl
        ifInfo str  = if hasInfo then text str else empty
                      -- see Note [Info Offset]
    in vcat [ whenPprDebug $ line $ text "# Unwinding for" <+> pprAsmLabel platform procLbl <> colon
            , pprData4' (pprAsmLabel platform fdeEndLabel <> char '-' <> pprAsmLabel platform fdeLabel)
            , line (pprAsmLabel platform fdeLabel <> colon)
            , pprData4' (pprAsmLabel platform frameLbl <> char '-' <> dwarfFrameLabel)    -- Reference to CIE
            , pprWord platform (pprAsmLabel platform procLbl <> ifInfo "-1") -- Code pointer
            , pprWord platform (pprAsmLabel platform procEnd <> char '-' <>
                                 pprAsmLabel platform procLbl <> ifInfo "+1") -- Block byte length
            ] $$
       vcat (S.evalState (mapM (pprFrameBlock platform) blocks) initUw) $$
       wordAlign platform $$
       line (pprAsmLabel platform fdeEndLabel <> colon)

-- | Generates unwind information for a block. We only generate
-- instructions where unwind information actually changes. This small
-- optimisations saves a lot of space, as subsequent blocks often have
-- the same unwind information.
pprFrameBlock :: forall doc. IsDoc doc => Platform -> DwarfFrameBlock -> S.State UnwindTable doc
pprFrameBlock platform (DwarfFrameBlock hasInfo uws0) =
    vcat <$> zipWithM pprFrameDecl (True : repeat False) uws0
  where
    pprFrameDecl :: Bool -> UnwindPoint -> S.State UnwindTable doc
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
             else let -- see Note [Info Offset]
                      needsOffset = firstDecl && hasInfo
                      lblDoc = pprAsmLabel platform lbl <>
                               if needsOffset then text "-1" else empty
                      doc = pprByte dW_CFA_set_loc $$ pprWord platform lblDoc $$
                            vcat (map (uncurry $ pprSetUnwind platform) changed)
                  in (doc, uws)

-- Note [Info Offset]
-- ~~~~~~~~~~~~~~~~~~
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
-- We apply this offset in several places:
--
--  * unwind information in .debug_frames
--  * the subprogram and lexical_block DIEs in .debug_info
--  * the ranges in .debug_aranges
--
-- In the latter two cases we apply the offset unconditionally.
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
pprSetUnwind :: IsDoc doc => Platform
             -> GlobalReg
                -- ^ the register to produce an unwinding table entry for
             -> (Maybe UnwindExpr, Maybe UnwindExpr)
                -- ^ the old and new values of the register
             -> doc
pprSetUnwind plat g  (_, Nothing)
  = pprUndefUnwind plat g
pprSetUnwind _    Sp (Just (UwReg s _), Just (UwReg s' o')) | s == s'
  = if o' >= 0
    then pprByte dW_CFA_def_cfa_offset $$ pprLEBWord (fromIntegral o')
    else pprByte dW_CFA_def_cfa_offset_sf $$ pprLEBInt o'
pprSetUnwind plat Sp (_, Just (UwReg (GlobalRegUse s' _) o'))
  = if o' >= 0
    then pprByte dW_CFA_def_cfa $$
         pprLEBRegNo plat s' $$
         pprLEBWord (fromIntegral o')
    else pprByte dW_CFA_def_cfa_sf $$
         pprLEBRegNo plat s' $$
         pprLEBInt o'
pprSetUnwind plat Sp (_, Just uw)
  = pprByte dW_CFA_def_cfa_expression $$ pprUnwindExpr plat False uw
pprSetUnwind plat g  (_, Just (UwDeref (UwReg (GlobalRegUse Sp _) o)))
  | o < 0 && ((-o) `mod` platformWordSizeInBytes plat) == 0 -- expected case
  = pprByte (dW_CFA_offset + dwarfGlobalRegNo plat g) $$
    pprLEBWord (fromIntegral ((-o) `div` platformWordSizeInBytes plat))
  | otherwise
  = pprByte dW_CFA_offset_extended_sf $$
    pprLEBRegNo plat g $$
    pprLEBInt o
pprSetUnwind plat g  (_, Just (UwDeref uw))
  = pprByte dW_CFA_expression $$
    pprLEBRegNo plat g $$
    pprUnwindExpr plat True uw
pprSetUnwind plat g  (_, Just (UwReg (GlobalRegUse g' _) 0))
  | g == g'
  = pprByte dW_CFA_same_value $$
    pprLEBRegNo plat g
pprSetUnwind plat g  (_, Just uw)
  = pprByte dW_CFA_val_expression $$
    pprLEBRegNo plat g $$
    pprUnwindExpr plat True uw

-- | Print the register number of the given 'GlobalReg' as an unsigned LEB128
-- encoded number.
pprLEBRegNo :: IsDoc doc => Platform -> GlobalReg -> doc
pprLEBRegNo plat = pprLEBWord . fromIntegral . dwarfGlobalRegNo plat

-- | Generates a DWARF expression for the given unwind expression. If
-- @spIsCFA@ is true, we see @Sp@ as the frame base CFA where it gets
-- mentioned.
pprUnwindExpr :: IsDoc doc => Platform -> Bool -> UnwindExpr -> doc
pprUnwindExpr platform spIsCFA expr
  = let pprE (UwConst i)
          | i >= 0 && i < 32 = pprByte (dW_OP_lit0 + fromIntegral i)
          | otherwise        = pprByte dW_OP_consts $$ pprLEBInt i -- lazy...
        pprE (UwReg r@(GlobalRegUse Sp _) i)
          | spIsCFA
                              = if i == 0
                                then pprByte dW_OP_call_frame_cfa
                                else pprE (UwPlus (UwReg r 0) (UwConst i))
        pprE (UwReg (GlobalRegUse g _) i)
                              = pprByte (dW_OP_breg0+dwarfGlobalRegNo platform g) $$
                                pprLEBInt i
        pprE (UwDeref u)      = pprE u $$ pprByte dW_OP_deref
        pprE (UwLabel l)      = pprByte dW_OP_addr $$ pprWord platform (pprAsmLabel platform l)
        pprE (UwPlus u1 u2)   = pprE u1 $$ pprE u2 $$ pprByte dW_OP_plus
        pprE (UwMinus u1 u2)  = pprE u1 $$ pprE u2 $$ pprByte dW_OP_minus
        pprE (UwTimes u1 u2)  = pprE u1 $$ pprE u2 $$ pprByte dW_OP_mul
    in line (text "\t.uleb128 2f-1f") $$ -- DW_FORM_block length
       -- computed as the difference of the following local labels 2: and 1:
       line (text "1:") $$
       pprE expr $$
       line (text "2:")

-- | Generate code for re-setting the unwind information for a
-- register to @undefined@
pprUndefUnwind :: IsDoc doc => Platform -> GlobalReg -> doc
pprUndefUnwind plat g  = pprByte dW_CFA_undefined $$
                         pprLEBRegNo plat g


-- | Align assembly at (machine) word boundary
wordAlign :: IsDoc doc => Platform -> doc
wordAlign plat =
  line $ text "\t.align " <> case platformOS plat of
    OSDarwin -> case platformWordSize plat of
      PW8 -> char '3'
      PW4 -> char '2'
    _other   -> int (platformWordSizeInBytes plat)
{-# SPECIALIZE wordAlign :: Platform -> SDoc #-}
{-# SPECIALIZE wordAlign :: Platform -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Assembly for a single byte of constant DWARF data
pprByte :: IsDoc doc => Word8 -> doc
pprByte x = line $ text "\t.byte " <> integer (fromIntegral x)
{-# SPECIALIZE pprByte :: Word8 -> SDoc #-}
{-# SPECIALIZE pprByte :: Word8 -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Assembly for a two-byte constant integer
pprHalf :: IsDoc doc => Word16 -> doc
pprHalf x = line $ text "\t.short" <+> integer (fromIntegral x)
{-# SPECIALIZE pprHalf :: Word16 -> SDoc #-}
{-# SPECIALIZE pprHalf :: Word16 -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Assembly for a constant DWARF flag
pprFlag :: IsDoc doc => Bool -> doc
pprFlag f = pprByte (if f then 0xff else 0x00)

-- | Assembly for 4 bytes of dynamic DWARF data
pprData4' :: IsDoc doc => Line doc -> doc
pprData4' x = line (text "\t.long " <> x)
{-# SPECIALIZE pprData4' :: SDoc -> SDoc #-}
{-# SPECIALIZE pprData4' :: HLine -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Assembly for 4 bytes of constant DWARF data
pprData4 :: IsDoc doc => Word -> doc
pprData4 = pprData4' . integer . fromIntegral

-- | Assembly for a DWARF word of dynamic data. This means 32 bit, as
-- we are generating 32 bit DWARF.
pprDwWord :: IsDoc doc => Line doc -> doc
pprDwWord = pprData4'
{-# SPECIALIZE pprDwWord :: SDoc -> SDoc #-}
{-# SPECIALIZE pprDwWord :: HLine -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Assembly for a machine word of dynamic data. Depends on the
-- architecture we are currently generating code for.
pprWord :: IsDoc doc => Platform -> Line doc -> doc
pprWord plat s =
  line $ case platformWordSize plat of
    PW4 -> text "\t.long " <> s
    PW8 -> text "\t.quad " <> s
{-# SPECIALIZE pprWord :: Platform -> SDoc -> SDoc #-}
{-# SPECIALIZE pprWord :: Platform -> HLine -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Prints a number in "little endian base 128" format. The idea is
-- to optimize for small numbers by stopping once all further bytes
-- would be 0. The highest bit in every byte signals whether there
-- are further bytes to read.
pprLEBWord :: IsDoc doc => Word -> doc
pprLEBWord x | x < 128   = pprByte (fromIntegral x)
             | otherwise = pprByte (fromIntegral $ 128 .|. (x .&. 127)) $$
                           pprLEBWord (x `shiftR` 7)
{-# SPECIALIZE pprLEBWord :: Word -> SDoc #-}
{-# SPECIALIZE pprLEBWord :: Word -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Same as @pprLEBWord@, but for a signed number
pprLEBInt :: IsDoc doc => Int -> doc
pprLEBInt x | x >= -64 && x < 64
                        = pprByte (fromIntegral (x .&. 127))
            | otherwise = pprByte (fromIntegral $ 128 .|. (x .&. 127)) $$
                          pprLEBInt (x `shiftR` 7)
{-# SPECIALIZE pprLEBInt :: Int -> SDoc #-}
{-# SPECIALIZE pprLEBInt :: Int -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Generates a dynamic null-terminated string. If required the
-- caller needs to make sure that the string is escaped properly.
pprString' :: IsDoc doc => Line doc -> doc
pprString' str = line (text "\t.asciz \"" <> str <> char '"')

-- | Generate a string constant. We take care to escape the string.
pprString :: IsDoc doc => String -> doc
pprString str
  = pprString' $ hcat $ map escapeChar $
    if str `lengthIs` utf8EncodedLength str
    then str
    else map (chr . fromIntegral) $ BS.unpack $ utf8EncodeByteString str

-- | Escape a single non-unicode character
escapeChar :: IsLine doc => Char -> doc
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
sectionOffset :: IsDoc doc => Platform -> Line doc -> Line doc -> doc
sectionOffset plat target section =
  case platformOS plat of
    OSDarwin  -> pprDwWord (target <> char '-' <> section)
    OSMinGW32 -> line (text "\t.secrel32 " <> target)
    _other    -> pprDwWord target
{-# SPECIALIZE sectionOffset :: Platform -> SDoc -> SDoc -> SDoc #-}
{-# SPECIALIZE sectionOffset :: Platform -> HLine -> HLine -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable
