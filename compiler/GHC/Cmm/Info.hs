
module GHC.Cmm.Info (
  mkEmptyContInfoTable,
  cmmToRawCmm,
  srtEscape,

  -- info table accessors
  closureInfoPtr,
  entryCode,
  getConstrTag,
  cmmGetClosureType,
  infoTable,
  infoTableConstrTag,
  infoTableSrtBitmap,
  infoTableClosureType,
  infoTablePtrs,
  infoTableNonPtrs,
  funInfoTable,
  funInfoArity,

  -- info table sizes and offsets
  stdInfoTableSizeW,
  fixedInfoTableSizeW,
  profInfoTableSizeW,
  maxStdInfoTableSizeW,
  maxRetInfoTableSizeW,
  stdInfoTableSizeB,
  conInfoTableSizeB,
  stdSrtBitmapOffset,
  stdClosureTypeOffset,
  stdPtrsOffset, stdNonPtrsOffset,
) where

import GHC.Prelude

import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.CLabel
import GHC.StgToCmm.CgUtils (CgStream)
import GHC.Runtime.Heap.Layout
import GHC.Data.Bitmap
import qualified GHC.Data.Stream as Stream
import GHC.Cmm.Dataflow.Label

import GHC.Platform
import GHC.Platform.Profile
import GHC.Data.Maybe
import GHC.Utils.Error (withTimingSilent)
import GHC.Utils.Panic
import GHC.Utils.Logger
import GHC.Utils.Monad
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Types.Unique.DSM

import Data.ByteString (ByteString)

-- When we split at proc points, we need an empty info table.
mkEmptyContInfoTable :: CLabel -> CmmInfoTable
mkEmptyContInfoTable info_lbl
  = CmmInfoTable { cit_lbl  = info_lbl
                 , cit_rep  = mkStackRep []
                 , cit_prof = NoProfilingInfo
                 , cit_srt  = Nothing
                 , cit_clo  = Nothing }

cmmToRawCmm :: Logger -> Profile -> CgStream CmmGroupSRTs a
            -> IO (CgStream RawCmmGroup a)
cmmToRawCmm logger profile cmms
  = do { let do_one :: [CmmDeclSRTs] -> UniqDSMT IO [RawCmmDecl]
             do_one cmm = setTagUDSMT 'i' $ do
               -- NB. strictness fixes a space leak.  DO NOT REMOVE.
               withTimingSilent logger (text "Cmm -> Raw Cmm") (\x -> seqList x ()) $ do
                 liftUniqDSM $
                   concatMapM (mkInfoTable profile) cmm
       ; return (Stream.mapM do_one cmms)
       }


-- Make a concrete info table, represented as a list of CmmStatic
-- (it can't be simply a list of Word, because the SRT field is
-- represented by a label+offset expression).
--
-- With tablesNextToCode, the layout is
--      <reversed variable part>
--      <normal forward StgInfoTable, but without
--              an entry point at the front>
--      <code>
--
-- Without tablesNextToCode, the layout of an info table is
--      <entry label>
--      <normal forward rest of StgInfoTable>
--      <forward variable part>
--
--      See rts/include/rts/storage/InfoTables.h
--
-- For return-points these are as follows
--
-- Tables next to code:
--
--                      <srt slot>
--                      <standard info table>
--      ret-addr -->    <entry code (if any)>
--
-- Not tables-next-to-code:
--
--      ret-addr -->    <ptr to entry code>
--                      <standard info table>
--                      <srt slot>
--
--  * The SRT slot is only there if there is SRT info to record

mkInfoTable :: Profile -> CmmDeclSRTs -> UniqDSM [RawCmmDecl]
mkInfoTable _ (CmmData sec dat) = return [CmmData sec dat]

mkInfoTable profile proc@(CmmProc infos entry_lbl live blocks)
  --
  -- in the non-tables-next-to-code case, procs can have at most a
  -- single info table associated with the entry label of the proc.
  --
  | not (platformTablesNextToCode platform)
  = case topInfoTable proc of   --  must be at most one
      -- no info table
      Nothing ->
         return [CmmProc mapEmpty entry_lbl live blocks]

      Just info@CmmInfoTable { cit_lbl = info_lbl } -> do
        (top_decls, (std_info, extra_bits)) <-
             mkInfoTableContents profile info Nothing
        let
          rel_std_info   = map (makeRelativeRefTo platform info_lbl) std_info
          rel_extra_bits = map (makeRelativeRefTo platform info_lbl) extra_bits
        --
        -- Separately emit info table (with the function entry
        -- point as first entry) and the entry code
        --
        return (top_decls ++
                [CmmProc mapEmpty entry_lbl live blocks,
                 mkRODataLits info_lbl
                    (CmmLabel entry_lbl : rel_std_info ++ rel_extra_bits)])

  --
  -- With tables-next-to-code, we can have many info tables,
  -- associated with some of the BlockIds of the proc.  For each info
  -- table we need to turn it into CmmStatics, and collect any new
  -- CmmDecls that arise from doing so.
  --
  | otherwise
  = do
    (top_declss, raw_infos) <-
       unzip `fmap` mapM do_one_info (mapToList (info_tbls infos))
    return (concat top_declss ++
            [CmmProc (mapFromList raw_infos) entry_lbl live blocks])

  where
   platform = profilePlatform profile
   do_one_info (lbl,itbl) = do
     (top_decls, (std_info, extra_bits)) <-
         mkInfoTableContents profile itbl Nothing
     let
        info_lbl = cit_lbl itbl
        rel_std_info   = map (makeRelativeRefTo platform info_lbl) std_info
        rel_extra_bits = map (makeRelativeRefTo platform info_lbl) extra_bits
     --
     return (top_decls, (lbl, CmmStaticsRaw info_lbl $ map CmmStaticLit $
                              reverse rel_extra_bits ++ rel_std_info))

-----------------------------------------------------
type InfoTableContents = ( [CmmLit]          -- The standard part
                         , [CmmLit] )        -- The "extra bits"
-- These Lits have *not* had mkRelativeTo applied to them

mkInfoTableContents :: Profile
                    -> CmmInfoTable
                    -> Maybe Int               -- Override default RTS type tag?
                    -> UniqDSM ([RawCmmDecl],             -- Auxiliary top decls
                               InfoTableContents)       -- Info tbl + extra bits

mkInfoTableContents profile
                    info@(CmmInfoTable { cit_lbl  = info_lbl
                                       , cit_rep  = smrep
                                       , cit_prof = prof
                                       , cit_srt = srt })
                    mb_rts_tag
  | RTSRep rts_tag rep <- smrep
  = mkInfoTableContents profile info{cit_rep = rep} (Just rts_tag)
    -- Completely override the rts_tag that mkInfoTableContents would
    -- otherwise compute, with the rts_tag stored in the RTSRep
    -- (which in turn came from a handwritten .cmm file)

  | StackRep frame <- smrep
  = do { (prof_lits, prof_data) <- mkProfLits platform prof
       ; let (srt_label, srt_bitmap) = mkSRTLit platform info_lbl srt
       ; (liveness_lit, liveness_data) <- mkLivenessBits platform frame
       ; let
             std_info = mkStdInfoTable profile prof_lits rts_tag srt_bitmap liveness_lit
             rts_tag | Just tag <- mb_rts_tag = tag
                     | null liveness_data     = rET_SMALL -- Fits in extra_bits
                     | otherwise              = rET_BIG   -- Does not; extra_bits is
                                                          -- a label
       ; return (prof_data ++ liveness_data, (std_info, srt_label)) }

  | HeapRep _ ptrs nonptrs closure_type <- smrep
  = do { let layout  = packIntsCLit platform ptrs nonptrs
       ; (prof_lits, prof_data) <- mkProfLits platform prof
       ; let (srt_label, srt_bitmap) = mkSRTLit platform info_lbl srt
       ; (mb_srt_field, mb_layout, extra_bits, ct_data)
                                <- mk_pieces closure_type srt_label
       ; let std_info = mkStdInfoTable profile prof_lits
                                       (mb_rts_tag   `orElse` rtsClosureType smrep)
                                       (mb_srt_field `orElse` srt_bitmap)
                                       (mb_layout    `orElse` layout)
       ; return (prof_data ++ ct_data, (std_info, extra_bits)) }
  where
    platform = profilePlatform profile
    mk_pieces :: ClosureTypeInfo -> [CmmLit]
              -> UniqDSM ( Maybe CmmLit  -- Override the SRT field with this
                         , Maybe CmmLit  -- Override the layout field with this
                         , [CmmLit]           -- "Extra bits" for info table
                         , [RawCmmDecl])      -- Auxiliary data decls
    mk_pieces (Constr con_tag con_descr) _no_srt    -- A data constructor
      = do { (descr_lit, decl) <- newStringLit con_descr
           ; return ( Just (CmmInt (fromIntegral con_tag)
                                   (halfWordWidth platform))
                    , Nothing, [descr_lit], [decl]) }

    mk_pieces Thunk srt_label
      = return (Nothing, Nothing, srt_label, [])

    mk_pieces (ThunkSelector offset) _no_srt
      = return (Just (CmmInt 0 (halfWordWidth platform)),
                Just (mkWordCLit platform (fromIntegral offset)), [], [])
         -- Layout known (one free var); we use the layout field for offset

    mk_pieces (Fun arity (ArgSpec fun_type)) srt_label
      = do { let extra_bits = packIntsCLit platform fun_type arity : srt_label
           ; return (Nothing, Nothing,  extra_bits, []) }

    mk_pieces (Fun arity (ArgGen arg_bits)) srt_label
      = do { (liveness_lit, liveness_data) <- mkLivenessBits platform arg_bits
           ; let fun_type | null liveness_data = aRG_GEN
                          | otherwise          = aRG_GEN_BIG
                 extra_bits = [ packIntsCLit platform fun_type arity ]
                           ++ (if inlineSRT platform then [] else [ srt_lit ])
                           ++ [ liveness_lit, slow_entry ]
           ; return (Nothing, Nothing, extra_bits, liveness_data) }
      where
        slow_entry = CmmLabel (toSlowEntryLbl platform info_lbl)
        srt_lit = case srt_label of
                    []          -> mkIntCLit platform 0
                    (lit:_rest) -> assert (null _rest) lit

    mk_pieces other _ = pprPanic "mk_pieces" (ppr other)

mkInfoTableContents _ _ _ = panic "mkInfoTableContents"   -- NonInfoTable dealt with earlier

packIntsCLit :: Platform -> Int -> Int -> CmmLit
packIntsCLit platform a b = packHalfWordsCLit platform
                           (toStgHalfWord platform (fromIntegral a))
                           (toStgHalfWord platform (fromIntegral b))


mkSRTLit :: Platform
         -> CLabel
         -> Maybe CLabel
         -> ([CmmLit],    -- srt_label, if any
             CmmLit)      -- srt_bitmap
mkSRTLit platform info_lbl (Just lbl)
  | inlineSRT platform
  = ([], CmmLabelDiffOff lbl info_lbl 0 (halfWordWidth platform))
mkSRTLit platform _ Nothing    = ([], CmmInt 0 (halfWordWidth platform))
mkSRTLit platform _ (Just lbl) = ([CmmLabel lbl], CmmInt 1 (halfWordWidth platform))


-- | Is the SRT offset field inline in the info table on this platform?
--
-- See the section "Referring to an SRT from the info table" in
-- Note [SRTs] in "GHC.Cmm.Info.Build"
inlineSRT :: Platform -> Bool
inlineSRT = pc_USE_INLINE_SRT_FIELD . platformConstants

-------------------------------------------------------------------------
--
--      Lay out the info table and handle relative offsets
--
-------------------------------------------------------------------------

-- This function takes
--   * the standard info table portion (StgInfoTable)
--   * the "extra bits" (StgFunInfoExtraRev etc.)
--   * the entry label
--   * the code
-- and lays them out in memory, producing a list of RawCmmDecl

-------------------------------------------------------------------------
--
--      Position independent code
--
-------------------------------------------------------------------------
-- In order to support position independent code, we mustn't put absolute
-- references into read-only space. Info tables in the tablesNextToCode
-- case must be in .text, which is read-only, so we doctor the CmmLits
-- to use relative offsets instead.

-- Note that this is done even when the -fPIC flag is not specified,
-- as we want to keep binary compatibility between PIC and non-PIC.

makeRelativeRefTo :: Platform -> CLabel -> CmmLit -> CmmLit
makeRelativeRefTo platform info_lbl lit
  = if platformTablesNextToCode platform
      then case lit of
         CmmLabel lbl        -> CmmLabelDiffOff lbl info_lbl 0   (wordWidth platform)
         CmmLabelOff lbl off -> CmmLabelDiffOff lbl info_lbl off (wordWidth platform)
         _                   -> lit
      else lit

-------------------------------------------------------------------------
--
--              Build a liveness mask for the stack layout
--
-------------------------------------------------------------------------

-- There are four kinds of things on the stack:
--
--      - pointer variables (bound in the environment)
--      - non-pointer variables (bound in the environment)
--      - free slots (recorded in the stack free list)
--      - non-pointer data slots (recorded in the stack free list)
--
-- The first two are represented with a 'Just' of a 'LocalReg'.
-- The last two with one or more 'Nothing' constructors.
-- Each 'Nothing' represents one used word.
--
-- The head of the stack layout is the top of the stack and
-- the least-significant bit.

mkLivenessBits :: Platform -> Liveness -> UniqDSM (CmmLit, [RawCmmDecl])
              -- ^ Returns:
              --   1. The bitmap (literal value or label)
              --   2. Large bitmap CmmData if needed

mkLivenessBits platform liveness
  | n_bits > mAX_SMALL_BITMAP_SIZE platform -- does not fit in one word
  = do { uniq <- getUniqueDSM
       ; let bitmap_lbl = mkBitmapLabel uniq
       ; return (CmmLabel bitmap_lbl,
                 [mkRODataLits bitmap_lbl lits]) }

  | otherwise -- Fits in one word
  = return (mkStgWordCLit platform bitmap_word, [])
  where
    n_bits = length liveness

    bitmap :: Bitmap
    bitmap = mkBitmap platform liveness

    small_bitmap = case bitmap of
                     []  -> toStgWord platform 0
                     [b] -> b
                     _   -> panic "mkLiveness"
    bitmap_word = toStgWord platform (fromIntegral n_bits)
              .|. (small_bitmap `shiftL` pc_BITMAP_BITS_SHIFT (platformConstants platform))

    lits = mkWordCLit platform (fromIntegral n_bits)
         : map (mkStgWordCLit platform) bitmap
      -- The first word is the size.  The structure must match
      -- StgLargeBitmap in rts/include/rts/storage/InfoTable.h

-------------------------------------------------------------------------
--
--      Generating a standard info table
--
-------------------------------------------------------------------------

-- The standard bits of an info table.  This part of the info table
-- corresponds to the StgInfoTable type defined in
-- rts/include/rts/storage/InfoTables.h.
--
-- Its shape varies with ticky/profiling/tables next to code etc
-- so we can't use constant offsets from Constants

mkStdInfoTable
   :: Profile
   -> (CmmLit,CmmLit)   -- Closure type descr and closure descr  (profiling)
   -> Int               -- Closure RTS tag
   -> CmmLit            -- SRT length
   -> CmmLit            -- layout field
   -> [CmmLit]

mkStdInfoTable profile (type_descr, closure_descr) cl_type srt layout_lit
 =      -- Parallel revertible-black hole field
    prof_info
        -- Ticky info (none at present)
        -- Debug info (none at present)
 ++ [layout_lit, tag, srt]

 where
    platform = profilePlatform profile
    prof_info
        | profileIsProfiling profile = [type_descr, closure_descr]
        | otherwise = []

    tag = CmmInt (fromIntegral cl_type) (halfWordWidth platform)

-------------------------------------------------------------------------
--
--      Making string literals
--
-------------------------------------------------------------------------

mkProfLits :: Platform -> ProfilingInfo -> UniqDSM ((CmmLit,CmmLit), [RawCmmDecl])
mkProfLits platform NoProfilingInfo = return ((zeroCLit platform, zeroCLit platform), [])
mkProfLits _ (ProfilingInfo td cd)
  = do { (td_lit, td_decl) <- newStringLit td
       ; (cd_lit, cd_decl) <- newStringLit cd
       ; return ((td_lit,cd_lit), [td_decl,cd_decl]) }

newStringLit :: ByteString -> UniqDSM (CmmLit, GenCmmDecl RawCmmStatics info stmt)
newStringLit bytes
  = do { uniq <- getUniqueDSM
       ; return (mkByteStringCLit (mkStringLitLabel uniq) bytes) }


-- Misc utils

-- | Value of the srt field of an info table when using an StgLargeSRT
srtEscape :: Platform -> StgHalfWord
srtEscape platform = toStgHalfWord platform (-1)

-------------------------------------------------------------------------
--
--      Accessing fields of an info table
--
-------------------------------------------------------------------------

-- | Wrap a 'CmmExpr' in an alignment check when @-falignment-sanitisation@ is
-- enabled.
wordAligned :: Platform -> DoAlignSanitisation -> CmmExpr -> CmmExpr
wordAligned platform align_check e
  | align_check
  = CmmMachOp (MO_AlignmentCheck (platformWordSizeInBytes platform) (wordWidth platform)) [e]
  | otherwise
  = e

-- | Takes a closure pointer and returns the info table pointer
closureInfoPtr :: Platform -> DoAlignSanitisation -> CmmExpr -> CmmExpr
closureInfoPtr platform align_check e =
    CmmMachOp (MO_RelaxedRead (wordWidth platform)) [wordAligned platform align_check e]

-- | Takes an info pointer (the first word of a closure) and returns its entry
-- code
entryCode :: Platform -> CmmExpr -> CmmExpr
entryCode platform e =
 if platformTablesNextToCode platform
      then e
      else cmmLoadBWord platform e

-- | Takes a closure pointer, and return the *zero-indexed*
-- constructor tag obtained from the info table
-- This lives in the SRT field of the info table
-- (constructors don't need SRTs).
getConstrTag :: Profile -> DoAlignSanitisation -> CmmExpr -> CmmExpr
getConstrTag profile align_check closure_ptr
  = CmmMachOp (MO_UU_Conv (halfWordWidth platform) (wordWidth platform)) [infoTableConstrTag profile info_table]
  where
    info_table = infoTable profile (closureInfoPtr platform align_check closure_ptr)
    platform   = profilePlatform profile

-- | Takes a closure pointer, and return the closure type
-- obtained from the info table
cmmGetClosureType :: Profile -> DoAlignSanitisation -> CmmExpr -> CmmExpr
cmmGetClosureType profile align_check closure_ptr
  = CmmMachOp (MO_UU_Conv (halfWordWidth platform) (wordWidth platform)) [infoTableClosureType profile info_table]
  where
    info_table = infoTable profile (closureInfoPtr platform align_check closure_ptr)
    platform   = profilePlatform profile

-- | Takes an info pointer (the first word of a closure)
-- and returns a pointer to the first word of the standard-form
-- info table, excluding the entry-code word (if present)
infoTable :: Profile -> CmmExpr -> CmmExpr
infoTable profile info_ptr
  | platformTablesNextToCode platform = cmmOffsetB platform info_ptr (- stdInfoTableSizeB profile)
  | otherwise                         = cmmOffsetW platform info_ptr 1 -- Past the entry code pointer
  where platform = profilePlatform profile

-- | Takes an info table pointer (from infoTable) and returns the constr tag
-- field of the info table (same as the srt_bitmap field)
infoTableConstrTag :: Profile -> CmmExpr -> CmmExpr
infoTableConstrTag = infoTableSrtBitmap

-- | Takes an info table pointer (from infoTable) and returns the srt_bitmap
-- field of the info table
infoTableSrtBitmap :: Profile -> CmmExpr -> CmmExpr
infoTableSrtBitmap profile info_tbl
  = CmmLoad (cmmOffsetB platform info_tbl (stdSrtBitmapOffset profile)) (bHalfWord platform) NaturallyAligned
    where platform = profilePlatform profile

-- | Takes an info table pointer (from infoTable) and returns the closure type
-- field of the info table.
infoTableClosureType :: Profile -> CmmExpr -> CmmExpr
infoTableClosureType profile info_tbl
  = CmmLoad (cmmOffsetB platform info_tbl (stdClosureTypeOffset profile)) (bHalfWord platform) NaturallyAligned
    where platform = profilePlatform profile

infoTablePtrs :: Profile -> CmmExpr -> CmmExpr
infoTablePtrs profile info_tbl
  = CmmLoad (cmmOffsetB platform info_tbl (stdPtrsOffset profile)) (bHalfWord platform) NaturallyAligned
    where platform = profilePlatform profile

infoTableNonPtrs :: Profile -> CmmExpr -> CmmExpr
infoTableNonPtrs profile info_tbl
  = CmmLoad (cmmOffsetB platform info_tbl (stdNonPtrsOffset profile)) (bHalfWord platform) NaturallyAligned
    where platform = profilePlatform profile

-- | Takes the info pointer of a function, and returns a pointer to the first
-- word of the StgFunInfoExtra struct in the info table.
funInfoTable :: Profile -> CmmExpr -> CmmExpr
funInfoTable profile info_ptr
  | platformTablesNextToCode platform
  = cmmOffsetB platform info_ptr (- stdInfoTableSizeB profile - pc_SIZEOF_StgFunInfoExtraRev (platformConstants platform))
  | otherwise
  = cmmOffsetW platform info_ptr (1 + stdInfoTableSizeW profile)
                                  -- Past the entry code pointer
  where
    platform = profilePlatform profile

-- | Takes the info pointer of a function, returns the function's arity
funInfoArity :: Profile -> CmmExpr -> CmmExpr
funInfoArity profile iptr
  = cmmToWord platform (cmmLoadIndex platform rep fun_info (offset `div` rep_bytes))
  where
   platform = profilePlatform profile
   fun_info = funInfoTable profile iptr
   rep = cmmBits (widthFromBytes rep_bytes)
   tablesNextToCode = platformTablesNextToCode platform

   (rep_bytes, offset)
    | tablesNextToCode = ( pc_REP_StgFunInfoExtraRev_arity pc
                         , pc_OFFSET_StgFunInfoExtraRev_arity pc )
    | otherwise        = ( pc_REP_StgFunInfoExtraFwd_arity pc
                         , pc_OFFSET_StgFunInfoExtraFwd_arity pc )

   pc = platformConstants platform

-----------------------------------------------------------------------------
--
--      Info table sizes & offsets
--
-----------------------------------------------------------------------------

stdInfoTableSizeW :: Profile -> WordOff
-- The size of a standard info table varies with profiling/ticky etc,
-- so we can't get it from Constants
-- It must vary in sync with mkStdInfoTable
stdInfoTableSizeW profile
  = fixedInfoTableSizeW
  + if profileIsProfiling profile
       then profInfoTableSizeW
       else 0

fixedInfoTableSizeW :: WordOff
fixedInfoTableSizeW = 2 -- layout, type

profInfoTableSizeW :: WordOff
profInfoTableSizeW = 2

maxStdInfoTableSizeW :: WordOff
maxStdInfoTableSizeW =
  1 {- entry, when !tablesNextToCode -}
  + fixedInfoTableSizeW
  + profInfoTableSizeW

maxRetInfoTableSizeW :: WordOff
maxRetInfoTableSizeW =
  maxStdInfoTableSizeW
  + 1 {- srt label -}

stdInfoTableSizeB  :: Profile -> ByteOff
stdInfoTableSizeB profile = stdInfoTableSizeW profile * profileWordSizeInBytes profile

-- | Byte offset of the SRT bitmap half-word which is in the *higher-addressed*
-- part of the type_lit
stdSrtBitmapOffset :: Profile -> ByteOff
stdSrtBitmapOffset profile = stdInfoTableSizeB profile - halfWordSize (profilePlatform profile)

-- | Byte offset of the closure type half-word
stdClosureTypeOffset :: Profile -> ByteOff
stdClosureTypeOffset profile = stdInfoTableSizeB profile - profileWordSizeInBytes profile

stdPtrsOffset :: Profile -> ByteOff
stdPtrsOffset profile = stdInfoTableSizeB profile - 2 * profileWordSizeInBytes profile

stdNonPtrsOffset :: Profile -> ByteOff
stdNonPtrsOffset profile = stdInfoTableSizeB profile - 2 * profileWordSizeInBytes profile
                                                     + halfWordSize (profilePlatform profile)

conInfoTableSizeB :: Profile -> Int
conInfoTableSizeB profile = stdInfoTableSizeB profile + profileWordSizeInBytes profile
