{-# LANGUAGE CPP #-}
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

#include "HsVersions.h"

import GhcPrelude

import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.CLabel
import GHC.Runtime.Heap.Layout
import GHC.Data.Bitmap
import Stream (Stream)
import qualified Stream
import GHC.Cmm.Dataflow.Collections

import GHC.Platform
import Maybes
import GHC.Driver.Session
import ErrUtils (withTimingSilent)
import Panic
import GHC.Types.Unique.Supply
import MonadUtils
import Util
import Outputable

import Data.ByteString (ByteString)
import Data.Bits

-- When we split at proc points, we need an empty info table.
mkEmptyContInfoTable :: CLabel -> CmmInfoTable
mkEmptyContInfoTable info_lbl
  = CmmInfoTable { cit_lbl  = info_lbl
                 , cit_rep  = mkStackRep []
                 , cit_prof = NoProfilingInfo
                 , cit_srt  = Nothing
                 , cit_clo  = Nothing }

cmmToRawCmm :: DynFlags -> Stream IO CmmGroupSRTs a
            -> IO (Stream IO RawCmmGroup a)
cmmToRawCmm dflags cmms
  = do { uniqs <- mkSplitUniqSupply 'i'
       ; let do_one :: UniqSupply -> [CmmDeclSRTs] -> IO (UniqSupply, [RawCmmDecl])
             do_one uniqs cmm =
               -- NB. strictness fixes a space leak.  DO NOT REMOVE.
               withTimingSilent dflags (text "Cmm -> Raw Cmm")
                                forceRes $
                 case initUs uniqs $ concatMapM (mkInfoTable dflags) cmm of
                   (b,uniqs') -> return (uniqs',b)
       ; return (snd <$> Stream.mapAccumL_ do_one uniqs cmms)
       }

    where forceRes (uniqs, rawcmms) =
            uniqs `seq` foldr (\decl r -> decl `seq` r) () rawcmms

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
--      See includes/rts/storage/InfoTables.h
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

mkInfoTable :: DynFlags -> CmmDeclSRTs -> UniqSM [RawCmmDecl]
mkInfoTable _ (CmmData sec dat) = return [CmmData sec dat]

mkInfoTable dflags proc@(CmmProc infos entry_lbl live blocks)
  --
  -- in the non-tables-next-to-code case, procs can have at most a
  -- single info table associated with the entry label of the proc.
  --
  | not (tablesNextToCode dflags)
  = case topInfoTable proc of   --  must be at most one
      -- no info table
      Nothing ->
         return [CmmProc mapEmpty entry_lbl live blocks]

      Just info@CmmInfoTable { cit_lbl = info_lbl } -> do
        (top_decls, (std_info, extra_bits)) <-
             mkInfoTableContents dflags info Nothing
        let
          rel_std_info   = map (makeRelativeRefTo dflags info_lbl) std_info
          rel_extra_bits = map (makeRelativeRefTo dflags info_lbl) extra_bits
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
   do_one_info (lbl,itbl) = do
     (top_decls, (std_info, extra_bits)) <-
         mkInfoTableContents dflags itbl Nothing
     let
        info_lbl = cit_lbl itbl
        rel_std_info   = map (makeRelativeRefTo dflags info_lbl) std_info
        rel_extra_bits = map (makeRelativeRefTo dflags info_lbl) extra_bits
     --
     return (top_decls, (lbl, CmmStaticsRaw info_lbl $ map CmmStaticLit $
                              reverse rel_extra_bits ++ rel_std_info))

-----------------------------------------------------
type InfoTableContents = ( [CmmLit]          -- The standard part
                         , [CmmLit] )        -- The "extra bits"
-- These Lits have *not* had mkRelativeTo applied to them

mkInfoTableContents :: DynFlags
                    -> CmmInfoTable
                    -> Maybe Int               -- Override default RTS type tag?
                    -> UniqSM ([RawCmmDecl],             -- Auxiliary top decls
                               InfoTableContents)       -- Info tbl + extra bits

mkInfoTableContents dflags
                    info@(CmmInfoTable { cit_lbl  = info_lbl
                                       , cit_rep  = smrep
                                       , cit_prof = prof
                                       , cit_srt = srt })
                    mb_rts_tag
  | RTSRep rts_tag rep <- smrep
  = mkInfoTableContents dflags info{cit_rep = rep} (Just rts_tag)
    -- Completely override the rts_tag that mkInfoTableContents would
    -- otherwise compute, with the rts_tag stored in the RTSRep
    -- (which in turn came from a handwritten .cmm file)

  | StackRep frame <- smrep
  = do { (prof_lits, prof_data) <- mkProfLits platform prof
       ; let (srt_label, srt_bitmap) = mkSRTLit dflags info_lbl srt
       ; (liveness_lit, liveness_data) <- mkLivenessBits dflags frame
       ; let
             std_info = mkStdInfoTable dflags prof_lits rts_tag srt_bitmap liveness_lit
             rts_tag | Just tag <- mb_rts_tag = tag
                     | null liveness_data     = rET_SMALL -- Fits in extra_bits
                     | otherwise              = rET_BIG   -- Does not; extra_bits is
                                                          -- a label
       ; return (prof_data ++ liveness_data, (std_info, srt_label)) }

  | HeapRep _ ptrs nonptrs closure_type <- smrep
  = do { let layout  = packIntsCLit platform ptrs nonptrs
       ; (prof_lits, prof_data) <- mkProfLits platform prof
       ; let (srt_label, srt_bitmap) = mkSRTLit dflags info_lbl srt
       ; (mb_srt_field, mb_layout, extra_bits, ct_data)
                                <- mk_pieces closure_type srt_label
       ; let std_info = mkStdInfoTable dflags prof_lits
                                       (mb_rts_tag   `orElse` rtsClosureType smrep)
                                       (mb_srt_field `orElse` srt_bitmap)
                                       (mb_layout    `orElse` layout)
       ; return (prof_data ++ ct_data, (std_info, extra_bits)) }
  where
    platform = targetPlatform dflags
    mk_pieces :: ClosureTypeInfo -> [CmmLit]
              -> UniqSM ( Maybe CmmLit  -- Override the SRT field with this
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
      = do { (liveness_lit, liveness_data) <- mkLivenessBits dflags arg_bits
           ; let fun_type | null liveness_data = aRG_GEN
                          | otherwise          = aRG_GEN_BIG
                 extra_bits = [ packIntsCLit platform fun_type arity ]
                           ++ (if inlineSRT dflags then [] else [ srt_lit ])
                           ++ [ liveness_lit, slow_entry ]
           ; return (Nothing, Nothing, extra_bits, liveness_data) }
      where
        slow_entry = CmmLabel (toSlowEntryLbl info_lbl)
        srt_lit = case srt_label of
                    []          -> mkIntCLit platform 0
                    (lit:_rest) -> ASSERT( null _rest ) lit

    mk_pieces other _ = pprPanic "mk_pieces" (ppr other)

mkInfoTableContents _ _ _ = panic "mkInfoTableContents"   -- NonInfoTable dealt with earlier

packIntsCLit :: Platform -> Int -> Int -> CmmLit
packIntsCLit platform a b = packHalfWordsCLit platform
                           (toStgHalfWord platform (fromIntegral a))
                           (toStgHalfWord platform (fromIntegral b))


mkSRTLit :: DynFlags
         -> CLabel
         -> Maybe CLabel
         -> ([CmmLit],    -- srt_label, if any
             CmmLit)      -- srt_bitmap
mkSRTLit dflags info_lbl (Just lbl)
  | inlineSRT dflags
  = ([], CmmLabelDiffOff lbl info_lbl 0 (halfWordWidth (targetPlatform dflags)))
mkSRTLit dflags _ Nothing    = ([], CmmInt 0 (halfWordWidth (targetPlatform dflags)))
mkSRTLit dflags _ (Just lbl) = ([CmmLabel lbl], CmmInt 1 (halfWordWidth (targetPlatform dflags)))


-- | Is the SRT offset field inline in the info table on this platform?
--
-- See the section "Referring to an SRT from the info table" in
-- Note [SRTs] in GHC.Cmm.Info.Build
inlineSRT :: DynFlags -> Bool
inlineSRT dflags = platformArch (targetPlatform dflags) == ArchX86_64
  && tablesNextToCode dflags

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

makeRelativeRefTo :: DynFlags -> CLabel -> CmmLit -> CmmLit

makeRelativeRefTo dflags info_lbl (CmmLabel lbl)
  | tablesNextToCode dflags
  = CmmLabelDiffOff lbl info_lbl 0 (wordWidth (targetPlatform dflags))
makeRelativeRefTo dflags info_lbl (CmmLabelOff lbl off)
  | tablesNextToCode dflags
  = CmmLabelDiffOff lbl info_lbl off (wordWidth (targetPlatform dflags))
makeRelativeRefTo _ _ lit = lit


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

mkLivenessBits :: DynFlags -> Liveness -> UniqSM (CmmLit, [RawCmmDecl])
              -- ^ Returns:
              --   1. The bitmap (literal value or label)
              --   2. Large bitmap CmmData if needed

mkLivenessBits dflags liveness
  | n_bits > mAX_SMALL_BITMAP_SIZE platform -- does not fit in one word
  = do { uniq <- getUniqueM
       ; let bitmap_lbl = mkBitmapLabel uniq
       ; return (CmmLabel bitmap_lbl,
                 [mkRODataLits bitmap_lbl lits]) }

  | otherwise -- Fits in one word
  = return (mkStgWordCLit platform bitmap_word, [])
  where
    platform = targetPlatform dflags
    n_bits = length liveness

    bitmap :: Bitmap
    bitmap = mkBitmap platform liveness

    small_bitmap = case bitmap of
                     []  -> toStgWord platform 0
                     [b] -> b
                     _   -> panic "mkLiveness"
    bitmap_word = toStgWord platform (fromIntegral n_bits)
              .|. (small_bitmap `shiftL` bITMAP_BITS_SHIFT dflags)

    lits = mkWordCLit platform (fromIntegral n_bits)
         : map (mkStgWordCLit platform) bitmap
      -- The first word is the size.  The structure must match
      -- StgLargeBitmap in includes/rts/storage/InfoTable.h

-------------------------------------------------------------------------
--
--      Generating a standard info table
--
-------------------------------------------------------------------------

-- The standard bits of an info table.  This part of the info table
-- corresponds to the StgInfoTable type defined in
-- includes/rts/storage/InfoTables.h.
--
-- Its shape varies with ticky/profiling/tables next to code etc
-- so we can't use constant offsets from Constants

mkStdInfoTable
   :: DynFlags
   -> (CmmLit,CmmLit)   -- Closure type descr and closure descr  (profiling)
   -> Int               -- Closure RTS tag
   -> CmmLit            -- SRT length
   -> CmmLit            -- layout field
   -> [CmmLit]

mkStdInfoTable dflags (type_descr, closure_descr) cl_type srt layout_lit
 =      -- Parallel revertible-black hole field
    prof_info
        -- Ticky info (none at present)
        -- Debug info (none at present)
 ++ [layout_lit, tag, srt]

 where
    platform = targetPlatform dflags
    prof_info
        | gopt Opt_SccProfilingOn dflags = [type_descr, closure_descr]
        | otherwise = []

    tag = CmmInt (fromIntegral cl_type) (halfWordWidth platform)

-------------------------------------------------------------------------
--
--      Making string literals
--
-------------------------------------------------------------------------

mkProfLits :: Platform -> ProfilingInfo -> UniqSM ((CmmLit,CmmLit), [RawCmmDecl])
mkProfLits platform NoProfilingInfo = return ((zeroCLit platform, zeroCLit platform), [])
mkProfLits _ (ProfilingInfo td cd)
  = do { (td_lit, td_decl) <- newStringLit td
       ; (cd_lit, cd_decl) <- newStringLit cd
       ; return ((td_lit,cd_lit), [td_decl,cd_decl]) }

newStringLit :: ByteString -> UniqSM (CmmLit, GenCmmDecl RawCmmStatics info stmt)
newStringLit bytes
  = do { uniq <- getUniqueM
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
wordAligned :: DynFlags -> CmmExpr -> CmmExpr
wordAligned dflags e
  | gopt Opt_AlignmentSanitisation dflags
  = CmmMachOp (MO_AlignmentCheck (platformWordSizeInBytes platform) (wordWidth platform)) [e]
  | otherwise
  = e
  where platform = targetPlatform dflags

closureInfoPtr :: DynFlags -> CmmExpr -> CmmExpr
-- Takes a closure pointer and returns the info table pointer
closureInfoPtr dflags e =
    CmmLoad (wordAligned dflags e) (bWord (targetPlatform dflags))

entryCode :: DynFlags -> CmmExpr -> CmmExpr
-- Takes an info pointer (the first word of a closure)
-- and returns its entry code
entryCode dflags e
 | tablesNextToCode dflags = e
 | otherwise               = CmmLoad e (bWord (targetPlatform dflags))

getConstrTag :: DynFlags -> CmmExpr -> CmmExpr
-- Takes a closure pointer, and return the *zero-indexed*
-- constructor tag obtained from the info table
-- This lives in the SRT field of the info table
-- (constructors don't need SRTs).
getConstrTag dflags closure_ptr
  = CmmMachOp (MO_UU_Conv (halfWordWidth platform) (wordWidth platform)) [infoTableConstrTag dflags info_table]
  where
    info_table = infoTable dflags (closureInfoPtr dflags closure_ptr)
    platform = targetPlatform dflags

cmmGetClosureType :: DynFlags -> CmmExpr -> CmmExpr
-- Takes a closure pointer, and return the closure type
-- obtained from the info table
cmmGetClosureType dflags closure_ptr
  = CmmMachOp (MO_UU_Conv (halfWordWidth platform) (wordWidth platform)) [infoTableClosureType dflags info_table]
  where
    info_table = infoTable dflags (closureInfoPtr dflags closure_ptr)
    platform = targetPlatform dflags

infoTable :: DynFlags -> CmmExpr -> CmmExpr
-- Takes an info pointer (the first word of a closure)
-- and returns a pointer to the first word of the standard-form
-- info table, excluding the entry-code word (if present)
infoTable dflags info_ptr
  | tablesNextToCode dflags = cmmOffsetB platform info_ptr (- stdInfoTableSizeB dflags)
  | otherwise               = cmmOffsetW platform info_ptr 1 -- Past the entry code pointer
  where platform = targetPlatform dflags

infoTableConstrTag :: DynFlags -> CmmExpr -> CmmExpr
-- Takes an info table pointer (from infoTable) and returns the constr tag
-- field of the info table (same as the srt_bitmap field)
infoTableConstrTag = infoTableSrtBitmap

infoTableSrtBitmap :: DynFlags -> CmmExpr -> CmmExpr
-- Takes an info table pointer (from infoTable) and returns the srt_bitmap
-- field of the info table
infoTableSrtBitmap dflags info_tbl
  = CmmLoad (cmmOffsetB platform info_tbl (stdSrtBitmapOffset dflags)) (bHalfWord platform)
    where platform = targetPlatform dflags

infoTableClosureType :: DynFlags -> CmmExpr -> CmmExpr
-- Takes an info table pointer (from infoTable) and returns the closure type
-- field of the info table.
infoTableClosureType dflags info_tbl
  = CmmLoad (cmmOffsetB platform info_tbl (stdClosureTypeOffset dflags)) (bHalfWord platform)
    where platform = targetPlatform dflags

infoTablePtrs :: DynFlags -> CmmExpr -> CmmExpr
infoTablePtrs dflags info_tbl
  = CmmLoad (cmmOffsetB platform info_tbl (stdPtrsOffset dflags)) (bHalfWord platform)
    where platform = targetPlatform dflags

infoTableNonPtrs :: DynFlags -> CmmExpr -> CmmExpr
infoTableNonPtrs dflags info_tbl
  = CmmLoad (cmmOffsetB platform info_tbl (stdNonPtrsOffset dflags)) (bHalfWord platform)
    where platform = targetPlatform dflags

funInfoTable :: DynFlags -> CmmExpr -> CmmExpr
-- Takes the info pointer of a function,
-- and returns a pointer to the first word of the StgFunInfoExtra struct
-- in the info table.
funInfoTable dflags info_ptr
  | tablesNextToCode dflags
  = cmmOffsetB platform info_ptr (- stdInfoTableSizeB dflags - sIZEOF_StgFunInfoExtraRev dflags)
  | otherwise
  = cmmOffsetW platform info_ptr (1 + stdInfoTableSizeW dflags)
                                  -- Past the entry code pointer
  where
    platform = targetPlatform dflags

-- Takes the info pointer of a function, returns the function's arity
funInfoArity :: DynFlags -> CmmExpr -> CmmExpr
funInfoArity dflags iptr
  = cmmToWord platform (cmmLoadIndex platform rep fun_info (offset `div` rep_bytes))
  where
   platform = targetPlatform dflags
   fun_info = funInfoTable dflags iptr
   rep = cmmBits (widthFromBytes rep_bytes)

   (rep_bytes, offset)
    | tablesNextToCode dflags = ( pc_REP_StgFunInfoExtraRev_arity pc
                                , oFFSET_StgFunInfoExtraRev_arity dflags )
    | otherwise               = ( pc_REP_StgFunInfoExtraFwd_arity pc
                                , oFFSET_StgFunInfoExtraFwd_arity dflags )

   pc = platformConstants dflags

-----------------------------------------------------------------------------
--
--      Info table sizes & offsets
--
-----------------------------------------------------------------------------

stdInfoTableSizeW :: DynFlags -> WordOff
-- The size of a standard info table varies with profiling/ticky etc,
-- so we can't get it from Constants
-- It must vary in sync with mkStdInfoTable
stdInfoTableSizeW dflags
  = fixedInfoTableSizeW
  + if gopt Opt_SccProfilingOn dflags
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

stdInfoTableSizeB  :: DynFlags -> ByteOff
stdInfoTableSizeB dflags = stdInfoTableSizeW dflags * platformWordSizeInBytes platform
   where platform = targetPlatform dflags

stdSrtBitmapOffset :: DynFlags -> ByteOff
-- Byte offset of the SRT bitmap half-word which is
-- in the *higher-addressed* part of the type_lit
stdSrtBitmapOffset dflags = stdInfoTableSizeB dflags - halfWordSize platform
   where platform = targetPlatform dflags

stdClosureTypeOffset :: DynFlags -> ByteOff
-- Byte offset of the closure type half-word
stdClosureTypeOffset dflags = stdInfoTableSizeB dflags - platformWordSizeInBytes platform
   where platform = targetPlatform dflags

stdPtrsOffset, stdNonPtrsOffset :: DynFlags -> ByteOff
stdPtrsOffset    dflags = stdInfoTableSizeB dflags - 2 * platformWordSizeInBytes platform
   where platform = targetPlatform dflags

stdNonPtrsOffset dflags = stdInfoTableSizeB dflags - 2 * platformWordSizeInBytes platform + halfWordSize platform
   where platform = targetPlatform dflags

conInfoTableSizeB :: DynFlags -> Int
conInfoTableSizeB dflags = stdInfoTableSizeB dflags + platformWordSizeInBytes platform
   where platform = targetPlatform dflags
