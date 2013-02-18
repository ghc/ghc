{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module CmmInfo (
  mkEmptyContInfoTable,
  cmmToRawCmm,
  mkInfoTable,
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
  stdSrtBitmapOffset,
  stdClosureTypeOffset,
  stdPtrsOffset, stdNonPtrsOffset,
) where

#include "HsVersions.h"

import Cmm
import CmmUtils
import CLabel
import SMRep
import Bitmap
import Stream (Stream)
import qualified Stream
import Hoopl

import Maybes
import DynFlags
import Panic
import UniqSupply
import MonadUtils
import Util
import Outputable

import Data.Bits
import Data.Word

-- When we split at proc points, we need an empty info table.
mkEmptyContInfoTable :: CLabel -> CmmInfoTable
mkEmptyContInfoTable info_lbl 
  = CmmInfoTable { cit_lbl  = info_lbl
                 , cit_rep  = mkStackRep []
                 , cit_prof = NoProfilingInfo
                 , cit_srt  = NoC_SRT }

cmmToRawCmm :: DynFlags -> Stream IO CmmGroup ()
            -> IO (Stream IO RawCmmGroup ())
cmmToRawCmm dflags cmms
  = do { uniqs <- mkSplitUniqSupply 'i'
       ; let do_one uniqs cmm = do
                case initUs uniqs $ concatMapM (mkInfoTable dflags) cmm of
                  (b,uniqs') -> return (uniqs',b)
                  -- NB. strictness fixes a space leak.  DO NOT REMOVE.
       ; return (Stream.mapAccumL do_one uniqs cmms >> return ())
       }

-- Make a concrete info table, represented as a list of CmmStatic
-- (it can't be simply a list of Word, because the SRT field is
-- represented by a label+offset expression).
--
-- With tablesNextToCode, the layout is
--	<reversed variable part>
--	<normal forward StgInfoTable, but without 
--		an entry point at the front>
--	<code>
--
-- Without tablesNextToCode, the layout of an info table is
--	<entry label>
--	<normal forward rest of StgInfoTable>
--	<forward variable part>
--
--	See includes/rts/storage/InfoTables.h
--
-- For return-points these are as follows
--
-- Tables next to code:
--
--			<srt slot>
--			<standard info table>
--  	ret-addr -->	<entry code (if any)>
--
-- Not tables-next-to-code:
--
--	ret-addr -->	<ptr to entry code>
--			<standard info table>
--			<srt slot>
--
--  * The SRT slot is only there if there is SRT info to record

mkInfoTable :: DynFlags -> CmmDecl -> UniqSM [RawCmmDecl]
mkInfoTable _ (CmmData sec dat)
  = return [CmmData sec dat]

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
                 mkDataLits Data info_lbl
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
     return (top_decls, (lbl, Statics info_lbl $ map CmmStaticLit $
                              reverse rel_extra_bits ++ rel_std_info))

-----------------------------------------------------
type InfoTableContents = ( [CmmLit]	     -- The standard part
                         , [CmmLit] )	     -- The "extra bits"
-- These Lits have *not* had mkRelativeTo applied to them

mkInfoTableContents :: DynFlags
                    -> CmmInfoTable
                    -> Maybe Int               -- Override default RTS type tag?
                    -> UniqSM ([RawCmmDecl],             -- Auxiliary top decls
                               InfoTableContents)	-- Info tbl + extra bits

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
  = do { (prof_lits, prof_data) <- mkProfLits dflags prof
       ; let (srt_label, srt_bitmap) = mkSRTLit dflags srt
       ; (liveness_lit, liveness_data) <- mkLivenessBits dflags frame
       ; let
             std_info = mkStdInfoTable dflags prof_lits rts_tag srt_bitmap liveness_lit
             rts_tag | Just tag <- mb_rts_tag = tag
                     | null liveness_data     = rET_SMALL -- Fits in extra_bits
                     | otherwise              = rET_BIG   -- Does not; extra_bits is
                                                          -- a label
       ; return (prof_data ++ liveness_data, (std_info, srt_label)) }

  | HeapRep _ ptrs nonptrs closure_type <- smrep
  = do { let layout  = packIntsCLit dflags ptrs nonptrs
       ; (prof_lits, prof_data) <- mkProfLits dflags prof
       ; let (srt_label, srt_bitmap) = mkSRTLit dflags srt
       ; (mb_srt_field, mb_layout, extra_bits, ct_data)
                                <- mk_pieces closure_type srt_label
       ; let std_info = mkStdInfoTable dflags prof_lits
                                       (mb_rts_tag   `orElse` rtsClosureType smrep)
                                       (mb_srt_field `orElse` srt_bitmap)
                                       (mb_layout    `orElse` layout)
       ; return (prof_data ++ ct_data, (std_info, extra_bits)) }
  where
    mk_pieces :: ClosureTypeInfo -> [CmmLit]
              -> UniqSM ( Maybe StgHalfWord  -- Override the SRT field with this
                 	, Maybe CmmLit       -- Override the layout field with this
                 	, [CmmLit]           -- "Extra bits" for info table
                 	, [RawCmmDecl])	     -- Auxiliary data decls 
    mk_pieces (Constr con_tag con_descr) _no_srt    -- A data constructor
      = do { (descr_lit, decl) <- newStringLit con_descr
           ; return ( Just (toStgHalfWord dflags (fromIntegral con_tag))
                    , Nothing, [descr_lit], [decl]) }

    mk_pieces Thunk srt_label
      = return (Nothing, Nothing, srt_label, [])

    mk_pieces (ThunkSelector offset) _no_srt
      = return (Just (toStgHalfWord dflags 0), Just (mkWordCLit dflags (fromIntegral offset)), [], [])
         -- Layout known (one free var); we use the layout field for offset

    mk_pieces (Fun arity (ArgSpec fun_type)) srt_label 
      = do { let extra_bits = packIntsCLit dflags fun_type arity : srt_label
           ; return (Nothing, Nothing,  extra_bits, []) }

    mk_pieces (Fun arity (ArgGen arg_bits)) srt_label
      = do { (liveness_lit, liveness_data) <- mkLivenessBits dflags arg_bits
           ; let fun_type | null liveness_data = aRG_GEN
                          | otherwise          = aRG_GEN_BIG
                 extra_bits = [ packIntsCLit dflags fun_type arity
                              , srt_lit, liveness_lit, slow_entry ]
           ; return (Nothing, Nothing, extra_bits, liveness_data) }
      where
        slow_entry = CmmLabel (toSlowEntryLbl info_lbl)
        srt_lit = case srt_label of
                    []          -> mkIntCLit dflags 0
                    (lit:_rest) -> ASSERT( null _rest ) lit

    mk_pieces other _ = pprPanic "mk_pieces" (ppr other)

mkInfoTableContents _ _ _ = panic "mkInfoTableContents"   -- NonInfoTable dealt with earlier

packIntsCLit :: DynFlags -> Int -> Int -> CmmLit
packIntsCLit dflags a b = packHalfWordsCLit dflags
                           (toStgHalfWord dflags (fromIntegral a))
                           (toStgHalfWord dflags (fromIntegral b))


mkSRTLit :: DynFlags
         -> C_SRT
         -> ([CmmLit],    -- srt_label, if any
             StgHalfWord) -- srt_bitmap
mkSRTLit dflags NoC_SRT                = ([], toStgHalfWord dflags 0)
mkSRTLit dflags (C_SRT lbl off bitmap) = ([cmmLabelOffW dflags lbl off], bitmap)


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
--	Position independent code
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
  = CmmLabelDiffOff lbl info_lbl 0
makeRelativeRefTo dflags info_lbl (CmmLabelOff lbl off)
  | tablesNextToCode dflags
  = CmmLabelDiffOff lbl info_lbl off
makeRelativeRefTo _ _ lit = lit


-------------------------------------------------------------------------
--
--		Build a liveness mask for the stack layout
--
-------------------------------------------------------------------------

-- There are four kinds of things on the stack:
--
--	- pointer variables (bound in the environment)
-- 	- non-pointer variables (bound in the environment)
-- 	- free slots (recorded in the stack free list)
-- 	- non-pointer data slots (recorded in the stack free list)
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
  | n_bits > mAX_SMALL_BITMAP_SIZE dflags -- does not fit in one word
  = do { uniq <- getUniqueUs
       ; let bitmap_lbl = mkBitmapLabel uniq
       ; return (CmmLabel bitmap_lbl, 
                 [mkRODataLits bitmap_lbl lits]) }

  | otherwise -- Fits in one word
  = return (mkStgWordCLit dflags bitmap_word, [])
  where
    n_bits = length liveness

    bitmap :: Bitmap
    bitmap = mkBitmap dflags liveness

    small_bitmap = case bitmap of 
                     []  -> toStgWord dflags 0
                     [b] -> b
		     _   -> panic "mkLiveness"
    bitmap_word = toStgWord dflags (fromIntegral n_bits)
              .|. (small_bitmap `shiftL` bITMAP_BITS_SHIFT dflags)

    lits = mkWordCLit dflags (fromIntegral n_bits)
         : map (mkStgWordCLit dflags) bitmap
      -- The first word is the size.  The structure must match
      -- StgLargeBitmap in includes/rts/storage/InfoTable.h

-------------------------------------------------------------------------
--
--	Generating a standard info table
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
   -> (CmmLit,CmmLit)	-- Closure type descr and closure descr  (profiling)
   -> Int               -- Closure RTS tag
   -> StgHalfWord       -- SRT length
   -> CmmLit		-- layout field
   -> [CmmLit]

mkStdInfoTable dflags (type_descr, closure_descr) cl_type srt_len layout_lit
 = 	-- Parallel revertible-black hole field
    prof_info
	-- Ticky info (none at present)
	-- Debug info (none at present)
 ++ [layout_lit, type_lit]

 where  
    prof_info 
	| gopt Opt_SccProfilingOn dflags = [type_descr, closure_descr]
	| otherwise = []

    type_lit = packHalfWordsCLit dflags (toStgHalfWord dflags (fromIntegral cl_type)) srt_len

-------------------------------------------------------------------------
--
--      Making string literals
--
-------------------------------------------------------------------------

mkProfLits :: DynFlags -> ProfilingInfo -> UniqSM ((CmmLit,CmmLit), [RawCmmDecl])
mkProfLits dflags NoProfilingInfo       = return ((zeroCLit dflags, zeroCLit dflags), [])
mkProfLits _ (ProfilingInfo td cd)
  = do { (td_lit, td_decl) <- newStringLit td
       ; (cd_lit, cd_decl) <- newStringLit cd
       ; return ((td_lit,cd_lit), [td_decl,cd_decl]) }

newStringLit :: [Word8] -> UniqSM (CmmLit, GenCmmDecl CmmStatics info stmt)
newStringLit bytes
  = do { uniq <- getUniqueUs
       ; return (mkByteStringCLit uniq bytes) }


-- Misc utils

-- | Value of the srt field of an info table when using an StgLargeSRT
srtEscape :: DynFlags -> StgHalfWord
srtEscape dflags = toStgHalfWord dflags (-1)

-------------------------------------------------------------------------
--
--	Accessing fields of an info table
--
-------------------------------------------------------------------------

closureInfoPtr :: DynFlags -> CmmExpr -> CmmExpr
-- Takes a closure pointer and returns the info table pointer
closureInfoPtr dflags e = CmmLoad e (bWord dflags)

entryCode :: DynFlags -> CmmExpr -> CmmExpr
-- Takes an info pointer (the first word of a closure)
-- and returns its entry code
entryCode dflags e
 | tablesNextToCode dflags = e
 | otherwise               = CmmLoad e (bWord dflags)

getConstrTag :: DynFlags -> CmmExpr -> CmmExpr
-- Takes a closure pointer, and return the *zero-indexed*
-- constructor tag obtained from the info table
-- This lives in the SRT field of the info table
-- (constructors don't need SRTs).
getConstrTag dflags closure_ptr
  = CmmMachOp (MO_UU_Conv (halfWordWidth dflags) (wordWidth dflags)) [infoTableConstrTag dflags info_table]
  where
    info_table = infoTable dflags (closureInfoPtr dflags closure_ptr)

cmmGetClosureType :: DynFlags -> CmmExpr -> CmmExpr
-- Takes a closure pointer, and return the closure type
-- obtained from the info table
cmmGetClosureType dflags closure_ptr
  = CmmMachOp (MO_UU_Conv (halfWordWidth dflags) (wordWidth dflags)) [infoTableClosureType dflags info_table]
  where
    info_table = infoTable dflags (closureInfoPtr dflags closure_ptr)

infoTable :: DynFlags -> CmmExpr -> CmmExpr
-- Takes an info pointer (the first word of a closure)
-- and returns a pointer to the first word of the standard-form
-- info table, excluding the entry-code word (if present)
infoTable dflags info_ptr
  | tablesNextToCode dflags = cmmOffsetB dflags info_ptr (- stdInfoTableSizeB dflags)
  | otherwise               = cmmOffsetW dflags info_ptr 1 -- Past the entry code pointer

infoTableConstrTag :: DynFlags -> CmmExpr -> CmmExpr
-- Takes an info table pointer (from infoTable) and returns the constr tag
-- field of the info table (same as the srt_bitmap field)
infoTableConstrTag = infoTableSrtBitmap

infoTableSrtBitmap :: DynFlags -> CmmExpr -> CmmExpr
-- Takes an info table pointer (from infoTable) and returns the srt_bitmap
-- field of the info table
infoTableSrtBitmap dflags info_tbl
  = CmmLoad (cmmOffsetB dflags info_tbl (stdSrtBitmapOffset dflags)) (bHalfWord dflags)

infoTableClosureType :: DynFlags -> CmmExpr -> CmmExpr
-- Takes an info table pointer (from infoTable) and returns the closure type
-- field of the info table.
infoTableClosureType dflags info_tbl
  = CmmLoad (cmmOffsetB dflags info_tbl (stdClosureTypeOffset dflags)) (bHalfWord dflags)

infoTablePtrs :: DynFlags -> CmmExpr -> CmmExpr
infoTablePtrs dflags info_tbl
  = CmmLoad (cmmOffsetB dflags info_tbl (stdPtrsOffset dflags)) (bHalfWord dflags)

infoTableNonPtrs :: DynFlags -> CmmExpr -> CmmExpr
infoTableNonPtrs dflags info_tbl
  = CmmLoad (cmmOffsetB dflags info_tbl (stdNonPtrsOffset dflags)) (bHalfWord dflags)

funInfoTable :: DynFlags -> CmmExpr -> CmmExpr
-- Takes the info pointer of a function,
-- and returns a pointer to the first word of the StgFunInfoExtra struct
-- in the info table.
funInfoTable dflags info_ptr
  | tablesNextToCode dflags
  = cmmOffsetB dflags info_ptr (- stdInfoTableSizeB dflags - sIZEOF_StgFunInfoExtraRev dflags)
  | otherwise
  = cmmOffsetW dflags info_ptr (1 + stdInfoTableSizeW dflags)
				-- Past the entry code pointer

-- Takes the info pointer of a function, returns the function's arity
funInfoArity :: DynFlags -> CmmExpr -> CmmExpr
funInfoArity dflags iptr
  = cmmToWord dflags (cmmLoadIndex dflags rep fun_info offset)
  where
   fun_info = funInfoTable dflags iptr
   rep = cmmBits (widthFromBytes rep_bytes)

   (rep_bytes, offset)
    | tablesNextToCode dflags = ( pc_REP_StgFunInfoExtraFwd_arity pc
                                , oFFSET_StgFunInfoExtraFwd_arity dflags )
    | otherwise               = ( pc_REP_StgFunInfoExtraRev_arity pc
                                , oFFSET_StgFunInfoExtraRev_arity dflags )

   pc = sPlatformConstants (settings dflags)

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
stdInfoTableSizeB dflags = stdInfoTableSizeW dflags * wORD_SIZE dflags

stdSrtBitmapOffset :: DynFlags -> ByteOff
-- Byte offset of the SRT bitmap half-word which is 
-- in the *higher-addressed* part of the type_lit
stdSrtBitmapOffset dflags = stdInfoTableSizeB dflags - hALF_WORD_SIZE dflags

stdClosureTypeOffset :: DynFlags -> ByteOff
-- Byte offset of the closure type half-word 
stdClosureTypeOffset dflags = stdInfoTableSizeB dflags - wORD_SIZE dflags

stdPtrsOffset, stdNonPtrsOffset :: DynFlags -> ByteOff
stdPtrsOffset    dflags = stdInfoTableSizeB dflags - 2 * wORD_SIZE dflags
stdNonPtrsOffset dflags = stdInfoTableSizeB dflags - 2 * wORD_SIZE dflags + hALF_WORD_SIZE dflags

