module CmmInfo (
  mkEmptyContInfoTable,
  cmmToRawCmm,
  mkInfoTable,
) where

#include "HsVersions.h"

import OldCmm
import CmmUtils

import CLabel

import Bitmap
import ClosureInfo
import CgInfoTbls
import CgCallConv
import CgUtils
import SMRep

import Constants
import Panic
import StaticFlags
import Unique
import UniqSupply

import Data.Bits

-- When we split at proc points, we need an empty info table.
mkEmptyContInfoTable :: CLabel -> CmmInfoTable
mkEmptyContInfoTable info_lbl = CmmInfoTable info_lbl False (ProfilingInfo zero zero) rET_SMALL
                                             (ContInfo [] NoC_SRT)
    where zero = CmmInt 0 wordWidth

cmmToRawCmm :: [Cmm] -> IO [RawCmm]
cmmToRawCmm cmm = do
  info_tbl_uniques <- mkSplitUniqSupply 'i'
  return $ zipWith raw_cmm (listSplitUniqSupply info_tbl_uniques) cmm
    where
      raw_cmm uniq_supply (Cmm procs) =
          Cmm $ concat $ zipWith mkInfoTable (uniqsFromSupply uniq_supply) procs

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

mkInfoTable :: Unique -> CmmTop -> [RawCmmTop]
mkInfoTable _    (CmmData sec dat) = [CmmData sec dat]
mkInfoTable uniq (CmmProc (CmmInfo _ _ info) entry_label blocks) =
    case info of
      -- Code without an info table.  Easy.
      CmmNonInfoTable -> [CmmProc Nothing entry_label blocks]

      CmmInfoTable info_label _ (ProfilingInfo ty_prof cl_prof) type_tag type_info ->
          let ty_prof'   = makeRelativeRefTo info_label ty_prof
              cl_prof'   = makeRelativeRefTo info_label cl_prof
          in case type_info of
          -- A function entry point.
          FunInfo (ptrs, nptrs) srt fun_arity pap_bitmap slow_entry ->
              mkInfoTableAndCode info_label std_info fun_extra_bits entry_label
                                 blocks
            where
              fun_type = argDescrType pap_bitmap
              fun_extra_bits =
                 [packHalfWordsCLit fun_type fun_arity] ++
                 case pap_bitmap of
                 ArgGen liveness ->
                     (if null srt_label then [mkIntCLit 0] else srt_label) ++
                     [makeRelativeRefTo info_label $ mkLivenessCLit liveness,
                      makeRelativeRefTo info_label slow_entry]
                 _ -> srt_label
              std_info = mkStdInfoTable ty_prof' cl_prof' type_tag srt_bitmap
                                        layout
              (srt_label, srt_bitmap) = mkSRTLit info_label srt
              layout = packHalfWordsCLit ptrs nptrs

          -- A constructor.
          ConstrInfo (ptrs, nptrs) con_tag descr ->
              mkInfoTableAndCode info_label std_info [con_name] entry_label
                                 blocks
              where
                std_info = mkStdInfoTable ty_prof' cl_prof' type_tag con_tag layout
                con_name = makeRelativeRefTo info_label descr
                layout = packHalfWordsCLit ptrs nptrs
          -- A thunk.
          ThunkInfo (ptrs, nptrs) srt ->
              mkInfoTableAndCode info_label std_info srt_label entry_label
                                 blocks
              where
                std_info = mkStdInfoTable ty_prof' cl_prof' type_tag srt_bitmap layout
                (srt_label, srt_bitmap) = mkSRTLit info_label srt
                layout = packHalfWordsCLit ptrs nptrs

          -- A selector thunk.
          ThunkSelectorInfo offset _srt ->
              mkInfoTableAndCode info_label std_info [{- no SRT -}] entry_label
                                 blocks
              where
                std_info = mkStdInfoTable ty_prof' cl_prof' type_tag 0 (mkWordCLit offset)

          -- A continuation/return-point.
          ContInfo stack_layout srt ->
              liveness_data ++
              mkInfoTableAndCode info_label std_info srt_label entry_label
                                 blocks
              where
                std_info = mkStdInfoTable ty_prof' cl_prof' maybe_big_type_tag srt_bitmap
                                          (makeRelativeRefTo info_label liveness_lit)
                (liveness_lit, liveness_data, liveness_tag) =
                    mkLiveness uniq stack_layout
                maybe_big_type_tag = if type_tag == rET_SMALL
                                     then liveness_tag
                                     else type_tag
                (srt_label, srt_bitmap) = mkSRTLit info_label srt

-- Handle the differences between tables-next-to-code
-- and not tables-next-to-code
mkInfoTableAndCode :: CLabel
                   -> [CmmLit]
                   -> [CmmLit]
                   -> CLabel
                   -> ListGraph CmmStmt
                   -> [RawCmmTop]
mkInfoTableAndCode info_lbl std_info extra_bits entry_lbl blocks
  | tablesNextToCode 	-- Reverse the extra_bits; and emit the top-level proc
  = [CmmProc (Just (Statics info_lbl $ map CmmStaticLit (reverse extra_bits ++ std_info)))
             entry_lbl blocks]

  | ListGraph [] <- blocks -- No code; only the info table is significant
  =		-- Use a zero place-holder in place of the 
		-- entry-label in the info table
    [mkRODataLits info_lbl (zeroCLit : std_info ++ extra_bits)]

  | otherwise	-- Separately emit info table (with the function entry 
  =		-- point as first entry) and the entry code 
    [CmmProc Nothing entry_lbl blocks,
     mkDataLits info_lbl (CmmLabel entry_lbl : std_info ++ extra_bits)]

mkSRTLit :: CLabel
         -> C_SRT
         -> ([CmmLit],    -- srt_label
             StgHalfWord) -- srt_bitmap
mkSRTLit _          NoC_SRT = ([], 0)
mkSRTLit info_label (C_SRT lbl off bitmap) =
    ([makeRelativeRefTo info_label (cmmLabelOffW lbl off)], bitmap)

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

-- TODO: refactor to use utility functions
-- TODO: combine with CgCallConv.mkLiveness (see comment there)
mkLiveness :: Unique
           -> [Maybe LocalReg]
           -> (CmmLit, [RawCmmTop], ClosureTypeTag)
              -- ^ Returns:
              --   1. The bitmap (literal value or label)
              --   2. Large bitmap CmmData if needed
              --   3. rET_SMALL or rET_BIG
mkLiveness uniq live =
  if length bits > mAX_SMALL_BITMAP_SIZE
    -- does not fit in one word
    then (CmmLabel big_liveness, [data_lits], rET_BIG)
    -- fits in one word
    else (mkWordCLit  small_liveness, [], rET_SMALL)
  where
    mkBits [] = []
    mkBits (reg:regs) = take sizeW bits ++ mkBits regs where
        sizeW = case reg of
                  Nothing -> 1
                  Just r -> (widthInBytes (typeWidth (localRegType r)) + wORD_SIZE - 1)
                            `quot` wORD_SIZE
                            -- number of words, rounded up
        bits = repeat $ is_non_ptr reg -- True <=> Non Ptr

    is_non_ptr Nothing    = True
    is_non_ptr (Just reg) = not $ isGcPtrType (localRegType reg)

    bits :: [Bool]
    bits = mkBits live

    bitmap :: Bitmap
    bitmap = mkBitmap bits

    small_bitmap = case bitmap of 
		   []  -> 0
                   [b] -> b
		   _   -> panic "mkLiveness"
    small_liveness =
        fromIntegral (length bits) .|. (small_bitmap `shiftL` bITMAP_BITS_SHIFT)

    big_liveness = mkBitmapLabel uniq
    lits = mkWordCLit (fromIntegral (length bits)) : map mkWordCLit bitmap
    data_lits = mkRODataLits big_liveness lits

-------------------------------------------------------------------------
--
--	Generating a standard info table
--
-------------------------------------------------------------------------

-- The standard bits of an info table.  This part of the info table
-- corresponds to the StgInfoTable type defined in InfoTables.h.
--
-- Its shape varies with ticky/profiling/tables next to code etc
-- so we can't use constant offsets from Constants

mkStdInfoTable
   :: CmmLit		-- closure type descr (profiling)
   -> CmmLit		-- closure descr (profiling)
   -> StgHalfWord	-- closure type
   -> StgHalfWord	-- SRT length
   -> CmmLit		-- layout field
   -> [CmmLit]

mkStdInfoTable type_descr closure_descr cl_type srt_len layout_lit
 = 	-- Parallel revertible-black hole field
    prof_info
	-- Ticky info (none at present)
	-- Debug info (none at present)
 ++ [layout_lit, type_lit]

 where  
    prof_info 
	| opt_SccProfilingOn = [type_descr, closure_descr]
	| otherwise	     = []

    type_lit = packHalfWordsCLit cl_type srt_len

