module CmmInfo (
  mkInfoTable
) where

#include "HsVersions.h"

import Cmm
import CmmUtils

import CLabel

import Bitmap
import ClosureInfo
import CgInfoTbls
import CgCallConv
import CgUtils

import Constants
import StaticFlags
import Unique
import Panic

import Data.Bits

mkInfoTable :: Unique -> CmmTop -> [RawCmmTop]
mkInfoTable uniq (CmmData sec dat) = [CmmData sec dat]
mkInfoTable uniq (CmmProc info entry_label arguments blocks) =
    case info of
      CmmNonInfo -> [CmmProc [] entry_label arguments blocks]
      CmmInfo (ProfilingInfo ty_prof cl_prof) _ type_tag
              (FunInfo (ptrs, nptrs) srt fun_type fun_arity pap_bitmap slow_entry) ->
          mkInfoTableAndCode info_label std_info fun_extra_bits entry_label arguments blocks
          where
            fun_extra_bits =
               [packHalfWordsCLit fun_type fun_arity] ++
               srt_label ++
               case pap_bitmap of
                 ArgGen liveness ->
                     [makeRelativeRefTo info_label $ mkLivenessCLit liveness,
                      makeRelativeRefTo info_label (CmmLabel slow_entry)]
                 _ -> []
            std_info = mkStdInfoTable ty_prof cl_prof type_tag srt_bitmap layout
            info_label = entryLblToInfoLbl entry_label
            (srt_label, srt_bitmap) =
                case srt of
                  NoC_SRT -> ([], 0)
                  (C_SRT lbl off bitmap) ->
                      ([makeRelativeRefTo info_label (cmmLabelOffW lbl off)],
                       bitmap)
            layout = packHalfWordsCLit ptrs nptrs

      CmmInfo (ProfilingInfo ty_prof cl_prof) _ type_tag
              (ConstrInfo (ptrs, nptrs) con_tag descr) ->
          mkInfoTableAndCode info_label std_info [con_name] entry_label arguments blocks
          where
            std_info = mkStdInfoTable ty_prof cl_prof type_tag con_tag layout
            info_label = entryLblToInfoLbl entry_label
            con_name = makeRelativeRefTo info_label (CmmLabel descr)
            layout = packHalfWordsCLit ptrs nptrs

      CmmInfo (ProfilingInfo ty_prof cl_prof) _ type_tag
              (ThunkInfo (ptrs, nptrs) srt) ->
          mkInfoTableAndCode info_label std_info srt_label entry_label arguments blocks
          where
            std_info = mkStdInfoTable ty_prof cl_prof type_tag srt_bitmap layout
            info_label = entryLblToInfoLbl entry_label
            (srt_label, srt_bitmap) =
                case srt of
                  NoC_SRT -> ([], 0)
                  (C_SRT lbl off bitmap) ->
                      ([makeRelativeRefTo info_label (cmmLabelOffW lbl off)],
                       bitmap)
            layout = packHalfWordsCLit ptrs nptrs

      CmmInfo (ProfilingInfo ty_prof cl_prof) _ type_tag (ContInfo stack_layout srt) ->
          liveness_data ++
          mkInfoTableAndCode info_label std_info srt_label entry_label arguments blocks
          where
            std_info = mkStdInfoTable ty_prof cl_prof type_tag srt_bitmap liveness_lit
            info_label = entryLblToInfoLbl entry_label
            (liveness_lit, liveness_data) = mkLiveness uniq stack_layout
            (srt_label, srt_bitmap) =
                case srt of
                  NoC_SRT -> ([], 0)
                  (C_SRT lbl off bitmap) ->
                      ([makeRelativeRefTo info_label (cmmLabelOffW lbl off)],
                       bitmap)

mkInfoTableAndCode info_lbl std_info extra_bits entry_lbl args blocks
  | tablesNextToCode 	-- Reverse the extra_bits; and emit the top-level proc
  = [CmmProc (map CmmStaticLit (reverse extra_bits ++ std_info)) entry_lbl args blocks]

  | null blocks -- No actual code; only the info table is significant
  =		-- Use a zero place-holder in place of the 
		-- entry-label in the info table
    [mkRODataLits info_lbl (zeroCLit : std_info ++ extra_bits)]

  | otherwise	-- Separately emit info table (with the function entry 
  =		-- point as first entry) and the entry code 
    [mkDataLits info_lbl (CmmLabel entry_lbl : std_info ++ extra_bits),
     CmmProc [] entry_lbl args blocks]

-- TODO: refactor to use utility functions
mkLiveness :: Unique -> [Maybe LocalReg] -> (CmmLit, [GenCmmTop CmmStatic [CmmStatic] CmmStmt])
mkLiveness uniq live
  = if length live > mAX_SMALL_BITMAP_SIZE
    then (CmmLabel big_liveness, [data_lits]) -- does not fit in one word
    else (mkWordCLit small_liveness, []) -- fits in one word
  where
    size = length live

    bits = mkBitmap (map is_non_ptr live)
    is_non_ptr Nothing = True
    is_non_ptr (Just reg) | localRegGCFollow reg == KindNonPtr = True
    is_non_ptr (Just reg) | localRegGCFollow reg == KindPtr = False

    big_liveness = mkBitmapLabel uniq
    data_lits = mkRODataLits big_liveness lits
    lits = mkWordCLit (fromIntegral size) : map mkWordCLit bits
  
    small_liveness =
        fromIntegral size .|. (small_bits `shiftL` bITMAP_BITS_SHIFT)
    small_bits = case bits of 
		   []  -> 0
		   [b] -> fromIntegral b
		   _   -> panic "mkLiveness"
