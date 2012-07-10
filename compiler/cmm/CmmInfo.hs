{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module CmmInfo (
  mkEmptyContInfoTable,
  cmmToRawCmm,
  mkInfoTable,
) where

#include "HsVersions.h"

import OldCmm as Old

import CmmUtils
import CLabel
import SMRep
import Bitmap
import Stream (Stream)
import qualified Stream

import Maybes
import Constants
import Panic
import Platform
import StaticFlags
import UniqSupply
import MonadUtils
import Util

import Data.Bits
import Data.Word

-- When we split at proc points, we need an empty info table.
mkEmptyContInfoTable :: CLabel -> CmmInfoTable
mkEmptyContInfoTable info_lbl 
  = CmmInfoTable { cit_lbl  = info_lbl
                 , cit_rep  = mkStackRep []
                 , cit_prof = NoProfilingInfo
                 , cit_srt  = NoC_SRT }

cmmToRawCmm :: Platform -> Stream IO Old.CmmGroup ()
            -> IO (Stream IO Old.RawCmmGroup ())
cmmToRawCmm platform cmms
  = do { uniqs <- mkSplitUniqSupply 'i'
       ; let do_one uniqs cmm = do
                case initUs uniqs $ concatMapM (mkInfoTable platform) cmm of
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

mkInfoTable :: Platform -> CmmDecl -> UniqSM [RawCmmDecl]
mkInfoTable _ (CmmData sec dat) 
  = return [CmmData sec dat]

mkInfoTable platform (CmmProc info entry_label blocks)
  | CmmNonInfoTable <- info   -- Code without an info table.  Easy.
  = return [CmmProc Nothing entry_label blocks]
                               
  | CmmInfoTable { cit_lbl = info_lbl } <- info
  = do { (top_decls, info_cts) <- mkInfoTableContents platform info Nothing
       ; return (top_decls  ++
                 mkInfoTableAndCode info_lbl info_cts
                                    entry_label blocks) }
  | otherwise = panic "mkInfoTable"
                  -- Patern match overlap check not clever enough

-----------------------------------------------------
type InfoTableContents = ( [CmmLit]	     -- The standard part
                         , [CmmLit] )	     -- The "extra bits"
-- These Lits have *not* had mkRelativeTo applied to them

mkInfoTableContents :: Platform
                    -> CmmInfoTable
                    -> Maybe StgHalfWord    -- Override default RTS type tag?
                    -> UniqSM ([RawCmmDecl],             -- Auxiliary top decls
                               InfoTableContents)	-- Info tbl + extra bits

mkInfoTableContents platform
                    info@(CmmInfoTable { cit_lbl  = info_lbl
                                       , cit_rep  = smrep
                                       , cit_prof = prof
                                       , cit_srt = srt }) 
                    mb_rts_tag
  | RTSRep rts_tag rep <- smrep
  = mkInfoTableContents platform info{cit_rep = rep} (Just rts_tag)
    -- Completely override the rts_tag that mkInfoTableContents would
    -- otherwise compute, with the rts_tag stored in the RTSRep
    -- (which in turn came from a handwritten .cmm file)

  | StackRep frame <- smrep
  = do { (prof_lits, prof_data) <- mkProfLits prof
       ; let (srt_label, srt_bitmap) = mkSRTLit srt
       ; (liveness_lit, liveness_data) <- mkLivenessBits frame
       ; let
             std_info = mkStdInfoTable prof_lits rts_tag srt_bitmap liveness_lit
             rts_tag | Just tag <- mb_rts_tag = tag
                     | null liveness_data     = rET_SMALL -- Fits in extra_bits
                     | otherwise              = rET_BIG   -- Does not; extra_bits is
                                                          -- a label
       ; return (prof_data ++ liveness_data, (std_info, srt_label)) }

  | HeapRep _ ptrs nonptrs closure_type <- smrep
  = do { let layout  = packHalfWordsCLit ptrs nonptrs
       ; (prof_lits, prof_data) <- mkProfLits prof
       ; let (srt_label, srt_bitmap) = mkSRTLit srt
       ; (mb_srt_field, mb_layout, extra_bits, ct_data)
                                <- mk_pieces closure_type srt_label
       ; let std_info = mkStdInfoTable prof_lits
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
      	   ; return (Just con_tag, Nothing, [descr_lit], [decl]) }

    mk_pieces Thunk srt_label
      = return (Nothing, Nothing, srt_label, [])

    mk_pieces (ThunkSelector offset) _no_srt
      = return (Just 0, Just (mkWordCLit offset), [], [])
         -- Layout known (one free var); we use the layout field for offset

    mk_pieces (Fun arity (ArgSpec fun_type)) srt_label 
      = do { let extra_bits = packHalfWordsCLit fun_type arity : srt_label
           ; return (Nothing, Nothing,  extra_bits, []) }

    mk_pieces (Fun arity (ArgGen arg_bits)) srt_label
      = do { (liveness_lit, liveness_data) <- mkLivenessBits arg_bits
           ; let fun_type | null liveness_data = aRG_GEN
                          | otherwise          = aRG_GEN_BIG
                 extra_bits = [ packHalfWordsCLit fun_type arity
                              , srt_lit, liveness_lit, slow_entry ]
           ; return (Nothing, Nothing, extra_bits, liveness_data) }
      where
        slow_entry = CmmLabel (toSlowEntryLbl info_lbl)
        srt_lit = case srt_label of
                    []          -> mkIntCLit 0
                    (lit:_rest) -> ASSERT( null _rest ) lit

    mk_pieces BlackHole _ = panic "mk_pieces: BlackHole"


mkInfoTableContents _ _ _ = panic "mkInfoTableContents"   -- NonInfoTable dealt with earlier

mkSRTLit :: C_SRT
         -> ([CmmLit],    -- srt_label, if any
             StgHalfWord) -- srt_bitmap
mkSRTLit NoC_SRT                = ([], 0)
mkSRTLit (C_SRT lbl off bitmap) = ([cmmLabelOffW lbl off], bitmap)


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

-- The value of tablesNextToCode determines the relative positioning
-- of the extra bits and the standard info table, and whether the
-- former is reversed or not.  It also decides whether pointers in the
-- info table should be expressed as offsets relative to the info
-- pointer or not (see "Position Independent Code" below.

mkInfoTableAndCode :: CLabel             -- Info table label
                   -> InfoTableContents
                   -> CLabel     	 -- Entry label
                   -> ListGraph CmmStmt  -- Entry code
                   -> [RawCmmDecl]
mkInfoTableAndCode info_lbl (std_info, extra_bits) entry_lbl blocks
  | tablesNextToCode 	-- Reverse the extra_bits; and emit the top-level proc
  = [CmmProc (Just $ Statics info_lbl $ map CmmStaticLit $
                     reverse rel_extra_bits ++ rel_std_info)
             entry_lbl blocks]

  | ListGraph [] <- blocks -- No code; only the info table is significant
  =		-- Use a zero place-holder in place of the 
		-- entry-label in the info table
    [mkRODataLits info_lbl (zeroCLit : rel_std_info ++ rel_extra_bits)]

  | otherwise	-- Separately emit info table (with the function entry 
  =		-- point as first entry) and the entry code 
    [CmmProc Nothing entry_lbl blocks,
     mkDataLits Data info_lbl (CmmLabel entry_lbl : rel_std_info ++ rel_extra_bits)]
  where
    rel_std_info   = map (makeRelativeRefTo info_lbl) std_info
    rel_extra_bits = map (makeRelativeRefTo info_lbl) extra_bits

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

makeRelativeRefTo :: CLabel -> CmmLit -> CmmLit
        
makeRelativeRefTo info_lbl (CmmLabel lbl)
  | tablesNextToCode
  = CmmLabelDiffOff lbl info_lbl 0
makeRelativeRefTo info_lbl (CmmLabelOff lbl off)
  | tablesNextToCode
  = CmmLabelDiffOff lbl info_lbl off
makeRelativeRefTo _ lit = lit


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

mkLivenessBits :: Liveness -> UniqSM (CmmLit, [RawCmmDecl])
              -- ^ Returns:
              --   1. The bitmap (literal value or label)
              --   2. Large bitmap CmmData if needed

mkLivenessBits liveness
  | n_bits > mAX_SMALL_BITMAP_SIZE    -- does not fit in one word
  = do { uniq <- getUniqueUs
       ; let bitmap_lbl = mkBitmapLabel uniq
       ; return (CmmLabel bitmap_lbl, 
                 [mkRODataLits bitmap_lbl lits]) }

  | otherwise -- Fits in one word
  = return (mkWordCLit bitmap_word, [])
  where
    n_bits = length liveness

    bitmap :: Bitmap
    bitmap = mkBitmap liveness

    small_bitmap = case bitmap of 
		     []  -> 0
                     [b] -> b
		     _   -> panic "mkLiveness"
    bitmap_word = fromIntegral n_bits
              .|. (small_bitmap `shiftL` bITMAP_BITS_SHIFT)

    lits = mkWordCLit (fromIntegral n_bits) : map mkWordCLit bitmap
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
   :: (CmmLit,CmmLit)	-- Closure type descr and closure descr  (profiling)
   -> StgHalfWord	-- Closure RTS tag 
   -> StgHalfWord	-- SRT length
   -> CmmLit		-- layout field
   -> [CmmLit]

mkStdInfoTable (type_descr, closure_descr) cl_type srt_len layout_lit
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

-------------------------------------------------------------------------
--
--      Making string literals
--
-------------------------------------------------------------------------

mkProfLits :: ProfilingInfo -> UniqSM ((CmmLit,CmmLit), [RawCmmDecl])
mkProfLits NoProfilingInfo       = return ((zeroCLit, zeroCLit), [])
mkProfLits (ProfilingInfo td cd)
  = do { (td_lit, td_decl) <- newStringLit td
       ; (cd_lit, cd_decl) <- newStringLit cd
       ; return ((td_lit,cd_lit), [td_decl,cd_decl]) }

newStringLit :: [Word8] -> UniqSM (CmmLit, GenCmmDecl CmmStatics info stmt)
newStringLit bytes
  = do { uniq <- getUniqueUs
       ; return (mkByteStringCLit uniq bytes) }

