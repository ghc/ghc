-----------------------------------------------------------------------------
--
-- Building info tables.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module CgInfoTbls (
	emitClosureCodeAndInfoTable,
	emitInfoTableAndCode,
	dataConTagZ,
	getSRTInfo,
	emitReturnTarget, emitAlgReturnTarget,
	emitReturnInstr,
	mkRetInfoTable,
	mkStdInfoTable,
	stdInfoTableSizeB,
	mkFunGenInfoExtraBits,
	entryCode, closureInfoPtr,
	getConstrTag,
	infoTable, infoTableClosureType,
	infoTablePtrs, infoTableNonPtrs,
	funInfoTable
  ) where


#include "HsVersions.h"

import ClosureInfo
import SMRep
import CgBindery
import CgCallConv
import CgUtils
import CgMonad

import CmmUtils
import Cmm
import MachOp
import CLabel
import StgSyn
import Name
import DataCon
import Unique
import StaticFlags

import Maybes
import Constants

import Outputable 

-------------------------------------------------------------------------
--
--	Generating the info table and code for a closure
--
-------------------------------------------------------------------------

-- Here we make a concrete info table, represented as a list of CmmAddr
-- (it can't be simply a list of Word, because the SRT field is
-- represented by a label+offset expression).

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
--	See includes/InfoTables.h

emitClosureCodeAndInfoTable :: ClosureInfo -> [LocalReg] -> CgStmts -> Code
emitClosureCodeAndInfoTable cl_info args body
 = do	{ ty_descr_lit <- 
		if opt_SccProfilingOn 
		   then mkStringCLit (closureTypeDescr cl_info)
		   else return (mkIntCLit 0)
  	; cl_descr_lit <- 
		if opt_SccProfilingOn 
		   then mkStringCLit cl_descr_string
		   else return (mkIntCLit 0)
	; let std_info = mkStdInfoTable ty_descr_lit cl_descr_lit 
					cl_type srt_len layout_lit

	; blks <- cgStmtsToBlocks body

        ; conName <-  
             if is_con
                then mkStringCLit $ fromJust conIdentity
                else return (mkIntCLit 0)

	; emitInfoTableAndCode info_lbl std_info (extra_bits conName) args blks }
  where
    info_lbl  = infoTableLabelFromCI cl_info

    cl_descr_string = closureValDescr cl_info
    cl_type = smRepClosureTypeInt (closureSMRep cl_info)

    srt = closureSRT cl_info	     
    needs_srt = needsSRT srt

    mb_con = isConstrClosure_maybe  cl_info
    is_con = isJust mb_con

    (srt_label,srt_len,conIdentity)
	= case mb_con of
	    Just con -> -- Constructors don't have an SRT
			-- We keep the *zero-indexed* tag in the srt_len
			-- field of the info table. 
			(mkIntCLit 0, fromIntegral (dataConTagZ con), Just $ dataConIdentity con) 

	    Nothing  -> -- Not a constructor
                        let (label, len) = srtLabelAndLength srt info_lbl
                        in (label, len, Nothing)

    ptrs       = closurePtrsSize cl_info
    nptrs      = size - ptrs
    size       = closureNonHdrSize cl_info
    layout_lit = packHalfWordsCLit ptrs nptrs

    extra_bits conName 
	| is_fun    = fun_extra_bits
	| is_con    = [conName]
	| needs_srt = [srt_label]
 	| otherwise = []

    maybe_fun_stuff = closureFunInfo cl_info
    is_fun = isJust maybe_fun_stuff
    (Just (arity, arg_descr)) = maybe_fun_stuff

    fun_extra_bits
	| ArgGen liveness <- arg_descr
	= [ fun_amode,
	    srt_label,
	    makeRelativeRefTo info_lbl $ mkLivenessCLit liveness, 
	    slow_entry ]
	| needs_srt = [fun_amode, srt_label]
	| otherwise = [fun_amode]

    slow_entry = makeRelativeRefTo info_lbl (CmmLabel slow_entry_label)
    slow_entry_label = mkSlowEntryLabel (closureName cl_info)

    fun_amode = packHalfWordsCLit fun_type arity
    fun_type  = argDescrType arg_descr

-- We keep the *zero-indexed* tag in the srt_len field of the info
-- table of a data constructor.
dataConTagZ :: DataCon -> ConTagZ
dataConTagZ con = dataConTag con - fIRST_TAG

-- A low-level way to generate the variable part of a fun-style info table.
-- (must match fun_extra_bits above).  Used by the C-- parser.
mkFunGenInfoExtraBits :: Int -> Int -> CmmLit -> CmmLit -> CmmLit -> [CmmLit]
mkFunGenInfoExtraBits fun_type arity srt_label liveness slow_entry
  = [ packHalfWordsCLit fun_type arity,
      srt_label,
      liveness,
      slow_entry ]

-------------------------------------------------------------------------
--
--	Generating the info table and code for a return point
--
-------------------------------------------------------------------------

--	Here's the layout of a return-point info table
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
--  * The SRT slot is only there is SRT info to record

emitReturnTarget
   :: Name
   -> CgStmts			-- The direct-return code (if any)
   -> SRT
   -> FCode CLabel
emitReturnTarget name stmts srt
  = do	{ live_slots <- getLiveStackSlots
	; liveness   <- buildContLiveness name live_slots
	; srt_info   <- getSRTInfo name srt

	; let
	      cl_type | isBigLiveness liveness = rET_BIG
                      | otherwise              = rET_SMALL
 
	      (std_info, extra_bits) = 
		   mkRetInfoTable info_lbl liveness srt_info cl_type

	; blks <- cgStmtsToBlocks stmts
	; emitInfoTableAndCode info_lbl std_info extra_bits args blks
	; return info_lbl }
  where
    args      = {- trace "emitReturnTarget: missing args" -} []
    uniq      = getUnique name
    info_lbl  = mkReturnInfoLabel uniq


mkRetInfoTable
  :: CLabel             -- info label
  -> Liveness		-- liveness
  -> C_SRT		-- SRT Info
  -> Int		-- type (eg. rET_SMALL)
  -> ([CmmLit],[CmmLit])
mkRetInfoTable info_lbl liveness srt_info cl_type
  =  (std_info, srt_slot)
  where
	(srt_label, srt_len) = srtLabelAndLength srt_info info_lbl
 
	srt_slot | needsSRT srt_info = [srt_label]
	         | otherwise         = []
 
	liveness_lit = makeRelativeRefTo info_lbl $ mkLivenessCLit liveness
	std_info = mkStdInfoTable zeroCLit zeroCLit cl_type srt_len liveness_lit

emitAlgReturnTarget
	:: Name				-- Just for its unique
	-> [(ConTagZ, CgStmts)]		-- Tagged branches
	-> Maybe CgStmts		-- Default branch (if any)
	-> SRT				-- Continuation's SRT
	-> Int                          -- family size
	-> FCode (CLabel, SemiTaggingStuff)

emitAlgReturnTarget name branches mb_deflt srt fam_sz
  = do  { blks <- getCgStmts $
		    emitSwitch tag_expr branches mb_deflt 0 (fam_sz - 1)
		-- NB: tag_expr is zero-based
	; lbl <- emitReturnTarget name blks srt 
	; return (lbl, Nothing) }
		-- Nothing: the internal branches in the switch don't have
		-- global labels, so we can't use them at the 'call site'
  where
    tag_expr = getConstrTag (CmmReg nodeReg)

--------------------------------
emitReturnInstr :: Code
emitReturnInstr 
  = do 	{ info_amode <- getSequelAmode
	; stmtC (CmmJump (entryCode info_amode) []) }

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
   -> Int		-- closure type
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
	
stdInfoTableSizeW :: WordOff
-- The size of a standard info table varies with profiling/ticky etc,
-- so we can't get it from Constants
-- It must vary in sync with mkStdInfoTable
stdInfoTableSizeW
  = size_fixed + size_prof
  where
    size_fixed = 2	-- layout, type
    size_prof | opt_SccProfilingOn = 2
	      | otherwise	   = 0

stdInfoTableSizeB = stdInfoTableSizeW * wORD_SIZE :: ByteOff

stdSrtBitmapOffset :: ByteOff
-- Byte offset of the SRT bitmap half-word which is 
-- in the *higher-addressed* part of the type_lit
stdSrtBitmapOffset = stdInfoTableSizeB - hALF_WORD_SIZE

stdClosureTypeOffset :: ByteOff
-- Byte offset of the closure type half-word 
stdClosureTypeOffset = stdInfoTableSizeB - wORD_SIZE

stdPtrsOffset, stdNonPtrsOffset :: ByteOff
stdPtrsOffset    = stdInfoTableSizeB - 2*wORD_SIZE
stdNonPtrsOffset = stdInfoTableSizeB - 2*wORD_SIZE + hALF_WORD_SIZE

-------------------------------------------------------------------------
--
--	Accessing fields of an info table
--
-------------------------------------------------------------------------

closureInfoPtr :: CmmExpr -> CmmExpr
-- Takes a closure pointer and returns the info table pointer
closureInfoPtr e = CmmLoad e wordRep

entryCode :: CmmExpr -> CmmExpr
-- Takes an info pointer (the first word of a closure)
-- and returns its entry code
entryCode e | tablesNextToCode = e
	    | otherwise	       = CmmLoad e wordRep

getConstrTag :: CmmExpr -> CmmExpr
-- Takes a closure pointer, and return the *zero-indexed*
-- constructor tag obtained from the info table
-- This lives in the SRT field of the info table
-- (constructors don't need SRTs).
getConstrTag closure_ptr 
  = CmmMachOp (MO_U_Conv halfWordRep wordRep) [infoTableConstrTag info_table]
  where
    info_table = infoTable (closureInfoPtr closure_ptr)

infoTable :: CmmExpr -> CmmExpr
-- Takes an info pointer (the first word of a closure)
-- and returns a pointer to the first word of the standard-form
-- info table, excluding the entry-code word (if present)
infoTable info_ptr
  | tablesNextToCode = cmmOffsetB info_ptr (- stdInfoTableSizeB)
  | otherwise	     = cmmOffsetW info_ptr 1	-- Past the entry code pointer

infoTableConstrTag :: CmmExpr -> CmmExpr
-- Takes an info table pointer (from infoTable) and returns the constr tag
-- field of the info table (same as the srt_bitmap field)
infoTableConstrTag = infoTableSrtBitmap

infoTableSrtBitmap :: CmmExpr -> CmmExpr
-- Takes an info table pointer (from infoTable) and returns the srt_bitmap
-- field of the info table
infoTableSrtBitmap info_tbl
  = CmmLoad (cmmOffsetB info_tbl stdSrtBitmapOffset) halfWordRep

infoTableClosureType :: CmmExpr -> CmmExpr
-- Takes an info table pointer (from infoTable) and returns the closure type
-- field of the info table.
infoTableClosureType info_tbl 
  = CmmLoad (cmmOffsetB info_tbl stdClosureTypeOffset) halfWordRep

infoTablePtrs :: CmmExpr -> CmmExpr
infoTablePtrs info_tbl 
  = CmmLoad (cmmOffsetB info_tbl stdPtrsOffset) halfWordRep

infoTableNonPtrs :: CmmExpr -> CmmExpr
infoTableNonPtrs info_tbl 
  = CmmLoad (cmmOffsetB info_tbl stdNonPtrsOffset) halfWordRep

funInfoTable :: CmmExpr -> CmmExpr
-- Takes the info pointer of a function,
-- and returns a pointer to the first word of the StgFunInfoExtra struct
-- in the info table.
funInfoTable info_ptr
  | tablesNextToCode
  = cmmOffsetB info_ptr (- stdInfoTableSizeB - sIZEOF_StgFunInfoExtraRev)
  | otherwise
  = cmmOffsetW info_ptr (1 + stdInfoTableSizeW)
				-- Past the entry code pointer

-------------------------------------------------------------------------
--
--	Emit the code for a closure (or return address)
--	and its associated info table
--
-------------------------------------------------------------------------

-- The complication here concerns whether or not we can
-- put the info table next to the code

emitInfoTableAndCode 
	:: CLabel 		-- Label of info table
	-> [CmmLit]		-- ...its invariant part
	-> [CmmLit] 		-- ...and its variant part
	-> [LocalReg]		-- ...args
	-> [CmmBasicBlock]	-- ...and body
	-> Code

emitInfoTableAndCode info_lbl std_info extra_bits args blocks
  | tablesNextToCode 	-- Reverse the extra_bits; and emit the top-level proc
  = emitProc (reverse extra_bits ++ std_info) 
	     entry_lbl args blocks
	-- NB: the info_lbl is discarded

  | null blocks -- No actual code; only the info table is significant
  =		-- Use a zero place-holder in place of the 
		-- entry-label in the info table
    emitRODataLits info_lbl (zeroCLit : std_info ++ extra_bits)

  | otherwise	-- Separately emit info table (with the function entry 
  =		-- point as first entry) and the entry code 
    do	{ emitDataLits info_lbl (CmmLabel entry_lbl : std_info ++ extra_bits)
	; emitProc [] entry_lbl args blocks }

  where
	entry_lbl = infoLblToEntryLbl info_lbl

-------------------------------------------------------------------------
--
--	Static reference tables
--
-------------------------------------------------------------------------

-- There is just one SRT for each top level binding; all the nested
-- bindings use sub-sections of this SRT.  The label is passed down to
-- the nested bindings via the monad.

getSRTInfo :: Name -> SRT -> FCode C_SRT
getSRTInfo id NoSRT = return NoC_SRT
getSRTInfo id (SRT off len bmp)
  | len > hALF_WORD_SIZE_IN_BITS || bmp == [fromIntegral srt_escape]
  = do	{ srt_lbl <- getSRTLabel
	; let srt_desc_lbl = mkSRTDescLabel id
	; emitRODataLits srt_desc_lbl
		   ( cmmLabelOffW srt_lbl off
		   : mkWordCLit (fromIntegral len)
		   : map mkWordCLit bmp)
	; return (C_SRT srt_desc_lbl 0 srt_escape) }

  | otherwise 
  = do	{ srt_lbl <- getSRTLabel
	; return (C_SRT srt_lbl off (fromIntegral (head bmp))) }
		-- The fromIntegral converts to StgHalfWord

srt_escape = (-1) :: StgHalfWord

srtLabelAndLength :: C_SRT -> CLabel -> (CmmLit, StgHalfWord)
srtLabelAndLength NoC_SRT _		
  = (zeroCLit, 0)
srtLabelAndLength (C_SRT lbl off bitmap) info_lbl
  = (makeRelativeRefTo info_lbl $ cmmLabelOffW lbl off, bitmap)

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
