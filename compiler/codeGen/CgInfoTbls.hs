-----------------------------------------------------------------------------
--
-- Building info tables.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module CgInfoTbls (
	emitClosureCodeAndInfoTable,
	emitInfoTableAndCode,
	emitReturnTarget, emitAlgReturnTarget,
	emitReturnInstr,
	stdInfoTableSizeB,
	entryCode, closureInfoPtr,
	getConstrTag,
        cmmGetClosureType,
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

import OldCmm
import CLabel
import Name
import Unique

import Constants
import DynFlags
import Util
import Outputable

-------------------------------------------------------------------------
--
--	Generating the info table and code for a closure
--
-------------------------------------------------------------------------

-- Here we make an info table of type 'CmmInfo'.  The concrete
-- representation as a list of 'CmmAddr' is handled later
-- in the pipeline by 'cmmToRawCmm'.

emitClosureCodeAndInfoTable :: ClosureInfo -> [CmmFormal] -> CgStmts -> Code
emitClosureCodeAndInfoTable cl_info args body
 = do   { dflags <- getDynFlags
        ; blks <- cgStmtsToBlocks body
        ; info <- mkCmmInfo cl_info
        ; emitInfoTableAndCode (entryLabelFromCI dflags cl_info) info args blks }

-- Convert from 'ClosureInfo' to 'CmmInfo'.
-- Not used for return points.  (The 'smRepClosureTypeInt' call would panic.)
mkCmmInfo :: ClosureInfo -> FCode CmmInfoTable
mkCmmInfo cl_info
  = do dflags <- getDynFlags
       return (CmmInfoTable { cit_lbl  = infoTableLabelFromCI cl_info,
                              cit_rep  = closureSMRep cl_info,
                              cit_prof = prof dflags,
                              cit_srt  = closureSRT cl_info })
  where
    prof dflags | not (dopt Opt_SccProfilingOn dflags) = NoProfilingInfo
                | otherwise = ProfilingInfo ty_descr_w8 val_descr_w8
    ty_descr_w8  = stringToWord8s (closureTypeDescr cl_info)
    val_descr_w8 = stringToWord8s (closureValDescr cl_info)

-------------------------------------------------------------------------
--
--	Generating the info table and code for a return point
--
-------------------------------------------------------------------------

-- The concrete representation as a list of 'CmmAddr' is handled later
-- in the pipeline by 'cmmToRawCmm'.

emitReturnTarget
   :: Name
   -> CgStmts			-- The direct-return code (if any)
   -> FCode CLabel
emitReturnTarget name stmts
  = do	{ srt_info   <- getSRTInfo
	; blks <- cgStmtsToBlocks stmts
        ; frame <- mkStackLayout
        ; let smrep    = mkStackRep (mkLiveness frame)
              info     = CmmInfoTable { cit_lbl  = info_lbl
                                      , cit_prof = NoProfilingInfo
                                      , cit_rep  = smrep
                                      , cit_srt  = srt_info }
        ; emitInfoTableAndCode entry_lbl info args blks
	; return info_lbl }
  where
    args      = {- trace "emitReturnTarget: missing args" -} []
    uniq      = getUnique name
    info_lbl  = mkReturnInfoLabel uniq
    entry_lbl = mkReturnPtLabel uniq

-- Build stack layout information from the state of the 'FCode' monad.
-- Should go away once 'codeGen' starts using the CPS conversion
-- pass to handle the stack.  Until then, this is really just
-- here to convert from the 'codeGen' representation of the stack
-- to the 'CmmInfo' representation of the stack.
--
-- See 'CmmInfo.mkLiveness' for where this is converted to a bitmap.

{-
This seems to be a very error prone part of the code.
It is surprisingly prone to off-by-one errors, because
it converts between offset form (codeGen) and list form (CmmInfo).
Thus a bit of explanation is in order.
Fortunately, this code should go away once the code generator
starts using the CPS conversion pass to handle the stack.

The stack looks like this:

             |             |
             |-------------|
frame_sp --> | return addr |
             |-------------|
             | dead slot   |
             |-------------|
             | live ptr b  |
             |-------------|
             | live ptr a  |
             |-------------|
real_sp  --> | return addr |
             +-------------+

Both 'frame_sp' and 'real_sp' are measured downwards
(i.e. larger frame_sp means smaller memory address).

For that frame we want a result like: [Just a, Just b, Nothing]
Note that the 'head' of the list is the top
of the stack, and that the return address
is not present in the list (it is always assumed).
-}
mkStackLayout :: FCode [Maybe LocalReg]
mkStackLayout = do
  StackUsage { realSp = real_sp,
               frameSp = frame_sp } <- getStkUsage
  binds <- getLiveStackBindings
  let frame_size = real_sp - frame_sp - retAddrSizeW
      rel_binds = reverse $ sortWith fst
                    [(offset - frame_sp - retAddrSizeW, b)
                    | (offset, b) <- binds]

  WARN( not (all (\bind -> fst bind >= 0) rel_binds),
        ppr binds $$ ppr rel_binds $$
        ppr frame_size $$ ppr real_sp $$ ppr frame_sp )
    return $ stack_layout rel_binds frame_size

stack_layout :: [(VirtualSpOffset, CgIdInfo)]
             -> WordOff
             -> [Maybe LocalReg]
stack_layout [] sizeW = replicate sizeW Nothing
stack_layout ((off, bind):binds) sizeW | off == sizeW - 1 =
  (Just stack_bind) : (stack_layout binds (sizeW - rep_size))
  where
    rep_size = cgRepSizeW (cgIdInfoArgRep bind)
    stack_bind = LocalReg unique machRep
    unique = getUnique (cgIdInfoId bind)
    machRep = argMachRep (cgIdInfoArgRep bind)
stack_layout binds@(_:_) sizeW | otherwise =
  Nothing : (stack_layout binds (sizeW - 1))

{- Another way to write the function that might be less error prone (untested)
stack_layout offsets sizeW = result
  where
    y = map (flip lookup offsets) [0..]
      -- offsets -> nothing and just (each slot is one word)
    x = take sizeW y -- set the frame size
    z = clip x -- account for multi-word slots
    result = map mk_reg z

    clip [] = []
    clip list@(x : _) = x : clip (drop count list)
      ASSERT(all isNothing (tail (take count list)))
    
    count Nothing = 1
    count (Just x) = cgRepSizeW (cgIdInfoArgRep x)

    mk_reg Nothing = Nothing
    mk_reg (Just x) = LocalReg unique machRep kind
      where
        unique = getUnique (cgIdInfoId x)
        machRep = argMachrep (cgIdInfoArgRep bind)
        kind = if isFollowableArg (cgIdInfoArgRep bind)
           then GCKindPtr
           else GCKindNonPtr
-}

emitAlgReturnTarget
	:: Name				-- Just for its unique
	-> [(ConTagZ, CgStmts)]		-- Tagged branches
	-> Maybe CgStmts		-- Default branch (if any)
	-> Int                          -- family size
	-> FCode (CLabel, SemiTaggingStuff)

emitAlgReturnTarget name branches mb_deflt fam_sz
  = do  { blks <- getCgStmts $
                    -- is the constructor tag in the node reg?
                    if isSmallFamily fam_sz
                        then do -- yes, node has constr. tag
                          let tag_expr = cmmConstrTag1 (CmmReg nodeReg)
                              branches' = [(tag+1,branch)|(tag,branch)<-branches]
                          emitSwitch tag_expr branches' mb_deflt 1 fam_sz
                        else do -- no, get tag from info table
                          dflags <- getDynFlags
                          let -- Note that ptr _always_ has tag 1
                              -- when the family size is big enough
                              untagged_ptr = cmmRegOffB nodeReg (-1)
                              tag_expr = getConstrTag dflags untagged_ptr
                          emitSwitch tag_expr branches mb_deflt 0 (fam_sz - 1)
	; lbl <- emitReturnTarget name blks
	; return (lbl, Nothing) }
		-- Nothing: the internal branches in the switch don't have
		-- global labels, so we can't use them at the 'call site'

--------------------------------
emitReturnInstr :: Maybe [GlobalReg] -> Code
emitReturnInstr live
  = do { dflags <- getDynFlags
       ; info_amode <- getSequelAmode
       ; stmtC (CmmJump (entryCode dflags info_amode) live) }

-----------------------------------------------------------------------------
--
--	Info table offsets
--
-----------------------------------------------------------------------------
	
stdInfoTableSizeW :: DynFlags -> WordOff
-- The size of a standard info table varies with profiling/ticky etc,
-- so we can't get it from Constants
-- It must vary in sync with mkStdInfoTable
stdInfoTableSizeW dflags
  = size_fixed + size_prof
  where
    size_fixed = 2	-- layout, type
    size_prof | dopt Opt_SccProfilingOn dflags = 2
              | otherwise                      = 0

stdInfoTableSizeB :: DynFlags -> ByteOff
stdInfoTableSizeB dflags = stdInfoTableSizeW dflags * wORD_SIZE

stdSrtBitmapOffset :: DynFlags -> ByteOff
-- Byte offset of the SRT bitmap half-word which is 
-- in the *higher-addressed* part of the type_lit
stdSrtBitmapOffset dflags = stdInfoTableSizeB dflags - hALF_WORD_SIZE

stdClosureTypeOffset :: DynFlags -> ByteOff
-- Byte offset of the closure type half-word 
stdClosureTypeOffset dflags = stdInfoTableSizeB dflags - wORD_SIZE

stdPtrsOffset, stdNonPtrsOffset :: DynFlags -> ByteOff
stdPtrsOffset    dflags = stdInfoTableSizeB dflags - 2*wORD_SIZE
stdNonPtrsOffset dflags = stdInfoTableSizeB dflags - 2*wORD_SIZE + hALF_WORD_SIZE

-------------------------------------------------------------------------
--
--	Accessing fields of an info table
--
-------------------------------------------------------------------------

closureInfoPtr :: CmmExpr -> CmmExpr
-- Takes a closure pointer and returns the info table pointer
closureInfoPtr e = CmmLoad e bWord

entryCode :: DynFlags -> CmmExpr -> CmmExpr
-- Takes an info pointer (the first word of a closure)
-- and returns its entry code
entryCode dflags e
 | tablesNextToCode dflags = e
 | otherwise               = CmmLoad e bWord

getConstrTag :: DynFlags -> CmmExpr -> CmmExpr
-- Takes a closure pointer, and return the *zero-indexed*
-- constructor tag obtained from the info table
-- This lives in the SRT field of the info table
-- (constructors don't need SRTs).
getConstrTag dflags closure_ptr
  = CmmMachOp (MO_UU_Conv halfWordWidth wordWidth) [infoTableConstrTag dflags info_table]
  where
    info_table = infoTable dflags (closureInfoPtr closure_ptr)

cmmGetClosureType :: DynFlags -> CmmExpr -> CmmExpr
-- Takes a closure pointer, and return the closure type
-- obtained from the info table
cmmGetClosureType dflags closure_ptr
  = CmmMachOp (MO_UU_Conv halfWordWidth wordWidth) [infoTableClosureType dflags info_table]
  where
    info_table = infoTable dflags (closureInfoPtr closure_ptr)

infoTable :: DynFlags -> CmmExpr -> CmmExpr
-- Takes an info pointer (the first word of a closure)
-- and returns a pointer to the first word of the standard-form
-- info table, excluding the entry-code word (if present)
infoTable dflags info_ptr
  | tablesNextToCode dflags = cmmOffsetB info_ptr (- stdInfoTableSizeB dflags)
  | otherwise               = cmmOffsetW info_ptr 1 -- Past the entry code pointer

infoTableConstrTag :: DynFlags -> CmmExpr -> CmmExpr
-- Takes an info table pointer (from infoTable) and returns the constr tag
-- field of the info table (same as the srt_bitmap field)
infoTableConstrTag = infoTableSrtBitmap

infoTableSrtBitmap :: DynFlags -> CmmExpr -> CmmExpr
-- Takes an info table pointer (from infoTable) and returns the srt_bitmap
-- field of the info table
infoTableSrtBitmap dflags info_tbl
  = CmmLoad (cmmOffsetB info_tbl (stdSrtBitmapOffset dflags)) bHalfWord

infoTableClosureType :: DynFlags -> CmmExpr -> CmmExpr
-- Takes an info table pointer (from infoTable) and returns the closure type
-- field of the info table.
infoTableClosureType dflags info_tbl
  = CmmLoad (cmmOffsetB info_tbl (stdClosureTypeOffset dflags)) bHalfWord

infoTablePtrs :: DynFlags -> CmmExpr -> CmmExpr
infoTablePtrs dflags info_tbl
  = CmmLoad (cmmOffsetB info_tbl (stdPtrsOffset dflags)) bHalfWord

infoTableNonPtrs :: DynFlags -> CmmExpr -> CmmExpr
infoTableNonPtrs dflags info_tbl
  = CmmLoad (cmmOffsetB info_tbl (stdNonPtrsOffset dflags)) bHalfWord

funInfoTable :: DynFlags -> CmmExpr -> CmmExpr
-- Takes the info pointer of a function,
-- and returns a pointer to the first word of the StgFunInfoExtra struct
-- in the info table.
funInfoTable dflags info_ptr
  | tablesNextToCode dflags
  = cmmOffsetB info_ptr (- stdInfoTableSizeB dflags - sIZEOF_StgFunInfoExtraRev)
  | otherwise
  = cmmOffsetW info_ptr (1 + stdInfoTableSizeW dflags)
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
	:: CLabel 		-- Label of entry or ret
        -> CmmInfoTable         -- ...the info table
        -> [CmmFormal]          -- ...args
	-> [CmmBasicBlock]	-- ...and body
	-> Code

emitInfoTableAndCode entry_ret_lbl info args blocks
  = emitProc (Just info) entry_ret_lbl args blocks

