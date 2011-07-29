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
	emitReturnTarget, emitAlgReturnTarget,
	emitReturnInstr,
	stdInfoTableSizeB,
	entryCode, closureInfoPtr,
	getConstrTag,
        cmmGetClosureType,
	infoTable, infoTableClosureType,
	infoTablePtrs, infoTableNonPtrs,
	funInfoTable, makeRelativeRefTo
  ) where


#include "HsVersions.h"

import ClosureInfo
import SMRep
import CgBindery
import CgCallConv
import CgUtils
import CgMonad

import OldCmmUtils
import OldCmm
import CLabel
import Name
import DataCon
import Unique
import StaticFlags

import Constants
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
 = do	{ blks <- cgStmtsToBlocks body
        ; info <- mkCmmInfo cl_info
        ; emitInfoTableAndCode (entryLabelFromCI cl_info) info args blks }

-- We keep the *zero-indexed* tag in the srt_len field of the info
-- table of a data constructor.
dataConTagZ :: DataCon -> ConTagZ
dataConTagZ con = dataConTag con - fIRST_TAG

-- Convert from 'ClosureInfo' to 'CmmInfo'.
-- Not used for return points.  (The 'smRepClosureTypeInt' call would panic.)
mkCmmInfo :: ClosureInfo -> FCode CmmInfo
mkCmmInfo cl_info = do
  prof <-
      if opt_SccProfilingOn
      then do ty_descr_lit <- mkStringCLit (closureTypeDescr cl_info)
              cl_descr_lit <- mkStringCLit (closureValDescr cl_info)
              return $ ProfilingInfo ty_descr_lit cl_descr_lit
      else return $ ProfilingInfo (mkIntCLit 0) (mkIntCLit 0)

  case cl_info of
    ConInfo { closureCon = con } -> do
       cstr <- mkByteStringCLit $ dataConIdentity con
       let conName = makeRelativeRefTo info_lbl cstr
           info = ConstrInfo (ptrs, nptrs)
                             (fromIntegral (dataConTagZ con))
                             conName
       return $ CmmInfo gc_target Nothing (CmmInfoTable (infoTableLabelFromCI cl_info) False prof cl_type info)

    ClosureInfo { closureName   = name,
                  closureLFInfo = lf_info,
                  closureSRT    = srt } ->
       return $ CmmInfo gc_target Nothing (CmmInfoTable (infoTableLabelFromCI cl_info) False prof cl_type info)
       where
         info =
             case lf_info of
               LFReEntrant _ arity _ arg_descr ->
                   FunInfo (ptrs, nptrs)
                           srt 
                           (fromIntegral arity)
                           arg_descr 
                           (CmmLabel (mkSlowEntryLabel name has_caf_refs))
               LFThunk _ _ _ (SelectorThunk offset) _ ->
                   ThunkSelectorInfo (fromIntegral offset) srt
               LFThunk _ _ _ _ _ ->
                   ThunkInfo (ptrs, nptrs) srt
               _ -> panic "unexpected lambda form in mkCmmInfo"
  where
    info_lbl = infoTableLabelFromCI cl_info
    has_caf_refs = clHasCafRefs cl_info

    cl_type  = smRepClosureTypeInt (closureSMRep cl_info)

    ptrs     = fromIntegral $ closurePtrsSize cl_info
    size     = fromIntegral $ closureNonHdrSize cl_info
    nptrs    = size - ptrs

    -- The gc_target is to inform the CPS pass when it inserts a stack check.
    -- Since that pass isn't used yet we'll punt for now.
    -- When the CPS pass is fully integrated, this should
    -- be replaced by the label that any heap check jumped to,
    -- so that branch can be shared by both the heap (from codeGen)
    -- and stack checks (from the CPS pass).
    gc_target = panic "TODO: gc_target"

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
        ; let info = CmmInfo
                       gc_target
                       Nothing
                       (CmmInfoTable info_lbl False
                        (ProfilingInfo zeroCLit zeroCLit)
                        rET_SMALL -- cmmToRawCmm may convert it to rET_BIG
                        (ContInfo frame srt_info))
        ; emitInfoTableAndCode entry_lbl info args blks
	; return info_lbl }
  where
    args      = {- trace "emitReturnTarget: missing args" -} []
    uniq      = getUnique name
    info_lbl  = mkReturnInfoLabel uniq
    entry_lbl = mkReturnPtLabel uniq

    -- The gc_target is to inform the CPS pass when it inserts a stack check.
    -- Since that pass isn't used yet we'll punt for now.
    -- When the CPS pass is fully integrated, this should
    -- be replaced by the label that any heap check jumped to,
    -- so that branch can be shared by both the heap (from codeGen)
    -- and stack checks (from the CPS pass).
    gc_target = panic "TODO: gc_target"


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
                          let -- Note that ptr _always_ has tag 1
                              -- when the family size is big enough
                              untagged_ptr = cmmRegOffB nodeReg (-1)
                              tag_expr = getConstrTag (untagged_ptr)
                          emitSwitch tag_expr branches mb_deflt 0 (fam_sz - 1)
	; lbl <- emitReturnTarget name blks
	; return (lbl, Nothing) }
		-- Nothing: the internal branches in the switch don't have
		-- global labels, so we can't use them at the 'call site'

--------------------------------
emitReturnInstr :: Code
emitReturnInstr 
  = do 	{ info_amode <- getSequelAmode
	; stmtC (CmmJump (entryCode info_amode) []) }

-----------------------------------------------------------------------------
--
--	Info table offsets
--
-----------------------------------------------------------------------------
	
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

stdInfoTableSizeB :: ByteOff
stdInfoTableSizeB = stdInfoTableSizeW * wORD_SIZE

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
closureInfoPtr e = CmmLoad e bWord

entryCode :: CmmExpr -> CmmExpr
-- Takes an info pointer (the first word of a closure)
-- and returns its entry code
entryCode e | tablesNextToCode = e
	    | otherwise	       = CmmLoad e bWord

getConstrTag :: CmmExpr -> CmmExpr
-- Takes a closure pointer, and return the *zero-indexed*
-- constructor tag obtained from the info table
-- This lives in the SRT field of the info table
-- (constructors don't need SRTs).
getConstrTag closure_ptr 
  = CmmMachOp (MO_UU_Conv halfWordWidth wordWidth) [infoTableConstrTag info_table]
  where
    info_table = infoTable (closureInfoPtr closure_ptr)

cmmGetClosureType :: CmmExpr -> CmmExpr
-- Takes a closure pointer, and return the closure type
-- obtained from the info table
cmmGetClosureType closure_ptr 
  = CmmMachOp (MO_UU_Conv halfWordWidth wordWidth) [infoTableClosureType info_table]
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
  = CmmLoad (cmmOffsetB info_tbl stdSrtBitmapOffset) bHalfWord

infoTableClosureType :: CmmExpr -> CmmExpr
-- Takes an info table pointer (from infoTable) and returns the closure type
-- field of the info table.
infoTableClosureType info_tbl 
  = CmmLoad (cmmOffsetB info_tbl stdClosureTypeOffset) bHalfWord

infoTablePtrs :: CmmExpr -> CmmExpr
infoTablePtrs info_tbl 
  = CmmLoad (cmmOffsetB info_tbl stdPtrsOffset) bHalfWord

infoTableNonPtrs :: CmmExpr -> CmmExpr
infoTableNonPtrs info_tbl 
  = CmmLoad (cmmOffsetB info_tbl stdNonPtrsOffset) bHalfWord

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
	:: CLabel 		-- Label of entry or ret
	-> CmmInfo 		-- ...the info table
	-> [CmmFormal]	-- ...args
	-> [CmmBasicBlock]	-- ...and body
	-> Code

emitInfoTableAndCode entry_ret_lbl info args blocks
  = emitProc info entry_ret_lbl args blocks

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
