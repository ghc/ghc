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

module StgCmmLayout (
	mkArgDescr, 
        emitCall, emitReturn, adjustHpBackwards,

	emitClosureProcAndInfoTable,
	emitClosureAndInfoTable,

	slowCall, directCall, 

	mkVirtHeapOffsets, mkVirtConstrOffsets, getHpRelOffset, hpRel,

	stdInfoTableSizeB,
	entryCode, closureInfoPtr,
	getConstrTag,
        cmmGetClosureType,
	infoTable, infoTableClosureType,
	infoTablePtrs, infoTableNonPtrs,
	funInfoTable
  ) where


#include "HsVersions.h"

import StgCmmClosure
import StgCmmEnv
import StgCmmTicky
import StgCmmMonad
import StgCmmUtils
import StgCmmProf

import MkGraph
import SMRep
import Cmm
import CmmUtils
import CLabel
import StgSyn
import Id
import Name
import TyCon		( PrimRep(..) )
import BasicTypes	( RepArity )
import DynFlags
import Module

import Constants
import Util
import Data.List
import Outputable
import FastString

------------------------------------------------------------------------
--		Call and return sequences
------------------------------------------------------------------------

-- | Return multiple values to the sequel
--
-- If the sequel is @Return@
--
-- >     return (x,y)
--
-- If the sequel is @AssignTo [p,q]@
--
-- >    p=x; q=y;
--
emitReturn :: [CmmExpr] -> FCode ReturnKind
emitReturn results
  = do { dflags    <- getDynFlags
       ; sequel    <- getSequel
       ; updfr_off <- getUpdFrameOff
       ; case sequel of
           Return _ ->
             do { adjustHpBackwards
                ; emit (mkReturnSimple dflags results updfr_off) }
           AssignTo regs adjust ->
             do { if adjust then adjustHpBackwards else return ()
                ; emitMultiAssign  regs results }
       ; return AssignedDirectly
       }


-- | @emitCall conv fun args@ makes a call to the entry-code of @fun@,
-- using the call/return convention @conv@, passing @args@, and
-- returning the results to the current sequel.
--
emitCall :: (Convention, Convention) -> CmmExpr -> [CmmExpr] -> FCode ReturnKind
emitCall convs fun args
  = emitCallWithExtraStack convs fun args noExtraStack


-- | @emitCallWithExtraStack conv fun args stack@ makes a call to the
-- entry-code of @fun@, using the call/return convention @conv@,
-- passing @args@, pushing some extra stack frames described by
-- @stack@, and returning the results to the current sequel.
--
emitCallWithExtraStack
   :: (Convention, Convention) -> CmmExpr -> [CmmExpr]
   -> (ByteOff, [(CmmExpr,ByteOff)]) -> FCode ReturnKind
emitCallWithExtraStack (callConv, retConv) fun args extra_stack
  = do	{ dflags <- getDynFlags
        ; adjustHpBackwards
	; sequel <- getSequel
	; updfr_off <- getUpdFrameOff
        ; case sequel of
            Return _ -> do
              emit $ mkForeignJumpExtra dflags callConv fun args updfr_off extra_stack
              return AssignedDirectly
            AssignTo res_regs _ -> do
              k <- newLabelC
              let area = Young k
                  (off, copyin) = copyInOflow dflags retConv area res_regs
                  copyout = mkCallReturnsTo dflags fun callConv args k off updfr_off
                                   extra_stack
              emit (copyout <*> mkLabel k <*> copyin)
              return (ReturnedTo k off)
      }


adjustHpBackwards :: FCode ()
-- This function adjusts and heap pointers just before a tail call or
-- return.  At a call or return, the virtual heap pointer may be less 
-- than the real Hp, because the latter was advanced to deal with 
-- the worst-case branch of the code, and we may be in a better-case 
-- branch.  In that case, move the real Hp *back* and retract some 
-- ticky allocation count.
--
-- It *does not* deal with high-water-mark adjustment.
-- That's done by functions which allocate heap.
adjustHpBackwards
  = do	{ hp_usg <- getHpUsage
	; let rHp = realHp hp_usg
	      vHp = virtHp hp_usg
	      adjust_words = vHp -rHp
	; new_hp <- getHpRelOffset vHp

	; emit (if adjust_words == 0
		then mkNop
		else mkAssign hpReg new_hp)	-- Generates nothing when vHp==rHp

	; tickyAllocHeap adjust_words		-- ...ditto

	; setRealHp vHp
	}


-------------------------------------------------------------------------
--	Making calls: directCall and slowCall
-------------------------------------------------------------------------

-- General plan is:
--   - we'll make *one* fast call, either to the function itself
--     (directCall) or to stg_ap_<pat>_fast (slowCall)
--     Any left-over arguments will be pushed on the stack,
--
--     e.g. Sp[old+8]  = arg1
--          Sp[old+16] = arg2
--          Sp[old+32] = stg_ap_pp_info
--          R2 = arg3
--          R3 = arg4
--          call f() return to Nothing updfr_off: 32


directCall :: Convention -> CLabel -> RepArity -> [StgArg] -> FCode ReturnKind
-- (directCall f n args)
-- calls f(arg1, ..., argn), and applies the result to the remaining args
-- The function f has arity n, and there are guaranteed at least n args
-- Both arity and args include void args
directCall conv lbl arity stg_args
  = do  { argreps <- getArgRepsAmodes stg_args
        ; direct_call "directCall" conv lbl arity argreps }


slowCall :: CmmExpr -> [StgArg] -> FCode ReturnKind
-- (slowCall fun args) applies fun to args, returning the results to Sequel
slowCall fun stg_args 
  = do  { dflags <- getDynFlags
        ; argsreps <- getArgRepsAmodes stg_args
        ; let (rts_fun, arity) = slowCallPattern (map fst argsreps)
        ; r <- direct_call "slow_call" NativeNodeCall
                 (mkRtsApFastLabel rts_fun) arity ((P,Just fun):argsreps)
        ; emitComment $ mkFastString ("slow_call for " ++
                                      showSDoc dflags (ppr fun) ++
                                      " with pat " ++ unpackFS rts_fun)
        ; return r
        }


--------------
direct_call :: String
            -> Convention     -- e.g. NativeNodeCall or NativeDirectCall
            -> CLabel -> RepArity
            -> [(ArgRep,Maybe CmmExpr)] -> FCode ReturnKind
direct_call caller call_conv lbl arity args
  | debugIsOn && real_arity > length args  -- Too few args
  = do -- Caller should ensure that there enough args!
       pprPanic "direct_call" $
            text caller <+> ppr arity <+>
            ppr lbl <+> ppr (length args) <+>
            ppr (map snd args) <+> ppr (map fst args)

  | null rest_args  -- Precisely the right number of arguments
  = emitCall (call_conv, NativeReturn) target (nonVArgs args)

  | otherwise       -- Note [over-saturated calls]
  = do dflags <- getDynFlags
       emitCallWithExtraStack (call_conv, NativeReturn)
                              target
                              (nonVArgs fast_args)
                              (mkStkOffsets (stack_args dflags))
  where
    target = CmmLit (CmmLabel lbl)
    (fast_args, rest_args) = splitAt real_arity args
    stack_args dflags = slowArgs dflags rest_args
    real_arity = case call_conv of
                   NativeNodeCall -> arity+1
                   _              -> arity


-- When constructing calls, it is easier to keep the ArgReps and the
-- CmmExprs zipped together.  However, a void argument has no
-- representation, so we need to use Maybe CmmExpr (the alternative of
-- using zeroCLit or even undefined would work, but would be ugly).
--
getArgRepsAmodes :: [StgArg] -> FCode [(ArgRep, Maybe CmmExpr)]
getArgRepsAmodes = mapM getArgRepAmode
  where getArgRepAmode arg
           | V <- rep  = return (V, Nothing)
           | otherwise = do expr <- getArgAmode (NonVoid arg)
                            return (rep, Just expr)
           where rep = toArgRep (argPrimRep arg)

nonVArgs :: [(ArgRep, Maybe CmmExpr)] -> [CmmExpr]
nonVArgs [] = []
nonVArgs ((_,Nothing)  : args) = nonVArgs args
nonVArgs ((_,Just arg) : args) = arg : nonVArgs args

{-
Note [over-saturated calls]

The natural thing to do for an over-saturated call would be to call
the function with the correct number of arguments, and then apply the
remaining arguments to the value returned, e.g.

  f a b c d   (where f has arity 2)
  -->
  r = call f(a,b)
  call r(c,d)

but this entails
  - saving c and d on the stack
  - making a continuation info table
  - at the continuation, loading c and d off the stack into regs
  - finally, call r

Note that since there are a fixed number of different r's
(e.g.  stg_ap_pp_fast), we can also pre-compile continuations
that correspond to each of them, rather than generating a fresh
one for each over-saturated call.

Not only does this generate much less code, it is faster too.  We will
generate something like:

Sp[old+16] = c
Sp[old+24] = d
Sp[old+32] = stg_ap_pp_info
call f(a,b) -- usual calling convention

For the purposes of the CmmCall node, we count this extra stack as
just more arguments that we are passing on the stack (cml_args).
-}

-- | 'slowArgs' takes a list of function arguments and prepares them for
-- pushing on the stack for "extra" arguments to a function which requires
-- fewer arguments than we currently have.
slowArgs :: DynFlags -> [(ArgRep, Maybe CmmExpr)] -> [(ArgRep, Maybe CmmExpr)]
slowArgs _ [] = []
slowArgs dflags args -- careful: reps contains voids (V), but args does not
  | dopt Opt_SccProfilingOn dflags
              = save_cccs ++ this_pat ++ slowArgs dflags rest_args
  | otherwise =              this_pat ++ slowArgs dflags rest_args
  where
    (arg_pat, n)            = slowCallPattern (map fst args)
    (call_args, rest_args)  = splitAt n args

    stg_ap_pat = mkCmmRetInfoLabel rtsPackageId arg_pat
    this_pat   = (N, Just (mkLblExpr stg_ap_pat)) : call_args
    save_cccs  = [(N, Just (mkLblExpr save_cccs_lbl)), (N, Just curCCS)]
    save_cccs_lbl = mkCmmRetInfoLabel rtsPackageId (fsLit "stg_restore_cccs")



-- These cases were found to cover about 99% of all slow calls:
slowCallPattern :: [ArgRep] -> (FastString, RepArity)
-- Returns the generic apply function and arity
slowCallPattern (P: P: P: P: P: P: _) = (fsLit "stg_ap_pppppp", 6)
slowCallPattern (P: P: P: P: P: _)    = (fsLit "stg_ap_ppppp", 5)
slowCallPattern (P: P: P: P: _)       = (fsLit "stg_ap_pppp", 4)
slowCallPattern (P: P: P: V: _)       = (fsLit "stg_ap_pppv", 4)
slowCallPattern (P: P: P: _)          = (fsLit "stg_ap_ppp", 3)
slowCallPattern (P: P: V: _)          = (fsLit "stg_ap_ppv", 3)
slowCallPattern (P: P: _)	      = (fsLit "stg_ap_pp", 2)
slowCallPattern (P: V: _)	      = (fsLit "stg_ap_pv", 2)
slowCallPattern (P: _)		      = (fsLit "stg_ap_p", 1)
slowCallPattern (V: _)		      = (fsLit "stg_ap_v", 1)
slowCallPattern (N: _)		      = (fsLit "stg_ap_n", 1)
slowCallPattern (F: _)		      = (fsLit "stg_ap_f", 1)
slowCallPattern (D: _)		      = (fsLit "stg_ap_d", 1)
slowCallPattern (L: _)		      = (fsLit "stg_ap_l", 1)
slowCallPattern []		      = (fsLit "stg_ap_0", 0)


-------------------------------------------------------------------------
-- Fix the byte-offsets of a bunch of things to push on the stack

-- This is used for pushing slow-call continuations.
-- See Note [over-saturated calls].

mkStkOffsets
  :: [(ArgRep, Maybe CmmExpr)]    -- things to make offsets for
  -> ( ByteOff                    -- OUTPUTS: Topmost allocated word
     , [(CmmExpr, ByteOff)] )     -- things with offsets (voids filtered out)
mkStkOffsets things
    = loop 0 [] (reverse things)
  where
    loop offset offs [] = (offset,offs)
    loop offset offs ((_,Nothing):things) = loop offset offs things
	-- ignore Void arguments
    loop offset offs ((rep,Just thing):things)
        = loop thing_off ((thing, thing_off):offs) things
	where
          thing_off = offset + argRepSizeW rep * wORD_SIZE
	    -- offset of thing is offset+size, because we're 
	    -- growing the stack *downwards* as the offsets increase.


-------------------------------------------------------------------------
--	Classifying arguments: ArgRep
-------------------------------------------------------------------------

-- ArgRep is not exported (even abstractly)
-- It's a local helper type for classification

data ArgRep = P 	-- GC Ptr
	  | N   -- One-word non-ptr
	  | L	-- Two-word non-ptr (long)
	  | V	-- Void
	  | F	-- Float
	  | D	-- Double
instance Outputable ArgRep where
  ppr P = text "P"
  ppr N = text "N"
  ppr L = text "L"
  ppr V = text "V"
  ppr F = text "F"
  ppr D = text "D"

toArgRep :: PrimRep -> ArgRep
toArgRep VoidRep   = V
toArgRep PtrRep    = P
toArgRep IntRep    = N
toArgRep WordRep   = N
toArgRep AddrRep   = N
toArgRep Int64Rep  = L
toArgRep Word64Rep = L
toArgRep FloatRep  = F
toArgRep DoubleRep = D

isNonV :: ArgRep -> Bool
isNonV V = False
isNonV _ = True

argRepSizeW :: ArgRep -> WordOff                -- Size in words
argRepSizeW N = 1
argRepSizeW P = 1
argRepSizeW F = 1
argRepSizeW L = wORD64_SIZE `quot` wORD_SIZE
argRepSizeW D = dOUBLE_SIZE `quot` wORD_SIZE
argRepSizeW V = 0

idArgRep :: Id -> ArgRep
idArgRep = toArgRep . idPrimRep

-------------------------------------------------------------------------
----	Laying out objects on the heap and stack
-------------------------------------------------------------------------

-- The heap always grows upwards, so hpRel is easy
hpRel :: VirtualHpOffset 	-- virtual offset of Hp
      -> VirtualHpOffset 	-- virtual offset of The Thing
      -> WordOff		-- integer word offset
hpRel hp off = off - hp

getHpRelOffset :: VirtualHpOffset -> FCode CmmExpr
getHpRelOffset virtual_offset
  = do	{ hp_usg <- getHpUsage
	; return (cmmRegOffW hpReg (hpRel (realHp hp_usg) virtual_offset)) }

mkVirtHeapOffsets
  :: DynFlags
  -> Bool		-- True <=> is a thunk
  -> [(PrimRep,a)]	-- Things to make offsets for
  -> (WordOff,		-- _Total_ number of words allocated
      WordOff,		-- Number of words allocated for *pointers*
      [(NonVoid a, VirtualHpOffset)])

-- Things with their offsets from start of object in order of
-- increasing offset; BUT THIS MAY BE DIFFERENT TO INPUT ORDER
-- First in list gets lowest offset, which is initial offset + 1.
--
-- Void arguments are removed, so output list may be shorter than
-- input list
--
-- mkVirtHeapOffsets always returns boxed things with smaller offsets
-- than the unboxed things

mkVirtHeapOffsets dflags is_thunk things
  = let non_void_things		      = filterOut (isVoidRep . fst)  things
	(ptrs, non_ptrs)    	      = partition (isGcPtrRep . fst) non_void_things
    	(wds_of_ptrs, ptrs_w_offsets) = mapAccumL computeOffset 0 ptrs
	(tot_wds, non_ptrs_w_offsets) = mapAccumL computeOffset wds_of_ptrs non_ptrs
    in
    (tot_wds, wds_of_ptrs, ptrs_w_offsets ++ non_ptrs_w_offsets)
  where
    hdr_size | is_thunk   = thunkHdrSize dflags
             | otherwise  = fixedHdrSize dflags

    computeOffset wds_so_far (rep, thing)
      = (wds_so_far + argRepSizeW (toArgRep rep), 
	 (NonVoid thing, hdr_size + wds_so_far))

mkVirtConstrOffsets :: DynFlags -> [(PrimRep,a)] -> (WordOff, WordOff, [(NonVoid a, VirtualHpOffset)])
-- Just like mkVirtHeapOffsets, but for constructors
mkVirtConstrOffsets dflags = mkVirtHeapOffsets dflags False


-------------------------------------------------------------------------
--
--	Making argument descriptors
--
--  An argument descriptor describes the layout of args on the stack,
--  both for 	* GC (stack-layout) purposes, and 
--		* saving/restoring registers when a heap-check fails
--
-- Void arguments aren't important, therefore (contrast constructSlowCall)
--
-------------------------------------------------------------------------

-- bring in ARG_P, ARG_N, etc.
#include "../includes/rts/storage/FunTypes.h"

mkArgDescr :: Name -> [Id] -> FCode ArgDescr
mkArgDescr _nm args 
  = case stdPattern arg_reps of
	Just spec_id -> return (ArgSpec spec_id)
	Nothing      -> return (ArgGen arg_bits)
  where
    arg_bits = argBits arg_reps
    arg_reps = filter isNonV (map idArgRep args)
	-- Getting rid of voids eases matching of standard patterns

argBits :: [ArgRep] -> [Bool]	-- True for non-ptr, False for ptr
argBits [] 		= []
argBits (P   : args) = False : argBits args
argBits (arg : args) = take (argRepSizeW arg) (repeat True) ++ argBits args

----------------------
stdPattern :: [ArgRep] -> Maybe StgHalfWord
stdPattern reps 
  = case reps of
	[]  -> Just ARG_NONE	-- just void args, probably
	[N] -> Just ARG_N
	[P] -> Just ARG_P
	[F] -> Just ARG_F
	[D] -> Just ARG_D
	[L] -> Just ARG_L

	[N,N] -> Just ARG_NN
	[N,P] -> Just ARG_NP
	[P,N] -> Just ARG_PN
	[P,P] -> Just ARG_PP

	[N,N,N] -> Just ARG_NNN
	[N,N,P] -> Just ARG_NNP
	[N,P,N] -> Just ARG_NPN
	[N,P,P] -> Just ARG_NPP
	[P,N,N] -> Just ARG_PNN
	[P,N,P] -> Just ARG_PNP
	[P,P,N] -> Just ARG_PPN
	[P,P,P] -> Just ARG_PPP

	[P,P,P,P]     -> Just ARG_PPPP
	[P,P,P,P,P]   -> Just ARG_PPPPP
	[P,P,P,P,P,P] -> Just ARG_PPPPPP
	
	_ -> Nothing

-------------------------------------------------------------------------
--
--	Generating the info table and code for a closure
--
-------------------------------------------------------------------------

-- Here we make an info table of type 'CmmInfo'.  The concrete
-- representation as a list of 'CmmAddr' is handled later
-- in the pipeline by 'cmmToRawCmm'.
-- When loading the free variables, a function closure pointer may be tagged,
-- so we must take it into account.

emitClosureProcAndInfoTable :: Bool                    -- top-level? 
                            -> Id                      -- name of the closure
                            -> LambdaFormInfo
                            -> CmmInfoTable
                            -> [NonVoid Id]            -- incoming arguments
                            -> ((Int, LocalReg, [LocalReg]) -> FCode ()) -- function body
                            -> FCode ()
emitClosureProcAndInfoTable top_lvl bndr lf_info info_tbl args body
 = do   {
        -- Bind the binder itself, but only if it's not a top-level
        -- binding. We need non-top let-bindings to refer to the
        -- top-level binding, which this binding would incorrectly shadow.
        ; node <- if top_lvl then return $ idToReg (NonVoid bndr)
                  else bindToReg (NonVoid bndr) lf_info
        ; dflags <- getDynFlags
        ; let node_points = nodeMustPointToIt dflags lf_info
        ; arg_regs <- bindArgsToRegs args
        ; let args' = if node_points then (node : arg_regs) else arg_regs
              conv  = if nodeMustPointToIt dflags lf_info then NativeNodeCall
                                                          else NativeDirectCall
              (offset, _) = mkCallEntry dflags conv args'
        ; emitClosureAndInfoTable info_tbl conv args' $ body (offset, node, arg_regs)
        }

-- Data constructors need closures, but not with all the argument handling
-- needed for functions. The shared part goes here.
emitClosureAndInfoTable ::
  CmmInfoTable -> Convention -> [LocalReg] -> FCode () -> FCode ()
emitClosureAndInfoTable info_tbl conv args body
  = do { blks <- getCode body
       ; let entry_lbl = toEntryLbl (cit_lbl info_tbl)
       ; emitProcWithConvention conv (Just info_tbl) entry_lbl args blks
       }

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
	      | otherwise	   = 0

stdInfoTableSizeB  :: DynFlags -> ByteOff
stdInfoTableSizeB dflags = stdInfoTableSizeW dflags * wORD_SIZE :: ByteOff

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

