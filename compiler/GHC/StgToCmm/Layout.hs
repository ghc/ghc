{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
--
-- Building info tables.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm.Layout (
        mkArgDescr,
        emitCall, emitReturn, adjustHpBackwards,

        emitClosureProcAndInfoTable,
        emitClosureAndInfoTable,

        slowCall, directCall,

        FieldOffOrPadding(..),
        ClosureHeader(..),
        mkVirtHeapOffsets,
        mkVirtHeapOffsetsWithPadding,
        mkVirtConstrOffsets,
        mkVirtConstrSizes,
        getHpRelOffset,

        ArgRep(..), toArgRep, argRepSizeW, -- re-exported from GHC.StgToCmm.ArgRep
        getArgAmode, getNonVoidArgAmodes
  ) where


import GHC.Prelude hiding ((<*>))

import GHC.Driver.Session
import GHC.Driver.Ppr

import GHC.StgToCmm.Closure
import GHC.StgToCmm.Env
import GHC.StgToCmm.ArgRep -- notably: ( slowCallPattern )
import GHC.StgToCmm.Ticky
import GHC.StgToCmm.Monad
import GHC.StgToCmm.Lit
import GHC.StgToCmm.Utils

import GHC.Cmm.Graph
import GHC.Runtime.Heap.Layout
import GHC.Cmm.BlockId
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.Info
import GHC.Cmm.CLabel
import GHC.Stg.Syntax
import GHC.Types.Id
import GHC.Core.TyCon    ( PrimRep(..), primRepSizeB )
import GHC.Types.Basic   ( RepArity )
import GHC.Platform
import GHC.Platform.Profile
import GHC.Unit

import GHC.Utils.Misc
import Data.List (mapAccumL, partition)
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Constants (debugIsOn)
import GHC.Data.FastString
import Control.Monad

------------------------------------------------------------------------
--                Call and return sequences
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
  = do { profile   <- getProfile
       ; platform  <- getPlatform
       ; sequel    <- getSequel
       ; updfr_off <- getUpdFrameOff
       ; case sequel of
           Return ->
             do { adjustHpBackwards
                ; let e = CmmLoad (CmmStackSlot Old updfr_off) (gcWord platform)
                ; emit (mkReturn profile (entryCode platform e) results updfr_off)
                }
           AssignTo regs adjust ->
             do { when adjust adjustHpBackwards
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
   -> [CmmExpr] -> FCode ReturnKind
emitCallWithExtraStack (callConv, retConv) fun args extra_stack
  = do  { profile <- getProfile
        ; adjustHpBackwards
        ; sequel <- getSequel
        ; updfr_off <- getUpdFrameOff
        ; case sequel of
            Return -> do
              emit $ mkJumpExtra profile callConv fun args updfr_off extra_stack
              return AssignedDirectly
            AssignTo res_regs _ -> do
              k <- newBlockId
              let area = Young k
                  (off, _, copyin) = copyInOflow profile retConv area res_regs []
                  copyout = mkCallReturnsTo profile fun callConv args k off updfr_off
                                   extra_stack
              tscope <- getTickScope
              emit (copyout <*> mkLabel k tscope <*> copyin)
              return (ReturnedTo k off)
      }


adjustHpBackwards :: FCode ()
-- This function adjusts the heap pointer just before a tail call or
-- return.  At a call or return, the virtual heap pointer may be less
-- than the real Hp, because the latter was advanced to deal with
-- the worst-case branch of the code, and we may be in a better-case
-- branch.  In that case, move the real Hp *back* and retract some
-- ticky allocation count.
--
-- It *does not* deal with high-water-mark adjustment.  That's done by
-- functions which allocate heap.
adjustHpBackwards
  = do  { hp_usg <- getHpUsage
        ; let rHp = realHp hp_usg
              vHp = virtHp hp_usg
              adjust_words = vHp -rHp
        ; new_hp <- getHpRelOffset vHp

        ; emit (if adjust_words == 0
                then mkNop
                else mkAssign hpReg new_hp) -- Generates nothing when vHp==rHp

        ; tickyAllocHeap False adjust_words -- ...ditto

        ; setRealHp vHp
        }


-------------------------------------------------------------------------
--        Making calls: directCall and slowCall
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
  = do  dflags <- getDynFlags
        profile <- getProfile
        let platform = profilePlatform profile
        argsreps <- getArgRepsAmodes stg_args
        let (rts_fun, arity) = slowCallPattern (map fst argsreps)

        (r, slow_code) <- getCodeR $ do
           r <- direct_call "slow_call" NativeNodeCall
                 (mkRtsApFastLabel rts_fun) arity ((P,Just fun):argsreps)
           emitComment $ mkFastString ("slow_call for " ++
                                      showSDoc dflags (pdoc platform fun) ++
                                      " with pat " ++ unpackFS rts_fun)
           return r

        -- Note [avoid intermediate PAPs]
        let n_args = length stg_args
        if n_args > arity && optLevel dflags >= 2
           then do
             ptr_opts <- getPtrOpts
             funv <- (CmmReg . CmmLocal) `fmap` assignTemp fun
             fun_iptr <- (CmmReg . CmmLocal) `fmap`
                    assignTemp (closureInfoPtr ptr_opts (cmmUntag platform funv))

             -- ToDo: we could do slightly better here by reusing the
             -- continuation from the slow call, which we have in r.
             -- Also we'd like to push the continuation on the stack
             -- before the branch, so that we only get one copy of the
             -- code that saves all the live variables across the
             -- call, but that might need some improvements to the
             -- special case in the stack layout code to handle this
             -- (see Note [diamond proc point]).

             fast_code <- getCode $
                emitCall (NativeNodeCall, NativeReturn)
                  (entryCode platform fun_iptr)
                  (nonVArgs ((P,Just funv):argsreps))

             slow_lbl <- newBlockId
             fast_lbl <- newBlockId
             is_tagged_lbl <- newBlockId
             end_lbl <- newBlockId

             let correct_arity = cmmEqWord platform (funInfoArity profile fun_iptr)
                                                    (mkIntExpr platform n_args)

             tscope <- getTickScope
             emit (mkCbranch (cmmIsTagged platform funv)
                             is_tagged_lbl slow_lbl (Just True)
                   <*> mkLabel is_tagged_lbl tscope
                   <*> mkCbranch correct_arity fast_lbl slow_lbl (Just True)
                   <*> mkLabel fast_lbl tscope
                   <*> fast_code
                   <*> mkBranch end_lbl
                   <*> mkLabel slow_lbl tscope
                   <*> slow_code
                   <*> mkLabel end_lbl tscope)
             return r

           else do
             emit slow_code
             return r


-- Note [avoid intermediate PAPs]
--
-- A slow call which needs multiple generic apply patterns will be
-- almost guaranteed to create one or more intermediate PAPs when
-- applied to a function that takes the correct number of arguments.
-- We try to avoid this situation by generating code to test whether
-- we are calling a function with the correct number of arguments
-- first, i.e.:
--
--   if (TAG(f) != 0} {  // f is not a thunk
--      if (f->info.arity == n) {
--         ... make a fast call to f ...
--      }
--   }
--   ... otherwise make the slow call ...
--
-- We *only* do this when the call requires multiple generic apply
-- functions, which requires pushing extra stack frames and probably
-- results in intermediate PAPs.  (I say probably, because it might be
-- that we're over-applying a function, but that seems even less
-- likely).
--
-- This very rarely applies, but if it does happen in an inner loop it
-- can have a severe impact on performance (#6084).


--------------
direct_call :: String
            -> Convention     -- e.g. NativeNodeCall or NativeDirectCall
            -> CLabel -> RepArity
            -> [(ArgRep,Maybe CmmExpr)] -> FCode ReturnKind
direct_call caller call_conv lbl arity args
  | debugIsOn && args `lengthLessThan` real_arity  -- Too few args
  = do -- Caller should ensure that there enough args!
       platform <- getPlatform
       pprPanic "direct_call" $
            text caller <+> ppr arity <+>
            pdoc platform lbl <+> ppr (length args) <+>
            pdoc platform (map snd args) <+> ppr (map fst args)

  | null rest_args  -- Precisely the right number of arguments
  = emitCall (call_conv, NativeReturn) target (nonVArgs args)

  | otherwise       -- Note [over-saturated calls]
  = do dflags <- getDynFlags
       emitCallWithExtraStack (call_conv, NativeReturn)
                              target
                              (nonVArgs fast_args)
                              (nonVArgs (stack_args dflags))
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
getArgRepsAmodes args = do
   platform <- profilePlatform <$> getProfile
   mapM (getArgRepAmode platform) args
  where getArgRepAmode platform arg
           | V <- rep  = return (V, Nothing)
           | otherwise = do expr <- getArgAmode (NonVoid arg)
                            return (rep, Just expr)
           where rep = toArgRep platform (argPrimRep arg)

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
  | sccProfilingEnabled dflags
              = save_cccs ++ this_pat ++ slowArgs dflags rest_args
  | otherwise =              this_pat ++ slowArgs dflags rest_args
  where
    (arg_pat, n)            = slowCallPattern (map fst args)
    (call_args, rest_args)  = splitAt n args

    stg_ap_pat = mkCmmRetInfoLabel rtsUnitId arg_pat
    this_pat   = (N, Just (mkLblExpr stg_ap_pat)) : call_args
    save_cccs  = [(N, Just (mkLblExpr save_cccs_lbl)), (N, Just cccsExpr)]
    save_cccs_lbl = mkCmmRetInfoLabel rtsUnitId (fsLit "stg_restore_cccs")

-------------------------------------------------------------------------
----        Laying out objects on the heap and stack
-------------------------------------------------------------------------

-- The heap always grows upwards, so hpRel is easy to compute
hpRel :: VirtualHpOffset         -- virtual offset of Hp
      -> VirtualHpOffset         -- virtual offset of The Thing
      -> WordOff                -- integer word offset
hpRel hp off = off - hp

getHpRelOffset :: VirtualHpOffset -> FCode CmmExpr
-- See Note [Virtual and real heap pointers] in GHC.StgToCmm.Monad
getHpRelOffset virtual_offset
  = do platform <- getPlatform
       hp_usg <- getHpUsage
       return (cmmRegOffW platform hpReg (hpRel (realHp hp_usg) virtual_offset))

data FieldOffOrPadding a
    = FieldOff (NonVoid a) -- Something that needs an offset.
               ByteOff     -- Offset in bytes.
    | Padding ByteOff  -- Length of padding in bytes.
              ByteOff  -- Offset in bytes.

-- | Used to tell the various @mkVirtHeapOffsets@ functions what kind
-- of header the object has.  This will be accounted for in the
-- offsets of the fields returned.
data ClosureHeader
  = NoHeader
  | StdHeader
  | ThunkHeader

mkVirtHeapOffsetsWithPadding
  :: Profile
  -> ClosureHeader            -- What kind of header to account for
  -> [NonVoid (PrimRep, a)]   -- Things to make offsets for
  -> ( WordOff                -- Total number of words allocated
     , WordOff                -- Number of words allocated for *pointers*
     , [FieldOffOrPadding a]  -- Either an offset or padding.
     )

-- Things with their offsets from start of object in order of
-- increasing offset; BUT THIS MAY BE DIFFERENT TO INPUT ORDER
-- First in list gets lowest offset, which is initial offset + 1.
--
-- mkVirtHeapOffsetsWithPadding always returns boxed things with smaller offsets
-- than the unboxed things

mkVirtHeapOffsetsWithPadding profile header things =
    assert (not (any (isVoidRep . fst . fromNonVoid) things))
    ( tot_wds
    , bytesToWordsRoundUp platform bytes_of_ptrs
    , concat (ptrs_w_offsets ++ non_ptrs_w_offsets) ++ final_pad
    )
  where
    platform = profilePlatform profile
    hdr_words = case header of
      NoHeader -> 0
      StdHeader -> fixedHdrSizeW profile
      ThunkHeader -> thunkHdrSize profile
    hdr_bytes = wordsToBytes platform hdr_words

    (ptrs, non_ptrs) = partition (isGcPtrRep . fst . fromNonVoid) things

    (bytes_of_ptrs, ptrs_w_offsets) =
       mapAccumL computeOffset 0 ptrs
    (tot_bytes, non_ptrs_w_offsets) =
       mapAccumL computeOffset bytes_of_ptrs non_ptrs

    tot_wds = bytesToWordsRoundUp platform tot_bytes

    final_pad_size = tot_wds * word_size - tot_bytes
    final_pad
        | final_pad_size > 0 = [(Padding final_pad_size
                                         (hdr_bytes + tot_bytes))]
        | otherwise          = []

    word_size = platformWordSizeInBytes platform

    computeOffset bytes_so_far nv_thing =
        (new_bytes_so_far, with_padding field_off)
      where
        (rep, thing) = fromNonVoid nv_thing

        -- Size of the field in bytes.
        !sizeB = primRepSizeB platform rep

        -- Align the start offset (eg, 2-byte value should be 2-byte aligned).
        -- But not more than to a word.
        !align = min word_size sizeB
        !start = roundUpTo bytes_so_far align
        !padding = start - bytes_so_far

        -- Final offset is:
        --   size of header + bytes_so_far + padding
        !final_offset = hdr_bytes + bytes_so_far + padding
        !new_bytes_so_far = start + sizeB
        field_off = FieldOff (NonVoid thing) final_offset

        with_padding field_off
            | padding == 0 = [field_off]
            | otherwise    = [ Padding padding (hdr_bytes + bytes_so_far)
                             , field_off
                             ]


mkVirtHeapOffsets
  :: Profile
  -> ClosureHeader            -- What kind of header to account for
  -> [NonVoid (PrimRep,a)]    -- Things to make offsets for
  -> (WordOff,                -- _Total_ number of words allocated
      WordOff,                -- Number of words allocated for *pointers*
      [(NonVoid a, ByteOff)])
mkVirtHeapOffsets profile header things =
    ( tot_wds
    , ptr_wds
    , [ (field, offset) | (FieldOff field offset) <- things_offsets ]
    )
  where
   (tot_wds, ptr_wds, things_offsets) =
       mkVirtHeapOffsetsWithPadding profile header things

-- | Just like mkVirtHeapOffsets, but for constructors
mkVirtConstrOffsets
  :: Profile -> [NonVoid (PrimRep, a)]
  -> (WordOff, WordOff, [(NonVoid a, ByteOff)])
mkVirtConstrOffsets profile = mkVirtHeapOffsets profile StdHeader

-- | Just like mkVirtConstrOffsets, but used when we don't have the actual
-- arguments. Useful when e.g. generating info tables; we just need to know
-- sizes of pointer and non-pointer fields.
mkVirtConstrSizes :: Profile -> [NonVoid PrimRep] -> (WordOff, WordOff)
mkVirtConstrSizes profile field_reps
  = (tot_wds, ptr_wds)
  where
    (tot_wds, ptr_wds, _) =
       mkVirtConstrOffsets profile
         (map (\nv_rep -> NonVoid (fromNonVoid nv_rep, ())) field_reps)

-------------------------------------------------------------------------
--
--        Making argument descriptors
--
--  An argument descriptor describes the layout of args on the stack,
--  both for         * GC (stack-layout) purposes, and
--                * saving/restoring registers when a heap-check fails
--
-- Void arguments aren't important, therefore (contrast constructSlowCall)
--
-------------------------------------------------------------------------

-- bring in ARG_P, ARG_N, etc.
#include "rts/storage/FunTypes.h"

mkArgDescr :: Platform -> [Id] -> ArgDescr
mkArgDescr platform args
  = let arg_bits = argBits platform arg_reps
        arg_reps = filter isNonV (map (idArgRep platform) args)
           -- Getting rid of voids eases matching of standard patterns
    in case stdPattern arg_reps of
         Just spec_id -> ArgSpec spec_id
         Nothing      -> ArgGen  arg_bits

argBits :: Platform -> [ArgRep] -> [Bool]        -- True for non-ptr, False for ptr
argBits _         []           = []
argBits platform (P   : args) = False : argBits platform args
argBits platform (arg : args) = take (argRepSizeW platform arg) (repeat True)
                                 ++ argBits platform args

----------------------
stdPattern :: [ArgRep] -> Maybe Int
stdPattern reps
  = case reps of
        []    -> Just ARG_NONE        -- just void args, probably
        [N]   -> Just ARG_N
        [P]   -> Just ARG_P
        [F]   -> Just ARG_F
        [D]   -> Just ARG_D
        [L]   -> Just ARG_L
        [V16] -> Just ARG_V16
        [V32] -> Just ARG_V32
        [V64] -> Just ARG_V64

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
--        Amodes for arguments
-------------------------------------------------------------------------

getArgAmode :: NonVoid StgArg -> FCode CmmExpr
getArgAmode (NonVoid (StgVarArg var)) = idInfoToAmode <$> getCgIdInfo var
getArgAmode (NonVoid (StgLitArg lit)) = cgLit lit

getNonVoidArgAmodes :: [StgArg] -> FCode [CmmExpr]
-- NB: Filters out void args,
--     so the result list may be shorter than the argument list
getNonVoidArgAmodes [] = return []
getNonVoidArgAmodes (arg:args)
  | isVoidRep (argPrimRep arg) = getNonVoidArgAmodes args
  | otherwise = do { amode  <- getArgAmode (NonVoid arg)
                   ; amodes <- getNonVoidArgAmodes args
                   ; return ( amode : amodes ) }

-------------------------------------------------------------------------
--
--        Generating the info table and code for a closure
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
 = do   { profile <- getProfile
        ; platform <- getPlatform
        -- Bind the binder itself, but only if it's not a top-level
        -- binding. We need non-top let-bindings to refer to the
        -- top-level binding, which this binding would incorrectly shadow.
        ; node <- if top_lvl then return $ idToReg platform (NonVoid bndr)
                  else bindToReg (NonVoid bndr) lf_info
        ; let node_points = nodeMustPointToIt profile lf_info
        ; arg_regs <- bindArgsToRegs args
        ; let args' = if node_points then (node : arg_regs) else arg_regs
              conv  = if nodeMustPointToIt profile lf_info then NativeNodeCall
                                                          else NativeDirectCall
              (offset, _, _) = mkCallEntry profile conv args' []
        ; emitClosureAndInfoTable (profilePlatform profile) info_tbl conv args' $ body (offset, node, arg_regs)
        }

-- Data constructors need closures, but not with all the argument handling
-- needed for functions. The shared part goes here.
emitClosureAndInfoTable
   :: Platform -> CmmInfoTable -> Convention -> [LocalReg] -> FCode () -> FCode ()
emitClosureAndInfoTable platform info_tbl conv args body
  = do { (_, blks) <- getCodeScoped body
       ; let entry_lbl = toEntryLbl platform (cit_lbl info_tbl)
       ; emitProcWithConvention conv (Just info_tbl) entry_lbl args blks
       }
