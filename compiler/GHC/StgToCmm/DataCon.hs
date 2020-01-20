{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
--
-- Stg to C--: code generation for constructors
--
-- This module provides the support code for StgToCmm to deal with with
-- constructors on the RHSs of let(rec)s.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm.DataCon (
        cgTopRhsCon, buildDynCon, bindConArgs
    ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Stg.Syntax
import CoreSyn  ( AltCon(..) )

import GHC.StgToCmm.Monad
import GHC.StgToCmm.Env
import GHC.StgToCmm.Heap
import GHC.StgToCmm.Layout
import GHC.StgToCmm.Utils
import GHC.StgToCmm.Closure

import CmmExpr
import CmmUtils
import CLabel
import MkGraph
import SMRep
import CostCentre
import Module
import DataCon
import DynFlags
import FastString
import Id
import Name (isInternalName)
import GHC.Types.RepType (countConRepArgs)
import Literal
import Outputable
import GHC.Platform
import Util
import MonadUtils (mapMaybeM)

import Control.Monad
import Data.Char
import Data.Maybe



---------------------------------------------------------------
--      Top-level constructors
---------------------------------------------------------------

cgTopRhsCon :: DynFlags
            -> Id               -- Name of thing bound to this RHS
            -> DataCon          -- Id
            -> [NonVoid StgArg] -- Args
            -> (CgIdInfo, FCode ())
cgTopRhsCon dflags id con args
  -- See Note [Precomputed INTLIKE closures]

  -- Internal intlike replaceable value
  | Just info <- intlike_info
  , isInternalName name
  = (info, pure ())

  -- External intlike replaceable value
  | Just info <- intlike_info
  = (info, gen_code)

  -- Any other value
  | otherwise = (id_Info, gen_code)

  where
   intlike_info  = getPreComputedLFInfo dflags id con args
   id_Info       = litIdInfo dflags id (mkConLFInfo con) (CmmLabel closure_label)
   name          = idName id
   skip_code     = isJust intlike_info && isInternalName name
   caffy         = idCafInfo id -- any stgArgHasCafRefs args
   closure_label = mkClosureLabel name caffy

   gen_code =
     do { this_mod <- getModuleName
        ; when (platformOS (targetPlatform dflags) == OSMinGW32) $
              -- Windows DLLs have a problem with static cross-DLL refs.
              MASSERT( not (isDllConApp dflags this_mod con (map fromNonVoid args)) )
        ; ASSERT( args `lengthIs` countConRepArgs con ) return ()

        -- LAY IT OUT
        ; let
            (tot_wds, --  #ptr_wds + #nonptr_wds
             ptr_wds, --  #ptr_wds
             nv_args_w_offsets) =
                 mkVirtHeapOffsetsWithPadding dflags StdHeader (addArgReps args)

            mk_payload (Padding len _) = return (CmmInt 0 (widthFromBytes len))
            mk_payload (FieldOff arg _) = do
                amode <- getArgAmode arg
                case amode of
                  CmmLit lit -> return lit
                  _          -> panic "GHC.StgToCmm.DataCon.cgTopRhsCon"

            nonptr_wds = tot_wds - ptr_wds

             -- we're not really going to emit an info table, so having
             -- to make a CmmInfoTable is a bit overkill, but mkStaticClosureFields
             -- needs to poke around inside it.
            info_tbl = mkDataConInfoTable dflags con True ptr_wds nonptr_wds


        ; payload <- mapM mk_payload nv_args_w_offsets
                -- NB1: nv_args_w_offsets is sorted into ptrs then non-ptrs
                -- NB2: all the amodes should be Lits!
                --      TODO (osa): Why?

        ; let closure_rep = mkStaticClosureFields
                             dflags
                             info_tbl
                             dontCareCCS                -- Because it's static data
                             caffy                      -- Has CAF refs
                             payload

                -- BUILD THE OBJECT
        ; emitDataLits closure_label closure_rep

        ; return () }


---------------------------------------------------------------
--      Lay out and allocate non-top-level constructors
---------------------------------------------------------------

buildDynCon :: Id                 -- Name of the thing to which this constr will
                                  -- be bound
            -> Bool               -- is it genuinely bound to that name, or just
                                  -- for profiling?
            -> CostCentreStack    -- Where to grab cost centre from;
                                  -- current CCS if currentOrSubsumedCCS
            -> DataCon            -- The data constructor
            -> [NonVoid StgArg]   -- Its args
            -> FCode (CgIdInfo, FCode CmmAGraph)
               -- Return details about how to find it and initialization code
buildDynCon binder actually_bound cc con args
    = do dflags <- getDynFlags
         buildDynCon' dflags (targetPlatform dflags) binder actually_bound cc con args


buildDynCon' :: DynFlags
             -> Platform
             -> Id -> Bool
             -> CostCentreStack
             -> DataCon
             -> [NonVoid StgArg]
             -> FCode (CgIdInfo, FCode CmmAGraph)

{- We used to pass a boolean indicating whether all the
args were of size zero, so we could use a static
constructor; but I concluded that it just isn't worth it.
Now I/O uses unboxed tuples there just aren't any constructors
with all size-zero args.

The reason for having a separate argument, rather than looking at
the addr modes of the args is that we may be in a "knot", and
premature looking at the args will cause the compiler to black-hole!
-}


-------- buildDynCon': Nullary constructors --------------
-- First we deal with the case of zero-arity constructors.  They
-- will probably be unfolded, so we don't expect to see this case much,
-- if at all, but it does no harm, and sets the scene for characters.
--
-- In the case of zero-arity constructors, or, more accurately, those
-- which have exclusively size-zero (VoidRep) args, we generate no code
-- at all.

buildDynCon' dflags _ binder _ _cc con []
  | isNullaryRepDataCon con
  = return (litIdInfo dflags binder (mkConLFInfo con)
                (CmmLabel (mkClosureLabel (dataConName con) (idCafInfo binder))),
            return mkNop)

-------- buildDynCon': Intlike constructors -----------
-- See Note [Precomputed INTLIKE closures]
buildDynCon' dflags _ binder _ _cc con [arg]
  | Just lfInfo <- getPreComputedLFInfo dflags binder con [arg]
  = return (lfInfo, return mkNop)


-------- buildDynCon': the general case -----------
buildDynCon' dflags _ binder actually_bound ccs con args
  = do  { (id_info, reg) <- rhsIdInfo binder lf_info
        ; return (id_info, gen_code reg)
        }
 where
  lf_info = mkConLFInfo con

  gen_code reg
    = do  { let (tot_wds, ptr_wds, args_w_offsets)
                  = mkVirtConstrOffsets dflags (addArgReps args)
                nonptr_wds = tot_wds - ptr_wds
                info_tbl = mkDataConInfoTable dflags con False
                                ptr_wds nonptr_wds
          ; let ticky_name | actually_bound = Just binder
                           | otherwise = Nothing

          ; hp_plus_n <- allocDynClosure ticky_name info_tbl lf_info
                                          use_cc blame_cc args_w_offsets
          ; return (mkRhsInit dflags reg lf_info hp_plus_n) }
    where
      use_cc      -- cost-centre to stick in the object
        | isCurrentCCS ccs = cccsExpr
        | otherwise        = panic "buildDynCon: non-current CCS not implemented"

      blame_cc = use_cc -- cost-centre on which to blame the alloc (same)


---------------------------------------------------------------
--      Binding constructor arguments
---------------------------------------------------------------

bindConArgs :: AltCon -> LocalReg -> [NonVoid Id] -> FCode [LocalReg]
-- bindConArgs is called from cgAlt of a case
-- (bindConArgs con args) augments the environment with bindings for the
-- binders args, assuming that we have just returned from a 'case' which
-- found a con
bindConArgs (DataAlt con) base args
  = ASSERT(not (isUnboxedTupleCon con))
    do dflags <- getDynFlags
       let (_, _, args_w_offsets) = mkVirtConstrOffsets dflags (addIdReps args)
           tag = tagForCon dflags con

           -- The binding below forces the masking out of the tag bits
           -- when accessing the constructor field.
           bind_arg :: (NonVoid Id, ByteOff) -> FCode (Maybe LocalReg)
           bind_arg (arg@(NonVoid b), offset)
             | isDeadBinder b  -- See Note [Dead-binder optimisation] in GHC.StgToCmm.Expr
             = return Nothing
             | otherwise
             = do { emit $ mkTaggedObjectLoad dflags (idToReg dflags arg)
                                              base offset tag
                  ; Just <$> bindArgToReg arg }

       mapMaybeM bind_arg args_w_offsets

bindConArgs _other_con _base args
  = ASSERT( null args ) return []

---------------------------------------------------------------
--      Computing LFInfo for precomputed closures
---------------------------------------------------------------

{- Note [Precomputed INTLIKE closures]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can replace INTLIKE constructors with static preallocated ones
built into the RTS if:

* They have tag one
* Their payload fits into mIN_INTLIKE <= val && val <= mAX_INTLIKE
* Their payload is a single word.

At compile time we do this replacement in thise module.
At runtime this is done by GC.

`getPreComputedLFInfo` checks if a given con application is a valid
for replacing it with RTS closures.

We (can) only do this replacement at compile time for (module) internal references.
Static closures of exported ids will be referenced by their label
which is based on the Ids name.
We could emit a indirection but it's more efficient to emit a intlike closure
than to emit an indirection in that case.
In order to change this we would have to export how id's should be referenced
as part of interface files.

If the RHS of a con is suitable for intlike replacement at compile
time we:

* Omit the static closure for locally bound Ids.
* Omit the static closure for top level Ids if they are internal.
* Always reference the RTS closure when computing the location of the Id for
  module internal references (cg_loc in CgIdInfo).

The effect of doing this is that we reduce binary size by avoiding
redundant static closures.
In some cases this can also help with cache pressure since we will
reference the RTS closures more often instead of various static ones
in generated code.

We don't support this optimization when compiling into Windows DLLs yet
because they don't support cross package data references well.

TODO:
Since float (and double on x64) have the same heap layout we could apply the
same logic.
-}


-- Determine if a given constructor application can be replaced with a reference
-- to an RTS intlike closure.
getPreComputedLFInfo :: DynFlags -> Id -> DataCon -> [NonVoid StgArg] -> Maybe CgIdInfo
getPreComputedLFInfo dflags binder con [arg]
  | dataConTag con == 1
  , NonVoid (StgLitArg litArg) <- arg
  , platformOS (targetPlatform dflags) /= OSMinGW32
    || not (positionIndependent dflags)
  , Just val <- isIntlikeLitArg litArg
  , val <= fromIntegral (mAX_INTLIKE dflags) -- Comparisons at type Integer!
  , val >= fromIntegral (mIN_INTLIKE dflags) -- ...ditto...
  = let intlike_lbl   = mkCmmClosureLabel rtsUnitId (fsLit "stg_INTLIKE")
        val_int = fromIntegral val :: Int
        -- INTLIKE closures consist of a header and one word payload
        offsetW = (val_int - mIN_INTLIKE dflags) * (fixedHdrSizeW dflags + 1)
        intlike_amode = cmmLabelOffW dflags intlike_lbl offsetW
    in  Just $! litIdInfo dflags binder (mkConLFInfo con) intlike_amode
  where
    isIntlikeLitArg (LitNullAddr) = Just 0
    isIntlikeLitArg (LitNumber numKind val _) =
      case numKind of
          -- Always WORD sized
          LitNumInt     -> Just val
          LitNumWord    -> Just val
          -- Only word sized if wordWidth == 8 bytes
          LitNumInt64
            | wordWidth dflags == W64 -> Just val
          LitNumWord64
            | wordWidth dflags == W64 -> Just val
          _ -> Nothing
    -- Always <= WORD sized.
    isIntlikeLitArg (LitChar c) = Just (fromIntegral $ ord c)
    -- TODO: If we can compute their bits we can apply the same
    -- logic.
    isIntlikeLitArg (LitFloat _f) = Nothing
    isIntlikeLitArg (LitDouble _d) = Nothing
    isIntlikeLitArg _ = Nothing
getPreComputedLFInfo _ _ _ _ = Nothing
