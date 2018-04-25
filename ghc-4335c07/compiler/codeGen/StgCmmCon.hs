{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
--
-- Stg to C--: code generation for constructors
--
-- This module provides the support code for StgCmm to deal with with
-- constructors on the RHSs of let(rec)s.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module StgCmmCon (
        cgTopRhsCon, buildDynCon, bindConArgs
    ) where

#include "HsVersions.h"

import GhcPrelude

import StgSyn
import CoreSyn  ( AltCon(..) )

import StgCmmMonad
import StgCmmEnv
import StgCmmHeap
import StgCmmLayout
import StgCmmUtils
import StgCmmClosure
import StgCmmProf ( curCCS )

import CmmExpr
import CLabel
import MkGraph
import SMRep
import CostCentre
import Module
import DataCon
import DynFlags
import FastString
import Id
import RepType (countConRepArgs)
import Literal
import PrelInfo
import Outputable
import Platform
import Util
import MonadUtils (mapMaybeM)

import Control.Monad
import Data.Char



---------------------------------------------------------------
--      Top-level constructors
---------------------------------------------------------------

cgTopRhsCon :: DynFlags
            -> Id               -- Name of thing bound to this RHS
            -> DataCon          -- Id
            -> [NonVoid StgArg] -- Args
            -> (CgIdInfo, FCode ())
cgTopRhsCon dflags id con args =
    let id_info = litIdInfo dflags id (mkConLFInfo con) (CmmLabel closure_label)
    in (id_info, gen_code)
  where
   name          = idName id
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
            is_thunk = False
            (tot_wds, --  #ptr_wds + #nonptr_wds
             ptr_wds, --  #ptr_wds
             nv_args_w_offsets) =
                 mkVirtHeapOffsetsWithPadding dflags is_thunk (addArgReps args)

            mk_payload (Padding len _) = return (CmmInt 0 (widthFromBytes len))
            mk_payload (FieldOff arg _) = do
                CmmLit lit <- getArgAmode arg
                return lit

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

-------- buildDynCon': Charlike and Intlike constructors -----------
{- The following three paragraphs about @Char@-like and @Int@-like
closures are obsolete, but I don't understand the details well enough
to properly word them, sorry. I've changed the treatment of @Char@s to
be analogous to @Int@s: only a subset is preallocated, because @Char@
has now 31 bits. Only literals are handled here. -- Qrczak

Now for @Char@-like closures.  We generate an assignment of the
address of the closure to a temporary.  It would be possible simply to
generate no code, and record the addressing mode in the environment,
but we'd have to be careful if the argument wasn't a constant --- so
for simplicity we just always assign to a temporary.

Last special case: @Int@-like closures.  We only special-case the
situation in which the argument is a literal in the range
@mIN_INTLIKE@..@mAX_INTLILKE@.  NB: for @Char@-like closures we can
work with any old argument, but for @Int@-like ones the argument has
to be a literal.  Reason: @Char@ like closures have an argument type
which is guaranteed in range.

Because of this, we use can safely return an addressing mode.

We don't support this optimisation when compiling into Windows DLLs yet
because they don't support cross package data references well.
-}

buildDynCon' dflags platform binder _ _cc con [arg]
  | maybeIntLikeCon con
  , platformOS platform /= OSMinGW32 || not (positionIndependent dflags)
  , NonVoid (StgLitArg (MachInt val)) <- arg
  , val <= fromIntegral (mAX_INTLIKE dflags) -- Comparisons at type Integer!
  , val >= fromIntegral (mIN_INTLIKE dflags) -- ...ditto...
  = do  { let intlike_lbl   = mkCmmClosureLabel rtsUnitId (fsLit "stg_INTLIKE")
              val_int = fromIntegral val :: Int
              offsetW = (val_int - mIN_INTLIKE dflags) * (fixedHdrSizeW dflags + 1)
                -- INTLIKE closures consist of a header and one word payload
              intlike_amode = cmmLabelOffW dflags intlike_lbl offsetW
        ; return ( litIdInfo dflags binder (mkConLFInfo con) intlike_amode
                 , return mkNop) }

buildDynCon' dflags platform binder _ _cc con [arg]
  | maybeCharLikeCon con
  , platformOS platform /= OSMinGW32 || not (positionIndependent dflags)
  , NonVoid (StgLitArg (MachChar val)) <- arg
  , let val_int = ord val :: Int
  , val_int <= mAX_CHARLIKE dflags
  , val_int >= mIN_CHARLIKE dflags
  = do  { let charlike_lbl   = mkCmmClosureLabel rtsUnitId (fsLit "stg_CHARLIKE")
              offsetW = (val_int - mIN_CHARLIKE dflags) * (fixedHdrSizeW dflags + 1)
                -- CHARLIKE closures consist of a header and one word payload
              charlike_amode = cmmLabelOffW dflags charlike_lbl offsetW
        ; return ( litIdInfo dflags binder (mkConLFInfo con) charlike_amode
                 , return mkNop) }

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
        | isCurrentCCS ccs = curCCS
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
             | isDeadBinder b =
                 -- Do not load unused fields from objects to local variables.
                 -- (CmmSink can optimize this, but it's cheap and common enough
                 -- to handle here)
                 return Nothing
             | otherwise      = do
                 emit $ mkTaggedObjectLoad dflags (idToReg dflags arg) base offset tag
                 Just <$> bindArgToReg arg

       mapMaybeM bind_arg args_w_offsets

bindConArgs _other_con _base args
  = ASSERT( null args ) return []
