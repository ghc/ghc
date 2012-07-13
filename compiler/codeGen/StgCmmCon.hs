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

import StgSyn
import CoreSyn  ( AltCon(..) )

import StgCmmMonad
import StgCmmEnv
import StgCmmHeap
import StgCmmUtils
import StgCmmClosure
import StgCmmProf

import CmmExpr
import CLabel
import MkGraph
import SMRep
import CostCentre
import Module
import Constants
import DataCon
import DynFlags
import FastString
import Id
import Literal
import PrelInfo
import Outputable
import Platform
import StaticFlags
import Util

import Control.Monad
import Data.Char



---------------------------------------------------------------
--      Top-level constructors
---------------------------------------------------------------

cgTopRhsCon :: Id               -- Name of thing bound to this RHS
            -> DataCon          -- Id
            -> [StgArg]         -- Args
            -> FCode CgIdInfo
cgTopRhsCon id con args
  = do {
          dflags <- getDynFlags
        ; when (platformOS (targetPlatform dflags) == OSMinGW32) $
              -- Windows DLLs have a problem with static cross-DLL refs.
              ASSERT( not (isDllConApp dflags con args) ) return ()
        ; ASSERT( args `lengthIs` dataConRepRepArity con ) return ()

        -- LAY IT OUT
        ; let
            name          = idName id
            caffy         = idCafInfo id -- any stgArgHasCafRefs args
            closure_label = mkClosureLabel name caffy

            (tot_wds, --  #ptr_wds + #nonptr_wds
             ptr_wds, --  #ptr_wds
             nv_args_w_offsets) = mkVirtConstrOffsets (addArgReps args)

            nonptr_wds = tot_wds - ptr_wds

             -- we're not really going to emit an info table, so having
             -- to make a CmmInfoTable is a bit overkill, but mkStaticClosureFields
             -- needs to poke around inside it.
            info_tbl = mkDataConInfoTable con True ptr_wds nonptr_wds

            get_lit (arg, _offset) = do { CmmLit lit <- getArgAmode arg
                                        ; return lit }

        ; payload <- mapM get_lit nv_args_w_offsets
                -- NB1: nv_args_w_offsets is sorted into ptrs then non-ptrs
                -- NB2: all the amodes should be Lits!

        ; let closure_rep = mkStaticClosureFields
                             info_tbl
                             dontCareCCS                -- Because it's static data
                             caffy                      -- Has CAF refs
                             payload

                -- BUILD THE OBJECT
        ; emitDataLits closure_label closure_rep

                -- RETURN
        ; return $ litIdInfo id (mkConLFInfo con) (CmmLabel closure_label) }


---------------------------------------------------------------
--      Lay out and allocate non-top-level constructors
---------------------------------------------------------------

buildDynCon :: Id                 -- Name of the thing to which this constr will
                                  -- be bound
            -> CostCentreStack    -- Where to grab cost centre from;
                                  -- current CCS if currentOrSubsumedCCS
            -> DataCon            -- The data constructor
            -> [StgArg]           -- Its args
            -> FCode (CgIdInfo, CmmAGraph)
               -- Return details about how to find it and initialization code
buildDynCon binder cc con args
    = do dflags <- getDynFlags
         buildDynCon' (targetPlatform dflags) binder cc con args

buildDynCon' :: Platform
             -> Id
             -> CostCentreStack
             -> DataCon
             -> [StgArg]
             -> FCode (CgIdInfo, CmmAGraph)

{- We used to pass a boolean indicating whether all the
args were of size zero, so we could use a static
construtor; but I concluded that it just isn't worth it.
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

buildDynCon' _ binder _cc con []
  = return (litIdInfo binder (mkConLFInfo con)
                (CmmLabel (mkClosureLabel (dataConName con) (idCafInfo binder))),
            mkNop)

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
for simplicity we just always asssign to a temporary.

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

buildDynCon' platform binder _cc con [arg]
  | maybeIntLikeCon con
  , platformOS platform /= OSMinGW32 || not opt_PIC
  , StgLitArg (MachInt val) <- arg
  , val <= fromIntegral mAX_INTLIKE     -- Comparisons at type Integer!
  , val >= fromIntegral mIN_INTLIKE     -- ...ditto...
  = do  { let intlike_lbl   = mkCmmGcPtrLabel rtsPackageId (fsLit "stg_INTLIKE_closure")
              val_int = fromIntegral val :: Int
              offsetW = (val_int - mIN_INTLIKE) * (fixedHdrSize + 1)
                -- INTLIKE closures consist of a header and one word payload
              intlike_amode = cmmLabelOffW intlike_lbl offsetW
        ; return (litIdInfo binder (mkConLFInfo con) intlike_amode, mkNop) }

buildDynCon' platform binder _cc con [arg]
  | maybeCharLikeCon con
  , platformOS platform /= OSMinGW32 || not opt_PIC
  , StgLitArg (MachChar val) <- arg
  , let val_int = ord val :: Int
  , val_int <= mAX_CHARLIKE
  , val_int >= mIN_CHARLIKE
  = do  { let charlike_lbl   = mkCmmGcPtrLabel rtsPackageId (fsLit "stg_CHARLIKE_closure")
              offsetW = (val_int - mIN_CHARLIKE) * (fixedHdrSize + 1)
                -- CHARLIKE closures consist of a header and one word payload
              charlike_amode = cmmLabelOffW charlike_lbl offsetW
        ; return (litIdInfo binder (mkConLFInfo con) charlike_amode, mkNop) }

-------- buildDynCon': the general case -----------
buildDynCon' _ binder ccs con args
  = do  { let (tot_wds, ptr_wds, args_w_offsets)
                = mkVirtConstrOffsets (addArgReps args)
                -- No void args in args_w_offsets
              nonptr_wds = tot_wds - ptr_wds
              info_tbl = mkDataConInfoTable con False ptr_wds nonptr_wds
        ; (tmp, init) <- allocDynClosure info_tbl lf_info
                                         use_cc blame_cc args_w_offsets
        ; regIdInfo binder lf_info tmp init }
  where
    lf_info = mkConLFInfo con

    use_cc      -- cost-centre to stick in the object
      | isCurrentCCS ccs = curCCS
      | otherwise        = panic "buildDynCon: non-current CCS not implemented"

    blame_cc = use_cc -- cost-centre on which to blame the alloc (same)


---------------------------------------------------------------
--      Binding constructor arguments
---------------------------------------------------------------

bindConArgs :: AltCon -> LocalReg -> [Id] -> FCode [LocalReg]
-- bindConArgs is called from cgAlt of a case
-- (bindConArgs con args) augments the environment with bindings for the
-- binders args, assuming that we have just returned from a 'case' which
-- found a con
bindConArgs (DataAlt con) base args
  = ASSERT(not (isUnboxedTupleCon con))
    mapM bind_arg args_w_offsets
  where
    (_, _, args_w_offsets) = mkVirtConstrOffsets (addIdReps args)

    tag = tagForCon con

          -- The binding below forces the masking out of the tag bits
          -- when accessing the constructor field.
    bind_arg :: (NonVoid Id, VirtualHpOffset) -> FCode LocalReg
    bind_arg (arg, offset)
        = do { emit $ mkTaggedObjectLoad (idToReg arg) base offset tag
             ; bindArgToReg arg }

bindConArgs _other_con _base args
  = ASSERT( null args ) return []

