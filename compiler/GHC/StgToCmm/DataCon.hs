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

import GHC.Cmm.Expr
import GHC.Cmm.Utils
import GHC.Cmm.CLabel
import GHC.Cmm.Graph
import GHC.Runtime.Layout
import CostCentre
import Module
import DataCon
import DynFlags
import FastString
import Id
import Name (isInternalName)
import GHC.Types.RepType (countConRepArgs)
import Literal
import PrelInfo
import Outputable
import GHC.Platform
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
cgTopRhsCon dflags id con args
  -- See Note [Precomputed static closures]

  -- Internal: We always refer to the existing closure,
  --           no code needed
  | Just info <- static_info
  , isInternalName name
  = (info, pure ())

  -- External: Internally use existing closure,
  --           but build closure for external users.
  | Just info <- static_info
  = (info, gen_code)

  -- Any other value
  | otherwise = (id_Info, gen_code)

  where
   static_info   = getStaticConInfo dflags id con args
   id_Info       = litIdInfo dflags id (mkConLFInfo con) (CmmLabel closure_label)
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
         buildDynCon' dflags binder actually_bound cc con args


buildDynCon' :: DynFlags
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

buildDynCon' dflags binder _ _cc con args
  | Just cgInfo <- getStaticConInfo dflags binder con args
  = return (cgInfo, return mkNop)

-------- buildDynCon': the general case -----------
buildDynCon' dflags binder actually_bound ccs con args
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

{- Note [Precomputed static closures]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can replace certain bindings with static ones
which already exist. For Char/Int closures of certain
these are built into the RTS.
There we have Char/Int closures for all values
in the range of mINT_INTLIKE .. mAX_INTLIKE (or CHARLIKE).

For zero-arity constructors we can reference the constructors
single static closure in it's defining Module.

At compile time we do this replacement in this module.
At runtime this is done by GC.

`getStaticConInfo` checks if a given constructor application
can be replaced with a reference to a static closure in the RTS

There are three cases here:
a)Bindings which we can "replace" with a reference to
  an existing closure. Reference the replacement closure
  when accessing the binding.
  We generate not code for the binding in this case.

b)Bindings which we can "replace" as in a). But we still
  generate a closure which will be referenced by modules
  importing this binding.

c)For any other binding generate a closure. Then reference
  this closure.

For externally visible bindings we must generate closures
since those may be referenced by their symbol `<name>_closure`
when imported. But we still reference the existing closures
for internal to the binding we compile to improve locality.

Overall this means we have smaller code size and better locality.

We don't support this optimization when compiling into Windows DLLs yet
because they don't support cross package data references well.
-}

{-# INLINE getStaticConInfo #-}
getStaticConInfo :: DynFlags -> Id -> DataCon -> [NonVoid StgArg] -> Maybe CgIdInfo
getStaticConInfo dflags binder con []
  -- Nullary constructors
  | isNullaryRepDataCon con
  = Just $ litIdInfo dflags binder (mkConLFInfo con)
                (CmmLabel (mkClosureLabel (dataConName con) (idCafInfo binder)))
getStaticConInfo dflags binder con [arg]
  -- Int/Char values with existing closures in the RTS
  | intClosure || charClosure
  , platformOS platform /= OSMinGW32 || not (positionIndependent dflags)
  , Just val <- getClosurePayload arg
  , inRange val
  = let intlike_lbl   = mkCmmClosureLabel rtsUnitId (fsLit label)
        val_int = fromIntegral val :: Int
        offsetW = (val_int - min_static_range) * (fixedHdrSizeW dflags + 1)
                -- INTLIKE/CHARLIKE closures consist of a header and one word payload
        static_amode = cmmLabelOffW dflags intlike_lbl offsetW
    in Just $ litIdInfo dflags binder (mkConLFInfo con) static_amode
  where
    platform = targetPlatform dflags
    intClosure = maybeIntLikeCon con
    charClosure = maybeCharLikeCon con
    getClosurePayload (NonVoid (StgLitArg (LitNumber LitNumInt val _))) = Just val
    getClosurePayload (NonVoid (StgLitArg (LitChar val))) = Just $ (fromIntegral . ord $ val)
    getClosurePayload _ = Nothing
    -- Avoid over/underflow by comparisons at type Integer!
    inRange :: Integer -> Bool
    inRange val
      = val >= min_static_range && val <= max_static_range

    min_static_range :: Num a => a
    min_static_range
      | intClosure = fromIntegral (mIN_INTLIKE dflags)
      | charClosure = fromIntegral (mIN_CHARLIKE dflags)
      | otherwise = panic "getStaticConInfo: Unknown closure type"
    max_static_range
      | intClosure = fromIntegral (mAX_INTLIKE dflags)
      | charClosure = fromIntegral (mAX_CHARLIKE dflags)
      | otherwise = panic "getStaticConInfo: Unknown closure type"
    label
      | intClosure = "stg_INTLIKE"
      | charClosure =  "stg_CHARLIKE"
      | otherwise = panic "getStaticConInfo: Unknown closure type"

getStaticConInfo _ _ _ _ = Nothing

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
