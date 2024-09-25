module GHC.Stg.Make
  ( MkStgRhs (..)
  , mkTopStgRhs
  , mkStgRhs
  , mkStgRhsCon_maybe
  , mkTopStgRhsCon_maybe
  )
where

import GHC.Prelude
import GHC.Unit.Module

import GHC.Core.DataCon
import GHC.Core.Type (Type)

import GHC.Stg.Syntax
import GHC.Stg.Utils (stripStgTicksTop)

import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.CostCentre
import GHC.Types.Demand    ( isAtMostOnceDmd )
import GHC.Types.Tickish

-- Represents the RHS of a binding for use with mk(Top)StgRhs and
-- mk(Top)StgRhsCon_maybe.
data MkStgRhs = MkStgRhs
  { rhs_args :: [Id]     -- ^ Empty for thunks
  , rhs_expr :: StgExpr  -- ^ RHS expression
  , rhs_type :: Type     -- ^ RHS type (only used in the JS backend: layering violation)
  , rhs_is_join :: !Bool -- ^ Is it a RHS for a join-point?
  }


-- Generate a top-level RHS. Any new cost centres generated for CAFs will be
-- appended to `CollectedCCs` argument.
mkTopStgRhs :: (Module -> DataCon -> [StgArg] -> Bool)
            -> Bool -> Module -> CollectedCCs
            -> Id -> MkStgRhs -> (StgRhs, CollectedCCs)
mkTopStgRhs allow_toplevel_con_app opt_AutoSccsOnIndividualCafs this_mod ccs bndr mk_rhs@(MkStgRhs bndrs rhs typ _)
  -- try to make a StgRhsCon first
  | Just rhs_con <- mkTopStgRhsCon_maybe (allow_toplevel_con_app this_mod) mk_rhs
  = ( rhs_con, ccs )

  | not (null bndrs)
  = -- The list of arguments is non-empty, so not CAF
    ( StgRhsClosure noExtFieldSilent
                    dontCareCCS
                    ReEntrant
                    bndrs rhs typ
    , ccs )

  -- Otherwise it's a CAF, see Note [Cost-centre initialization plan].
  | opt_AutoSccsOnIndividualCafs
  = ( StgRhsClosure noExtFieldSilent
                    caf_ccs
                    upd_flag [] rhs typ
    , collectCC caf_cc caf_ccs ccs )

  | otherwise
  = ( StgRhsClosure noExtFieldSilent
                    all_cafs_ccs
                    upd_flag [] rhs typ
    , ccs )

  where
    upd_flag | isAtMostOnceDmd (idDemandInfo bndr) = SingleEntry
             | otherwise                           = Updatable

    -- CAF cost centres generated for -fcaf-all
    caf_cc = mkAutoCC bndr modl
    caf_ccs = mkSingletonCCS caf_cc
           -- careful: the binder might be :Main.main,
           -- which doesn't belong to module mod_name.
           -- bug #249, tests prof001, prof002
    modl | Just m <- nameModule_maybe (idName bndr) = m
         | otherwise = this_mod

    -- default CAF cost centre
    (_, all_cafs_ccs) = getAllCAFsCC this_mod

-- Generate a non-top-level RHS. Cost-centre is always currentCCS,
-- see Note [Cost-centre initialization plan].
mkStgRhs :: Id -> MkStgRhs -> StgRhs
mkStgRhs bndr mk_rhs@(MkStgRhs bndrs rhs typ is_join)
  -- try to make a StgRhsCon first
  | Just rhs_con <- mkStgRhsCon_maybe mk_rhs
  = rhs_con

  | otherwise
  = StgRhsClosure noExtFieldSilent
                  currentCCS
                  upd_flag bndrs rhs typ
  where
    upd_flag | is_join                             = JumpedTo
             | not (null bndrs)                    = ReEntrant
             | isAtMostOnceDmd (idDemandInfo bndr) = SingleEntry
             | otherwise                           = Updatable

  {-
    SDM: disabled.  Eval/Apply can't handle functions with arity zero very
    well; and making these into simple non-updatable thunks breaks other
    assumptions (namely that they will be entered only once).

    upd_flag | isPAP env rhs  = ReEntrant
             | otherwise      = Updatable

-- Detect thunks which will reduce immediately to PAPs, and make them
-- non-updatable.  This has several advantages:
--
--         - the non-updatable thunk behaves exactly like the PAP,
--
--         - the thunk is more efficient to enter, because it is
--           specialised to the task.
--
--         - we save one update frame, one stg_update_PAP, one update
--           and lots of PAP_enters.
--
--         - in the case where the thunk is top-level, we save building
--           a black hole and furthermore the thunk isn't considered to
--           be a CAF any more, so it doesn't appear in any SRTs.
--
-- We do it here, because the arity information is accurate, and we need
-- to do it before the SRT pass to save the SRT entries associated with
-- any top-level PAPs.

isPAP env (StgApp f args) = listLengthCmp args arity == LT -- idArity f > length args
                              where
                                 arity = stgArity f (lookupBinding env f)
isPAP env _               = False

-}

{- ToDo:
          upd = if isOnceDem dem
                    then (if isNotTop toplev
                            then SingleEntry    -- HA!  Paydirt for "dem"
                            else
                     (if debugIsOn then trace "WARNING: SE CAFs unsupported, forcing UPD instead" else id) $
                     Updatable)
                else Updatable
        -- For now we forbid SingleEntry CAFs; they tickle the
        -- ASSERT in rts/Storage.c line 215 at newCAF() re mut_link,
        -- and I don't understand why.  There's only one SE_CAF (well,
        -- only one that tickled a great gaping bug in an earlier attempt
        -- at ClosureInfo.getEntryConvention) in the whole of nofib,
        -- specifically Main.lvl6 in spectral/cryptarithm2.
        -- So no great loss.  KSW 2000-07.
-}


-- | Try to make a non top-level StgRhsCon if appropriate
mkStgRhsCon_maybe :: MkStgRhs -> Maybe StgRhs
mkStgRhsCon_maybe (MkStgRhs bndrs rhs typ is_join)
  | [] <- bndrs
  , not is_join
  , (ticks, StgConApp con mn args _) <- stripStgTicksTop (not . tickishIsCode) rhs
  = Just (StgRhsCon currentCCS con mn ticks args typ)

  | otherwise = Nothing


-- | Try to make a top-level StgRhsCon if appropriate
mkTopStgRhsCon_maybe :: (DataCon -> [StgArg] -> Bool) -> MkStgRhs -> Maybe StgRhs
mkTopStgRhsCon_maybe allow_static_con_app (MkStgRhs bndrs rhs typ is_join)
  | [] <- bndrs
  , not is_join -- shouldn't happen at top-level
  , (ticks, StgConApp con mn args _) <- stripStgTicksTop (not . tickishIsCode) rhs
  , allow_static_con_app con args
  = Just (StgRhsCon dontCareCCS con mn ticks args typ)

  | otherwise = Nothing
