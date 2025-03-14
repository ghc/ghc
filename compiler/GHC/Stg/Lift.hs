

-- | Implements a selective lambda lifter, running late in the optimisation
-- pipeline.
--
-- If you are interested in the cost model that is employed to decide whether
-- to lift a binding or not, look at "GHC.Stg.Lift.Analysis".
-- "GHC.Stg.Lift.Monad" contains the transformation monad that hides away some
-- plumbing of the transformation.
module GHC.Stg.Lift
   (
    -- * Late lambda lifting in STG
    -- $note
   StgLiftConfig (..),
   stgLiftLams
   )
where

import GHC.Prelude

import GHC.Types.Basic
import GHC.Types.Id
import GHC.Stg.FVs ( annBindingFreeVars )
import GHC.Stg.Lift.Config
import GHC.Stg.Lift.Analysis
import GHC.Stg.Lift.Monad
import GHC.Stg.Syntax
import GHC.Unit.Module (Module)
import GHC.Types.Unique.Supply
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.Var.Set
import Control.Monad ( when )
import Data.Maybe ( isNothing )

-- Note [Late lambda lifting in STG]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- $note
-- See also the <https://gitlab.haskell.org/ghc/ghc/wikis/late-lam-lift wiki page>
-- and #9476.
--
-- The basic idea behind lambda lifting is to turn locally defined functions
-- into top-level functions. Free variables are then passed as additional
-- arguments at *call sites* instead of having a closure allocated for them at
-- *definition site*. Example:
--
-- @
--    let x = ...; y = ... in
--    let f = {x y} \a -> a + x + y in
--    let g = {f x} \b -> f b + x in
--    g 5
-- @
--
-- Lambda lifting @f@ would
--
--   1. Turn @f@'s free variables into formal parameters
--   2. Update @f@'s call site within @g@ to @f x y b@
--   3. Update @g@'s closure: Add @y@ as an additional free variable, while
--      removing @f@, because @f@ no longer allocates and can be floated to
--      top-level.
--   4. Actually float the binding of @f@ to top-level, eliminating the @let@
--      in the process.
--
-- This results in the following program (with free var annotations):
--
-- @
--    f x y a = a + x + y;
--    let x = ...; y = ... in
--    let g = {x y} \b -> f x y b + x in
--    g 5
-- @
--
-- This optimisation is all about lifting only when it is beneficial to do so.
-- The above seems like a worthwhile lift, judging from heap allocation:
-- We eliminate @f@'s closure, saving to allocate a closure with 2 words, while
-- not changing the size of @g@'s closure.
--
-- You can probably sense that there's some kind of cost model at play here.
-- And you are right! But we also employ a couple of other heuristics for the
-- lifting decision which are outlined in "GHC.Stg.Lift.Analysis#when".
--
-- The transformation is done in "GHC.Stg.Lift", which calls out to
-- 'GHC.Stg.Lift.Analysis.goodToLift' for its lifting decision.  It relies on
-- "GHC.Stg.Lift.Monad", which abstracts some subtle STG invariants into a
-- monadic substrate.
--
-- Suffice to say: We trade heap allocation for stack allocation.
-- The additional arguments have to passed on the stack (or in registers,
-- depending on architecture) every time we call the function to save a single
-- heap allocation when entering the let binding. Nofib suggests a mean
-- improvement of about 1% for this pass, so it seems like a worthwhile thing to
-- do. Compile-times went up by 0.6%, so all in all a very modest change.
--
-- For a concrete example, look at @spectral/atom@. There's a call to 'zipWith'
-- that is ultimately compiled to something like this
-- (module desugaring/lowering to actual STG):
--
-- @
--    propagate dt = ...;
--    runExperiment ... =
--      let xs = ... in
--      let ys = ... in
--      let go = {dt go} \xs ys -> case (xs, ys) of
--            ([], []) -> []
--            (x:xs', y:ys') -> propagate dt x y : go xs' ys'
--      in go xs ys
-- @
--
-- This will lambda lift @go@ to top-level, speeding up the resulting program
-- by roughly one percent:
--
-- @
--    propagate dt = ...;
--    go dt xs ys = case (xs, ys) of
--      ([], []) -> []
--      (x:xs', y:ys') -> propagate dt x y : go dt xs' ys'
--    runExperiment ... =
--      let xs = ... in
--      let ys = ... in
--      in go dt xs ys
-- @


-- | Lambda lifts bindings to top-level deemed worth lifting (see 'goodToLift').
--
-- (Mostly) textbook instance of the lambda lifting transformation, selecting
-- which bindings to lambda lift by consulting 'goodToLift'.
stgLiftLams :: Module -> StgLiftConfig -> UniqSupply -> [InStgTopBinding] -> [OutStgTopBinding]
stgLiftLams this_mod cfg us = runLiftM cfg us . foldr (liftTopLvl this_mod) (pure ())

liftTopLvl :: Module -> InStgTopBinding -> LiftM () -> LiftM ()
liftTopLvl _ (StgTopStringLit bndr lit) rest = withSubstBndr bndr $ \bndr' -> do
  addTopStringLit bndr' lit
  rest
liftTopLvl this_mod (StgTopLifted bind) rest = do
  let is_rec = isRec $ fst $ decomposeStgBinding bind
  when is_rec startBindingGroup
  let bind_w_fvs = annBindingFreeVars this_mod bind
  withLiftedBind TopLevel (tagSkeletonTopBind bind_w_fvs) NilSk $ \mb_bind' -> do
    -- We signal lifting of a binding through returning Nothing.
    -- Should never happen for a top-level binding, though, since we are already
    -- at top-level.
    case mb_bind' of
      Nothing -> pprPanic "StgLiftLams" (text "Lifted top-level binding")
      Just bind' -> addLiftedBinding bind'
    when is_rec endBindingGroup
    rest

withLiftedBind
  :: TopLevelFlag
  -> LlStgBinding
  -> Skeleton
  -> (Maybe OutStgBinding -> LiftM a)
  -> LiftM a
withLiftedBind top_lvl bind scope k
  = withLiftedBindPairs top_lvl rec pairs scope (k . fmap (mkStgBinding rec))
  where
    (rec, pairs) = decomposeStgBinding bind

withLiftedBindPairs
  :: TopLevelFlag
  -> RecFlag
  -> [(BinderInfo, LlStgRhs)]
  -> Skeleton
  -> (Maybe [(Id, OutStgRhs)] -> LiftM a)
  -> LiftM a
withLiftedBindPairs top rec pairs scope k = do
  let (infos, rhss) = unzip pairs
  let bndrs = map binderInfoBndr infos
  expander <- liftedIdsExpander
  cfg <- getConfig
  case goodToLift cfg top rec expander pairs scope of
    -- @abs_ids@ is the set of all variables that need to become parameters.
    Just abs_ids -> withLiftedBndrs abs_ids bndrs $ \bndrs' -> do
      -- Within this block, all binders in @bndrs@ will be noted as lifted, so
      -- that the return value of @liftedIdsExpander@ in this context will also
      -- expand the bindings in @bndrs@ to their free variables.
      -- Now we can recurse into the RHSs and see if we can lift any further
      -- bindings. We pass the set of expanded free variables (thus OutIds) on
      -- to @liftRhs@ so that it can add them as parameter binders.
      when (isRec rec) startBindingGroup
      rhss' <- traverse (liftRhs (Just abs_ids)) rhss
      let pairs' = zip bndrs' rhss'
      addLiftedBinding (mkStgBinding rec pairs')
      when (isRec rec) endBindingGroup
      k Nothing
    Nothing -> withSubstBndrs bndrs $ \bndrs' -> do
      -- Don't lift the current binding, but possibly some bindings in their
      -- RHSs.
      rhss' <- traverse (liftRhs Nothing) rhss
      let pairs' = zip bndrs' rhss'
      k (Just pairs')

liftRhs
  :: Maybe (DIdSet)
  -- ^ @Just former_fvs@ <=> this RHS was lifted and we have to add @former_fvs@
  -- as lambda binders, discarding all free vars.
  -> LlStgRhs
  -> LiftM OutStgRhs
liftRhs mb_former_fvs rhs@(StgRhsCon ccs con mn ts args typ)
  = assertPpr (isNothing mb_former_fvs)
              (text "Should never lift a constructor"
               $$ pprStgRhs panicStgPprOpts rhs) $
    StgRhsCon ccs con mn ts <$> traverse liftArgs args <*> pure typ
liftRhs Nothing (StgRhsClosure _ ccs upd infos body typ) =
  -- This RHS wasn't lifted.
  withSubstBndrs (map binderInfoBndr infos) $ \bndrs' ->
    StgRhsClosure noExtFieldSilent ccs upd bndrs' <$> liftExpr body <*> pure typ
liftRhs (Just former_fvs) (StgRhsClosure _ ccs upd infos body typ) =
  -- This RHS was lifted. Insert extra binders for @former_fvs@.
  withSubstBndrs (map binderInfoBndr infos) $ \bndrs' -> do
    let bndrs'' = dVarSetElems former_fvs ++ bndrs'
    StgRhsClosure noExtFieldSilent ccs upd bndrs'' <$> liftExpr body <*> pure typ

liftArgs :: InStgArg -> LiftM OutStgArg
liftArgs a@(StgLitArg _) = pure a
liftArgs (StgVarArg occ) = do
  assertPprM (not <$> isLifted occ) (text "StgArgs should never be lifted" $$ ppr occ)
  StgVarArg <$> substOcc occ

liftExpr :: LlStgExpr -> LiftM OutStgExpr
liftExpr (StgLit lit) = pure (StgLit lit)
liftExpr (StgTick t e) = StgTick t <$> liftExpr e
liftExpr (StgApp f args) = do
  f' <- substOcc f
  args' <- traverse liftArgs args
  fvs' <- formerFreeVars f
  let top_lvl_args = map StgVarArg fvs' ++ args'
  pure (StgApp f' top_lvl_args)
liftExpr (StgConApp con mn args tys) = StgConApp con mn <$> traverse liftArgs args <*> pure tys
liftExpr (StgOpApp op args ty) = StgOpApp op <$> traverse liftArgs args <*> pure ty
liftExpr (StgCase scrut info ty alts) = do
  scrut' <- liftExpr scrut
  withSubstBndr (binderInfoBndr info) $ \bndr' -> do
    alts' <- traverse liftAlt alts
    pure (StgCase scrut' bndr' ty alts')
liftExpr (StgLet scope bind body)
  = withLiftedBind NotTopLevel bind scope $ \mb_bind' -> do
      body' <- liftExpr body
      case mb_bind' of
        Nothing -> pure body' -- withLiftedBindPairs decided to lift it and already added floats
        Just bind' -> pure (StgLet noExtFieldSilent bind' body')
liftExpr (StgLetNoEscape scope bind body)
  = withLiftedBind NotTopLevel bind scope $ \mb_bind' -> do
      body' <- liftExpr body
      case mb_bind' of
        Nothing -> pprPanic "stgLiftLams" (text "Should never decide to lift LNEs")
        Just bind' -> pure (StgLetNoEscape noExtFieldSilent bind' body')

liftAlt :: LlStgAlt -> LiftM OutStgAlt
liftAlt alt@GenStgAlt{alt_con=_, alt_bndrs=infos, alt_rhs=rhs} =
  withSubstBndrs (map binderInfoBndr infos) $ \bndrs' ->
    do !rhs' <- liftExpr rhs
       return $! alt {alt_bndrs = bndrs', alt_rhs = rhs'}
