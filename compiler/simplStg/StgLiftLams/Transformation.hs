{-# LANGUAGE CPP #-}

-- | (Mostly) textbook instance of the lambda lifting transformation,
-- selecting which bindings to lambda lift by consulting 'goodToLift'.
module StgLiftLams.Transformation (stgLiftLams) where

#include "HsVersions.h"

import GhcPrelude

import BasicTypes
import DynFlags
import Id
import IdInfo
import StgFVs ( annBindingFreeVars )
import StgLiftLams.Analysis
import StgLiftLams.LiftM
import StgSyn
import Outputable
import UniqSupply
import Util
import VarSet
import Control.Monad ( when )
import Data.Maybe ( isNothing )

-- | Lambda lifts bindings to top-level deemed worth lifting (see 'goodToLift').
stgLiftLams :: DynFlags -> UniqSupply -> [InStgTopBinding] -> [OutStgTopBinding]
stgLiftLams dflags us = runLiftM dflags us . foldr liftTopLvl (pure ())

liftTopLvl :: InStgTopBinding -> LiftM () -> LiftM ()
liftTopLvl (StgTopStringLit bndr lit) rest = withSubstBndr bndr $ \bndr' -> do
  addTopStringLit bndr' lit
  rest
liftTopLvl (StgTopLifted bind) rest = do
  let is_rec = isRec $ fst $ decomposeStgBinding bind
  when is_rec startBindingGroup
  let bind_w_fvs = annBindingFreeVars bind
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
  | isTopLevel top_lvl
  = withCaffyness (is_caffy pairs) go
  | otherwise
  = go
  where
    (rec, pairs) = decomposeStgBinding bind
    is_caffy = any (mayHaveCafRefs . idCafInfo . binderInfoBndr . fst)
    go = withLiftedBindPairs top_lvl rec pairs scope (k . fmap (mkStgBinding rec))

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
  dflags <- getDynFlags
  case goodToLift dflags top rec expander pairs scope of
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
liftRhs mb_former_fvs rhs@(StgRhsCon ccs con args)
  = ASSERT2(isNothing mb_former_fvs, text "Should never lift a constructor" $$ ppr rhs)
    StgRhsCon ccs con <$> traverse liftArgs args
liftRhs Nothing (StgRhsClosure _ ccs upd infos body) = do
  -- This RHS wasn't lifted.
  withSubstBndrs (map binderInfoBndr infos) $ \bndrs' ->
    StgRhsClosure noExtSilent ccs upd bndrs' <$> liftExpr body
liftRhs (Just former_fvs) (StgRhsClosure _ ccs upd infos body) = do
  -- This RHS was lifted. Insert extra binders for @former_fvs@.
  withSubstBndrs (map binderInfoBndr infos) $ \bndrs' -> do
    let bndrs'' = dVarSetElems former_fvs ++ bndrs'
    StgRhsClosure noExtSilent ccs upd bndrs'' <$> liftExpr body

liftArgs :: InStgArg -> LiftM OutStgArg
liftArgs a@(StgLitArg _) = pure a
liftArgs (StgVarArg occ) = do
  ASSERTM2( not <$> isLifted occ, text "StgArgs should never be lifted" $$ ppr occ )
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
liftExpr (StgConApp con args tys) = StgConApp con <$> traverse liftArgs args <*> pure tys
liftExpr (StgOpApp op args ty) = StgOpApp op <$> traverse liftArgs args <*> pure ty
liftExpr (StgLam _ _) = pprPanic "stgLiftLams" (text "StgLam")
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
        Just bind' -> pure (StgLet noExtSilent bind' body')
liftExpr (StgLetNoEscape scope bind body)
  = withLiftedBind NotTopLevel bind scope $ \mb_bind' -> do
      body' <- liftExpr body
      case mb_bind' of
        Nothing -> pprPanic "stgLiftLams" (text "Should never decide to lift LNEs")
        Just bind' -> pure (StgLetNoEscape noExtSilent bind' body')

liftAlt :: LlStgAlt -> LiftM OutStgAlt
liftAlt (con, infos, rhs) = withSubstBndrs (map binderInfoBndr infos) $ \bndrs' ->
  (,,) con bndrs' <$> liftExpr rhs
