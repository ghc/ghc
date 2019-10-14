{-# LANGUAGE CPP #-}

module GHC.Stg.DepAnal (depSortStgPgm) where

import GhcPrelude

import Id
import Name (Name)
import NameEnv
import Outputable
import GHC.Stg.Syntax
import VarSet

import Data.Graph (SCC (..))

--------------------------------------------------------------------------------
-- * Dependency analysis

-- | Set of bound variables
type BVs = DVarSet

-- | Set of free variables
type FVs = DVarSet

-- | Dependency analysis on STG terms.
--
-- Dependencies of a binding are just free variables in the binding. This
-- includes imported ids and ids in the current module. For recursive groups we
-- just return one set of free variables which is just the union of dependencies
-- of all bindings in the group.
--
-- Implementation: pass bound variables (BVs) to recursive calls, get free
-- variables (FVs) back.
--
annTopBindingsDeps :: [StgTopBinding] -> [(StgTopBinding, FVs)]
annTopBindingsDeps bs = zip bs (map top_bind bs)
  where
    top_bind :: StgTopBinding -> FVs

    top_bind StgTopStringLit{} =
      emptyDVarSet

    top_bind (StgTopLifted bs) =
      binding emptyDVarSet bs

    binding :: BVs -> StgBinding -> FVs

    binding bounds (StgNonRec _ r) =
      rhs bounds r

    binding bounds (StgRec bndrs) =
      unionDVarSets $
        map (bind_non_rec (extendDVarSetList bounds (map fst bndrs))) bndrs

    bind_non_rec :: BVs -> (Id, StgRhs) -> FVs
    bind_non_rec bounds (_, r) =
        rhs bounds r

    rhs :: BVs -> StgRhs -> FVs

    rhs bounds (StgRhsClosure _ _ _ as e) =
      expr (extendDVarSetList bounds as) e

    rhs bounds (StgRhsCon _ _ as) =
      args bounds as

    var :: BVs -> Var -> FVs
    var bounds v
      | not (elemDVarSet v bounds)
      = unitDVarSet v
      | otherwise
      = emptyDVarSet

    arg :: BVs -> StgArg -> FVs
    arg bounds (StgVarArg v) = var bounds v
    arg _ StgLitArg{}        = emptyDVarSet

    args :: BVs -> [StgArg] -> FVs
    args bounds as =
      unionDVarSets (map (arg bounds) as)

    expr :: BVs -> StgExpr -> FVs

    expr bounds (StgApp f as) =
      var bounds f `unionDVarSet` args bounds as

    expr _ StgLit{} =
      emptyDVarSet

    expr bounds (StgConApp _ as _) =
      args bounds as

    expr bounds (StgOpApp _ as _) =
      args bounds as

    expr _ lam@StgLam{} =
      pprPanic "annTopBindingsDeps" (text "Found lambda:" $$ ppr lam)

    expr bounds (StgCase scrut scrut_bndr _ as) =
      expr bounds scrut `unionDVarSet`
        alts (extendDVarSet bounds scrut_bndr) as

    expr bounds (StgLet _ bs e) =
      binding bounds bs `unionDVarSet`
        expr (extendDVarSetList bounds (bindersOf bs)) e

    expr bounds (StgLetNoEscape _ bs e) =
      binding bounds bs `unionDVarSet`
        expr (extendDVarSetList bounds (bindersOf bs)) e

    expr bounds (StgTick _ e) =
      expr bounds e

    alts :: BVs -> [StgAlt] -> FVs
    alts bounds = unionDVarSets . map (alt bounds)

    alt :: BVs -> StgAlt -> FVs
    alt bounds (_, bndrs, e) =
      expr (extendDVarSetList bounds bndrs) e

--------------------------------------------------------------------------------
-- * Dependency sorting

-- | Dependency sort a STG program so that dependencies come before uses.
depSortStgPgm :: [StgTopBinding] -> [StgTopBinding]
depSortStgPgm = map fst . depSort . annTopBindingsDeps

-- | Sort free-variable-annotated STG bindings so that dependencies come before
-- uses.
depSort :: [(StgTopBinding, FVs)] -> [(StgTopBinding, FVs)]
depSort = concatMap get_binds . depAnal defs uses
  where
    uses, defs :: (StgTopBinding, FVs) -> [Name]

    -- TODO (osa): I'm unhappy about two things in this code:
    --
    --     * Why do we need Name instead of Id for uses and dependencies?
    --     * Why do we need a [Name] instead of `Set Name`? Surely depAnal
    --       doesn't need any ordering.

    uses (StgTopStringLit{}, _) = []
    uses (StgTopLifted{}, fvs)  = map idName (dVarSetElems fvs)

    defs (bind, _) = map idName (bindersOfTop bind)

    get_binds (AcyclicSCC bind) =
      [bind]
    get_binds (CyclicSCC binds) =
      pprPanic "depSortStgBinds" (text "Found cyclic SCC:" $$ ppr binds)
