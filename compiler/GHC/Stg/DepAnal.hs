{-# LANGUAGE CPP #-}

module GHC.Stg.DepAnal (depSortStgPgm) where

import GhcPrelude

import GHC.Stg.FVs
import GHC.Stg.Syntax
import GHC.Types.Id
import GHC.Types.Name (Name, nameIsLocalOrFrom)
import GHC.Types.Name.Env
import Outputable
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Types.Var.Set
import GHC.Types.Module (Module)

import Data.Graph (SCC (..))

--------------------------------------------------------------------------------
-- * Dependency analysis

-- | Set of bound variables
type BVs = VarSet

-- | Set of free variables
type FVs = VarSet

-- | Dependency analysis on STG terms.
--
-- Dependencies of a binding are just free variables in the binding. This
-- includes imported ids and ids in the current module. For recursive groups we
-- just return one set of free variables which is just the union of dependencies
-- of all bindings in the group.
--
-- Implementation: pass bound variables (BVs) to recursive calls, get free
-- variables (FVs) back. We ignore imported FVs as they do not change the
-- ordering but it improves performance.
--
annTopBindingsDeps :: Module -> [StgTopBinding] -> [(CgStgTopBinding, FVs)]
annTopBindingsDeps this_mod bs = map top_bind bs
  where
    top_bind :: StgTopBinding -> (CgStgTopBinding, FVs)
    top_bind (StgTopStringLit id bs) =
      (StgTopStringLit id bs, emptyVarSet)

    top_bind (StgTopLifted bs) =
      (StgTopLifted (annBindingFreeVars bs), binding emptyVarSet bs)

    binding :: BVs -> StgBinding -> FVs
    binding bounds (StgNonRec _ r) =
      rhs bounds r
    binding bounds (StgRec bndrs) =
      unionVarSets $
        map (bind_non_rec (extendVarSetList bounds (map fst bndrs))) bndrs

    bind_non_rec :: BVs -> (Id, StgRhs) -> FVs
    bind_non_rec bounds (_, r) =
        rhs bounds r

    rhs :: BVs -> StgRhs -> FVs
    rhs bounds (StgRhsClosure _ _ _ as e) =
      expr (extendVarSetList bounds as) e

    rhs bounds (StgRhsCon _ _ as) =
      args bounds as

    var :: BVs -> Var -> FVs
    var bounds v
      | not (elemVarSet v bounds)
      , nameIsLocalOrFrom this_mod (idName v)
      = unitVarSet v
      | otherwise
      = emptyVarSet

    arg :: BVs -> StgArg -> FVs
    arg bounds (StgVarArg v) = var bounds v
    arg _ StgLitArg{} = emptyVarSet

    args :: BVs -> [StgArg] -> FVs
    args bounds as = unionVarSets (map (arg bounds) as)

    expr :: BVs -> StgExpr -> FVs
    expr bounds (StgApp f as) =
      var bounds f `unionVarSet` args bounds as

    expr _ StgLit{} =
      emptyVarSet

    expr bounds (StgConApp _ as _) =
      args bounds as
    expr bounds (StgOpApp _ as _) =
      args bounds as
    expr _ lam@StgLam{} =
      pprPanic "annTopBindingsDeps" (text "Found lambda:" $$ ppr lam)
    expr bounds (StgCase scrut scrut_bndr _ as) =
      expr bounds scrut `unionVarSet`
        alts (extendVarSet bounds scrut_bndr) as
    expr bounds (StgLet _ bs e) =
      binding bounds bs `unionVarSet`
        expr (extendVarSetList bounds (bindersOf bs)) e
    expr bounds (StgLetNoEscape _ bs e) =
      binding bounds bs `unionVarSet`
        expr (extendVarSetList bounds (bindersOf bs)) e

    expr bounds (StgTick _ e) =
      expr bounds e

    alts :: BVs -> [StgAlt] -> FVs
    alts bounds = unionVarSets . map (alt bounds)

    alt :: BVs -> StgAlt -> FVs
    alt bounds (_, bndrs, e) =
      expr (extendVarSetList bounds bndrs) e

--------------------------------------------------------------------------------
-- * Dependency sorting

-- | Dependency sort a STG program so that dependencies come before uses.
depSortStgPgm :: Module -> [StgTopBinding] -> [CgStgTopBinding]
depSortStgPgm this_mod =
    {-# SCC "STG.depSort" #-}
    map fst . depSort . annTopBindingsDeps this_mod

-- | Sort free-variable-annotated STG bindings so that dependencies come before
-- uses.
depSort :: [(CgStgTopBinding, FVs)] -> [(CgStgTopBinding, FVs)]
depSort = concatMap get_binds . depAnal defs uses
  where
    uses, defs :: (CgStgTopBinding, FVs) -> [Name]

    -- TODO (osa): I'm unhappy about two things in this code:
    --
    --     * Why do we need Name instead of Id for uses and dependencies?
    --     * Why do we need a [Name] instead of `Set Name`? Surely depAnal
    --       doesn't need any ordering.

    uses (StgTopStringLit{}, _) = []
    uses (StgTopLifted{}, fvs)  = map idName (nonDetEltsUniqSet fvs)

    defs (bind, _) = map idName (bindersOfTop bind)

    get_binds (AcyclicSCC bind) =
      [bind]
    get_binds (CyclicSCC binds) =
      pprPanic "depSortStgBinds" (text "Found cyclic SCC:" $$ ppr binds)
