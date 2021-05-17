{-# LANGUAGE TypeFamilies #-}

{- |
Non-global free variable analysis on STG terms. This pass annotates
non-top-level closure bindings with captured variables. Global variables are not
captured. For example, in a top-level binding like (pseudo-STG)

    f = \[x,y] .
      let g = \[p] . reverse (x ++ p)
      in g y

In g, `reverse` and `(++)` are global variables so they're not considered free.
`p` is an argument, so `x` is the only actual free variable here. The annotated
version is thus:

    f = \[x,y] .
      let g = [x] \[p] . reverse (x ++ p)
      in g y

Note that non-top-level recursive bindings are also considered free within the
group:

    map = {} \r [f xs0]
      let {
        Rec {
          go = {f, go} \r [xs1]
            case xs1 of {
              [] -> [] [];
              : x xs2 ->
                  let { xs' = {go, xs2} \u [] go xs2; } in
                  let { x' = {f, x} \u [] f x; } in
                  : [x' xs'];
            };
        end Rec }
      } in go xs0;

Here go is free in its RHS.

Top-level closure bindings never capture variables as all of their free
variables are global.
-}
module GHC.Stg.FVs (
    depSortWithAnnotStgPgm,
    annBindingFreeVars
  ) where

import GHC.Prelude

import GHC.Stg.Syntax
import GHC.Types.Id
import GHC.Types.Name (Name, nameIsLocalOrFrom)
import GHC.Types.Name.Env
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.Tickish ( GenTickish(Breakpoint) )
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Types.Var.Set
import GHC.Unit.Module (Module)
import GHC.Utils.Misc

import Data.Graph (SCC (..))
import Data.Bifunctor (first)

--------------------------------------------------------------------------------
-- * Dependency sorting

-- | Dependency sort a STG program so that dependencies come before uses; also
--  perform non-global free variable analysis by annotating non-top-level
-- closure bindings with captured variables.
depSortWithAnnotStgPgm :: Module -> [StgTopBinding] -> [CgStgTopBinding]
depSortWithAnnotStgPgm this_mod =
    {-# SCC "STG.depSortWithAnnotStgPgm" #-}
    map fst . depSort . annAndDepAnalStgFVs this_mod

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

    get_binds :: SCC (CgStgTopBinding, FVs) -> [(CgStgTopBinding, FVs)]
    get_binds (AcyclicSCC bind) =
      [bind]
    get_binds (CyclicSCC binds) =
      pprPanic "depSortStgBinds"
               (text "Found cyclic SCC:"
               $$ ppr (map (first (pprStgTopBinding panicStgPprOpts)) binds))

--------------------------------------------------------------------------------
-- * Non-global free variable analysis

newtype Env
  = Env
  { locals :: IdSet
  }

-- Note [Tracking local binders]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 'locals' contains non-toplevel, non-imported binders.
-- We maintain the set in 'expr', 'alt' and 'rhs', which are the only
-- places where new local binders are introduced.
-- Why do it there rather than in 'binding'? Two reasons:
--
--   1. We call 'binding' from 'annAndDepAnalStgFVs', which would
--      add top-level bindings to the 'locals' set.
--   2. In the let(-no-escape) case, we need to extend the environment
--      prior to analysing the body, but we also need the fvs from the
--      body to analyse the RHSs. No way to do this without some
--      knot-tying.

emptyEnv :: Env
emptyEnv = Env emptyVarSet

addLocals :: [Id] -> Env -> Env
addLocals bndrs env
  = env { locals = extendVarSetList (locals env) bndrs }

mkFreeVarSet :: Env -> [Id] -> DIdSet
mkFreeVarSet env = mkDVarSet . filter (`elemVarSet` locals env)

boundIds :: StgBinding -> [Id]
boundIds (StgNonRec b _) = [b]
boundIds (StgRec pairs)  = map fst pairs

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

-- | Perform Dependency sorting as well as annotate each binding with non-global free variables
annAndDepAnalStgFVs :: Module -> [StgTopBinding] -> [(CgStgTopBinding,FVs)]
annAndDepAnalStgFVs this_mod bs = map go bs
  where
    go (StgTopStringLit id bs) = (StgTopStringLit id bs, emptyVarSet)
    go (StgTopLifted bind)
      | (bind', fvs, _) <- binding this_mod emptyVarSet emptyEnv emptyDVarSet bind
      = (StgTopLifted bind', fvs)

annBindingFreeVars :: Module -> StgBinding -> CgStgBinding
annBindingFreeVars this_mod = fstOf3 . binding this_mod emptyVarSet emptyEnv emptyDVarSet

binding :: Module -> BVs -> Env -> DIdSet -> StgBinding -> (CgStgBinding, FVs, DIdSet)
binding mod bounds env body_fv b =
  case b of
    StgNonRec bndr r -> (StgNonRec bndr r', fvs, id_set)
      where
        -- See Note [Tracking local binders]
        (r', fvs, rhs_id_set) = rhs bounds env r
        id_set = delDVarSet body_fv bndr `unionDVarSet` rhs_id_set
    StgRec pairs -> (StgRec pairs', fvs, id_sets)
      where
        -- See Note [Tracking local binders]
        bndrs = map fst pairs
        bounds' = extendVarSetList bounds bndrs
        (rhss, rhs_fvss, rhs_id_sets) = mapAndUnzip3 (rhs bounds' env . snd) pairs
        fvs = unionVarSets rhs_fvss
        pairs' = zip bndrs rhss
        id_sets = delDVarSetList (unionDVarSets (body_fv:rhs_id_sets)) bndrs
  where
    var_fvs :: BVs -> Var -> FVs
    var_fvs bounds v
      | not (elemVarSet v bounds)
      , nameIsLocalOrFrom mod (idName v)
      = unitVarSet v
      | otherwise
      = emptyVarSet

    expr :: BVs -> Env -> StgExpr -> (CgStgExpr, FVs, DIdSet)
    expr bounds env = go
      where
        go (StgApp f as)
          | (args_fvs, id_set) <- args bounds env as
          = ( StgApp f as
            , var_fvs bounds f `unionVarSet` args_fvs
            , unionDVarSet id_set (mkFreeVarSet env [f]))
        go (StgLit lit) = (StgLit lit, emptyVarSet, emptyDVarSet)
        go (StgConApp dc n as tys)
          | (args_fvs, id_set) <- args bounds env as
          = (StgConApp dc n as tys, args_fvs, id_set)
        go (StgOpApp op as ty)
          | (fvs, id_set) <- args bounds env as
          = (StgOpApp op as ty, fvs, id_set)
        go (StgCase scrut bndr ty alts)
          | (scrut',scrut_fvs,scrut_id_set) <- expr bounds env scrut
          -- See Note [Tracking local binders]
          , (alts',alts_fvss,alts_id_sets)
              <- mapAndUnzip3 (alt (extendVarSet bounds bndr) (addLocals [bndr] env)) alts
          , fvs' <- scrut_fvs `unionVarSet` (unionVarSets alts_fvss)
          , alts_id_set <- unionDVarSets alts_id_sets
          , id_set' <- delDVarSet (unionDVarSet scrut_id_set alts_id_set) bndr
          = (StgCase scrut' bndr ty alts', fvs',id_set')
        go (StgLet ext bind body) = go_bind (StgLet ext) bind body
        go (StgLetNoEscape ext bind body) = go_bind (StgLetNoEscape ext) bind body
        go (StgTick tick e)
          | (e',fvs, id_set) <- expr bounds env e
          , id_set' <- unionDVarSet (tickish tick) id_set
          = (StgTick tick e', fvs, id_set')
            where
              tickish (Breakpoint _ _ ids) = mkDVarSet ids
              tickish _                    = emptyDVarSet

        go_bind dc bind body = (dc bind' body', fvs, id_set)
          where
            -- See Note [Tracking local binders]
            env' = addLocals (boundIds bind) env
            (body', body_fvs, body_set_ids)
                = expr (extendVarSetList bounds (bindersOf bind)) env' body
            (bind', bind_fvs, id_set)
                = binding mod bounds env' body_set_ids bind
            fvs = bind_fvs `unionVarSet` body_fvs


    rhs :: BVs -> Env -> StgRhs -> (CgStgRhs, FVs, DIdSet)
    rhs bounds env (StgRhsClosure _ ccs uf bs body)
      | (body', fvss, id_set)
        <- expr (extendVarSetList bounds bs) (addLocals bs env) body
      , id_set' <- delDVarSetList id_set bs
      = (StgRhsClosure id_set' ccs uf bs body', fvss, id_set')
    rhs bounds env (StgRhsCon ccs dc mu ts bs)
      | (fvs, id_set) <- args bounds env bs
      = (StgRhsCon ccs dc mu ts bs, fvs, id_set)

    args :: BVs -> Env -> [StgArg] -> (FVs, DIdSet)
    args bounds env = foldl' f (emptyVarSet, emptyDVarSet)
      where
        f (fvs,ids) StgLitArg{}  = (fvs, ids )
        f (fvs,ids) (StgVarArg v)  = (fvs', ids')
          where
            !fvs' = var_fvs bounds v `unionVarSet` fvs
            !ids' | v `elemVarSet` locals env
                  = extendDVarSet ids v
                  | otherwise = ids

    alt :: BVs -> Env -> StgAlt -> (CgStgAlt, FVs, DIdSet)
    alt bounds env (con,bndrs,e)
      | (e', fvs, id_set)
        <- expr (extendVarSetList bounds bndrs) (addLocals bndrs env) e
      , id_set' <- delDVarSetList id_set bndrs
      = ((con,bndrs, e'), fvs, id_set')
