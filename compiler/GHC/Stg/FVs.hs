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

import GHC.Prelude hiding (mod)

import GHC.Stg.Syntax
import GHC.Types.Id
import GHC.Types.Name (Name, nameIsExternalFrom)
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
-- closure bindings with captured variables. The returned bindings:
--   * Are in dependency order
--   * Each StgRhsClosure is correctly annotated (in its extension field)
--     with the free variables needed in the closure
--   * Each StgCase is correctly annotated (in its extension field) with
--     the variables that must be saved across the case
depSortWithAnnotStgPgm :: Module -> [StgTopBinding] -> [CgStgTopBinding]
depSortWithAnnotStgPgm this_mod =
    {-# SCC "STG.depSortWithAnnotStgPgm" #-}
    map fst . depSort . annStgFVs this_mod

-- | Sort free-variable-annotated STG bindings so that dependencies come before
-- uses.
depSort :: [(CgStgTopBinding, TopIds)] -> [(CgStgTopBinding, TopIds)]
depSort = concatMap get_binds . depAnal defs uses
  where
    uses, defs :: (CgStgTopBinding, TopIds) -> [Name]

    -- TODO (osa): I'm unhappy about two things in this code:
    --
    --     * Why do we need Name instead of Id for uses and dependencies?
    --     * Why do we need a [Name] instead of `Set Name`? Surely depAnal
    --       doesn't need any ordering.

    uses (StgTopStringLit{}, _) = []
    uses (StgTopLifted{}, fvs)  = map idName (nonDetEltsUniqSet fvs)

    defs (bind, _) = map idName (bindersOfTop bind)

    get_binds :: SCC (CgStgTopBinding, TopIds) -> [(CgStgTopBinding, TopIds)]
    get_binds (AcyclicSCC bind) =
      [bind]
    get_binds (CyclicSCC binds) =
      pprPanic "depSortStgBinds"
               (text "Found cyclic SCC:"
               $$ ppr (map (first (pprStgTopBinding panicStgPprOpts)) binds))

--------------------------------------------------------------------------------
-- * Non-global free variable analysis

data Env
  = Env
  { locals :: IdSet
  , mod    :: Module
  }

-- Note [Tracking local binders]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 'locals' contains non-toplevel, non-imported binders.
-- We maintain the set in 'expr', 'alt' and 'rhs', which are the only
-- places where new local binders are introduced.
-- Why do it there rather than in 'binding'? Two reasons:
--
--   1. We call 'bindingFVs' from 'annStgFVs', which would
--      add top-level bindings to the 'locals' set.
--   2. In the let(-no-escape) case, we need to extend the environment
--      prior to analysing the body, but we also need the fvs from the
--      body to analyse the RHSs. No way to do this without some
--      knot-tying.

emptyEnv :: Module -> Env
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

-- | Set of locally-bound, not-top-level binders in scope.
-- That is, variables bound by a let (but not let-no-escape), a lambda
-- (in a StgRhsClsoure), a case binder, or a case alternative.  These
-- are the variables that must be captured in a function closure, if they
-- are free in the RHS. Example
--   f = \x. let g = \y. x+1
--           let h = \z. g z + 1
--           in h x
-- In the body of h we have NestedIds = {x, g, z}.  Note that f is top level
-- and does not appear in NestedIds
type NestedIds = VarSet

-- | Set of variables that are:
--    (a) bound at the top level of this module, and
--    (b) appear free in the expression
-- It is a /non-deterministic/ set because we use it only to perform dependency
-- analysis on the top-level bindings.
type TopIds = VarSet

-- | Dependency analysis on STG terms.
--
-- Dependencies of a binding are just free variables in the binding. This
-- includes imported ids and ids in the current module. For recursive groups we
-- just return one set of free variables which is just the union of dependencies
-- of all bindings in the group.
--
-- Implementation: pass bound variables (NestedIds) to recursive calls, get free
-- variables (TopIds) back. We ignore imported TopIds as they do not change the
-- ordering but it improves performance (see `nameIsExternalFrom` call in `vars_fvs`).
--

-- | Annotate each binding with non-global free variables
annStgFVs :: Module -> [StgTopBinding] -> [(CgStgTopBinding,TopIds)]
annStgFVs this_mod bs = map go bs
  where
    go (StgTopStringLit id bs) = (StgTopStringLit id bs, emptyVarSet)
    go (StgTopLifted bind)
      | (bind', fvs, _) <- bindingFVs emptyVarSet (emptyEnv this_mod) emptyDVarSet bind
      = (StgTopLifted bind', fvs)

annBindingFreeVars :: Module -> StgBinding -> CgStgBinding
annBindingFreeVars this_mod = fstOf3 . bindingFVs emptyVarSet (emptyEnv this_mod) emptyDVarSet

bindingFVs :: NestedIds -> Env -> DIdSet -> StgBinding -> (CgStgBinding, TopIds, DIdSet)
bindingFVs bounds env body_fv b =
  case b of
    StgNonRec bndr r -> (StgNonRec bndr r', fvs, id_set)
      where
        -- See Note [Tracking local binders]
        (r', fvs, rhs_id_set) = rhsFVs bounds env r
        id_set = delDVarSet body_fv bndr `unionDVarSet` rhs_id_set
    StgRec pairs -> (StgRec pairs', fvs, id_sets)
      where
        -- See Note [Tracking local binders]
        bndrs = map fst pairs
        bounds' = extendVarSetList bounds bndrs
        (rhss, rhs_fvss, rhs_id_sets) = mapAndUnzip3 (rhsFVs bounds' env . snd) pairs
        fvs = unionVarSets rhs_fvss
        pairs' = zip bndrs rhss
        id_sets = delDVarSetList (unionDVarSets (body_fv:rhs_id_sets)) bndrs
  where
    var_fvs :: Var -> TopIds
    var_fvs v | nameIsExternalFrom (mod env) (idName v) = unitVarSet v
              | otherwise                               = emptyVarSet


    exprFVs :: NestedIds -> Env -> StgExpr -> (CgStgExpr, TopIds, DIdSet)
    exprFVs bounds env = go
      where
        go (StgApp f as)
          | (args_fvs, id_set) <- argsFVs bounds env as
          = ( StgApp f as
            , var_fvs f `unionVarSet` args_fvs
            , unionDVarSet (id_set `dVarSetIntersectVarSet` locals env) (mkFreeVarSet env [f]))
        go (StgLit lit) = (StgLit lit, emptyVarSet, emptyDVarSet)
        go (StgConApp dc n as tys)
          | (args_fvs, id_set) <- argsFVs bounds env as
          = (StgConApp dc n as tys, args_fvs, id_set)
        go (StgOpApp op as ty)
          | (fvs, id_set) <- argsFVs bounds env as
          = (StgOpApp op as ty, fvs, id_set)
        go (StgCase scrut bndr ty alts)
          | (scrut',scrut_fvs,scrut_id_set) <- exprFVs bounds env scrut
          -- See Note [Tracking local binders]
          , (alts',alts_fvss,alts_id_sets)
              <- mapAndUnzip3 (altFVs (extendVarSet bounds bndr) (addLocals [bndr] env)) alts
          , fvs' <- scrut_fvs `unionVarSet` unionVarSets alts_fvss
          , alts_id_set <- unionDVarSets alts_id_sets
          , id_set' <- delDVarSet (unionDVarSet scrut_id_set alts_id_set) bndr
          = (StgCase scrut' bndr ty alts', fvs',id_set')
        go (StgLet ext bind body) = go_bind (StgLet ext) bind body
        go (StgLetNoEscape ext bind body) = go_bind (StgLetNoEscape ext) bind body
        go (StgTick tick e)
          | (e',fvs, id_set) <- exprFVs bounds env e
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
                = exprFVs (extendVarSetList bounds (bindersOf bind)) env' body
            (bind', bind_fvs, id_set)
                = bindingFVs bounds env' body_set_ids bind
            fvs = bind_fvs `unionVarSet` body_fvs


    rhsFVs :: NestedIds -> Env -> StgRhs -> (CgStgRhs, TopIds, DIdSet)
    rhsFVs bounds env (StgRhsClosure _ ccs uf bs body)
      | (body', fvss, id_set)
        <- exprFVs (extendVarSetList bounds bs) (addLocals bs env) body
      , id_set' <- delDVarSetList id_set bs
      = (StgRhsClosure id_set' ccs uf bs body', fvss, id_set')
    rhsFVs bounds env (StgRhsCon ccs dc mu ts bs)
      | (fvs, id_set) <- argsFVs bounds env bs
      = (StgRhsCon ccs dc mu ts bs, fvs, id_set)

    argsFVs :: NestedIds -> Env -> [StgArg] -> (TopIds, DIdSet)
    argsFVs _ env = foldl' f (emptyVarSet, emptyDVarSet)
      where
        f (fvs,ids) StgLitArg{}  = (fvs, ids )
        f (fvs,ids) (StgVarArg v)  = (fvs', ids')
          where
            !fvs' = var_fvs v `unionVarSet` fvs
            !ids' | v `elemVarSet` locals env
                  = extendDVarSet ids v
                  | otherwise = ids

    altFVs :: NestedIds -> Env -> StgAlt -> (CgStgAlt, TopIds, DIdSet)
    altFVs bounds env (con,bndrs,e)
      | (e', fvs, id_set)
        <- exprFVs (extendVarSetList bounds bndrs) (addLocals bndrs env) e
      , id_set' <- delDVarSetList id_set bndrs
      = ((con,bndrs, e'), fvs, id_set')
