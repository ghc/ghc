{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

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
import GHC.Stg.Utils (bindersOf)
import GHC.Types.Id
import GHC.Types.Name (Name, nameIsLocalOrFrom)
import GHC.Types.Tickish ( GenTickish(Breakpoint) )
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Types.Var.Set
import GHC.Unit.Module (Module)
import GHC.Utils.Misc

import Data.Graph (SCC (..))
import GHC.Data.Graph.Directed( Node(..), stronglyConnCompFromEdgedVerticesUniq )

{- Note [Why do we need dependency analysis?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The program needs to be in dependency order for the SRT algorithm to
work (see CmmBuildInfoTables, which also includes a detailed
description of the algorithm).

But isn't it in correct dependency order already?  No:

* The simplifier does not guarantee to produce programs in dependency
  order (see #16192 and Note [Glomming] in GHC.Core.Opt.OccurAnal).
  This could be solved by a final run of the occurrence analyser, but
  that's more work

* We also don't guarantee that StgLiftLams will preserve the order or
  only create minimal recursive groups.
-}

--------------------------------------------------------------------------------
-- | Dependency sort a STG program, and annotate it with free variables
-- The returned bindings:
--   * Are in dependency order
--   * Each StgRhsClosure is correctly annotated (in its extension field)
--     with the free variables needed in the closure
--   * Each StgCase is correctly annotated (in its extension field) with
--     the variables that must be saved across the case
depSortWithAnnotStgPgm :: Module -> [StgTopBinding] -> [(CgStgTopBinding,ImpFVs)]
depSortWithAnnotStgPgm this_mod binds
  = {-# SCC "STG.depSortWithAnnotStgPgm" #-}
    zip lit_binds (repeat emptyVarSet) ++ map from_scc sccs
  where
    lit_binds :: [CgStgTopBinding]
    pairs     :: [(Id, StgRhs)]
    (lit_binds, pairs) = flattenTopStgBindings binds

    nodes :: [Node Name (Id, CgStgRhs, ImpFVs)]
    nodes = map (annotateTopPair env0) pairs
    env0 = Env { locals = emptyVarSet, mod = this_mod }

    -- Do strongly connected component analysis.  Why?
    -- See Note [Why do we need dependency analysis?]
    sccs :: [SCC (Id,CgStgRhs,ImpFVs)]
    sccs  = stronglyConnCompFromEdgedVerticesUniq nodes

    from_scc = \case
      AcyclicSCC (bndr,rhs,imp_fvs) -> (StgTopLifted (StgNonRec bndr rhs), imp_fvs)
      CyclicSCC triples             -> (StgTopLifted (StgRec pairs), imp_fvs)
        where
          (ids,rhss,imp_fvss) = unzip3 triples
          pairs = zip ids rhss
          imp_fvs = unionVarSets imp_fvss


flattenTopStgBindings :: [StgTopBinding] -> ([CgStgTopBinding], [(Id,StgRhs)])
flattenTopStgBindings binds
  = go [] [] binds
  where
    go lits pairs [] = (lits, pairs)
    go lits pairs (bind:binds)
      = case bind of
          StgTopStringLit bndr rhs -> go (StgTopStringLit bndr rhs:lits) pairs binds
          StgTopLifted stg_bind -> go lits (flatten_one stg_bind ++ pairs) binds

    flatten_one (StgNonRec b r) = [(b,r)]
    flatten_one (StgRec pairs)  = pairs

annotateTopPair :: Env -> (Id, StgRhs) -> Node Name (Id, CgStgRhs, ImpFVs)
annotateTopPair env0 (bndr, rhs)
  = DigraphNode { node_key          = idName bndr
                , node_payload      = (bndr, rhs', imp_fvs)
                , node_dependencies = map idName (nonDetEltsUniqSet top_fvs) }
  where
    (rhs', imp_fvs, top_fvs, _) = rhsFVs env0 rhs

--------------------------------------------------------------------------------
-- * Non-global free variable analysis

data Env
  = Env
  { -- | Set of locally-bound, not-top-level binders in scope.
    -- That is, variables bound by a let (but not let-no-escape), a lambda
    -- (in a StgRhsClsoure), a case binder, or a case alternative.  These
    -- are the variables that must be captured in a function closure, if they
    -- are free in the RHS. Example
    --   f = \x. let g = \y. x+1
    --           let h = \z. g z + 1
    --           in h x
    -- In the body of h we have locals = {x, g, z}.  Note that f is top level
    -- and does not appear in locals.
    locals :: IdSet
  , mod    :: Module
  }

addLocals :: [Id] -> Env -> Env
addLocals bndrs env
  = env { locals = extendVarSetList (locals env) bndrs }

--------------------------------------------------------------------------------
-- | TopFVs: set of variables that are:
--    (a) bound at the top level of this module, and
--    (b) appear free in the expression
-- It is a /non-deterministic/ set because we use it only to perform dependency
-- analysis on the top-level bindings.
type TopFVs   = IdSet

-- | ImpFVs: set of variables that are imported
--
-- It is a /non-deterministic/ set because we use it only to perform module
-- dependency analysis.
type ImpFVs   = IdSet

-- | LocalFVs: set of variable that are:
--     (a) bound locally (by a lambda, non-top-level let, or case); that is,
--         it appears in the 'locals' field of 'Env'
--     (b) appear free in the expression
-- It is a /deterministic/ set because it is used to annotate closures with
-- their free variables, and we want closure layout to be deterministic.
--
-- Invariant: the LocalFVs returned is a subset of the 'locals' field of Env
type LocalFVs = DIdSet

-- | Dependency analysis on STG terms.
--
-- Dependencies of a binding are just free variables in the binding. This
-- includes imported ids and ids in the current module. For recursive groups we
-- just return one set of free variables which is just the union of dependencies
-- of all bindings in the group.
--
-- Implementation: pass bound variables (NestedIds) to recursive calls, get free
-- variables (TopFVs) back. We ignore imported TopFVs as they do not change the
-- ordering but it improves performance (see `nameIsExternalFrom` call in `vars_fvs`).
--

annBindingFreeVars :: Module -> StgBinding -> CgStgBinding
annBindingFreeVars this_mod = fstOf4 . bindingFVs (Env emptyVarSet this_mod) emptyDVarSet

bindingFVs :: Env -> LocalFVs -> StgBinding -> (CgStgBinding, ImpFVs, TopFVs, LocalFVs)
bindingFVs env body_fv b =
  case b of
    StgNonRec bndr r -> (StgNonRec bndr r', imp_fvs, top_fvs, lcl_fvs)
      where
        (r', imp_fvs, top_fvs, rhs_lcl_fvs) = rhsFVs env r
        lcl_fvs = delDVarSet body_fv bndr `unionDVarSet` rhs_lcl_fvs

    StgRec pairs -> (StgRec pairs', imp_fvs, top_fvs, lcl_fvss)
      where
        bndrs = map fst pairs
        env' = addLocals bndrs env
        (rhss, rhs_imp_fvss, rhs_top_fvss, rhs_lcl_fvss) = mapAndUnzip4 (rhsFVs env' . snd) pairs
        top_fvs = unionVarSets rhs_top_fvss
        imp_fvs = unionVarSets rhs_imp_fvss
        pairs' = zip bndrs rhss
        lcl_fvss = delDVarSetList (unionDVarSets (body_fv:rhs_lcl_fvss)) bndrs

varFVs :: Env -> Id -> (ImpFVs, TopFVs, LocalFVs) -> (ImpFVs, TopFVs, LocalFVs)
varFVs env v (imp_fvs, top_fvs, lcl_fvs)
  | v `elemVarSet` locals env                -- v is locally bound
  = (imp_fvs, top_fvs, lcl_fvs `extendDVarSet` v)
  | nameIsLocalOrFrom (mod env) (idName v)   -- v is bound at top level
  = (imp_fvs, top_fvs `extendVarSet` v, lcl_fvs)
  | otherwise                                -- v is imported
  = (imp_fvs `extendVarSet` v, top_fvs, lcl_fvs)

exprFVs :: Env -> StgExpr -> (CgStgExpr, ImpFVs, TopFVs, LocalFVs)
exprFVs env = go
  where
    go (StgApp f as)
      | (imp_fvs, top_fvs, lcl_fvs) <- varFVs env f (argsFVs env as)
      = (StgApp f as, imp_fvs, top_fvs, lcl_fvs)

    go (StgLit lit) = (StgLit lit, emptyVarSet, emptyVarSet, emptyDVarSet)

    go (StgConApp dc n as tys)
      | (imp_fvs, top_fvs, lcl_fvs) <- argsFVs env as
      = (StgConApp dc n as tys, imp_fvs, top_fvs, lcl_fvs)

    go (StgOpApp op as ty)
      | (imp_fvs, top_fvs, lcl_fvs) <- argsFVs env as
      = (StgOpApp op as ty, imp_fvs, top_fvs, lcl_fvs)

    go (StgCase scrut bndr ty alts)
      | (scrut',scrut_imp_fvs,scrut_top_fvs,scrut_lcl_fvs) <- exprFVs env scrut
      , (alts',alts_imp_fvss,alts_top_fvss,alts_lcl_fvss)
          <- mapAndUnzip4 (altFVs (addLocals [bndr] env)) alts
      , let top_fvs = scrut_top_fvs `unionVarSet` unionVarSets alts_top_fvss
            imp_fvs = scrut_imp_fvs `unionVarSet` unionVarSets alts_imp_fvss
            alts_lcl_fvs = unionDVarSets alts_lcl_fvss
            lcl_fvs = delDVarSet (unionDVarSet scrut_lcl_fvs alts_lcl_fvs) bndr
      = (StgCase scrut' bndr ty alts', imp_fvs, top_fvs, lcl_fvs)

    go (StgLet ext         bind body) = go_bind (StgLet ext) bind body
    go (StgLetNoEscape ext bind body) = go_bind (StgLetNoEscape ext) bind body

    go (StgTick tick e)
      | (e', imp_fvs, top_fvs, lcl_fvs) <- exprFVs env e
      , let lcl_fvs' = unionDVarSet (tickish tick) lcl_fvs
      = (StgTick tick e', imp_fvs, top_fvs, lcl_fvs')
        where
          tickish (Breakpoint _ _ ids) = mkDVarSet ids
          tickish _                    = emptyDVarSet

    go_bind dc bind body = (dc bind' body', imp_fvs, top_fvs, lcl_fvs)
      where
        env' = addLocals (bindersOf bind) env
        (body', body_imp_fvs, body_top_fvs, body_lcl_fvs) = exprFVs env' body
        (bind', bind_imp_fvs, bind_top_fvs, lcl_fvs)      = bindingFVs env' body_lcl_fvs bind
        top_fvs = bind_top_fvs `unionVarSet` body_top_fvs
        imp_fvs = bind_imp_fvs `unionVarSet` body_imp_fvs


rhsFVs :: Env -> StgRhs -> (CgStgRhs, ImpFVs, TopFVs, LocalFVs)
rhsFVs env (StgRhsClosure _ ccs uf bs body typ)
  | (body', imp_fvs, top_fvs, lcl_fvs) <- exprFVs (addLocals bs env) body
  , let lcl_fvs' = delDVarSetList lcl_fvs bs
  = (StgRhsClosure lcl_fvs' ccs uf bs body' typ, imp_fvs, top_fvs, lcl_fvs')
rhsFVs env (StgRhsCon ccs dc mu ts bs typ)
  | (imp_fvs, top_fvs, lcl_fvs) <- argsFVs env bs
  = (StgRhsCon ccs dc mu ts bs typ, imp_fvs, top_fvs, lcl_fvs)

argsFVs :: Env -> [StgArg] -> (ImpFVs, TopFVs, LocalFVs)
argsFVs env = foldl' f (emptyVarSet, emptyVarSet, emptyDVarSet)
  where
    f (imp_fvs,fvs,ids) StgLitArg{}   = (imp_fvs, fvs, ids)
    f (imp_fvs,fvs,ids) (StgVarArg v) = varFVs env v (imp_fvs, fvs, ids)

altFVs :: Env -> StgAlt -> (CgStgAlt, ImpFVs, TopFVs, LocalFVs)
altFVs env GenStgAlt{alt_con=con, alt_bndrs=bndrs, alt_rhs=e}
  | (e', imp_fvs, top_fvs, lcl_fvs) <- exprFVs (addLocals bndrs env) e
  , let lcl_fvs' = delDVarSetList lcl_fvs bndrs
  , let newAlt   = GenStgAlt{alt_con=con, alt_bndrs=bndrs, alt_rhs=e'}
  = (newAlt, imp_fvs, top_fvs, lcl_fvs')
