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
    annTopBindingsFreeVars,
    annBindingFreeVars
  ) where

import GHC.Prelude

import GHC.Stg.Syntax
import GHC.Types.Id
import GHC.Types.Var.Set
import GHC.Core    ( Tickish(Breakpoint) )
import GHC.Utils.Outputable
import GHC.Utils.Misc

import Data.Maybe ( mapMaybe )

newtype Env
  = Env
  { locals :: IdSet
  }

emptyEnv :: Env
emptyEnv = Env emptyVarSet

addLocals :: [Id] -> Env -> Env
addLocals bndrs env
  = env { locals = extendVarSetList (locals env) bndrs }

-- | Annotates a top-level STG binding group with its free variables.
annTopBindingsFreeVars :: [StgTopBinding] -> [CgStgTopBinding]
annTopBindingsFreeVars = map go
  where
    go (StgTopStringLit id bs) = StgTopStringLit id bs
    go (StgTopLifted bind)
      = StgTopLifted (annBindingFreeVars bind)

-- | Annotates an STG binding with its free variables.
annBindingFreeVars :: StgBinding -> CgStgBinding
annBindingFreeVars = fst . binding emptyEnv emptyDVarSet

boundIds :: StgBinding -> [Id]
boundIds (StgNonRec b _) = [b]
boundIds (StgRec pairs)  = map fst pairs

-- Note [Tracking local binders]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 'locals' contains non-toplevel, non-imported binders.
-- We maintain the set in 'expr', 'alt' and 'rhs', which are the only
-- places where new local binders are introduced.
-- Why do it there rather than in 'binding'? Two reasons:
--
--   1. We call 'binding' from 'annTopBindingsFreeVars', which would
--      add top-level bindings to the 'locals' set.
--   2. In the let(-no-escape) case, we need to extend the environment
--      prior to analysing the body, but we also need the fvs from the
--      body to analyse the RHSs. No way to do this without some
--      knot-tying.

-- | This makes sure that only local, non-global free vars make it into the set.
mkFreeVarSet :: Env -> [Id] -> DIdSet
mkFreeVarSet env = mkDVarSet . filter (`elemVarSet` locals env)

args :: Env -> [StgArg] -> DIdSet
args env = mkFreeVarSet env . mapMaybe f
  where
    f (StgVarArg occ) = Just occ
    f _               = Nothing

binding :: Env -> DIdSet -> StgBinding -> (CgStgBinding, DIdSet)
binding env body_fv (StgNonRec bndr r) = (StgNonRec bndr r', fvs)
  where
    -- See Note [Tracking local binders]
    (r', rhs_fvs) = rhs env r
    fvs = delDVarSet body_fv bndr `unionDVarSet` rhs_fvs
binding env body_fv (StgRec pairs) = (StgRec pairs', fvs)
  where
    -- See Note [Tracking local binders]
    bndrs = map fst pairs
    (rhss, rhs_fvss) = mapAndUnzip (rhs env . snd) pairs
    pairs' = zip bndrs rhss
    fvs = delDVarSetList (unionDVarSets (body_fv:rhs_fvss)) bndrs

expr :: Env -> StgExpr -> (CgStgExpr, DIdSet)
expr env = go
  where
    go (StgApp occ as)
      = (StgApp occ as, unionDVarSet (args env as) (mkFreeVarSet env [occ]))
    go (StgLit lit) = (StgLit lit, emptyDVarSet)
    go (StgConApp dc as tys) = (StgConApp dc as tys, args env as)
    go (StgOpApp op as ty) = (StgOpApp op as ty, args env as)
    go StgLam{} = pprPanic "StgFVs: StgLam" empty
    go (StgCase scrut bndr ty alts) = (StgCase scrut' bndr ty alts', fvs)
      where
        (scrut', scrut_fvs) = go scrut
        -- See Note [Tracking local binders]
        (alts', alt_fvss) = mapAndUnzip (alt (addLocals [bndr] env)) alts
        alt_fvs = unionDVarSets alt_fvss
        fvs = delDVarSet (unionDVarSet scrut_fvs alt_fvs) bndr
    go (StgLet ext bind body) = go_bind (StgLet ext) bind body
    go (StgLetNoEscape ext bind body) = go_bind (StgLetNoEscape ext) bind body
    go (StgTick tick e) = (StgTick tick e', fvs')
      where
        (e', fvs) = go e
        fvs' = unionDVarSet (tickish tick) fvs
        tickish (Breakpoint _ ids) = mkDVarSet ids
        tickish _                  = emptyDVarSet

    go_bind dc bind body = (dc bind' body', fvs)
      where
        -- See Note [Tracking local binders]
        env' = addLocals (boundIds bind) env
        (body', body_fvs) = expr env' body
        (bind', fvs) = binding env' body_fvs bind

rhs :: Env -> StgRhs -> (CgStgRhs, DIdSet)
rhs env (StgRhsClosure _ ccs uf bndrs body)
  = (StgRhsClosure fvs ccs uf bndrs body', fvs)
  where
    -- See Note [Tracking local binders]
    (body', body_fvs) = expr (addLocals bndrs env) body
    fvs = delDVarSetList body_fvs bndrs
rhs env (StgRhsCon ccs dc as) = (StgRhsCon ccs dc as, args env as)

alt :: Env -> StgAlt -> (CgStgAlt, DIdSet)
alt env (con, bndrs, e) = ((con, bndrs, e'), fvs)
  where
    -- See Note [Tracking local binders]
    (e', rhs_fvs) = expr (addLocals bndrs env) e
    fvs = delDVarSetList rhs_fvs bndrs
