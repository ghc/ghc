{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

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
import GHC.Types.Tickish ( GenTickish(Breakpoint) )
import GHC.Utils.Misc

import Data.Maybe ( mapMaybe )

newtype Env
  = Env
  { locals :: IdSet
  }

{-
    Note [The FVPass Constraint]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Currently the free variable pass is used in two places:
* The late lambda lifting pass in order to estimate the size of closures
* Before codegen to figure out the actual free variables.

However we have a different parameterisation for each of these.
In order to avoid code duplication I've added the `FVPass i o` constraint.

The principle is simple: We don't modify the input except for replacing
XRhsClosure extension points with DIdSet.

FVPass encodes this fact in the form of a constraint.
-}

type FVPass i o = (BinderP i ~ Id,
                   BinderP o ~ Id,

                   -- The RhsClosure extension will contain the free variables afterwards
                   XRhsClosure o ~ DIdSet,
                   XLet i ~ XLet o,
                   XLetNoEscape i ~ XLetNoEscape o,
                   XStgApp i ~ XStgApp o
                  )

emptyEnv :: Env
emptyEnv = Env emptyVarSet

addLocals :: [Id] -> Env -> Env
addLocals bndrs env
  = env { locals = extendVarSetList (locals env) bndrs }

-- | Annotates a top-level STG binding group with its free variables.
annTopBindingsFreeVars :: FVPass i o => [GenStgTopBinding i] -> [GenStgTopBinding o]
annTopBindingsFreeVars = map go
  where
    go (StgTopStringLit id bs) = StgTopStringLit id bs
    go (StgTopLifted bind)
      = StgTopLifted (annBindingFreeVars bind)

-- | Annotates an STG binding with its free variables.
annBindingFreeVars :: FVPass i o => GenStgBinding i -> GenStgBinding o
annBindingFreeVars = fst . binding emptyEnv emptyDVarSet

boundIds :: (BinderP i ~ Id) => GenStgBinding i -> [Id]
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

binding :: FVPass i o => Env -> DIdSet -> GenStgBinding i -> (GenStgBinding o, DIdSet)
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

expr :: FVPass i o => Env -> GenStgExpr i -> (GenStgExpr o, DIdSet)
expr env = go
  where
    go (StgApp ext occ as)
      = (StgApp ext occ as, unionDVarSet (args env as) (mkFreeVarSet env [occ]))
    go (StgLit lit) = (StgLit lit, emptyDVarSet)
    go (StgConApp dc n as tys) = (StgConApp dc n as tys, args env as)
    go (StgOpApp op as ty) = (StgOpApp op as ty, args env as)
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
        tickish (Breakpoint _ _ ids) = mkDVarSet ids
        tickish _                    = emptyDVarSet

    go_bind dc bind body = (dc bind' body', fvs)
      where
        -- See Note [Tracking local binders]
        env' = addLocals (boundIds bind) env
        (body', body_fvs) = expr env' body
        (bind', fvs) = binding env' body_fvs bind

rhs :: FVPass i o => Env -> GenStgRhs i -> (GenStgRhs o, DIdSet)
rhs env (StgRhsClosure _ ccs uf bndrs body)
  = (StgRhsClosure fvs ccs uf bndrs body', fvs)
  where
    -- See Note [Tracking local binders]
    (body', body_fvs) = expr (addLocals bndrs env) body
    fvs = delDVarSetList body_fvs bndrs
rhs env (StgRhsCon ccs dc mu ts as) = (StgRhsCon ccs dc mu ts as, args env as)

alt :: FVPass i o => Env -> GenStgAlt i -> (GenStgAlt o, DIdSet)
alt env (con, bndrs, e) = ((con, bndrs, e'), fvs)
  where
    -- See Note [Tracking local binders]
    (e', rhs_fvs) = expr (addLocals bndrs env) e
    fvs = delDVarSetList rhs_fvs bndrs
