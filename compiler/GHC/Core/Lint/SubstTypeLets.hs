{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998
-}

module GHC.Core.Lint.SubstTypeLets(
         substTypeLets
     ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.Subst
import GHC.Core.Utils( mkInScopeSetBndrs )

import GHC.Types.Var

import GHC.Utils.Misc( mapSnd )
import GHC.Utils.Outputable
import GHC.Utils.Panic

{- Note [Substituting type-lets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When desugaring pattern matching we really, really need non-Lint-acceptable type-lets.
Suppose we have
   f (MkT a (Just a (x::a)) (y::a)) = rhs1
   f (MkT b (Nothing b) (z::b)) = rhs2
where
   MkT :: ∀ a. Maybe a -> a -> T

We desugar this to
  f x = case x of
          MkT w (v :: Maybe w) (p:w)
              ->  let { a=w, b=w }
                  in let { y:a=p, z:b=p }
                  in case v of
                      Just a (x:a) -> rhs1 [y::a]
                      Nothing b    -> rhs2 [z::b]

Look at those type-lets { a=w, b=w }.  They make the type variables in the
/two/ separately-typechecked clauses for `f` line up with the /single/ pattern
match on `x`, which binds the type variable `w`.

Key point: the body of the let is only type-correct /after/ substituting
a:=w, b:=w.  Even the next let, { y:a=p } isn't type-correct without that
substitution, because (p:w).

So the `substTypeLets` pass does this:
  - It runs as part of Lint, as a pre-pass before the main Lint
  - It runs only when we are Linting the output of the desugarer
  - The result of substTypeLets is discarded after linting

When it finds a nested type-let
    let @a = ty in body
it substitutes a:=ty in `body`

Wrinkles

(STL1) It only substitutes /nested/ type-lets, not top level.

(STL2) You might think that we'd run it unconditionally, after desugaring.  But actually,
  the Simplifier (or SimpleOpt) will deal with these type-lets, so it is just Lint
  that we must placate.  We don't want to incur the cost of this pass except when
  we are Linting.

  TL;DR: we do substTypeLets as a pre-pass to the Lint pass that immediately follows
  desugaring. See `GHC.Core.lintPassResult`, and the `lpr_preSubst` field in
  `LintPassResultConfig`.

(STL3) Should `substTypeLets` process (stable) unfoldings? It does not need to
  because all unfoldings have `simpleOptExpr` applied to them, so the tricky
  type-lets will already be substituted.

  Of course we stil need to apply the current substitution, but that is done
  automatically by `substBndr`.
-}

substTypeLets :: CoreProgram -> CoreProgram
substTypeLets binds = map stl_top binds
  where
     stl_top (NonRec b r) = NonRec b (stlExpr empty_subst r)
     stl_top (Rec prs)    = Rec (mapSnd (stlExpr empty_subst) prs)

     empty_subst = mkEmptySubst $
                   mkInScopeSetBndrs binds

----------------------
stlBind :: Subst -> CoreBind -> (Subst, CoreBind)
stlBind subst (Rec prs)
  = assertPpr (not (any isTyVar bndrs)) (ppr prs) $
    (subst', Rec prs')
  where
    (bndrs,rhss) = unzip prs
    (subst', bndrs') = substRecBndrs subst bndrs
       -- substRecBndrs: see (STL3) in Note [Substituting type-lets]
    rhss' = map (stlExpr subst') rhss
    prs'  = bndrs' `zip` rhss'

stlBind subst (NonRec bndr rhs)
  = (subst', NonRec bndr' (stlExpr subst rhs))
  where
    (subst', bndr')  = substBndr subst bndr
      -- substBndr: see (STL3) in Note [Substituting type-lets]

----------------------
stlExpr :: Subst -> CoreExpr -> CoreExpr

stlExpr subst (Let (NonRec tv (Type ty)) body)
  = -- This equation is the main payload of the entire pass!
    stlExpr (extendTvSubst subst tv (substTy subst ty)) body

stlExpr subst (Let bind body)
  = Let bind' (stlExpr subst' body)
  where
    (subst', bind') = stlBind subst bind

stlExpr subst (Lam bndr body)
  = Lam bndr' (stlExpr subst' body)
  where
    (subst', bndr') = substBndr subst bndr

stlExpr subst (Case scrut bndr ty alts)
  = Case (stlExpr subst scrut) bndr' (substTy subst ty)
         (map stl_alt alts)
  where
    (subst', bndr') = substBndr subst bndr

    stl_alt (Alt con bndrs rhs)
       = Alt con bndrs' (stlExpr subst'' rhs)
       where
         (subst'', bndrs') = substBndrs subst' bndrs

-- Simple cases
stlExpr _     (Lit l)       = Lit l
stlExpr subst (Var v)       = lookupIdSubst subst v
stlExpr subst (App e1 e2)   = App (stlExpr subst e1) (stlExpr subst e2)
stlExpr subst (Type ty)     = Type (substTy subst ty)
stlExpr subst (Tick t e)    = Tick (substTickish subst t) (stlExpr subst e)
stlExpr subst (Cast e co)   = Cast (stlExpr subst e) (substCo subst co)
stlExpr subst (Coercion co) = Coercion (substCo subst co)
