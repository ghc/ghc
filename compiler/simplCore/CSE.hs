{-
(c) The AQUA Project, Glasgow University, 1993-1998

\section{Common subexpression}
-}

{-# LANGUAGE CPP #-}

module CSE (cseProgram) where

#include "HsVersions.h"

import CoreSubst
import Var              ( Var )
import Id               ( Id, idType, idUnfolding, idInlineActivation
                        , zapIdOccInfo, zapIdUsageInfo )
import CoreUtils        ( mkAltExpr
                        , exprIsTrivial, exprOkForSpeculation
                        , stripTicksE, stripTicksT, mkTicks )
import Type             ( Type, tyConAppArgs, isUnliftedType )
import CoreSyn
import Outputable
import BasicTypes       ( isAlwaysActive )
import TrieMap

import Data.List

{-
                        Simple common sub-expression
                        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we see
        x1 = C a b
        x2 = C x1 b
we build up a reverse mapping:   C a b  -> x1
                                 C x1 b -> x2
and apply that to the rest of the program.

When we then see
        y1 = C a b
        y2 = C y1 b
we replace the C a b with x1.  But then we *dont* want to
add   x1 -> y1  to the mapping.  Rather, we want the reverse, y1 -> x1
so that a subsequent binding
        y2 = C y1 b
will get transformed to C x1 b, and then to x2.

So we carry an extra var->var substitution which we apply *before* looking up in the
reverse mapping.


Note [Shadowing]
~~~~~~~~~~~~~~~~
We have to be careful about shadowing.
For example, consider
        f = \x -> let y = x+x in
                      h = \x -> x+x
                  in ...

Here we must *not* do CSE on the inner x+x!  The simplifier used to guarantee no
shadowing, but it doesn't any more (it proved too hard), so we clone as we go.
We can simply add clones to the substitution already described.


Note [CSE for bindings]
~~~~~~~~~~~~~~~~~~~~~~~
Let-bindings have two cases, implemnted by cseRhs.

* Trivial RHS:
     let x = y in ...(h x)....

  Here we want to extend the /substitution/ with x -> y, so that the
  (h x) in the body might CSE with an enclosing (let v = h y in ...).
  NB: the substitution maps InIds, so we extend the substitution with
      a biding for the original InId 'x'

  How can we have a trivial RHS? Doens't the simplifier inline them?

    - First, the original RHS might have been (g z) which has CSE'd
      with an enclosing (let y = g z in ...).  This is super-important.
      See Trac #5996:
         x1 = C a b
         x2 = C x1 b
         y1 = C a b
         y2 = C y1 b
      Here we CSE y1's rhs to 'x1', and then we must add (y1->x1) to
      the substitution so that we can CSE the binding for y2.

    - Second, we use cseRHS for case expression scrutinees too;
      see Note [CSE for case expressions]

* Non-trivial RHS
     let x = h y in ...(h y)...

  Here we want to extend the /reverse mapping (cs_map)/ so that
  we CSE the (h y) call to x.

Notice that
  - the trivial-RHS situation extends the substitution (cs_subst)
  - the non-trivial-RHS situation extends the reverse mapping (cs_map)

Note [CSE for case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  case scrut_expr of x { ...alts... }
This is very like a strict let-binding
  let !x = scrut_expr in ...
So we use (cseRhs x scrut_expr) to process scrut_expr and x, and as a
result all the stuff under Note [CSE for bindings] applies directly.

For example:

* Trivial scrutinee
     f = \x -> case x of wild {
                 (a:as) -> case a of wild1 {
                             (p,q) -> ...(wild1:as)...

  Here, (wild1:as) is morally the same as (a:as) and hence equal to
  wild. But that's not quite obvious.  In the rest of the compiler we
  want to keep it as (wild1:as), but for CSE purpose that's a bad
  idea.

  By using cseRhs we add the binding (wild1 -> a) to the substitution,
  which does exactly the right thing.

  (Notice this is exactly backwards to what the simplifier does, which
  is to try to replaces uses of 'a' with uses of 'wild1'.)

  This is the main reason that cseRHs is called with a trivial rhs.

* Non-trivial scrutinee
     case (f x) of y { pat -> ...let y = f x in ... }

  By using cseRhs we'll add (f x :-> y) to the cs_map, and
  thereby CSE the inner (f x) to y.

Note [CSE for INLINE and NOINLINE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are some subtle interactions of CSE with functions that the user
has marked as INLINE or NOINLINE. (Examples from Roman Leshchinskiy.)
Consider

        yes :: Int  {-# NOINLINE yes #-}
        yes = undefined

        no :: Int   {-# NOINLINE no #-}
        no = undefined

        foo :: Int -> Int -> Int  {-# NOINLINE foo #-}
        foo m n = n

        {-# RULES "foo/no" foo no = id #-}

        bar :: Int -> Int
        bar = foo yes

We do not expect the rule to fire.  But if we do CSE, then we risk
getting yes=no, and the rule does fire.  Actually, it won't because
NOINLINE means that 'yes' will never be inlined, not even if we have
yes=no.  So that's fine (now; perhaps in the olden days, yes=no would
have substituted even if 'yes' was NOINLINE).

But we do need to take care.  Consider

        {-# NOINLINE bar #-}
        bar = <rhs>     -- Same rhs as foo

        foo = <rhs>

If CSE produces
        foo = bar
then foo will never be inlined to <rhs> (when it should be, if <rhs>
is small).  The conclusion here is this:

   We should not add
       <rhs> :-> bar
  to the CSEnv if 'bar' has any constraints on when it can inline;
  that is, if its 'activation' not always active.  Otherwise we
  might replace <rhs> by 'bar', and then later be unable to see that it
  really was <rhs>.

Note that we do not (currently) do CSE on the unfolding stored inside
an Id, even if is a 'stable' unfolding.  That means that when an
unfolding happens, it is always faithful to what the stable unfolding
originally was.

Note [CSE for stable unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   {-# Unf = Stable (\pq. build blah) #-}
   foo = x

Here 'foo' has a stable unfolding, but its (optimised) RHS is trivial.
(Turns out that this actually happens for the enumFromTo method of
the Integer instance of Enum in GHC.Enum.)  Then we obviously do NOT
want to extend the substitution with (foo->x)!   See similar
SimplUtils Note [Stable unfoldings and postInlineUnconditionally].

Nor do we want to change the reverse mapping. Suppose we have

   {-# Unf = Stable (\pq. build blah) #-}
   foo = <expr>
   bar = <expr>

There could conceivably be merit in rewriting the RHS of bar:
   bar = foo
but now bar's inlining behaviour will change, and importing
modules might see that.  So it seems dodgy and we don't do it.

Note [Corner case for case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consdider
   case x |> co of (y::Array# Int) { ... }

Is it ok to extend the substutition with (y -> x |> co)?
Because y is of unlifted type, this is only OK if (x |> co) is
ok-for-speculation, else we'll destroy the let/app invariant.
But surely it is ok-for-speculation, becasue it's a trivial
expression, and x's type is also unlifted, presumably.

Well, maybe not if you are using unsafe casts.  I actually found
a case where we had
   (x :: HValue) |> (UnsafeCo :: HValue ~ Array# Int)
This is a vanishingly strange corner case, but we still have
to check.

We do the check in cseRhs, but it can't fire when cseRhs is called
from a let-binding, because they are always ok-for-speculation.  Never
mind!


************************************************************************
*                                                                      *
\section{Common subexpression}
*                                                                      *
************************************************************************
-}

cseProgram :: CoreProgram -> CoreProgram
cseProgram binds = snd (mapAccumL cseBind emptyCSEnv binds)

cseBind :: CSEnv -> CoreBind -> (CSEnv, CoreBind)
cseBind env (NonRec b e)
  = (env2, NonRec b'' e')
  where
    (env1, b') = addBinder env b
    (env2, (b'', e')) = cseRhs env1 b b' e

cseBind env (Rec pairs)
  = (env2, Rec pairs')
  where
    (env1, bs')    = addRecBinders env (map fst pairs)
    (env2, pairs') = mapAccumL cse_rhs env1 (bs' `zip` pairs)
    cse_rhs env (b', (b,e)) = cseRhs env b b' e

cseRhs :: CSEnv -> InId -> OutId -> InExpr -> (CSEnv, (OutId, OutExpr))
cseRhs env in_id out_id rhs
  | no_cse      = (env,                              (out_id, rhs'))
  | ok_to_subst = (extendCSSubst env in_id rhs',     (out_id, rhs'))
  | otherwise   = (extendCSEnv env rhs' id_expr', (zapped_id, rhs'))
  where
    id_expr'  = varToCoreExpr out_id
    rhs'      = tryForCSE env rhs
    zapped_id = zapIdUsageInfo out_id
       -- Putting the Id into the cs_map makes it possible that
       -- it'll become shared more than it is now, which would
       -- invalidate (the usage part of) its demand info.
       --    This caused Trac #100218.
       -- Easiest thing is to zap the usage info; subsequently
       -- performing late demand-analysis will restore it.  Don't zap
       -- the strictness info; it's not necessary to do so, and losing
       -- it is bad for performance if you don't do late demand
       -- analysis

    no_cse = not (isAlwaysActive (idInlineActivation out_id))
             -- See Note [CSE for INLINE and NOINLINE]
          || isStableUnfolding (idUnfolding out_id)
             -- See Note [CSE for stable unfoldings]

    -- See Note [CSE for bindings]
    ok_to_subst = exprIsTrivial rhs'
               && (not (isUnliftedType (idType out_id))
                   || exprOkForSpeculation rhs')
               -- See Note [Corner case for case expressions]

tryForCSE :: CSEnv -> InExpr -> OutExpr
tryForCSE env expr
  | exprIsTrivial expr'              = expr'       -- No point
  | Just e <- lookupCSEnv env expr'' = mkTicks ticks e
  | otherwise                        = expr'
    -- The varToCoreExpr is needed if we have
    --   case e of xco { ...case e of yco { ... } ... }
    -- Then CSE will substitute yco -> xco;
    -- but these are /coercion/ variables
  where
    expr'  = cseExpr env expr
    expr'' = stripTicksE tickishFloatable expr'
    ticks  = stripTicksT tickishFloatable expr'
    -- We don't want to lose the source notes when a common sub
    -- expression gets eliminated. Hence we push all (!) of them on
    -- top of the replaced sub-expression. This is probably not too
    -- useful in practice, but upholds our semantics.

cseExpr :: CSEnv -> InExpr -> OutExpr
cseExpr env (Type t)               = Type (substTy (csEnvSubst env) t)
cseExpr env (Coercion c)           = Coercion (substCo (csEnvSubst env) c)
cseExpr _   (Lit lit)              = Lit lit
cseExpr env (Var v)                = lookupSubst env v
cseExpr env (App f a)              = App (cseExpr env f) (tryForCSE env a)
cseExpr env (ConApp dc args)       = ConApp dc (map (tryForCSE env) args)
cseExpr env (Tick t e)             = Tick t (cseExpr env e)
cseExpr env (Cast e co)            = Cast (cseExpr env e) (substCo (csEnvSubst env) co)
cseExpr env (Lam b e)              = let (env', b') = addBinder env b
                                     in Lam b' (cseExpr env' e)
cseExpr env (Let bind e)           = let (env', bind') = cseBind env bind
                                     in Let bind' (cseExpr env' e)
cseExpr env (Case e bndr ty alts)  = cseCase env e bndr ty alts

cseCase :: CSEnv -> InExpr -> InId -> InType -> [InAlt] -> OutExpr
cseCase env scrut bndr ty alts
  = Case scrut' bndr3 ty (map cse_alt alts)
  where
    bndr1 = zapIdOccInfo bndr
      -- Zapping the OccInfo is needed because the extendCSEnv
      -- in cse_alt may mean that a dead case binder
      -- becomes alive, and Lint rejects that
    (env1, bndr2)              = addBinder env bndr1
    (alt_env, (bndr3, scrut')) = cseRhs env1 bndr bndr2 scrut
         -- cseRhs: see Note [CSE for case expressions]

    con_target :: OutExpr
    con_target = lookupSubst alt_env bndr

    arg_tys :: [OutType]
    arg_tys = tyConAppArgs (idType bndr3)

    cse_alt (DataAlt con, args, rhs)
        | not (null args)
                -- Don't try CSE if there are no args; it just increases the number
                -- of live vars.  E.g.
                --      case x of { True -> ....True.... }
                -- Don't replace True by x!
                -- Hence the 'null args', which also deal with literals and DEFAULT
        = (DataAlt con, args', tryForCSE new_env rhs)
        where
          (env', args') = addBinders alt_env args
          new_env       = extendCSEnv env' con_expr con_target
          con_expr      = mkAltExpr (DataAlt con) args' arg_tys

    cse_alt (con, args, rhs)
        = (con, args', tryForCSE env' rhs)
        where
          (env', args') = addBinders alt_env args

{-
************************************************************************
*                                                                      *
\section{The CSE envt}
*                                                                      *
************************************************************************
-}

type InExpr  = CoreExpr         -- Pre-cloning
type InId    = Id
type InAlt   = CoreAlt
type InType  = Type

type OutExpr  = CoreExpr        -- Post-cloning
type OutId    = Id
type OutType  = Type

data CSEnv
  = CS { cs_subst :: Subst  -- Maps InBndrs to OutExprs
            -- The substitution variables to
            -- /trivial/ OutExprs, not arbitrary expressions

       , cs_map   :: CoreMap OutExpr   -- The reverse mapping
            -- Maps a OutExpr to a /trivial/ OutExpr
            -- The key of cs_map is stripped of all Ticks
       }

emptyCSEnv :: CSEnv
emptyCSEnv = CS { cs_map = emptyCoreMap, cs_subst = emptySubst }

lookupCSEnv :: CSEnv -> OutExpr -> Maybe OutExpr
lookupCSEnv (CS { cs_map = csmap }) expr
  = lookupCoreMap csmap expr

extendCSEnv :: CSEnv -> OutExpr -> OutExpr -> CSEnv
extendCSEnv cse expr triv_expr
  = cse { cs_map = extendCoreMap (cs_map cse) sexpr triv_expr }
  where
    sexpr = stripTicksE tickishFloatable expr

csEnvSubst :: CSEnv -> Subst
csEnvSubst = cs_subst

lookupSubst :: CSEnv -> Id -> OutExpr
lookupSubst (CS { cs_subst = sub}) x = lookupIdSubst (text "CSE.lookupSubst") sub x

extendCSSubst :: CSEnv -> Id  -> CoreExpr -> CSEnv
extendCSSubst cse x rhs = cse { cs_subst = extendSubst (cs_subst cse) x rhs }

addBinder :: CSEnv -> Var -> (CSEnv, Var)
addBinder cse v = (cse { cs_subst = sub' }, v')
                where
                  (sub', v') = substBndr (cs_subst cse) v

addBinders :: CSEnv -> [Var] -> (CSEnv, [Var])
addBinders cse vs = (cse { cs_subst = sub' }, vs')
                where
                  (sub', vs') = substBndrs (cs_subst cse) vs

addRecBinders :: CSEnv -> [Id] -> (CSEnv, [Id])
addRecBinders cse vs = (cse { cs_subst = sub' }, vs')
                where
                  (sub', vs') = substRecBndrs (cs_subst cse) vs
