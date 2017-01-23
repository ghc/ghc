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
                        , exprIsLiteralString
                        , stripTicksE, stripTicksT, mkTicks )
import Literal          ( litIsTrivial )
import Type             ( tyConAppArgs )
import CoreSyn
import Outputable
import BasicTypes       ( isAlwaysActive )
import TrieMap
import Data.List        ( mapAccumL )

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
Let-bindings have two cases, implemented by addBinding.

* SUBSTITUTE: applies when the RHS is a variable or literal

     let x = y in ...(h x)....

  Here we want to extend the /substitution/ with x -> y, so that the
  (h x) in the body might CSE with an enclosing (let v = h y in ...).
  NB: the substitution maps InIds, so we extend the substitution with
      a biding for the original InId 'x'

  How can we have a variable on the RHS? Doesn't the simplifier inline them?

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

* EXTEND THE REVERSE MAPPING: applies in all other cases

     let x = h y in ...(h y)...

  Here we want to extend the /reverse mapping (cs_map)/ so that
  we CSE the (h y) call to x.

  Note that we use EXTEND even for a trivial expression, provided it
  is not a variable or literal. In particular this /includes/ type
  applications. This can be important (Trac #13156); e.g.
     case f @ Int of { r1 ->
     case f @ Int of { r2 -> ...
  Here we want to common-up the two uses of (f @ Int) so we can
  remove one of the case expressions.

  See also Note [Corner case for case expressions] for another
  reason not to use SUBSTITUTE for all trivial expressions.

Notice that
  - The SUBSTITUTE situation extends the substitution (cs_subst)
  - The EXTEND situation extends the reverse mapping (cs_map)

Notice also that in the SUBSTITUTE case we leave behind a binding
  x = y
even though we /also/ carry a substitution x -> y.  Can we just drop
the binding instead?  Well, not at top level! See SimplUtils
Note [Top level and postInlineUnconditionally]; and in any case CSE
applies only to the /bindings/ of the program, and we leave it to the
simplifier to propate effects to the RULES.  Finally, it doesn't seem
worth the effort to discard the nested bindings because the simplifier
will do it next.

Note [CSE for case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  case scrut_expr of x { ...alts... }
This is very like a strict let-binding
  let !x = scrut_expr in ...
So we use (addBinding x scrut_expr) to process scrut_expr and x, and as a
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

  By using addBinding we add the binding (wild1 -> a) to the substitution,
  which does exactly the right thing.

  (Notice this is exactly backwards to what the simplifier does, which
  is to try to replaces uses of 'a' with uses of 'wild1'.)

  This is the main reason that cseRHs is called with a trivial rhs.

* Non-trivial scrutinee
     case (f x) of y { pat -> ...let y = f x in ... }

  By using addBinding we'll add (f x :-> y) to the cs_map, and
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
Here is another reason that we do not use SUBSTITUTE for
all trivial expressions. Consider
   case x |> co of (y::Array# Int) { ... }

We do not want to extend the substitution with (y -> x |> co); since y
is of unlifted type, this would destroy the let/app invariant if (x |>
co) was not ok-for-speculation.

But surely (x |> co) is ok-for-speculation, becasue it's a trivial
expression, and x's type is also unlifted, presumably.  Well, maybe
not if you are using unsafe casts.  I actually found a case where we
had
   (x :: HValue) |> (UnsafeCo :: HValue ~ Array# Int)


************************************************************************
*                                                                      *
\section{Common subexpression}
*                                                                      *
************************************************************************
-}

cseProgram :: CoreProgram -> CoreProgram
cseProgram binds = snd (mapAccumL (cseBind True) emptyCSEnv binds)

cseBind :: Bool -> CSEnv -> CoreBind -> (CSEnv, CoreBind)
cseBind toplevel env (NonRec b e)
  = (env2, NonRec b2 e1)
  where
    e1         = tryForCSE toplevel env e
    (env1, b1) = addBinder env b
    (env2, b2) = addBinding env1 b b1 e1

cseBind toplevel env (Rec pairs)
  = (env2, Rec pairs')
  where
    (bndrs, rhss)  = unzip pairs
    (env1, bndrs1) = addRecBinders env bndrs
    rhss1          = map (tryForCSE toplevel env1) rhss
                     -- Process rhss in extended env1
    (env2, pairs') = foldl do_one (env1, []) (zip3 bndrs bndrs1 rhss1)
    do_one (env, pairs) (b, b1, e1)
         = (env1, (b2, e1) : pairs)
       where
         (env1, b2) = addBinding env b b1 e1

addBinding :: CSEnv                      -- Includes InId->OutId cloning
           -> InId
           -> OutId -> OutExpr           -- Processed binding
           -> (CSEnv, OutId)             -- Final env, final bndr
-- Extend the CSE env with a mapping [rhs -> out-id]
-- unless we can instead just substitute [in-id -> rhs]
addBinding env in_id out_id rhs'
  | no_cse    = (env,                              out_id)
  | use_subst = (extendCSSubst env in_id rhs',     out_id)
  | otherwise = (extendCSEnv env rhs' id_expr', zapped_id)
  where
    id_expr'  = varToCoreExpr out_id
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

    -- Should we use SUBSTITUTE or EXTEND?
    -- See Note [CSE for bindings]
    use_subst = case rhs' of
                   Var {} -> True
                   Lit l  -> litIsTrivial l
                   _      -> False

{-
Note [Take care with literal strings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider this example:

  x = "foo"#
  y = "foo"#
  ...x...y...x...y....

We would normally turn this into:

  x = "foo"#
  y = x
  ...x...x...x...x....

But this breaks an invariant of Core, namely that the RHS of a top-level binding
of type Addr# must be a string literal, not another variable. See Note
[CoreSyn top-level string literals] in CoreSyn.

For this reason, we special case top-level bindings to literal strings and leave
the original RHS unmodified. This produces:

  x = "foo"#
  y = "foo"#
  ...x...x...x...x....
-}

tryForCSE :: Bool -> CSEnv -> InExpr -> OutExpr
tryForCSE toplevel env expr
  | toplevel && exprIsLiteralString expr = expr
      -- See Note [Take care with literal strings]
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
cseExpr env (Type t)              = Type (substTy (csEnvSubst env) t)
cseExpr env (Coercion c)          = Coercion (substCo (csEnvSubst env) c)
cseExpr _   (Lit lit)             = Lit lit
cseExpr env (Var v)               = lookupSubst env v
cseExpr env (App f a)             = App (cseExpr env f) (tryForCSE False env a)
cseExpr env (Tick t e)            = Tick t (cseExpr env e)
cseExpr env (Cast e co)           = Cast (cseExpr env e) (substCo (csEnvSubst env) co)
cseExpr env (Lam b e)             = let (env', b') = addBinder env b
                                    in Lam b' (cseExpr env' e)
cseExpr env (Let bind e)          = let (env', bind') = cseBind False env bind
                                    in Let bind' (cseExpr env' e)
cseExpr env (Case e bndr ty alts) = cseCase env e bndr ty alts

cseCase :: CSEnv -> InExpr -> InId -> InType -> [InAlt] -> OutExpr
cseCase env scrut bndr ty alts
  = Case scrut1 bndr3 ty (map cse_alt alts)
  where
    scrut1 = tryForCSE False env scrut

    bndr1 = zapIdOccInfo bndr
      -- Zapping the OccInfo is needed because the extendCSEnv
      -- in cse_alt may mean that a dead case binder
      -- becomes alive, and Lint rejects that
    (env1, bndr2)    = addBinder env bndr1
    (alt_env, bndr3) = addBinding env1 bndr bndr2 scrut1
         -- addBinding: see Note [CSE for case expressions]

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
        = (DataAlt con, args', tryForCSE False new_env rhs)
        where
          (env', args') = addBinders alt_env args
          new_env       = extendCSEnv env' con_expr con_target
          con_expr      = mkAltExpr (DataAlt con) args' arg_tys

    cse_alt (con, args, rhs)
        = (con, args', tryForCSE False env' rhs)
        where
          (env', args') = addBinders alt_env args

{-
************************************************************************
*                                                                      *
\section{The CSE envt}
*                                                                      *
************************************************************************
-}

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
