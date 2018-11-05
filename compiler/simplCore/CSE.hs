{-
(c) The AQUA Project, Glasgow University, 1993-1998

\section{Common subexpression}
-}

{-# LANGUAGE CPP #-}

module CSE (cseProgram, cseOneExpr) where

#include "HsVersions.h"

import GhcPrelude

import CoreSubst
import Var              ( Var )
import VarEnv           ( elemInScopeSet, mkInScopeSet )
import Id               ( Id, idType, isDeadBinder
                        , idInlineActivation, setInlineActivation
                        , zapIdOccInfo, zapIdUsageInfo, idInlinePragma
                        , isJoinId, isJoinId_maybe )
import CoreUtils        ( mkAltExpr, eqExpr
                        , exprIsTickedString
                        , stripTicksE, stripTicksT, mkTicks )
import CoreFVs          ( exprFreeVars )
import Type             ( tyConAppArgs )
import CoreSyn
import Outputable
import BasicTypes
import CoreMap
import Util             ( filterOut )
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

* SUBSTITUTE: applies when the RHS is a variable

     let x = y in ...(h x)....

  Here we want to extend the /substitution/ with x -> y, so that the
  (h x) in the body might CSE with an enclosing (let v = h y in ...).
  NB: the substitution maps InIds, so we extend the substitution with
      a binding for the original InId 'x'

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

    - Second, we use addBinding for case expression scrutinees too;
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

  This is the main reason that addBinding is called with a trivial rhs.

* Non-trivial scrutinee
     case (f x) of y { pat -> ...let z = f x in ... }

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

An except to the rule is when the INLINE pragma is not from the user, e.g. from
WorkWrap (see Note [Wrapper activation]). We can tell because noUserInlineSpec
is then true.

Note that we do not (currently) do CSE on the unfolding stored inside
an Id, even if it is a 'stable' unfolding.  That means that when an
unfolding happens, it is always faithful to what the stable unfolding
originally was.

Note [CSE for stable unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   {-# Unf = Stable (\pq. build blah) #-}
   foo = x

Here 'foo' has a stable unfolding, but its (optimised) RHS is trivial.
(Turns out that this actually happens for the enumFromTo method of
the Integer instance of Enum in GHC.Enum.)  Suppose moreover that foo's
stable unfolding originates from an INLINE or INLINEABLE pragma on foo.
Then we obviously do NOT want to extend the substitution with (foo->x),
because we promised to inline foo as what the user wrote.  See similar
SimplUtils Note [Stable unfoldings and postInlineUnconditionally].

Nor do we want to change the reverse mapping. Suppose we have

   {-# Unf = Stable (\pq. build blah) #-}
   foo = <expr>
   bar = <expr>

There could conceivably be merit in rewriting the RHS of bar:
   bar = foo
but now bar's inlining behaviour will change, and importing
modules might see that.  So it seems dodgy and we don't do it.

Stable unfoldings are also created during worker/wrapper when we decide
that a function's definition is so small that it should always inline.
In this case we still want to do CSE (#13340). Hence the use of
isAnyInlinePragma rather than isStableUnfolding.

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

Note [CSE for join points?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must not be naive about join points in CSE:
   join j = e in
   if b then jump j else 1 + e
The expression (1 + jump j) is not good (see Note [Invariants on join points] in
CoreSyn). This seems to come up quite seldom, but it happens (first seen
compiling ppHtml in Haddock.Backends.Xhtml).

We could try and be careful by tracking which join points are still valid at
each subexpression, but since join points aren't allocated or shared, there's
less to gain by trying to CSE them. (#13219)

Note [Look inside join-point binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Another way how CSE for joint points is tricky is

  let join foo x = (x, 42)
      join bar x = (x, 42)
  in … jump foo 1 … jump bar 2 …

naively, CSE would turn this into

  let join foo x = (x, 42)
      join bar = foo
  in … jump foo 1 … jump bar 2 …

but now bar is a join point that claims arity one, but its right-hand side
is not a lambda, breaking the join-point invariant (this was #15002).

So `cse_bind` must zoom past the lambdas of a join point (using
`collectNBinders`) and resume searching for CSE opportunities only in
the body of the join point.

Note [CSE for recursive bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  f = \x ... f....
  g = \y ... g ...
where the "..." are identical.  Could we CSE them?  In full generality
with mutual recursion it's quite hard; but for self-recursive bindings
(which are very common) it's rather easy:

* Maintain a separate cs_rec_map, that maps
      (\f. (\x. ...f...) ) -> f
  Note the \f in the domain of the mapping!

* When we come across the binding for 'g', look up (\g. (\y. ...g...))
  Bingo we get a hit.  So we can replace the 'g' binding with
     g = f

We can't use cs_map for this, because the key isn't an expression of
the program; it's a kind of synthetic key for recursive bindings.


************************************************************************
*                                                                      *
\section{Common subexpression}
*                                                                      *
************************************************************************
-}

cseProgram :: CoreProgram -> CoreProgram
cseProgram binds = snd (mapAccumL (cseBind TopLevel) emptyCSEnv binds)

cseBind :: TopLevelFlag -> CSEnv -> CoreBind -> (CSEnv, CoreBind)
cseBind toplevel env (NonRec b e)
  = (env2, NonRec b2 e2)
  where
    (env1, b1)       = addBinder env b
    (env2, (b2, e2)) = cse_bind toplevel env1 (b,e) b1

cseBind toplevel env (Rec [(in_id, rhs)])
  | noCSE in_id
  = (env1, Rec [(out_id, rhs')])

  -- See Note [CSE for recursive bindings]
  | Just previous <- lookupCSRecEnv env out_id rhs''
  , let previous' = mkTicks ticks previous
        out_id'   = delayInlining toplevel out_id
  = -- We have a hit in the recursive-binding cache
    (extendCSSubst env1 in_id previous', NonRec out_id' previous')

  | otherwise
  = (extendCSRecEnv env1 out_id rhs'' id_expr', Rec [(zapped_id, rhs')])

  where
    (env1, [out_id]) = addRecBinders env [in_id]
    rhs'  = cseExpr env1 rhs
    rhs'' = stripTicksE tickishFloatable rhs'
    ticks = stripTicksT tickishFloatable rhs'
    id_expr'  = varToCoreExpr out_id
    zapped_id = zapIdUsageInfo out_id

cseBind toplevel env (Rec pairs)
  = (env2, Rec pairs')
  where
    (env1, bndrs1) = addRecBinders env (map fst pairs)
    (env2, pairs') = mapAccumL do_one env1 (zip pairs bndrs1)

    do_one env (pr, b1) = cse_bind toplevel env pr b1

-- | Given a binding of @in_id@ to @in_rhs@, and a fresh name to refer
-- to @in_id@ (@out_id@, created from addBinder or addRecBinders),
-- first try to CSE @in_rhs@, and then add the resulting (possibly CSE'd)
-- binding to the 'CSEnv', so that we attempt to CSE any expressions
-- which are equal to @out_rhs@.
cse_bind :: TopLevelFlag -> CSEnv -> (InId, InExpr) -> OutId -> (CSEnv, (OutId, OutExpr))
cse_bind toplevel env (in_id, in_rhs) out_id
  | isTopLevel toplevel, exprIsTickedString in_rhs
      -- See Note [Take care with literal strings]
  = (env', (out_id', in_rhs))

  | Just arity <- isJoinId_maybe in_id
      -- See Note [Look inside join-point binders]
  = let (params, in_body) = collectNBinders arity in_rhs
        (env', params') = addBinders env params
        out_body = tryForCSE env' in_body
    in (env, (out_id, mkLams params' out_body))

  | otherwise
  = (env', (out_id'', out_rhs))
  where
    (env', out_id') = addBinding env in_id out_id out_rhs
    (cse_done, out_rhs) = try_for_cse env in_rhs
    out_id'' | cse_done  = delayInlining toplevel out_id'
             | otherwise = out_id'

delayInlining :: TopLevelFlag -> Id -> Id
-- Add a NOINLINE[2] if the Id doesn't have an INLNE pragma already
delayInlining top_lvl bndr
  | isTopLevel top_lvl
  , isAlwaysActive (idInlineActivation bndr)
  = bndr `setInlineActivation` activeAfterInitial
  | otherwise
  = bndr

addBinding :: CSEnv                      -- Includes InId->OutId cloning
           -> InVar                      -- Could be a let-bound type
           -> OutId -> OutExpr           -- Processed binding
           -> (CSEnv, OutId)             -- Final env, final bndr
-- Extend the CSE env with a mapping [rhs -> out-id]
-- unless we can instead just substitute [in-id -> rhs]
--
-- It's possible for the binder to be a type variable (see
-- Note [Type-let] in CoreSyn), in which case we can just substitute.
addBinding env in_id out_id rhs'
  | not (isId in_id) = (extendCSSubst env in_id rhs',     out_id)
  | noCSE in_id      = (env,                              out_id)
  | use_subst        = (extendCSSubst env in_id rhs',     out_id)
  | otherwise        = (extendCSEnv env rhs' id_expr', zapped_id)
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

    -- Should we use SUBSTITUTE or EXTEND?
    -- See Note [CSE for bindings]
    use_subst = case rhs' of
                   Var {} -> True
                   _      -> False

-- | Given a binder `let x = e`, this function
-- determines whether we should add `e -> x` to the cs_map
noCSE :: InId -> Bool
noCSE id =  not (isAlwaysActive (idInlineActivation id)) &&
            not (noUserInlineSpec (inlinePragmaSpec (idInlinePragma id)))
             -- See Note [CSE for INLINE and NOINLINE]
         || isAnyInlinePragma (idInlinePragma id)
             -- See Note [CSE for stable unfoldings]
         || isJoinId id
             -- See Note [CSE for join points?]


{- Note [Take care with literal strings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

Now 'y' will be discarded as dead code, and we are done.

The net effect is that for the y-binding we want to
  - Use SUBSTITUTE, by extending the substitution with  y :-> x
  - but leave the original binding for y undisturbed

This is done by cse_bind.  I got it wrong the first time (Trac #13367).

Note [Delay inlining after CSE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose (Trac #15445) we have
   f,g :: Num a => a -> a
   f x = ...f (x-1).....
   g y = ...g (y-1) ....

and we make some specialisations of 'g', either automatically, or via
a SPECIALISE pragma.  Then CSE kicks in and notices that the RHSs of
'f' and 'g' are identical, so we get
   f x = ...f (x-1)...
   g = f
   {-# RULES g @Int _ = $sg #-}

Now there is terrible danger that, in an importing module, we'll inline
'g' before we have a chance to run its specialisation!

Solution: during CSE, when adding a top-level
  g = f
binding after a "hit" in the CSE cache, add a NOINLINE[2] activation
to it, to ensure it's not inlined right away.

Why top level only?  Because for nested bindings we are already past
phase 2 and will never return there.
-}

tryForCSE :: CSEnv -> InExpr -> OutExpr
tryForCSE env expr = snd (try_for_cse env expr)

try_for_cse :: CSEnv -> InExpr -> (Bool, OutExpr)
-- (False, e') => We did not CSE the entire expression,
--                but we might have CSE'd some sub-expressions,
--                yielding e'
--
-- (True, te') => We CSE'd the entire expression,
--                yielding the trivial expression te'
try_for_cse env expr
  | Just e <- lookupCSEnv env expr'' = (True,  mkTicks ticks e)
  | otherwise                        = (False, expr')
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

-- | Runs CSE on a single expression.
--
-- This entry point is not used in the compiler itself, but is provided
-- as a convenient entry point for users of the GHC API.
cseOneExpr :: InExpr -> OutExpr
cseOneExpr e = cseExpr env e
  where env = emptyCSEnv {cs_subst = mkEmptySubst (mkInScopeSet (exprFreeVars e)) }

cseExpr :: CSEnv -> InExpr -> OutExpr
cseExpr env (Type t)              = Type (substTy (csEnvSubst env) t)
cseExpr env (Coercion c)          = Coercion (substCo (csEnvSubst env) c)
cseExpr _   (Lit lit)             = Lit lit
cseExpr env (Var v)               = lookupSubst env v
cseExpr env (App f a)             = App (cseExpr env f) (tryForCSE env a)
cseExpr env (Tick t e)            = Tick t (cseExpr env e)
cseExpr env (Cast e co)           = Cast (tryForCSE env e) (substCo (csEnvSubst env) co)
cseExpr env (Lam b e)             = let (env', b') = addBinder env b
                                    in Lam b' (cseExpr env' e)
cseExpr env (Let bind e)          = let (env', bind') = cseBind NotTopLevel env bind
                                    in Let bind' (cseExpr env' e)
cseExpr env (Case e bndr ty alts) = cseCase env e bndr ty alts

cseCase :: CSEnv -> InExpr -> InId -> InType -> [InAlt] -> OutExpr
cseCase env scrut bndr ty alts
  = Case scrut1 bndr3 ty' $
    combineAlts alt_env (map cse_alt alts)
  where
    ty' = substTy (csEnvSubst env) ty
    scrut1 = tryForCSE env scrut

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

    -- Given case x of { K y z -> ...K y z... }
    -- CSE K y z into x...
    cse_alt (DataAlt con, args, rhs)
        | not (null args)
                -- ... but don't try CSE if there are no args; it just increases the number
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

combineAlts :: CSEnv -> [InAlt] -> [InAlt]
-- See Note [Combine case alternatives]
combineAlts env ((_,bndrs1,rhs1) : rest_alts)
  | all isDeadBinder bndrs1
  = (DEFAULT, [], rhs1) : filtered_alts
  where
    in_scope = substInScope (csEnvSubst env)
    filtered_alts = filterOut identical rest_alts
    identical (_con, bndrs, rhs) = all ok bndrs && eqExpr in_scope rhs1 rhs
    ok bndr = isDeadBinder bndr || not (bndr `elemInScopeSet` in_scope)

combineAlts _ alts = alts  -- Default case

{- Note [Combine case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
combineAlts is just a more heavyweight version of the use of
combineIdenticalAlts in SimplUtils.prepareAlts.  The basic idea is
to transform

    DEFAULT -> e1
    K x     -> e1
    W y z   -> e2
===>
   DEFAULT -> e1
   W y z   -> e2

In the simplifier we use cheapEqExpr, because it is called a lot.
But here in CSE we use the full eqExpr.  After all, two alternatives usually
differ near the root, so it probably isn't expensive to compare the full
alternative.  It seems like the same kind of thing that CSE is supposed
to be doing, which is why I put it here.

I acutally saw some examples in the wild, where some inlining made e1 too
big for cheapEqExpr to catch it.


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

       , cs_rec_map :: CoreMap OutExpr
            -- See Note [CSE for recursive bindings]
       }

emptyCSEnv :: CSEnv
emptyCSEnv = CS { cs_map = emptyCoreMap, cs_rec_map = emptyCoreMap
                , cs_subst = emptySubst }

lookupCSEnv :: CSEnv -> OutExpr -> Maybe OutExpr
lookupCSEnv (CS { cs_map = csmap }) expr
  = lookupCoreMap csmap expr

extendCSEnv :: CSEnv -> OutExpr -> OutExpr -> CSEnv
extendCSEnv cse expr triv_expr
  = cse { cs_map = extendCoreMap (cs_map cse) sexpr triv_expr }
  where
    sexpr = stripTicksE tickishFloatable expr

extendCSRecEnv :: CSEnv -> OutId -> OutExpr -> OutExpr -> CSEnv
-- See Note [CSE for recursive bindings]
extendCSRecEnv cse bndr expr triv_expr
  = cse { cs_rec_map = extendCoreMap (cs_rec_map cse) (Lam bndr expr) triv_expr }

lookupCSRecEnv :: CSEnv -> OutId -> OutExpr -> Maybe OutExpr
-- See Note [CSE for recursive bindings]
lookupCSRecEnv (CS { cs_rec_map = csmap }) bndr expr
  = lookupCoreMap csmap (Lam bndr expr)

csEnvSubst :: CSEnv -> Subst
csEnvSubst = cs_subst

lookupSubst :: CSEnv -> Id -> OutExpr
lookupSubst (CS { cs_subst = sub}) x = lookupIdSubst (text "CSE.lookupSubst") sub x

extendCSSubst :: CSEnv -> Id  -> CoreExpr -> CSEnv
extendCSSubst cse x rhs = cse { cs_subst = extendSubst (cs_subst cse) x rhs }

-- | Add clones to the substitution to deal with shadowing.  See
-- Note [Shadowing] for more details.  You should call this whenever
-- you go under a binder.
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
