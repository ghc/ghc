{-
(c) The AQUA Project, Glasgow University, 1993-1998

\section{Common subexpression}
-}

module GHC.Core.Opt.CSE (cseProgram, cseOneExpr) where

import GHC.Prelude

import GHC.Core.Subst
import GHC.Types.Var.Env ( mkInScopeSet )
import GHC.Types.Id
import GHC.Core.Utils   ( mkAltExpr
                        , exprIsTickedString
                        , stripTicksE, stripTicksT, mkTicks )
import GHC.Core.FVs     ( exprFreeVars )
import GHC.Core.Type    ( tyConAppArgs )
import GHC.Core
import GHC.Utils.Outputable
import GHC.Types.Basic
import GHC.Types.Tickish
import GHC.Core.Map.Expr
import GHC.Utils.Misc   ( filterOut, equalLength )
import GHC.Utils.Panic
import Data.Functor.Identity ( Identity (..) )
import Data.List        ( mapAccumL )

{-
                        Simple common sub-expression
                        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we see
  x1 = C a b
  x2 = C x1 b
we build up a reverse mapping:
  C a b  :-> x1
  C x1 b :-> x2
and apply that to the rest of the program.

When we then see
  y1 = C a b
  y2 = C y1 b
we replace the C a b with x1.  But then we *don't* want to
add   x1 -> y1  to the mapping.  Rather, we want the reverse, y1 -> x1
so that a subsequent binding
        y2 = C y1 b
will get transformed to C x1 b, and then to x2.

So we carry an extra var->var substitution cs_canon which we apply *before*
looking up in the reverse mapping. We call this step "canonicalisation", because
it makes α-equivalent expressions /syntactically/ equal by choosing the names
consistently.

Note [CSE for bindings] explains both cases (EXTEND and CANONICALISE) in detail.

Note [Shadowing in CSE]
~~~~~~~~~~~~~~~~~~~~~~~
We have to be careful about shadowing.
For example, consider
        f = \x -> let y = x+x in
                      h = \x -> x+x
                  in ...

Here we must *not* do CSE on the inner x+x!  The simplifier used to guarantee no
shadowing, but it doesn't any more (it proved too hard), so we clone as we go.

A similar tricky situation is this, with x_123 and y_123 sharing the same unique:

    let x_123 = e1 in
    let y_123 = e2 in
    let foo = e1

Naively applying e1 = x_123 during CSE we would get:

    let x_123 = e1 in
    let y_123 = e2 in
    let foo = x_123

But x_123 is shadowed by y_123 and things would go terribly wrong! One more reason
why we have to substitute binders as we go so we will properly get:

    let x1 = e1 in
    let x2 = e2 in
    let foo = x1

It may be tempting to do the cloning using the same substitution cs_canon that
does the canonicalisation.
We may not do so; see Note [Canonicalisation reverts binder swap transformation].

Hence we maintain two substitutions: cs_subst to implement cloning, and cs_canon
to implement the var->var substitution for canonicalising keys prior to lookup
in the reverse mapping.

Note [CSE for bindings]
~~~~~~~~~~~~~~~~~~~~~~~
Let-bindings have two cases, implemented by extendCSEnvWithBinding.

* CANONICALISE: applies when the RHS is a variable

     let x = y in ...(h x)....

  Here we want to extend the canon. substitution cs_canon with x :-> y, so that
  the (h x) in the body might CSE with an enclosing (let v = h y in ...).

  NB: cs_canon is distinct from cs_subst, which clones InIds into OutIds in
      order to handle Note [Shadowing in CSE].
      cs_canon maps from OutIds to a subset of OutIds, so it uses the same
      InScopeSet as cs_subst.
  Couldn't we merge cs_subst and cs_canon? In theory yes, in practice no;
  see Note [Canonicalisation reverts binder swap transformation].

  How can we have a variable on the RHS? Doesn't the simplifier inline them?

    - First, the original RHS might have been (g z) which has CSE'd
      with an enclosing (let y = g z in ...).  This is super-important.
      See #5996:
         x1 = C a b
         x2 = C x1 b
         y1 = C a b
         y2 = C y1 b
      Here we CSE y1's rhs to 'x1', and then we must add (y1:->x1) to
      the substitution so that we can CSE the binding for y2.

    - Second, we use extendCSEnvWithBinding for case expression scrutinees too;
      see Note [CSE for case expressions].

  Do note that adding a mapping (x:->y) to cs_canon by itself does not
  substitute any occurrence of x in the program.

* EXTEND THE REVERSE MAPPING: applies in all other cases

     let x = h y in ...(h y)...

  Here we want to extend the /reverse mapping (cs_map)/ so that
  we CSE the (h y) call to x.

  Note that we use EXTEND even for a trivial expression, provided it
  is not a variable or literal. In particular this /includes/ type
  applications. This can be important (#13156); e.g.
     case f @ Int of { r1 ->
     case f @ Int of { r2 -> ...
  Here we want to common-up the two uses of (f @ Int) so we can
  remove one of the case expressions.

  - The CANONICALISE situation extends the canon. substitution (cs_canon)
  - The EXTEND situation extends the reverse mapping (cs_map)

Note [CSE for case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  case scrut_expr of x { ...alts... }
This is very like a strict let-binding
  let !x = scrut_expr in ...
So we use (extendCSEnvWithBinding x scrut_expr) to process scrut_expr and x, and as a
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

  By using extendCSEnvWithBinding we add the binding (wild1 -> a) to the substitution,
  which does exactly the right thing.

  (Notice this is exactly backwards to what the simplifier does, which
  is to try to replaces uses of 'a' with uses of 'wild1'.)

  This is the main reason that extendCSEnvWithBinding is called with a trivial rhs.

* Non-trivial scrutinee
     case (f x) of y { pat -> ...let z = f x in ... }

  By using extendCSEnvWithBinding we'll add (f x :-> y) to the cs_map, and
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

An exception to the rule is when the INLINE pragma is not from the user, e.g. from
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
because we promised to inline foo as what the user wrote.  See similar Note
[Stable unfoldings and postInlineUnconditionally] in GHC.Core.Opt.Simplify.Utils.

Nor do we want to change the reverse mapping. Suppose we have

   foo {-# Unf = Stable (\pq. build blah) #-}
       = <expr>
   bar = <expr>

There could conceivably be merit in rewriting the RHS of bar:
   bar = foo
but now bar's inlining behaviour will change, and importing
modules might see that.  So it seems dodgy and we don't do it.

Wrinkles

* Stable unfoldings are also created during worker/wrapper when we
  decide that a function's definition is so small that it should
  always inline, or indeed for the wrapper function itself.  In this
  case we still want to do CSE (#13340). Hence the use of
  isStableUserUnfolding/isStableSystemUnfolding rather than
  isStableUnfolding.

* Consider
     foo = <expr>
     bar {-# Unf = Stable ... #-}
        = <expr>
  where the unfolding was added by strictness analysis, say.  Then
  CSE goes ahead, so we get
     bar = foo
  or possibly (due to Note [Dealing with ticks])
     bar = tick t1 (tick t2 foo)
  in both cases we would really like to get rid of the stable unfolding so
  that the Simplifier inlines the possibly trivial RHS rather than the stable
  unfolding, which would in turn keep alive other bindings.

  Hence the zapStableUnfolding in cse_bind.  Not a big deal.

Note [CSE for join points?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must not be naive about join points in CSE:
   join j = e in
   if b then jump j else 1 + e
The expression (1 + jump j) is not good (see Note [Invariants on join points] in
GHC.Core). This seems to come up quite seldom, but it happens (first seen
compiling ppHtml in Haddock.Backends.Xhtml).

We could try and be careful by tracking which join points are still valid at
each subexpression, but since join points aren't allocated or shared, there's
less to gain by trying to CSE them. (#13219)

Note [Look inside join-point binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Another way how CSE for join points is tricky is

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

Note [Separate envs for let rhs and body]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Substituting occurrences of the binder in the rhs with the
 renamed binder is wrong for non-recursive bindings. Why?
Consider this core.

    let {x_123 = e} in
    let {y_123 = \eta0 -> x_123} in ...

In the second line the y_123 on the lhs and x_123 on the rhs refer to different binders
even if they share the same unique.

If we apply the substitution `123 => x2_124}` to both the lhs and rhs we  will transform
`let y_123 = \eta0 -> x_123` into `let x2_124 = \eta0 -> x2_124`.
However x2_124 on the rhs is not in scope and really shouldn't have been renamed at all.
Because really this should still be x_123! In fact this exact thing happened in #21685.

To fix this we pass two different cse envs to cse_bind. One we use the cse the rhs of the binding.
And one we update with the result of cseing the rhs which we then use going forward for the
body/rest of the module.

Note [Canonicalisation reverts binder swap transformation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The CSEnv maintains two substitutions:

  * cs_subst clones every binder so that CSE does not need to worry about
    shadowing. See Note [Shadowing in CSE].
  * cs_canon implements the CANONICALISE case of Note [CSE for bindings], but
    is never used to actually substitute an expression in the resulting program.

These substitutions must stay distinct. Consider

  data T a :: UnliftedType where MkT :: !a -> T a
  case x of x' { __DEFAULT -> let y = MkT x' in ... }

Note that the RHS of y satisfies the Note [Core let-can-float invariant] because
x' is a case binder and thus evaluated.
Note that the scrutinee x is trivial; hence case CANONICALISE applies and we
extend cs_canon with

  x' :-> x

Now, /if/ we were to merge cs_canon into cs_subst, then we would apply this
"reverse binder swap" substitution to the final program and we'd get

  case x of x' { __DEFAULT -> let y = MkT x in ... }

now `MkT x` is no longer ok-for-spec and the program violates the let-can-float
invariant. This is only temporary, because the next run of the occurrence
analyser will perform a Note [Binder swap] again, however it will trip up
CoreLint nonetheless.
Hence cs_canon is distinct from cs_subst, and the former is only applied before
looking up a canonicalised key in the reverse mapping.

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
    -- See Note [Separate envs for let rhs and body]
    (env1, b1)       = addBinder env b
    (env2, (b2, e2)) = cse_bind toplevel env env1 (b,e) b1

cseBind toplevel env (Rec [(in_id, rhs)])
  | noCSE in_id
  = (env1, Rec [(out_id, rhs')])

  -- See Note [CSE for recursive bindings]
  | Just previous <- lookupCSRecEnv env out_id rhs''
  , let previous' = mkTicks ticks previous
        out_id'   = delayInlining toplevel out_id
  = -- We have a hit in the recursive-binding cache
    (env1, NonRec out_id' previous')

  | otherwise
  = (extendCSRecEnv env1 out_id rhs'' id_expr', Rec [(zapped_id, rhs')])

  where
    (env1, Identity out_id) = addRecBinders env (Identity in_id)
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

    do_one env (pr, b1) = cse_bind toplevel env env pr b1

-- | Given a binding of @in_id@ to @in_rhs@, and a fresh name to refer
-- to @in_id@ (@out_id@, created from addBinder or addRecBinders),
-- first try to CSE @in_rhs@, and then add the resulting (possibly CSE'd)
-- binding to the 'CSEnv', so that we attempt to CSE any expressions
-- which are equal to @out_rhs@.
-- We use a different env for cse on the rhs and for extendCSEnvWithBinding
-- for reasons explain in See Note [Separate envs for let rhs and body]
cse_bind :: TopLevelFlag -> CSEnv -> CSEnv -> (InId, InExpr) -> OutId -> (CSEnv, (OutId, OutExpr))
cse_bind toplevel env_rhs env_body (_in_id, in_rhs) out_id
  | isTopLevel toplevel, exprIsTickedString in_rhs
      -- See Note [Take care with literal strings]
  = (env_body', (out_id', in_rhs))

  | JoinPoint arity <- idJoinPointHood out_id
      -- See Note [Look inside join-point binders]
  = let (params, in_body) = collectNBinders arity in_rhs
        (env', params') = addBinders env_rhs params
        out_body = tryForCSE env' in_body
    in (env_body , (out_id, mkLams params' out_body))

  | otherwise
  = (env_body', (out_id'', out_rhs))
  where
    (env_body', out_id') = extendCSEnvWithBinding env_body out_id out_rhs cse_done
    (cse_done, out_rhs)  = try_for_cse env_rhs in_rhs
    out_id'' | cse_done  = zapStableUnfolding $
                           delayInlining toplevel out_id'
             | otherwise = out_id'

delayInlining :: TopLevelFlag -> Id -> Id
-- Add a NOINLINE[2] if the Id doesn't have an INLNE pragma already
-- See Note [Delay inlining after CSE]
delayInlining top_lvl bndr
  | isTopLevel top_lvl
  , isAlwaysActive (idInlineActivation bndr)
  , idHasRules bndr  -- Only if the Id has some RULES,
                     -- which might otherwise get lost
       -- These rules are probably auto-generated specialisations,
       -- since Ids with manual rules usually have manually-inserted
       -- delayed inlining anyway
  = bndr `setInlineActivation` activateAfterInitial
  | otherwise
  = bndr

extendCSEnvWithBinding
           :: CSEnv             -- Includes InId->OutId cloning
           -> OutVar -> OutExpr -- Processed binding
           -> Bool              -- True <=> RHS was CSE'd and is a variable
                                --          or maybe (Tick t variable)
           -> (CSEnv, OutVar)    -- Final env, final bndr
-- Extend the CSE env with a mapping [rhs -> out-id]
-- unless we can instead just canonicalise [out-id -> rhs-id]
--
-- It's possible for the binder to be a type variable,
-- in which case we can just CANONICALISE.
-- See Note [CSE for bindings]
extendCSEnvWithBinding env v rhs' cse_done
  -- Should we use CANONICALISE or EXTEND? See Note [CSE for bindings]
  | not (isId v)   = (extendCSCanon env v rhs', v)              -- CANONICALISE
  | noCSE v        = (env,                      v)
  | Var{} <- rhs'  = (extendCSCanon env v rhs', v)              -- CANONICALISE
  | cse_done       = (env,                      v)
                    -- See Note [Dealing with ticks]
  | otherwise      = (extendCSEnv env rhs' id_expr', zapped_id) -- EXTEND
  where
    id_expr'  = varToCoreExpr v
    zapped_id = zapIdUsageInfo v
       -- Putting the Id into the cs_map makes it possible that
       -- it'll become shared more than it is now, which would
       -- invalidate (the usage part of) its demand info.
       --    This caused #100218.
       -- Easiest thing is to zap the usage info; subsequently
       -- performing late demand-analysis will restore it.  Don't zap
       -- the strictness info; it's not necessary to do so, and losing
       -- it is bad for performance if you don't do late demand
       -- analysis

-- | Given a binder `let x = e`, this function
-- determines whether we should add `e -> x` to the cs_map
noCSE :: InId -> Bool
noCSE id
  | isJoinId id                = no_cse  -- See Note [CSE for join points?]
  | isStableUserUnfolding  unf = no_cse  -- See Note [CSE for stable unfoldings]
  | user_activation_control    = no_cse  -- See Note [CSE for INLINE and NOINLINE]
  | otherwise = yes_cse
   where
     unf = idUnfolding id
     user_activation_control = not (isAlwaysActive (idInlineActivation id))
                            && not (noUserInlineSpec (inlinePragmaSpec (idInlinePragma id)))
     yes_cse = False
     no_cse  = True

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
[Core top-level string literals] in GHC.Core.

For this reason, we special case top-level bindings to literal strings and leave
the original RHS unmodified. This produces:

  x = "foo"#
  y = "foo"#
  ...x...x...x...x....

Now 'y' will be discarded as dead code, and we are done.

The net effect is that for the y-binding we want to
  - Use CANONICALISE, by extending the canon. substitution with  y :-> x
  - but leave the original binding for y undisturbed

This is done by cse_bind.  I got it wrong the first time (#13367).

Note [Dealing with ticks]
~~~~~~~~~~~~~~~~~~~~~~~~~
Ticks complicate CSE a bit, as I discovered in the fallout from
fixing #19360.

* To get more CSE-ing, we strip all the tickishFloatable ticks from
  an expression
  - when inserting into the cs_map (see extendCSEnv)
  - when looking up in the cs_map (see call to lookupCSEnv in try_for_cse)
  Quite why only the tickishFloatable ticks, I'm not quite sure.

  AK: I think we only do this for floatable ticks since generally we don't mind them
  being less accurate as much. E.g. consider
    case e of
      C1 -> f (<tick1> e1)
      C2 -> f (<tick2> e1)
  If the ticks are (floatable) source notes nothing too bad happens if the debug info for
  both branches says the code comes from the same source location. Even if it will be inaccurate
  for one of the branches. We should probably still consider this worthwhile.
  However if the ticks are cost centres we really don't want the cost of both branches to be
  attributed to the same cost centre. Because a user might explicitly have inserted different
  cost centres in order to distinguish between evaluations resulting from the two different branches.
  e.g. something like this:
    case e of
      C1 -> f ({ SCC "evalAlt1"} e1)
      C1 -> f ({ SCC "evalAlt2"} e1)
  But it's still a bit suspicious.

* If we get a hit in cs_map, we wrap the result in the ticks from the
  thing we are looking up (see try_for_cse)

Net result: if we get a hit, we might replace
  let x = tick t1 (tick t2 e)
with
  let x = tick t1 (tick t2 y)
where 'y' is the variable that 'e' maps to.  Now consider extendCSEnvWithBinding for
the binding for 'x':

* We can't use CANONICALISE because those ticks might not be trivial (we
  use tickishIsCode in exprIsTrivial)

* We should not use EXTEND, because we definitely don't want to
  add  (tick t1 (tick t2 y)) :-> x
  to the cs_map. Remember we strip off the ticks, so that would amount
  to adding y :-> x, very silly.

TL;DR: we do neither; hence the cse_done case in extendCSEnvWithBinding.


Note [Delay inlining after CSE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose (#15445) we have
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

Solution: during CSE, after a "hit" in the CSE cache
  * when adding a binding
        g = f
  * for a top-level function g
  * and g has specialisation RULES
add a NOINLINE[2] activation to it, to ensure it's not inlined
right away.

Notes:
* Why top level only?  Because for nested bindings we are already past
  phase 2 and will never return there.

* Why "only if g has RULES"?  Because there is no point in
  doing this if there are no RULES; and other things being
  equal it delays optimisation to delay inlining (#17409)


---- Historical note ---

This patch is simpler and more direct than an earlier
version:

  commit 2110738b280543698407924a16ac92b6d804dc36
  Author: Simon Peyton Jones <simonpj@microsoft.com>
  Date:   Mon Jul 30 13:43:56 2018 +0100

  Don't inline functions with RULES too early

We had to revert this patch because it made GHC itself slower.

Why? It delayed inlining of /all/ functions with RULES, and that was
very bad in GHC.Tc.Solver.Flatten.flatten_ty_con_app

* It delayed inlining of liftM
* That delayed the unravelling of the recursion in some dictionary
  bindings.
* That delayed some eta expansion, leaving
     flatten_ty_con_app = \x y. let <stuff> in \z. blah
* That allowed the float-out pass to put sguff between
  the \y and \z.
* And that permanently stopped eta expansion of the function,
  even once <stuff> was simplified.

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
cseExpr env (Type t)              = Type (substTyUnchecked (csEnvSubst env) t)
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
    combineAlts (map cse_alt alts)
  where
    ty' = substTyUnchecked (csEnvSubst env) ty
    (cse_done, scrut1) = try_for_cse env scrut

    bndr1 = zapIdOccInfo bndr
      -- Zapping the OccInfo is needed because the extendCSEnv
      -- in cse_alt may mean that a dead case binder
      -- becomes alive, and Lint rejects that
    (env1, bndr2)    = addBinder env bndr1
    (alt_env, bndr3) = extendCSEnvWithBinding env1 bndr2 scrut1 cse_done
         -- extendCSEnvWithBinding: see Note [CSE for case expressions]

    con_target :: OutExpr
    con_target = lookupSubst alt_env bndr

    arg_tys :: [OutType]
    arg_tys = tyConAppArgs (idType bndr3)

    -- See Note [CSE for case alternatives]
    cse_alt (Alt (DataAlt con) args rhs)
        = Alt (DataAlt con) args' (tryForCSE new_env rhs)
        where
          (env', args') = addBinders alt_env args
          new_env       = extendCSEnv env' con_expr con_target
          con_expr      = mkAltExpr (DataAlt con) args' arg_tys

    cse_alt (Alt con args rhs)
        = Alt con args' (tryForCSE env' rhs)
        where
          (env', args') = addBinders alt_env args

combineAlts :: [OutAlt] -> [OutAlt]
-- See Note [Combine case alternatives]
combineAlts alts
  | (Just alt1, rest_alts) <- find_bndr_free_alt alts
  , Alt _ bndrs1 rhs1 <- alt1
  , let filtered_alts = filterOut (identical_alt rhs1) rest_alts
  , not (equalLength rest_alts filtered_alts)
  = assertPpr (all isDeadBinder bndrs1) (ppr alts) $
    Alt DEFAULT [] rhs1 : filtered_alts

  | otherwise
  = alts
  where

    find_bndr_free_alt :: [CoreAlt] -> (Maybe CoreAlt, [CoreAlt])
       -- The (Just alt) is an alt where all fields are dead
    find_bndr_free_alt []
      = (Nothing, [])
    find_bndr_free_alt (alt@(Alt _ bndrs _) : alts)
      | all isDeadBinder bndrs = (Just alt, alts)
      | otherwise              = case find_bndr_free_alt alts of
                                   (mb_bf, alts) -> (mb_bf, alt:alts)

    identical_alt rhs1 (Alt _ _ rhs) = eqCoreExpr rhs1 rhs
       -- Even if this alt has binders, they will have been cloned
       -- If any of these binders are mentioned in 'rhs', then
       -- 'rhs' won't compare equal to 'rhs1' (which is from an
       -- alt with no binders).

{- Note [CSE for case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider   case e of x
            K1 y -> ....(K1 y)...
            K2   -> ....K2....

We definitely want to CSE that (K1 y) into just x.

But what about the lone K2?  At first you would think "no" because
turning K2 into 'x' increases the number of live variables.  But

* Turning K2 into x increases the chance of combining identical alts.
  Example      case xs of
                  (_:_) -> f xs
                  []    -> f []
  See #17901 and simplCore/should_compile/T17901 for more examples
  of this kind.

* The next run of the simplifier will turn 'x' back into K2, so we won't
  permanently bloat the free-var count.


Note [Combine case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
combineAlts is just a more heavyweight version of the use of
combineIdenticalAlts in GHC.Core.Opt.Simplify.Utils.prepareAlts.  The basic idea is
to transform

    DEFAULT -> e1
    K x     -> e1
    W y z   -> e2
===>
   DEFAULT -> e1
   W y z   -> e2

In the simplifier we use cheapEqExpr, because it is called a lot.
But here in CSE we use the full eqCoreExpr.  After all, two alternatives usually
differ near the root, so it probably isn't expensive to compare the full
alternative.  It seems like the same kind of thing that CSE is supposed
to be doing, which is why I put it here.

I actually saw some examples in the wild, where some inlining made e1 too
big for cheapEqExpr to catch it.

Note [Combine case alts: awkward corner]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We check isDeadBinder on field binders in order to collapse into a DEFAULT alt.
But alas, the simplifer often zaps occ-info on field binders in DataAlts when
the case binder is alive; see Note [DataAlt occ info] in GHC.Core.Opt.Simplify.

* One alternative (perhaps a good one) would be to do OccAnal
  just before CSE.  Then perhaps we could get rid of combineIdenticalAlts
  in the Simplifier, which might save work.

* Another would be for CSE to return free vars as it goes.

* But the current solution is to accept that we do not catch cases such as
      case x of c
        A _   -> blah[c]
        B _ _ -> blah[c]
  where the case binder c is alive and no alternative is DEFAULT.
  But the current solution is at least cheap.

************************************************************************
*                                                                      *
\section{The CSE envt}
*                                                                      *
************************************************************************
-}

data CSEnv
  = CS { cs_subst :: Subst  -- Maps InBndrs to OutExprs
            -- The cloning substitution; maps variables to
            -- /trivial/ OutExprs, not arbitrary expressions

       , cs_canon :: Subst  -- Maps OutBndrs to OutExprs
            -- The canonicalising substitution to apply before applying the
            -- reverse mapping cs_subst.
            -- Maps to /trivial/ OutExprs.

       , cs_map   :: CoreMap OutExpr
            -- The "reverse" mapping.
            -- Maps a OutExpr to a /trivial/ OutExpr
            -- The key of cs_subst is stripped of all Ticks
            -- It maps arbitrary expressions to trivial expressions
            -- representing the same value. E.g @C a b@ to @x1@.
            -- Canonicalise key with cs_canon before looking up in here.

       , cs_rec_map :: CoreMap OutExpr
            -- See Note [CSE for recursive bindings]
       }

emptyCSEnv :: CSEnv
emptyCSEnv = CS { cs_map = emptyCoreMap, cs_rec_map = emptyCoreMap
                , cs_subst = emptySubst, cs_canon = emptySubst }

lookupCSEnv :: CSEnv -> OutExpr -> Maybe OutExpr
lookupCSEnv cse expr
  = lookupCoreMap (cs_map cse) (canonCSEnv cse expr)

-- | @extendCSEnv env e triv_expr@ will replace any occurrence of @e@ with @triv_expr@ going forward.
extendCSEnv :: CSEnv -> OutExpr -> OutExpr -> CSEnv
extendCSEnv cse expr triv_expr
  = cse { cs_map = extendCoreMap (cs_map cse) sexpr triv_expr }
  where
    sexpr = canonCSEnv cse $ stripTicksE tickishFloatable expr

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
lookupSubst (CS { cs_subst = sub}) x = lookupIdSubst sub x

extendCSCanon :: CSEnv -> OutVar -> OutExpr -> CSEnv
extendCSCanon cse x y = cse { cs_canon = extendSubst (cs_canon cse) x y' }
  where
    y' = canonCSEnv cse y -- canonicalise y first!

canonCSEnv :: CSEnv -> OutExpr -> OutExpr
canonCSEnv cse@(CS { cs_canon = sub }) e = substExpr (sub `setInScope` is) e
  where
    is = getSubstInScope (cs_subst cse)
    -- We do not separately maintain the in-scope set of cs_canon; it's just
    -- the one from the substitution used for cloning.

-- | Add clones to the substitution to deal with shadowing.  See
-- Note [Shadowing in CSE] for more details.  You should call this whenever
-- you go under a binder.
addBinder :: CSEnv -> Var -> (CSEnv, Var)
addBinder cse v = (cse { cs_subst = sub' }, v')
                where
                  (sub', v') = substBndr (cs_subst cse) v

addBinders :: CSEnv -> [Var] -> (CSEnv, [Var])
addBinders cse vs = (cse { cs_subst = sub' }, vs')
                where
                  (sub', vs') = substBndrs (cs_subst cse) vs

addRecBinders :: Traversable f => CSEnv -> f Id -> (CSEnv, f Id)
addRecBinders = \ cse vs ->
    let (sub', vs') = substRecBndrs (cs_subst cse) vs
    in (cse { cs_subst = sub' }, vs')
{-# INLINE addRecBinders #-}
