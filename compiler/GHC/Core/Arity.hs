{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


        Arity and eta expansion
-}

{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Arity and eta expansion
module GHC.Core.Arity
   ( manifestArity, joinRhsArity, exprArity, typeArity
   , exprEtaExpandArity, findRhsArity, etaExpand
   , etaExpandToJoinPoint, etaExpandToJoinPointRule
   , exprBotStrictness_maybe
   )
where

#include "HsVersions.h"

import GhcPrelude

import GHC.Core
import GHC.Core.FVs
import GHC.Core.Utils
import GHC.Core.Subst
import GHC.Types.Demand
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Id
import GHC.Core.Type as Type
import GHC.Core.TyCon     ( initRecTc, checkRecTc )
import GHC.Core.Predicate ( isDictTy )
import GHC.Core.Coercion as Coercion
import GHC.Types.Basic
import GHC.Types.Unique
import GHC.Driver.Session ( DynFlags, GeneralFlag(..), gopt )
import Outputable
import FastString
import Util     ( debugIsOn )

{-
************************************************************************
*                                                                      *
              manifestArity and exprArity
*                                                                      *
************************************************************************

exprArity is a cheap-and-cheerful version of exprEtaExpandArity.
It tells how many things the expression can be applied to before doing
any work.  It doesn't look inside cases, lets, etc.  The idea is that
exprEtaExpandArity will do the hard work, leaving something that's easy
for exprArity to grapple with.  In particular, Simplify uses exprArity to
compute the ArityInfo for the Id.

Originally I thought that it was enough just to look for top-level lambdas, but
it isn't.  I've seen this

        foo = PrelBase.timesInt

We want foo to get arity 2 even though the eta-expander will leave it
unchanged, in the expectation that it'll be inlined.  But occasionally it
isn't, because foo is blacklisted (used in a rule).

Similarly, see the ok_note check in exprEtaExpandArity.  So
        f = __inline_me (\x -> e)
won't be eta-expanded.

And in any case it seems more robust to have exprArity be a bit more intelligent.
But note that   (\x y z -> f x y z)
should have arity 3, regardless of f's arity.
-}

manifestArity :: CoreExpr -> Arity
-- ^ manifestArity sees how many leading value lambdas there are,
--   after looking through casts
manifestArity (Lam v e) | isId v        = 1 + manifestArity e
                        | otherwise     = manifestArity e
manifestArity (Tick t e) | not (tickishIsCode t) =  manifestArity e
manifestArity (Cast e _)                = manifestArity e
manifestArity _                         = 0

joinRhsArity :: CoreExpr -> JoinArity
-- Join points are supposed to have manifestly-visible
-- lambdas at the top: no ticks, no casts, nothing
-- Moreover, type lambdas count in JoinArity
joinRhsArity (Lam _ e) = 1 + joinRhsArity e
joinRhsArity _         = 0


---------------
exprArity :: CoreExpr -> Arity
-- ^ An approximate, fast, version of 'exprEtaExpandArity'
exprArity e = go e
  where
    go (Var v)                     = idArity v
    go (Lam x e) | isId x          = go e + 1
                 | otherwise       = go e
    go (Tick t e) | not (tickishIsCode t) = go e
    go (Cast e co)                 = trim_arity (go e) (coercionRKind co)
                                        -- Note [exprArity invariant]
    go (App e (Type _))            = go e
    go (App f a) | exprIsTrivial a = (go f - 1) `max` 0
        -- See Note [exprArity for applications]
        -- NB: coercions count as a value argument

    go _                           = 0

    trim_arity :: Arity -> Type -> Arity
    trim_arity arity ty = arity `min` length (typeArity ty)

---------------
typeArity :: Type -> [OneShotInfo]
-- How many value arrows are visible in the type?
-- We look through foralls, and newtypes
-- See Note [exprArity invariant]
typeArity ty
  = go initRecTc ty
  where
    go rec_nts ty
      | Just (_, ty')  <- splitForAllTy_maybe ty
      = go rec_nts ty'

      | Just (arg,res) <- splitFunTy_maybe ty
      = typeOneShot arg : go rec_nts res

      | Just (tc,tys) <- splitTyConApp_maybe ty
      , Just (ty', _) <- instNewTyCon_maybe tc tys
      , Just rec_nts' <- checkRecTc rec_nts tc  -- See Note [Expanding newtypes]
                                                -- in GHC.Core.TyCon
--   , not (isClassTyCon tc)    -- Do not eta-expand through newtype classes
--                              -- See Note [Newtype classes and eta expansion]
--                              (no longer required)
      = go rec_nts' ty'
        -- Important to look through non-recursive newtypes, so that, eg
        --      (f x)   where f has arity 2, f :: Int -> IO ()
        -- Here we want to get arity 1 for the result!
        --
        -- AND through a layer of recursive newtypes
        -- e.g. newtype Stream m a b = Stream (m (Either b (a, Stream m a b)))

      | otherwise
      = []

---------------
exprBotStrictness_maybe :: CoreExpr -> Maybe (Arity, StrictSig)
-- A cheap and cheerful function that identifies bottoming functions
-- and gives them a suitable strictness signatures.  It's used during
-- float-out
exprBotStrictness_maybe e
  = case getBotArity (arityType env e) of
        Nothing -> Nothing
        Just ar -> Just (ar, sig ar)
  where
    env    = AE { ae_ped_bot = True, ae_cheap_fn = \ _ _ -> False }
    sig ar = mkClosedStrictSig (replicate ar topDmd) botDiv

{-
Note [exprArity invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~
exprArity has the following invariants:

  (1) If typeArity (exprType e) = n,
      then manifestArity (etaExpand e n) = n

      That is, etaExpand can always expand as much as typeArity says
      So the case analysis in etaExpand and in typeArity must match

  (2) exprArity e <= typeArity (exprType e)

  (3) Hence if (exprArity e) = n, then manifestArity (etaExpand e n) = n

      That is, if exprArity says "the arity is n" then etaExpand really
      can get "n" manifest lambdas to the top.

Why is this important?  Because
  - In GHC.Iface.Tidy we use exprArity to fix the *final arity* of
    each top-level Id, and in
  - In CorePrep we use etaExpand on each rhs, so that the visible lambdas
    actually match that arity, which in turn means
    that the StgRhs has the right number of lambdas

An alternative would be to do the eta-expansion in GHC.Iface.Tidy, at least
for top-level bindings, in which case we would not need the trim_arity
in exprArity.  That is a less local change, so I'm going to leave it for today!

Note [Newtype classes and eta expansion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NB: this nasty special case is no longer required, because
    for newtype classes we don't use the class-op rule mechanism
    at all.  See Note [Single-method classes] in TcInstDcls. SLPJ May 2013

-------- Old out of date comments, just for interest -----------
We have to be careful when eta-expanding through newtypes.  In general
it's a good idea, but annoyingly it interacts badly with the class-op
rule mechanism.  Consider

   class C a where { op :: a -> a }
   instance C b => C [b] where
     op x = ...

These translate to

   co :: forall a. (a->a) ~ C a

   $copList :: C b -> [b] -> [b]
   $copList d x = ...

   $dfList :: C b -> C [b]
   {-# DFunUnfolding = [$copList] #-}
   $dfList d = $copList d |> co@[b]

Now suppose we have:

   dCInt :: C Int

   blah :: [Int] -> [Int]
   blah = op ($dfList dCInt)

Now we want the built-in op/$dfList rule will fire to give
   blah = $copList dCInt

But with eta-expansion 'blah' might (and in #3772, which is
slightly more complicated, does) turn into

   blah = op (\eta. ($dfList dCInt |> sym co) eta)

and now it is *much* harder for the op/$dfList rule to fire, because
exprIsConApp_maybe won't hold of the argument to op.  I considered
trying to *make* it hold, but it's tricky and I gave up.

The test simplCore/should_compile/T3722 is an excellent example.
-------- End of old out of date comments, just for interest -----------


Note [exprArity for applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we come to an application we check that the arg is trivial.
   eg  f (fac x) does not have arity 2,
                 even if f has arity 3!

* We require that is trivial rather merely cheap.  Suppose f has arity 2.
  Then    f (Just y)
  has arity 0, because if we gave it arity 1 and then inlined f we'd get
          let v = Just y in \w. <f-body>
  which has arity 0.  And we try to maintain the invariant that we don't
  have arity decreases.

*  The `max 0` is important!  (\x y -> f x) has arity 2, even if f is
   unknown, hence arity 0


************************************************************************
*                                                                      *
           Computing the "arity" of an expression
*                                                                      *
************************************************************************

Note [Definition of arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The "arity" of an expression 'e' is n if
   applying 'e' to *fewer* than n *value* arguments
   converges rapidly

Or, to put it another way

   there is no work lost in duplicating the partial
   application (e x1 .. x(n-1))

In the divergent case, no work is lost by duplicating because if the thing
is evaluated once, that's the end of the program.

Or, to put it another way, in any context C

   C[ (\x1 .. xn. e x1 .. xn) ]
         is as efficient as
   C[ e ]

It's all a bit more subtle than it looks:

Note [One-shot lambdas]
~~~~~~~~~~~~~~~~~~~~~~~
Consider one-shot lambdas
                let x = expensive in \y z -> E
We want this to have arity 1 if the \y-abstraction is a 1-shot lambda.

Note [Dealing with bottom]
~~~~~~~~~~~~~~~~~~~~~~~~~~
A Big Deal with computing arities is expressions like

   f = \x -> case x of
               True  -> \s -> e1
               False -> \s -> e2

This happens all the time when f :: Bool -> IO ()
In this case we do eta-expand, in order to get that \s to the
top, and give f arity 2.

This isn't really right in the presence of seq.  Consider
        (f bot) `seq` 1

This should diverge!  But if we eta-expand, it won't.  We ignore this
"problem" (unless -fpedantic-bottoms is on), because being scrupulous
would lose an important transformation for many programs. (See
#5587 for an example.)

Consider also
        f = \x -> error "foo"
Here, arity 1 is fine.  But if it is
        f = \x -> case x of
                        True  -> error "foo"
                        False -> \y -> x+y
then we want to get arity 2.  Technically, this isn't quite right, because
        (f True) `seq` 1
should diverge, but it'll converge if we eta-expand f.  Nevertheless, we
do so; it improves some programs significantly, and increasing convergence
isn't a bad thing.  Hence the ABot/ATop in ArityType.

So these two transformations aren't always the Right Thing, and we
have several tickets reporting unexpected behaviour resulting from
this transformation.  So we try to limit it as much as possible:

 (1) Do NOT move a lambda outside a known-bottom case expression
       case undefined of { (a,b) -> \y -> e }
     This showed up in #5557

 (2) Do NOT move a lambda outside a case if all the branches of
     the case are known to return bottom.
        case x of { (a,b) -> \y -> error "urk" }
     This case is less important, but the idea is that if the fn is
     going to diverge eventually anyway then getting the best arity
     isn't an issue, so we might as well play safe

 (3) Do NOT move a lambda outside a case unless
     (a) The scrutinee is ok-for-speculation, or
     (b) more liberally: the scrutinee is cheap (e.g. a variable), and
         -fpedantic-bottoms is not enforced (see #2915 for an example)

Of course both (1) and (2) are readily defeated by disguising the bottoms.

4. Note [Newtype arity]
~~~~~~~~~~~~~~~~~~~~~~~~
Non-recursive newtypes are transparent, and should not get in the way.
We do (currently) eta-expand recursive newtypes too.  So if we have, say

        newtype T = MkT ([T] -> Int)

Suppose we have
        e = coerce T f
where f has arity 1.  Then: etaExpandArity e = 1;
that is, etaExpandArity looks through the coerce.

When we eta-expand e to arity 1: eta_expand 1 e T
we want to get:                  coerce T (\x::[T] -> (coerce ([T]->Int) e) x)

  HOWEVER, note that if you use coerce bogusly you can ge
        coerce Int negate
  And since negate has arity 2, you might try to eta expand.  But you can't
  decompose Int to a function type.   Hence the final case in eta_expand.

Note [The state-transformer hack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
        f = e
where e has arity n.  Then, if we know from the context that f has
a usage type like
        t1 -> ... -> tn -1-> t(n+1) -1-> ... -1-> tm -> ...
then we can expand the arity to m.  This usage type says that
any application (x e1 .. en) will be applied to uniquely to (m-n) more args
Consider f = \x. let y = <expensive>
                 in case x of
                      True  -> foo
                      False -> \(s:RealWorld) -> e
where foo has arity 1.  Then we want the state hack to
apply to foo too, so we can eta expand the case.

Then we expect that if f is applied to one arg, it'll be applied to two
(that's the hack -- we don't really know, and sometimes it's false)
See also Id.isOneShotBndr.

Note [State hack and bottoming functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's a terrible idea to use the state hack on a bottoming function.
Here's what happens (#2861):

  f :: String -> IO T
  f = \p. error "..."

Eta-expand, using the state hack:

  f = \p. (\s. ((error "...") |> g1) s) |> g2
  g1 :: IO T ~ (S -> (S,T))
  g2 :: (S -> (S,T)) ~ IO T

Extrude the g2

  f' = \p. \s. ((error "...") |> g1) s
  f = f' |> (String -> g2)

Discard args for bottomming function

  f' = \p. \s. ((error "...") |> g1 |> g3
  g3 :: (S -> (S,T)) ~ (S,T)

Extrude g1.g3

  f'' = \p. \s. (error "...")
  f' = f'' |> (String -> S -> g1.g3)

And now we can repeat the whole loop.  Aargh!  The bug is in applying the
state hack to a function which then swallows the argument.

This arose in another guise in #3959.  Here we had

     catch# (throw exn >> return ())

Note that (throw :: forall a e. Exn e => e -> a) is called with [a = IO ()].
After inlining (>>) we get

     catch# (\_. throw {IO ()} exn)

We must *not* eta-expand to

     catch# (\_ _. throw {...} exn)

because 'catch#' expects to get a (# _,_ #) after applying its argument to
a State#, not another function!

In short, we use the state hack to allow us to push let inside a lambda,
but not to introduce a new lambda.


Note [ArityType]
~~~~~~~~~~~~~~~~
ArityType is the result of a compositional analysis on expressions,
from which we can decide the real arity of the expression (extracted
with function exprEtaExpandArity).

Here is what the fields mean. If an arbitrary expression 'f' has
ArityType 'at', then

 * If at = ABot n, then (f x1..xn) definitely diverges. Partial
   applications to fewer than n args may *or may not* diverge.

   We allow ourselves to eta-expand bottoming functions, even
   if doing so may lose some `seq` sharing,
       let x = <expensive> in \y. error (g x y)
       ==> \y. let x = <expensive> in error (g x y)

 * If at = ATop as, and n=length as,
   then expanding 'f' to (\x1..xn. f x1 .. xn) loses no sharing,
   assuming the calls of f respect the one-shot-ness of
   its definition.

   NB 'f' is an arbitrary expression, eg (f = g e1 e2).  This 'f'
   can have ArityType as ATop, with length as > 0, only if e1 e2 are
   themselves.

 * In both cases, f, (f x1), ... (f x1 ... f(n-1)) are definitely
   really functions, or bottom, but *not* casts from a data type, in
   at least one case branch.  (If it's a function in one case branch but
   an unsafe cast from a data type in another, the program is bogus.)
   So eta expansion is dynamically ok; see Note [State hack and
   bottoming functions], the part about catch#

Example:
      f = \x\y. let v = <expensive> in
          \s(one-shot) \t(one-shot). blah
      'f' has ArityType [ManyShot,ManyShot,OneShot,OneShot]
      The one-shot-ness means we can, in effect, push that
      'let' inside the \st.


Suppose f = \xy. x+y
Then  f             :: AT [False,False] ATop
      f v           :: AT [False]       ATop
      f <expensive> :: AT []            ATop

-------------------- Main arity code ----------------------------
-}

-- See Note [ArityType]
data ArityType = ATop [OneShotInfo] | ABot Arity
     -- There is always an explicit lambda
     -- to justify the [OneShot], or the Arity

instance Outputable ArityType where
  ppr (ATop os) = text "ATop" <> parens (ppr (length os))
  ppr (ABot n)  = text "ABot" <> parens (ppr n)

vanillaArityType :: ArityType
vanillaArityType = ATop []      -- Totally uninformative

-- ^ The Arity returned is the number of value args the
-- expression can be applied to without doing much work
exprEtaExpandArity :: DynFlags -> CoreExpr -> Arity
-- exprEtaExpandArity is used when eta expanding
--      e  ==>  \xy -> e x y
exprEtaExpandArity dflags e
  = case (arityType env e) of
      ATop oss -> length oss
      ABot n   -> n
  where
    env = AE { ae_cheap_fn = mk_cheap_fn dflags isCheapApp
             , ae_ped_bot  = gopt Opt_PedanticBottoms dflags }

getBotArity :: ArityType -> Maybe Arity
-- Arity of a divergent function
getBotArity (ABot n) = Just n
getBotArity _        = Nothing

mk_cheap_fn :: DynFlags -> CheapAppFun -> CheapFun
mk_cheap_fn dflags cheap_app
  | not (gopt Opt_DictsCheap dflags)
  = \e _     -> exprIsCheapX cheap_app e
  | otherwise
  = \e mb_ty -> exprIsCheapX cheap_app e
             || case mb_ty of
                  Nothing -> False
                  Just ty -> isDictTy ty


----------------------
findRhsArity :: DynFlags -> Id -> CoreExpr -> Arity -> (Arity, Bool)
-- This implements the fixpoint loop for arity analysis
-- See Note [Arity analysis]
-- If findRhsArity e = (n, is_bot) then
--  (a) any application of e to <n arguments will not do much work,
--      so it is safe to expand e  ==>  (\x1..xn. e x1 .. xn)
--  (b) if is_bot=True, then e applied to n args is guaranteed bottom
findRhsArity dflags bndr rhs old_arity
  = go (get_arity init_cheap_app)
       -- We always call exprEtaExpandArity once, but usually
       -- that produces a result equal to old_arity, and then
       -- we stop right away (since arities should not decrease)
       -- Result: the common case is that there is just one iteration
  where
    is_lam = has_lam rhs

    has_lam (Tick _ e) = has_lam e
    has_lam (Lam b e)  = isId b || has_lam e
    has_lam _          = False

    init_cheap_app :: CheapAppFun
    init_cheap_app fn n_val_args
      | fn == bndr = True   -- On the first pass, this binder gets infinite arity
      | otherwise  = isCheapApp fn n_val_args

    go :: (Arity, Bool) -> (Arity, Bool)
    go cur_info@(cur_arity, _)
      | cur_arity <= old_arity = cur_info
      | new_arity == cur_arity = cur_info
      | otherwise = ASSERT( new_arity < cur_arity )
#if defined(DEBUG)
                    pprTrace "Exciting arity"
                       (vcat [ ppr bndr <+> ppr cur_arity <+> ppr new_arity
                             , ppr rhs])
#endif
                    go new_info
      where
        new_info@(new_arity, _) = get_arity cheap_app

        cheap_app :: CheapAppFun
        cheap_app fn n_val_args
          | fn == bndr = n_val_args < cur_arity
          | otherwise  = isCheapApp fn n_val_args

    get_arity :: CheapAppFun -> (Arity, Bool)
    get_arity cheap_app
      = case (arityType env rhs) of
          ABot n -> (n, True)
          ATop (os:oss) | isOneShotInfo os || is_lam
                  -> (1 + length oss, False)    -- Don't expand PAPs/thunks
          ATop _  -> (0,              False)    -- Note [Eta expanding thunks]
       where
         env = AE { ae_cheap_fn = mk_cheap_fn dflags cheap_app
                  , ae_ped_bot  = gopt Opt_PedanticBottoms dflags }

{-
Note [Arity analysis]
~~~~~~~~~~~~~~~~~~~~~
The motivating example for arity analysis is this:

  f = \x. let g = f (x+1)
          in \y. ...g...

What arity does f have?  Really it should have arity 2, but a naive
look at the RHS won't see that.  You need a fixpoint analysis which
says it has arity "infinity" the first time round.

This example happens a lot; it first showed up in Andy Gill's thesis,
fifteen years ago!  It also shows up in the code for 'rnf' on lists
in #4138.

The analysis is easy to achieve because exprEtaExpandArity takes an
argument
     type CheapFun = CoreExpr -> Maybe Type -> Bool
used to decide if an expression is cheap enough to push inside a
lambda.  And exprIsCheapX in turn takes an argument
     type CheapAppFun = Id -> Int -> Bool
which tells when an application is cheap. This makes it easy to
write the analysis loop.

The analysis is cheap-and-cheerful because it doesn't deal with
mutual recursion.  But the self-recursive case is the important one.


Note [Eta expanding through dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the experimental -fdicts-cheap flag is on, we eta-expand through
dictionary bindings.  This improves arities. Thereby, it also
means that full laziness is less prone to floating out the
application of a function to its dictionary arguments, which
can thereby lose opportunities for fusion.  Example:
        foo :: Ord a => a -> ...
     foo = /\a \(d:Ord a). let d' = ...d... in \(x:a). ....
        -- So foo has arity 1

     f = \x. foo dInt $ bar x

The (foo DInt) is floated out, and makes ineffective a RULE
     foo (bar x) = ...

One could go further and make exprIsCheap reply True to any
dictionary-typed expression, but that's more work.

Note [Eta expanding thunks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't eta-expand
   * Trivial RHSs     x = y
   * PAPs             x = map g
   * Thunks           f = case y of p -> \x -> blah

When we see
     f = case y of p -> \x -> blah
should we eta-expand it? Well, if 'x' is a one-shot state token
then 'yes' because 'f' will only be applied once.  But otherwise
we (conservatively) say no.  My main reason is to avoid expanding
PAPSs
        f = g d  ==>  f = \x. g d x
because that might in turn make g inline (if it has an inline pragma),
which we might not want.  After all, INLINE pragmas say "inline only
when saturated" so we don't want to be too gung-ho about saturating!
-}

arityLam :: Id -> ArityType -> ArityType
arityLam id (ATop as) = ATop (idStateHackOneShotInfo id : as)
arityLam _  (ABot n)  = ABot (n+1)

floatIn :: Bool -> ArityType -> ArityType
-- We have something like (let x = E in b),
-- where b has the given arity type.
floatIn _     (ABot n)  = ABot n
floatIn True  (ATop as) = ATop as
floatIn False (ATop as) = ATop (takeWhile isOneShotInfo as)
   -- If E is not cheap, keep arity only for one-shots

arityApp :: ArityType -> Bool -> ArityType
-- Processing (fun arg) where at is the ArityType of fun,
-- Knock off an argument and behave like 'let'
arityApp (ABot 0)      _     = ABot 0
arityApp (ABot n)      _     = ABot (n-1)
arityApp (ATop [])     _     = ATop []
arityApp (ATop (_:as)) cheap = floatIn cheap (ATop as)

andArityType :: ArityType -> ArityType -> ArityType   -- Used for branches of a 'case'
andArityType (ABot n1) (ABot n2)  = ABot (n1 `max` n2) -- Note [ABot branches: use max]
andArityType (ATop as)  (ABot _)  = ATop as
andArityType (ABot _)   (ATop bs) = ATop bs
andArityType (ATop as)  (ATop bs) = ATop (as `combine` bs)
  where      -- See Note [Combining case branches]
    combine (a:as) (b:bs) = (a `bestOneShot` b) : combine as bs
    combine []     bs     = takeWhile isOneShotInfo bs
    combine as     []     = takeWhile isOneShotInfo as

{- Note [ABot branches: use max]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider   case x of
             True  -> \x.  error "urk"
             False -> \xy. error "urk2"

Remember: ABot n means "if you apply to n args, it'll definitely diverge".
So we need (ABot 2) for the whole thing, the /max/ of the ABot arities.

Note [Combining case branches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  go = \x. let z = go e0
               go2 = \x. case x of
                           True  -> z
                           False -> \s(one-shot). e1
           in go2 x
We *really* want to eta-expand go and go2.
When combining the branches of the case we have
     ATop [] `andAT` ATop [OneShotLam]
and we want to get ATop [OneShotLam].  But if the inner
lambda wasn't one-shot we don't want to do this.
(We need a proper arity analysis to justify that.)

So we combine the best of the two branches, on the (slightly dodgy)
basis that if we know one branch is one-shot, then they all must be.

Note [Arity trimming]
~~~~~~~~~~~~~~~~~~~~~
Consider ((\x y. blah) |> co), where co :: (Int->Int->Int) ~ (Int -> F a) , and
F is some type family.

Because of Note [exprArity invariant], item (2), we must return with arity at
most 1, because typeArity (Int -> F a) = 1.  So we have to trim the result of
calling arityType on (\x y. blah).  Failing to do so, and hence breaking the
exprArity invariant, led to #5441.

How to trim?  For ATop, it's easy.  But we must take great care with ABot.
Suppose the expression was (\x y. error "urk"), we'll get (ABot 2).  We
absolutely must not trim that to (ABot 1), because that claims that
((\x y. error "urk") |> co) diverges when given one argument, which it
absolutely does not. And Bad Things happen if we think something returns bottom
when it doesn't (#16066).

So, do not reduce the 'n' in (ABot n); rather, switch (conservatively) to ATop.

Historical note: long ago, we unconditionally switched to ATop when we
encountered a cast, but that is far too conservative: see #5475
-}

---------------------------
type CheapFun = CoreExpr -> Maybe Type -> Bool
        -- How to decide if an expression is cheap
        -- If the Maybe is Just, the type is the type
        -- of the expression; Nothing means "don't know"

data ArityEnv
  = AE { ae_cheap_fn :: CheapFun
       , ae_ped_bot  :: Bool       -- True <=> be pedantic about bottoms
  }

arityType :: ArityEnv -> CoreExpr -> ArityType

arityType env (Cast e co)
  = case arityType env e of
      ATop os -> ATop (take co_arity os)
      -- See Note [Arity trimming]
      ABot n | co_arity < n -> ATop (replicate co_arity noOneShotInfo)
             | otherwise    -> ABot n
  where
    co_arity = length (typeArity (coercionRKind co))
    -- See Note [exprArity invariant] (2); must be true of
    -- arityType too, since that is how we compute the arity
    -- of variables, and they in turn affect result of exprArity
    -- #5441 is a nice demo
    -- However, do make sure that ATop -> ATop and ABot -> ABot!
    --   Casts don't affect that part. Getting this wrong provoked #5475

arityType _ (Var v)
  | strict_sig <- idStrictness v
  , not $ isTopSig strict_sig
  , (ds, res) <- splitStrictSig strict_sig
  , let arity = length ds
  = if isDeadEndDiv res then ABot arity
                    else ATop (take arity one_shots)
  | otherwise
  = ATop (take (idArity v) one_shots)
  where
    one_shots :: [OneShotInfo]  -- One-shot-ness derived from the type
    one_shots = typeArity (idType v)

        -- Lambdas; increase arity
arityType env (Lam x e)
  | isId x    = arityLam x (arityType env e)
  | otherwise = arityType env e

        -- Applications; decrease arity, except for types
arityType env (App fun (Type _))
   = arityType env fun
arityType env (App fun arg )
   = arityApp (arityType env fun) (ae_cheap_fn env arg Nothing)

        -- Case/Let; keep arity if either the expression is cheap
        -- or it's a 1-shot lambda
        -- The former is not really right for Haskell
        --      f x = case x of { (a,b) -> \y. e }
        --  ===>
        --      f x y = case x of { (a,b) -> e }
        -- The difference is observable using 'seq'
        --
arityType env (Case scrut _ _ alts)
  | exprIsDeadEnd scrut || null alts
  = ABot 0     -- Do not eta expand
               -- See Note [Dealing with bottom (1)]
  | otherwise
  = case alts_type of
     ABot n  | n>0       -> ATop []    -- Don't eta expand
             | otherwise -> ABot 0     -- if RHS is bottomming
                                       -- See Note [Dealing with bottom (2)]

     ATop as | not (ae_ped_bot env)    -- See Note [Dealing with bottom (3)]
             , ae_cheap_fn env scrut Nothing -> ATop as
             | exprOkForSpeculation scrut    -> ATop as
             | otherwise                     -> ATop (takeWhile isOneShotInfo as)
  where
    alts_type = foldr1 andArityType [arityType env rhs | (_,_,rhs) <- alts]

arityType env (Let b e)
  = floatIn (cheap_bind b) (arityType env e)
  where
    cheap_bind (NonRec b e) = is_cheap (b,e)
    cheap_bind (Rec prs)    = all is_cheap prs
    is_cheap (b,e) = ae_cheap_fn env e (Just (idType b))

arityType env (Tick t e)
  | not (tickishIsCode t)     = arityType env e

arityType _ _ = vanillaArityType

{-
%************************************************************************
%*                                                                      *
              The main eta-expander
%*                                                                      *
%************************************************************************

We go for:
   f = \x1..xn -> N  ==>   f = \x1..xn y1..ym -> N y1..ym
                                 (n >= 0)

where (in both cases)

        * The xi can include type variables

        * The yi are all value variables

        * N is a NORMAL FORM (i.e. no redexes anywhere)
          wanting a suitable number of extra args.

The biggest reason for doing this is for cases like

        f = \x -> case x of
                    True  -> \y -> e1
                    False -> \y -> e2

Here we want to get the lambdas together.  A good example is the nofib
program fibheaps, which gets 25% more allocation if you don't do this
eta-expansion.

We may have to sandwich some coerces between the lambdas
to make the types work.   exprEtaExpandArity looks through coerces
when computing arity; and etaExpand adds the coerces as necessary when
actually computing the expansion.

Note [No crap in eta-expanded code]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The eta expander is careful not to introduce "crap".  In particular,
given a CoreExpr satisfying the 'CpeRhs' invariant (in CorePrep), it
returns a CoreExpr satisfying the same invariant. See Note [Eta
expansion and the CorePrep invariants] in CorePrep.

This means the eta-expander has to do a bit of on-the-fly
simplification but it's not too hard.  The alternative, of relying on
a subsequent clean-up phase of the Simplifier to de-crapify the result,
means you can't really use it in CorePrep, which is painful.

Note [Eta expansion for join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The no-crap rule is very tiresome to guarantee when
we have join points. Consider eta-expanding
   let j :: Int -> Int -> Bool
       j x = e
   in b

The simple way is
  \(y::Int). (let j x = e in b) y

The no-crap way is
  \(y::Int). let j' :: Int -> Bool
                 j' x = e y
             in b[j'/j] y
where I have written to stress that j's type has
changed.  Note that (of course!) we have to push the application
inside the RHS of the join as well as into the body.  AND if j
has an unfolding we have to push it into there too.  AND j might
be recursive...

So for now I'm abandoning the no-crap rule in this case. I think
that for the use in CorePrep it really doesn't matter; and if
it does, then CoreToStg.myCollectArgs will fall over.

(Moreover, I think that casts can make the no-crap rule fail too.)

Note [Eta expansion and SCCs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that SCCs are not treated specially by etaExpand.  If we have
        etaExpand 2 (\x -> scc "foo" e)
        = (\xy -> (scc "foo" e) y)
So the costs of evaluating 'e' (not 'e y') are attributed to "foo"

Note [Eta expansion and source notes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CorePrep puts floatable ticks outside of value applications, but not
type applications. As a result we might be trying to eta-expand an
expression like

  (src<...> v) @a

which we want to lead to code like

  \x -> src<...> v @a x

This means that we need to look through type applications and be ready
to re-add floats on the top.

-}

-- | @etaExpand n e@ returns an expression with
-- the same meaning as @e@, but with arity @n@.
--
-- Given:
--
-- > e' = etaExpand n e
--
-- We should have that:
--
-- > ty = exprType e = exprType e'
etaExpand :: Arity              -- ^ Result should have this number of value args
          -> CoreExpr           -- ^ Expression to expand
          -> CoreExpr
-- etaExpand arity e = res
-- Then 'res' has at least 'arity' lambdas at the top
--
-- etaExpand deals with for-alls. For example:
--              etaExpand 1 E
-- where  E :: forall a. a -> a
-- would return
--      (/\b. \y::a -> E b y)
--
-- It deals with coerces too, though they are now rare
-- so perhaps the extra code isn't worth it

etaExpand n orig_expr
  = go n orig_expr
  where
      -- Strip off existing lambdas and casts before handing off to mkEtaWW
      -- Note [Eta expansion and SCCs]
    go 0 expr = expr
    go n (Lam v body) | isTyVar v = Lam v (go n     body)
                      | otherwise = Lam v (go (n-1) body)
    go n (Cast expr co)           = Cast (go n expr) co
    go n expr
      = -- pprTrace "ee" (vcat [ppr orig_expr, ppr expr, ppr etas]) $
        retick $ etaInfoAbs etas (etaInfoApp subst' sexpr etas)
      where
          in_scope = mkInScopeSet (exprFreeVars expr)
          (in_scope', etas) = mkEtaWW n (ppr orig_expr) in_scope (exprType expr)
          subst' = mkEmptySubst in_scope'

          -- Find ticks behind type apps.
          -- See Note [Eta expansion and source notes]
          (expr', args) = collectArgs expr
          (ticks, expr'') = stripTicksTop tickishFloatable expr'
          sexpr = foldl' App expr'' args
          retick expr = foldr mkTick expr ticks

                                -- Abstraction    Application
--------------
data EtaInfo = EtaVar Var       -- /\a. []        [] a
                                -- \x.  []        [] x
             | EtaCo Coercion   -- [] |> sym co   [] |> co

instance Outputable EtaInfo where
   ppr (EtaVar v) = text "EtaVar" <+> ppr v
   ppr (EtaCo co) = text "EtaCo"  <+> ppr co

pushCoercion :: Coercion -> [EtaInfo] -> [EtaInfo]
pushCoercion co1 (EtaCo co2 : eis)
  | isReflCo co = eis
  | otherwise   = EtaCo co : eis
  where
    co = co1 `mkTransCo` co2

pushCoercion co eis = EtaCo co : eis

--------------
etaInfoAbs :: [EtaInfo] -> CoreExpr -> CoreExpr
etaInfoAbs []               expr = expr
etaInfoAbs (EtaVar v : eis) expr = Lam v (etaInfoAbs eis expr)
etaInfoAbs (EtaCo co : eis) expr = Cast (etaInfoAbs eis expr) (mkSymCo co)

--------------
etaInfoApp :: Subst -> CoreExpr -> [EtaInfo] -> CoreExpr
-- (etaInfoApp s e eis) returns something equivalent to
--             ((substExpr s e) `appliedto` eis)

etaInfoApp subst (Lam v1 e) (EtaVar v2 : eis)
  = etaInfoApp (GHC.Core.Subst.extendSubstWithVar subst v1 v2) e eis

etaInfoApp subst (Cast e co1) eis
  = etaInfoApp subst e (pushCoercion co' eis)
  where
    co' = GHC.Core.Subst.substCo subst co1

etaInfoApp subst (Case e b ty alts) eis
  = Case (subst_expr subst e) b1 ty' alts'
  where
    (subst1, b1) = substBndr subst b
    alts' = map subst_alt alts
    ty'   = etaInfoAppTy (GHC.Core.Subst.substTy subst ty) eis
    subst_alt (con, bs, rhs) = (con, bs', etaInfoApp subst2 rhs eis)
              where
                 (subst2,bs') = substBndrs subst1 bs

etaInfoApp subst (Let b e) eis
  | not (isJoinBind b)
    -- See Note [Eta expansion for join points]
  = Let b' (etaInfoApp subst' e eis)
  where
    (subst', b') = substBindSC subst b

etaInfoApp subst (Tick t e) eis
  = Tick (substTickish subst t) (etaInfoApp subst e eis)

etaInfoApp subst expr _
  | (Var fun, _) <- collectArgs expr
  , Var fun' <- lookupIdSubst (text "etaInfoApp" <+> ppr fun) subst fun
  , isJoinId fun'
  = subst_expr subst expr

etaInfoApp subst e eis
  = go (subst_expr subst e) eis
  where
    go e []                  = e
    go e (EtaVar v    : eis) = go (App e (varToCoreExpr v)) eis
    go e (EtaCo co    : eis) = go (Cast e co) eis


--------------
etaInfoAppTy :: Type -> [EtaInfo] -> Type
-- If                    e :: ty
-- then   etaInfoApp e eis :: etaInfoApp ty eis
etaInfoAppTy ty []               = ty
etaInfoAppTy ty (EtaVar v : eis) = etaInfoAppTy (applyTypeToArg ty (varToCoreExpr v)) eis
etaInfoAppTy _  (EtaCo co : eis) = etaInfoAppTy (coercionRKind co) eis

--------------
-- | @mkEtaWW n _ fvs ty@ will compute the 'EtaInfo' necessary for eta-expanding
-- an expression @e :: ty@ to take @n@ value arguments, where @fvs@ are the
-- free variables of @e@.
--
-- Note that this function is entirely unconcerned about cost centres and other
-- semantically-irrelevant source annotations, so call sites must take care to
-- preserve that info. See Note [Eta expansion and SCCs].
mkEtaWW
  :: Arity
  -- ^ How many value arguments to eta-expand
  -> SDoc
  -- ^ The pretty-printed original expression, for warnings.
  -> InScopeSet
  -- ^ A super-set of the free vars of the expression to eta-expand.
  -> Type
  -> (InScopeSet, [EtaInfo])
  -- ^ The variables in 'EtaInfo' are fresh wrt. to the incoming 'InScopeSet'.
  -- The outgoing 'InScopeSet' extends the incoming 'InScopeSet' with the
  -- fresh variables in 'EtaInfo'.

mkEtaWW orig_n ppr_orig_expr in_scope orig_ty
  = go orig_n empty_subst orig_ty []
  where
    empty_subst = mkEmptyTCvSubst in_scope

    go :: Arity              -- Number of value args to expand to
       -> TCvSubst -> Type   -- We are really looking at subst(ty)
       -> [EtaInfo]          -- Accumulating parameter
       -> (InScopeSet, [EtaInfo])
    go n subst ty eis       -- See Note [exprArity invariant]

       ----------- Done!  No more expansion needed
       | n == 0
       = (getTCvInScope subst, reverse eis)

       ----------- Forall types  (forall a. ty)
       | Just (tcv,ty') <- splitForAllTy_maybe ty
       , let (subst', tcv') = Type.substVarBndr subst tcv
       = let ((n_subst, n_tcv), n_n)
               -- We want to have at least 'n' lambdas at the top.
               -- If tcv is a tyvar, it corresponds to one Lambda (/\).
               --   And we won't reduce n.
               -- If tcv is a covar, we could eta-expand the expr with one
               --   lambda \co:ty. e co. In this case we generate a new variable
               --   of the coercion type, update the scope, and reduce n by 1.
               | isTyVar tcv = ((subst', tcv'), n)
               | otherwise   = (freshEtaId n subst' (varType tcv'), n-1)
           -- Avoid free vars of the original expression
         in go n_n n_subst ty' (EtaVar n_tcv : eis)

       ----------- Function types  (t1 -> t2)
       | Just (arg_ty, res_ty) <- splitFunTy_maybe ty
       , not (isTypeLevPoly arg_ty)
          -- See Note [Levity polymorphism invariants] in GHC.Core
          -- See also test case typecheck/should_run/EtaExpandLevPoly

       , let (subst', eta_id') = freshEtaId n subst arg_ty
           -- Avoid free vars of the original expression
       = go (n-1) subst' res_ty (EtaVar eta_id' : eis)

       ----------- Newtypes
       -- Given this:
       --      newtype T = MkT ([T] -> Int)
       -- Consider eta-expanding this
       --      eta_expand 1 e T
       -- We want to get
       --      coerce T (\x::[T] -> (coerce ([T]->Int) e) x)
       | Just (co, ty') <- topNormaliseNewType_maybe ty
       , let co' = Coercion.substCo subst co
             -- Remember to apply the substitution to co (#16979)
             -- (or we could have applied to ty, but then
             --  we'd have had to zap it for the recursive call)
       = go n subst ty' (pushCoercion co' eis)

       | otherwise       -- We have an expression of arity > 0,
                         -- but its type isn't a function, or a binder
                         -- is levity-polymorphic
       = WARN( True, (ppr orig_n <+> ppr orig_ty) $$ ppr_orig_expr )
         (getTCvInScope subst, reverse eis)
        -- This *can* legitimately happen:
        -- e.g.  coerce Int (\x. x) Essentially the programmer is
        -- playing fast and loose with types (Happy does this a lot).
        -- So we simply decline to eta-expand.  Otherwise we'd end up
        -- with an explicit lambda having a non-function type



--------------
-- Don't use short-cutting substitution - we may be changing the types of join
-- points, so applying the in-scope set is necessary
-- TODO Check if we actually *are* changing any join points' types

subst_expr :: Subst -> CoreExpr -> CoreExpr
subst_expr = substExpr (text "GHC.Core.Arity:substExpr")


--------------

-- | Split an expression into the given number of binders and a body,
-- eta-expanding if necessary. Counts value *and* type binders.
etaExpandToJoinPoint :: JoinArity -> CoreExpr -> ([CoreBndr], CoreExpr)
etaExpandToJoinPoint join_arity expr
  = go join_arity [] expr
  where
    go 0 rev_bs e         = (reverse rev_bs, e)
    go n rev_bs (Lam b e) = go (n-1) (b : rev_bs) e
    go n rev_bs e         = case etaBodyForJoinPoint n e of
                              (bs, e') -> (reverse rev_bs ++ bs, e')

etaExpandToJoinPointRule :: JoinArity -> CoreRule -> CoreRule
etaExpandToJoinPointRule _ rule@(BuiltinRule {})
  = WARN(True, (sep [text "Can't eta-expand built-in rule:", ppr rule]))
      -- How did a local binding get a built-in rule anyway? Probably a plugin.
    rule
etaExpandToJoinPointRule join_arity rule@(Rule { ru_bndrs = bndrs, ru_rhs = rhs
                                               , ru_args  = args })
  | need_args == 0
  = rule
  | need_args < 0
  = pprPanic "etaExpandToJoinPointRule" (ppr join_arity $$ ppr rule)
  | otherwise
  = rule { ru_bndrs = bndrs ++ new_bndrs, ru_args = args ++ new_args
         , ru_rhs = new_rhs }
  where
    need_args = join_arity - length args
    (new_bndrs, new_rhs) = etaBodyForJoinPoint need_args rhs
    new_args = varsToCoreExprs new_bndrs

-- Adds as many binders as asked for; assumes expr is not a lambda
etaBodyForJoinPoint :: Int -> CoreExpr -> ([CoreBndr], CoreExpr)
etaBodyForJoinPoint need_args body
  = go need_args (exprType body) (init_subst body) [] body
  where
    go 0 _  _     rev_bs e
      = (reverse rev_bs, e)
    go n ty subst rev_bs e
      | Just (tv, res_ty) <- splitForAllTy_maybe ty
      , let (subst', tv') = Type.substVarBndr subst tv
      = go (n-1) res_ty subst' (tv' : rev_bs) (e `App` varToCoreExpr tv')
      | Just (arg_ty, res_ty) <- splitFunTy_maybe ty
      , let (subst', b) = freshEtaId n subst arg_ty
      = go (n-1) res_ty subst' (b : rev_bs) (e `App` Var b)
      | otherwise
      = pprPanic "etaBodyForJoinPoint" $ int need_args $$
                                         ppr body $$ ppr (exprType body)

    init_subst e = mkEmptyTCvSubst (mkInScopeSet (exprFreeVars e))

--------------
freshEtaId :: Int -> TCvSubst -> Type -> (TCvSubst, Id)
-- Make a fresh Id, with specified type (after applying substitution)
-- It should be "fresh" in the sense that it's not in the in-scope set
-- of the TvSubstEnv; and it should itself then be added to the in-scope
-- set of the TvSubstEnv
--
-- The Int is just a reasonable starting point for generating a unique;
-- it does not necessarily have to be unique itself.
freshEtaId n subst ty
      = (subst', eta_id')
      where
        ty'     = Type.substTyUnchecked subst ty
        eta_id' = uniqAway (getTCvInScope subst) $
                  mkSysLocalOrCoVar (fsLit "eta") (mkBuiltinUnique n) ty'
                  -- "OrCoVar" since this can be used to eta-expand
                  -- coercion abstractions
        subst'  = extendTCvInScope subst eta_id'
