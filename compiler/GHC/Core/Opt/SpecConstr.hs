{-# LANGUAGE CPP, LambdaCase #-}
#if __GLASGOW_HASKELL__ < 905
{-# LANGUAGE PatternSynonyms #-}
#endif
{-
ToDo [Oct 2013]
~~~~~~~~~~~~~~~
1. Nuke ForceSpecConstr for good (it is subsumed by GHC.Types.SPEC in ghc-prim)
2. Nuke NoSpecConstr


(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[SpecConstr]{Specialise over constructors}
-}



{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.Core.Opt.SpecConstr(
        specConstrProgram,
        SpecConstrAnnotation(..),
        SpecFailWarning(..)
    ) where

import GHC.Prelude

import GHC.Driver.DynFlags ( DynFlags(..), GeneralFlag( Opt_SpecConstrKeen )
                          , gopt, hasPprDebug )

import GHC.Core
import GHC.Core.Subst
import GHC.Core.Utils
import GHC.Core.Unfold
import GHC.Core.Opt.Simplify.Inline
import GHC.Core.FVs     ( exprsFreeVarsList, exprFreeVars )
import GHC.Core.Opt.Monad
import GHC.Core.Opt.WorkWrap.Utils
import GHC.Core.Opt.OccurAnal( BinderSwapDecision(..), scrutOkForBinderSwap )
import GHC.Core.DataCon
import GHC.Core.Class( classTyVars )
import GHC.Core.Coercion hiding( substCo )
import GHC.Core.Rules
import GHC.Core.Predicate ( typeDeterminesValue )
import GHC.Core.Type     hiding ( substTy )
import GHC.Core.TyCon   (TyCon, tyConName )
import GHC.Core.Multiplicity
import GHC.Core.Ppr     ( pprParendExpr )
import GHC.Core.Make    ( mkImpossibleExpr )

import GHC.Unit.Module
import GHC.Unit.Module.ModGuts

import GHC.Types.Error (MessageClass(..), Severity(..), DiagnosticReason(WarningWithoutFlag), ResolvedDiagnosticReason (..))
import GHC.Types.Literal ( litIsLifted )
import GHC.Types.Id
import GHC.Types.Id.Info ( IdDetails(..) )
import GHC.Types.Id.Make ( voidArgId, voidPrimId )
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Name
import GHC.Types.Tickish
import GHC.Types.Basic
import GHC.Types.Demand
import GHC.Types.Cpr
import GHC.Types.Unique.Supply
import GHC.Types.Unique.FM
import GHC.Types.Unique( hasKey )

import GHC.Data.Maybe     ( orElse, catMaybes, isJust, isNothing )
import GHC.Data.FastString

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Monad

import GHC.Builtin.Names ( specTyConKey )

import GHC.Exts( SpecConstrAnnotation(..) )
import GHC.Serialized   ( deserializeWithData )

import Control.Monad
import Data.List ( sortBy, partition, dropWhileEnd, mapAccumL )
import Data.Maybe( mapMaybe )
import Data.Ord( comparing )
import Data.Tuple

{-
-----------------------------------------------------
                        Game plan
-----------------------------------------------------

Consider
        drop n []     = []
        drop 0 xs     = []
        drop n (x:xs) = drop (n-1) xs

After the first time round, we could pass n unboxed.  This happens in
numerical code too.  Here's what it looks like in Core:

        drop n xs = case xs of
                      []     -> []
                      (y:ys) -> case n of
                                  I# n# -> case n# of
                                             0 -> []
                                             _ -> drop (I# (n# -# 1#)) xs

Notice that the recursive call has an explicit constructor as argument.
Noticing this, we can make a specialised version of drop

        RULE: drop (I# n#) xs ==> drop' n# xs

        drop' n# xs = let n = I# n# in ...orig RHS...

Now the simplifier will apply the specialisation in the rhs of drop', giving

        drop' n# xs = case xs of
                      []     -> []
                      (y:ys) -> case n# of
                                  0 -> []
                                  _ -> drop' (n# -# 1#) xs

Much better!

We'd also like to catch cases where a parameter is carried along unchanged,
but evaluated each time round the loop:

        f i n = if i>0 || i>n then i else f (i*2) n

Here f isn't strict in n, but we'd like to avoid evaluating it each iteration.
In Core, by the time we've w/wd (f is strict in i) we get

        f i# n = case i# ># 0 of
                   False -> I# i#
                   True  -> case n of { I# n# ->
                            case i# ># n# of
                                False -> I# i#
                                True  -> f (i# *# 2#) n

At the call to f, we see that the argument, n is known to be (I# n#),
and n is evaluated elsewhere in the body of f, so we can play the same
trick as above.


Note [Reboxing]
~~~~~~~~~~~~~~~
We must be careful not to allocate the same constructor twice.  Consider
        f p = (...(case p of (a,b) -> e)...p...,
               ...let t = (r,s) in ...t...(f t)...)
At the recursive call to f, we can see that t is a pair.  But we do NOT want
to make a specialised copy:
        f' a b = let p = (a,b) in (..., ...)
because now t is allocated by the caller, then r and s are passed to the
recursive call, which allocates the (r,s) pair again.

This happens if
  (a) the argument p is used in other than a case-scrutinisation way.
  (b) the argument to the call is not a 'fresh' tuple; you have to
        look into its unfolding to see that it's a tuple

Hence the "OR" part of Note [Good arguments] below.

ALTERNATIVE 2: pass both boxed and unboxed versions.  This no longer saves
allocation, but does perhaps save evals. In the RULE we'd have
something like

  f (I# x#) = f' (I# x#) x#

If at the call site the (I# x) was an unfolding, then we'd have to
rely on CSE to eliminate the duplicate allocation.... This alternative
doesn't look attractive enough to pursue.

ALTERNATIVE 3: ignore the reboxing problem.  The trouble is that
the conservative reboxing story prevents many useful functions from being
specialised.  Example:
        foo :: Maybe Int -> Int -> Int
        foo   (Just m) 0 = 0
        foo x@(Just m) n = foo x (n-m)
Here the use of 'x' will clearly not require boxing in the specialised function.

The strictness analyser has the same problem, in fact.  Example:
        f p@(a,b) = ...
If we pass just 'a' and 'b' to the worker, it might need to rebox the
pair to create (a,b).  A more sophisticated analysis might figure out
precisely the cases in which this could happen, but the strictness
analyser does no such analysis; it just passes 'a' and 'b', and hopes
for the best.

So my current choice is to make SpecConstr similarly aggressive, and
ignore the bad potential of reboxing.


Note [Good arguments]
~~~~~~~~~~~~~~~~~~~~~
So we look for

* A self-recursive function.  Ignore mutual recursion for now,
  because it's less common, and the code is simpler for self-recursion.

* EITHER

   a) At a recursive call, one or more parameters is an explicit
      constructor application
        AND
      That same parameter is scrutinised by a case somewhere in
      the RHS of the function

  OR

    b) At a recursive call, one or more parameters has an unfolding
       that is an explicit constructor application
        AND
      That same parameter is scrutinised by a case somewhere in
      the RHS of the function
        AND
      Those are the only uses of the parameter (see Note [Reboxing])


What to abstract over
~~~~~~~~~~~~~~~~~~~~~
There's a bit of a complication with type arguments.  If the call
site looks like

        f p = ...f ((:) [a] x xs)...

then our specialised function look like

        f_spec x xs = let p = (:) [a] x xs in ....as before....

This only makes sense if either
  a) the type variable 'a' is in scope at the top of f, or
  b) the type variable 'a' is an argument to f (and hence fs)

Actually, (a) may hold for value arguments too, in which case
we may not want to pass them.  Suppose 'x' is in scope at f's
defn, but xs is not.  Then we'd like

        f_spec xs = let p = (:) [a] x xs in ....as before....

Similarly (b) may hold too.  If x is already an argument at the
call, no need to pass it again.

Finally, if 'a' is not in scope at the call site, we could abstract
it as we do the term variables:

        f_spec a x xs = let p = (:) [a] x xs in ...as before...

So the grand plan is:

        * abstract the call site to a constructor-only pattern
          e.g.  C x (D (f p) (g q))  ==>  C s1 (D s2 s3)

        * Find the free variables of the abstracted pattern

        * Pass these variables, less any that are in scope at
          the fn defn.  But see Note [Shadowing in SpecConstr] below.


NOTICE that we only abstract over variables that are not in scope,
so we're in no danger of shadowing variables used in "higher up"
in f_spec's RHS.


Note [Shadowing in SpecConstr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In this pass we gather up usage information that may mention variables
that are bound between the usage site and the definition site; or (more
seriously) may be bound to something different at the definition site.
For example:

        f x = letrec g y v = let x = ...
                             in ...(g (a,b) x)...

Since 'x' is in scope at the call site, we may make a rewrite rule that
looks like
        RULE forall a,b. g (a,b) x = ...
But this rule will never match, because it's really a different 'x' at
the call site -- and that difference will be manifest by the time the
simplifier gets to it.  [A worry: the simplifier doesn't *guarantee*
no-shadowing, so perhaps it may not be distinct?]

Anyway, the rule isn't actually wrong, it's just not useful.  One possibility
is to run deShadowBinds before running SpecConstr, but instead we run the
simplifier.  That gives the simplest possible program for SpecConstr to
chew on; and it virtually guarantees no shadowing.

Note [Specialising for constant parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This one is about specialising on a *constant* (but not necessarily
constructor) argument

    foo :: Int -> (Int -> Int) -> Int
    foo 0 f = 0
    foo m f = foo (f m) (+1)

It produces

    lvl_rmV :: GHC.Base.Int -> GHC.Base.Int
    lvl_rmV =
      \ (ds_dlk :: GHC.Base.Int) ->
        case ds_dlk of wild_alH { GHC.Base.I# x_alG ->
        GHC.Base.I# (GHC.Prim.+# x_alG 1)

    T.$wfoo :: GHC.Prim.Int# -> (GHC.Base.Int -> GHC.Base.Int) ->
    GHC.Prim.Int#
    T.$wfoo =
      \ (ww_sme :: GHC.Prim.Int#) (w_smg :: GHC.Base.Int -> GHC.Base.Int) ->
        case ww_sme of ds_Xlw {
          __DEFAULT ->
        case w_smg (GHC.Base.I# ds_Xlw) of w1_Xmo { GHC.Base.I# ww1_Xmz ->
        T.$wfoo ww1_Xmz lvl_rmV
        };
          0 -> 0
        }

The recursive call has lvl_rmV as its argument, so we could create a specialised copy
with that argument baked in; that is, not passed at all.   Now it can perhaps be inlined.

When is this worth it?  Call the constant 'lvl'
- If 'lvl' has an unfolding that is a constructor, see if the corresponding
  parameter is scrutinised anywhere in the body.

- If 'lvl' has an unfolding that is a inlinable function, see if the corresponding
  parameter is applied (...to enough arguments...?)

  Also do this is if the function has RULES?

Also

Note [Specialising for lambda parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    foo :: Int -> (Int -> Int) -> Int
    foo 0 f = 0
    foo m f = foo (f m) (\n -> n-m)

This is subtly different from the previous one in that we get an
explicit lambda as the argument:

    T.$wfoo :: GHC.Prim.Int# -> (GHC.Base.Int -> GHC.Base.Int) ->
    GHC.Prim.Int#
    T.$wfoo =
      \ (ww_sm8 :: GHC.Prim.Int#) (w_sma :: GHC.Base.Int -> GHC.Base.Int) ->
        case ww_sm8 of ds_Xlr {
          __DEFAULT ->
        case w_sma (GHC.Base.I# ds_Xlr) of w1_Xmf { GHC.Base.I# ww1_Xmq ->
        T.$wfoo
          ww1_Xmq
          (\ (n_ad3 :: GHC.Base.Int) ->
             case n_ad3 of wild_alB { GHC.Base.I# x_alA ->
             GHC.Base.I# (GHC.Prim.-# x_alA ds_Xlr)
             })
        };
          0 -> 0
        }

I wonder if SpecConstr couldn't be extended to handle this? After all,
lambda is a sort of constructor for functions and perhaps it already
has most of the necessary machinery?

Furthermore, there's an immediate win, because you don't need to allocate the lambda
at the call site; and if perchance it's called in the recursive call, then you
may avoid allocating it altogether.  Just like for constructors.

Looks cool, but probably rare...but it might be easy to implement.


Note [SpecConstr for casts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    data family T a :: *
    data instance T Int = T Int

    foo n = ...
       where
         go (T 0) = 0
         go (T n) = go (T (n-1))

The recursive call ends up looking like
        go (T (I# ...) `cast` g)
So we want to spot the constructor application inside the cast.
That's why we have the Cast case in argToPat

Note [Seeding recursive groups]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a recursive group that is either
  * nested, or
  * top-level, but with no exported Ids
we can see all the calls to the function, so we seed the specialisation
loop from the calls in the body, and /not/ from the calls in the RHS.
Consider:

  bar m n = foo n (n,n) (n,n) (n,n) (n,n)
   where
     foo n p q r s
       | n == 0    = m
       | n > 3000  = case p of { (p1,p2) -> foo (n-1) (p2,p1) q r s }
       | n > 2000  = case q of { (q1,q2) -> foo (n-1) p (q2,q1) r s }
       | n > 1000  = case r of { (r1,r2) -> foo (n-1) p q (r2,r1) s }
       | otherwise = case s of { (s1,s2) -> foo (n-1) p q r (s2,s1) }

If we start with the RHSs of 'foo', we get lots and lots of specialisations,
most of which are not needed.  But if we start with the (single) call
in the rhs of 'bar' we get exactly one fully-specialised copy, and all
the recursive calls go to this fully-specialised copy. Indeed, the original
function is later collected as dead code.  This is very important in
specialising the loops arising from stream fusion, for example in NDP where
we were getting literally hundreds of (mostly unused) specialisations of
a local function.

In a case like the above we end up never calling the original un-specialised
function.  (Although we still leave its code around just in case.)

Wrinkles

* Boring calls. If we find any boring calls in the body, including
  *unsaturated* ones, such as
      letrec foo x y = ....foo...
      in map foo xs
  then we will end up calling the un-specialised function, so then we
  *should* use the calls in the un-specialised RHS as seeds.  We call
  these "boring call patterns", and callsToNewPats reports if it finds
  any of these.  Then 'specialise' unleashes the usage info from the
  un-specialised RHS.

* Exported Ids. `specialise` /also/ unleashes `si_mb_unspec`
  for exported Ids.  That way we are sure to generate usage info from
  the /un-specialised/ RHS of an exported function.

More precisely:

* Always start from the calls in the body of the let or (for top level)
  calls in the rest of the module.  See the body_calls in the call to
  `specialise` in `specNonRec`, and to `go` in `specRec`.

* si_mb_unspec holds the usage from the unspecialised RHS.
  See `initSpecInfo`.

* `specialise` will unleash si_mb_unspec, if
  - `callsToNewPats` reports "boring calls found", or
  - this is a top-level exported Id.

Historical note.  At an earlier point, if a top-level Id was exported,
we used only seeds from the RHS, and /not/from the body. But Dimitrios
had an example where using call patterns from the body (the other defns
in the module) was crucial.  And doing so improved nofib allocation results:
    multiplier: 4%   better
    minimax:    2.8% better
In any case, it is easier to do!

Note [Do not specialise diverging functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Specialising a function that just diverges is a waste of code.
Furthermore, it broke GHC (simpl014) thus:
   {-# STR Sb #-}
   f = \x. case x of (a,b) -> f x
If we specialise f we get
   f = \x. case x of (a,b) -> fspec a b
But fspec doesn't have decent strictness info.  As it happened,
(f x) :: IO t, so the state hack applied and we eta expanded fspec,
and hence f.  But now f's strictness is less than its arity, which
breaks an invariant.


Note [Forcing specialisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With stream fusion and in other similar cases, we want to fully
specialise some (but not necessarily all!) loops regardless of their
size and the number of specialisations.

We allow a library to do this, in one of two ways (one which is
deprecated):

  1) Add a parameter of type GHC.Types.SPEC (from ghc-prim) to the loop body.

  2) (Deprecated) Annotate a type with ForceSpecConstr from GHC.Exts,
     and then add *that* type as a parameter to the loop body

The reason #2 is deprecated is because it requires GHCi, which isn't
available for things like a cross compiler using stage1.

Here's a (simplified) example from the `vector` package. You may bring
the special 'force specialization' type into scope by saying:

  import GHC.Types (SPEC(..))

or by defining your own type (again, deprecated):

  data SPEC = SPEC | SPEC2
  {-# ANN type SPEC ForceSpecConstr #-}

(Note this is the exact same definition of GHC.Types.SPEC, just
without the annotation.)

After that, you say:

  foldl :: (a -> b -> a) -> a -> Stream b -> a
  {-# INLINE foldl #-}
  foldl f z (Stream step s _) = foldl_loop SPEC z s
    where
      foldl_loop !sPEC z s = case step s of
                              Yield x s' -> foldl_loop sPEC (f z x) s'
                              Skip       -> foldl_loop sPEC z s'
                              Done       -> z

SpecConstr will spot the SPEC parameter and always fully specialise
foldl_loop. Note that

  * We have to prevent the SPEC argument from being removed by
    w/w which is why (a) SPEC is a sum type, and (b) we have to seq on
    the SPEC argument.

  * And lastly, the SPEC argument is ultimately eliminated by
    SpecConstr itself so there is no runtime overhead.

This is all quite ugly; we ought to come up with a better design.

ForceSpecConstr arguments are spotted in scExpr' and scTopBinds which then set
sc_force to True when calling specLoop. This flag does four things:

(FS1) Ignore specConstrThreshold, to specialise functions of arbitrary size
        (see scTopBind)
(FS2) Ignore specConstrCount, to make arbitrary numbers of specialisations
        (see specialise)
(FS3) Specialise even for arguments that are not scrutinised in the loop
        (see argToPat; #4448)
(FS4) Only specialise on recursive types a finite number of times
        (see sc_recursive; #5550; Note [Limit recursive specialisation])
(FS5) Use a different restriction on the maximum number of arguments which
        the optimisation will specialise. We tried removing the limit on worker
        args for forced specs (#14003) but this caused issues when specializing
        code for large data structures (#25197).
        This is handled by `too_many_worker_args` in `callsToNewPats`

The flag holds only for specialising a single binding group, and NOT
for nested bindings.  (So really it should be passed around explicitly
and not stored in ScEnv.)  #14379 turned out to be caused by
   f SPEC x = let g1 x = ...
              in ...
We force-specialise f (because of the SPEC), but that generates a specialised
copy of g1 (as well as the original).  Alas g1 has a nested binding g2; and
in each copy of g1 we get an unspecialised and specialised copy of g2; and so
on. Result, exponential.  So the force-spec flag now only applies to one
level of bindings at a time.

Mechanism for this one-level-only thing:

 - Switch it on at the call to specRec, in scExpr and scTopBinds
 - Switch it off when doing the RHSs;
   this can be done very conveniently in decreaseSpecCount

What alternatives did I consider?

* Annotating the loop itself doesn't work because (a) it is local and
  (b) it will be w/w'ed and having w/w propagating annotations somehow
  doesn't seem like a good idea. The types of the loop arguments
  really seem to be the most persistent thing.

* Annotating the types that make up the loop state doesn't work,
  either, because (a) it would prevent us from using types like Either
  or tuples here, (b) we don't want to restrict the set of types that
  can be used in Stream states and (c) some types are fixed by the
  user (e.g., the accumulator here) but we still want to specialise as
  much as possible.

Alternatives to ForceSpecConstr
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Instead of giving the loop an extra argument of type SPEC, we
also considered *wrapping* arguments in SPEC, thus
  data SPEC a = SPEC a | SPEC2

  loop = \arg -> case arg of
                     SPEC state ->
                        case state of (x,y) -> ... loop (SPEC (x',y')) ...
                        S2 -> error ...
The idea is that a SPEC argument says "specialise this argument
regardless of whether the function case-analyses it".  But this
doesn't work well:
  * SPEC must still be a sum type, else the strictness analyser
    eliminates it
  * But that means that 'loop' won't be strict in its real payload
This loss of strictness in turn screws up specialisation, because
we may end up with calls like
   loop (SPEC (case z of (p,q) -> (q,p)))
Without the SPEC, if 'loop' were strict, the case would move out
and we'd see loop applied to a pair. But if 'loop' isn't strict
this doesn't look like a specialisable call.

Note [Limit recursive specialisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is possible for ForceSpecConstr to cause an infinite loop of specialisation.
Because there is no limit on the number of specialisations, a recursive call with
a recursive constructor as an argument (for example, list cons) will generate
a specialisation for that constructor. If the resulting specialisation also
contains a recursive call with the constructor, this could proceed indefinitely.

For example, if ForceSpecConstr is on:
  loop :: [Int] -> [Int] -> [Int]
  loop z []         = z
  loop z (x:xs)     = loop (x:z) xs
this example will create a specialisation for the pattern
  loop (a:b) c      = loop' a b c

  loop' a b []      = (a:b)
  loop' a b (x:xs)  = loop (x:(a:b)) xs
and a new pattern is found:
  loop (a:(b:c)) d  = loop'' a b c d
which can continue indefinitely.

Roman's suggestion to fix this was to stop after a couple of times on recursive types,
but still specialising on non-recursive types as much as possible.

To implement this, we count the number of times we have gone round the
"specialise recursively" loop ('go' in 'specRec').  Once have gone round
more than N times (controlled by -fspec-constr-recursive=N) we check

  - If sc_force is off, and sc_count is (Just max) then we don't
    need to do anything: trim_pats will limit the number of specs

  - Otherwise check if any function has now got more than (sc_count env)
    specialisations.  If sc_count is "no limit" then we arbitrarily
    choose 10 as the limit (ugh).

See #5550.   Also #13623, where this test had become over-aggressive,
and we lost a wonderful specialisation that we really wanted!

Note [NoSpecConstr]
~~~~~~~~~~~~~~~~~~~
The ignoreDataCon stuff allows you to say
    {-# ANN type T NoSpecConstr #-}
to mean "don't specialise on arguments of this type".  It was added
before we had ForceSpecConstr.  Lacking ForceSpecConstr we specialised
regardless of size; and then we needed a way to turn that *off*.  Now
that we have ForceSpecConstr, this NoSpecConstr is probably redundant.
(Used only for PArray, TODO: remove?)

Note [SpecConstr and strict fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We treat strict fields in SpecConstr the same way we do in W/W.
That is we make the specialized function strict in arguments
representing strict fields. See Note [Call-by-value for worker args]
for why we do this.

(SCF1) The arg_id might be an /imported/ Id like M.foo_acf (see #24944).
  We don't want to make
     case M.foo_acf of M.foo_acf { DEFAULT -> blah }
  because the binder of a case-expression should never be imported.  Rather,
  we must localise it thus:
     case M.foo_acf of foo_acf { DEFAULT -> blah }
  We keep the same unique, so in the next round of simplification we'll replace
  any M.foo_acf's in `blah` by `foo_acf`.

  c.f. Note [Localise pattern binders] in GHC.HsToCore.Utils.

Note [Specialising on dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In #21386, SpecConstr saw this call:

   $wgo 100# @.. ($fMonadStateT @.. @.. $fMonadIdentity)

where $wgo :: Int# -> forall m. Monad m => blah

You might think that the type-class Specialiser would have specialised
this, but there are good reasons why not: the Specialiser ran too early.
But regardless, SpecConstr can and should!  It's easy:

* isValue: treat ($fblah d1 .. dn)
  like a constructor application.

* scApp: treat (op_sel d), a class method selection,
  like a case expression

* Float that dictionary application to top level, thus
    lvl = $fMonadStateT @.. @.. $fMonadIdentity
  so the call looks like
    ($wgo 100# @.. lvl)

  Why? This way dictionaries will appear as top level binders which we
  can trivially match in rules.  (CSE runs before SpecConstr, so we
  may hope to common-up duplicate top-level dictionaries.)
  For the floating part, see the "Arguments" case of Note
  [Floating to the top] in GHC.Core.Opt.SetLevels.

  We could be more clever, perhaps, and generate a RULE like
     $wgo _  @.. ($fMonadStateT @.. @.. $fMonadIdentity) = $s$wgo ...
  but that would mean making argToPat able to spot dfun applications as
  well as constructor applications.

Wrinkles:

* This should all work perfectly fine for newtype classes.  Mind you,
  currently newtype classes are inlined fairly agressively, but we
  may change that. And it would take extra code to exclude them, as
  well as being unnecessary.

* In isValue, we (mis-) use LambdaVal for this ($fblah d1 .. dn)
  because ConVal requires us to list the data constructor and
  fields, and that is (a) inconvenient and (b) unnecessary for
  class methods.

-----------------------------------------------------
                Stuff not yet handled
-----------------------------------------------------

Here are notes arising from Roman's work that I don't want to lose.

Example 1
~~~~~~~~~
    data T a = T !a

    foo :: Int -> T Int -> Int
    foo 0 t = 0
    foo x t | even x    = case t of { T n -> foo (x-n) t }
            | otherwise = foo (x-1) t

SpecConstr does no specialisation, because the second recursive call
looks like a boxed use of the argument.  A pity.

    $wfoo_sFw :: GHC.Prim.Int# -> T.T GHC.Base.Int -> GHC.Prim.Int#
    $wfoo_sFw =
      \ (ww_sFo [Just L] :: GHC.Prim.Int#) (w_sFq [Just L] :: T.T GHC.Base.Int) ->
         case ww_sFo of ds_Xw6 [Just L] {
           __DEFAULT ->
                case GHC.Prim.remInt# ds_Xw6 2 of wild1_aEF [Dead Just A] {
                  __DEFAULT -> $wfoo_sFw (GHC.Prim.-# ds_Xw6 1) w_sFq;
                  0 ->
                    case w_sFq of wild_Xy [Just L] { T.T n_ad5 [Just U(L)] ->
                    case n_ad5 of wild1_aET [Just A] { GHC.Base.I# y_aES [Just L] ->
                    $wfoo_sFw (GHC.Prim.-# ds_Xw6 y_aES) wild_Xy
                    } } };
           0 -> 0

Example 2
~~~~~~~~~
    data a :*: b = !a :*: !b
    data T a = T !a

    foo :: (Int :*: T Int) -> Int
    foo (0 :*: t) = 0
    foo (x :*: t) | even x    = case t of { T n -> foo ((x-n) :*: t) }
                  | otherwise = foo ((x-1) :*: t)

Very similar to the previous one, except that the parameters are now in
a strict tuple. Before SpecConstr, we have

    $wfoo_sG3 :: GHC.Prim.Int# -> T.T GHC.Base.Int -> GHC.Prim.Int#
    $wfoo_sG3 =
      \ (ww_sFU [Just L] :: GHC.Prim.Int#) (ww_sFW [Just L] :: T.T
    GHC.Base.Int) ->
        case ww_sFU of ds_Xws [Just L] {
          __DEFAULT ->
        case GHC.Prim.remInt# ds_Xws 2 of wild1_aEZ [Dead Just A] {
          __DEFAULT ->
            case ww_sFW of tpl_B2 [Just L] { T.T a_sFo [Just A] ->
            $wfoo_sG3 (GHC.Prim.-# ds_Xws 1) tpl_B2             -- $wfoo1
            };
          0 ->
            case ww_sFW of wild_XB [Just A] { T.T n_ad7 [Just S(L)] ->
            case n_ad7 of wild1_aFd [Just L] { GHC.Base.I# y_aFc [Just L] ->
            $wfoo_sG3 (GHC.Prim.-# ds_Xws y_aFc) wild_XB        -- $wfoo2
            } } };
          0 -> 0 }

We get two specialisations:
"SC:$wfoo1" [0] __forall {a_sFB :: GHC.Base.Int sc_sGC :: GHC.Prim.Int#}
                  Foo.$wfoo sc_sGC (Foo.T @ GHC.Base.Int a_sFB)
                  = Foo.$s$wfoo1 a_sFB sc_sGC ;
"SC:$wfoo2" [0] __forall {y_aFp :: GHC.Prim.Int# sc_sGC :: GHC.Prim.Int#}
                  Foo.$wfoo sc_sGC (Foo.T @ GHC.Base.Int (GHC.Base.I# y_aFp))
                  = Foo.$s$wfoo y_aFp sc_sGC ;

But perhaps the first one isn't good.  After all, we know that tpl_B2 is
a T (I# x) really, because T is strict and Int has one constructor.  (We can't
unbox the strict fields, because T is polymorphic!)

************************************************************************
*                                                                      *
\subsection{Top level wrapper stuff}
*                                                                      *
************************************************************************
-}

specConstrProgram :: ModGuts -> CoreM ModGuts
specConstrProgram guts
  = do { env0 <- initScEnv guts
       ; us   <- getUniqueSupplyM
       ; let (_usg, binds', warnings) = initUs_ us $
                              scTopBinds env0 (mg_binds guts)

       ; when (not (null warnings)) $ msg specConstr_warn_class (warn_msg warnings)

       ; return (guts { mg_binds = binds' }) }

  where
    specConstr_warn_class = MCDiagnostic SevWarning (ResolvedDiagnosticReason WarningWithoutFlag) Nothing
    warn_msg :: SpecFailWarnings -> SDoc
    warn_msg warnings = text "SpecConstr encountered one or more function(s) with a SPEC argument that resulted in too many arguments," $$
                        text "which resulted in no specialization being generated for these functions:" $$
                        nest 2 (vcat (map ppr warnings)) $$
                        (text "If this is expected you might want to increase -fmax-forced-spec-args to force specialization anyway.")
scTopBinds :: ScEnv -> [InBind] -> UniqSM (ScUsage, [OutBind], [SpecFailWarning])
scTopBinds _env []     = return (nullUsage, [], [])
scTopBinds env  (b:bs) = do { (usg, b', bs', warnings) <- scBind TopLevel env b $
                                                (\env -> scTopBinds env bs)
                            ; return (usg, b' ++ bs', warnings) }

{-
************************************************************************
*                                                                      *
\subsection{Environment: goes downwards}
*                                                                      *
************************************************************************

Note [ConVal work-free-ness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The sc_vals field keeps track of in-scope value bindings, and is used in
two ways:

(1) To do case-of-known-constructor in a case expression.  E.g. if sc_vals
    includes [x :-> ConVal Just e], then we can simplify
      case x of Just y -> ...
    with the case-of-known-constructor transformation. (Yes this is
    done by the Simplifier, but SpecConstr creates new opportunities when
    it makes a specialised RHS for a function.)

    For (1) it is crucial that the arguments are /work-free/; see (CV1)
    below.

(2) To figure out call pattresns. E.g. if sc_vals includes
    [x :-> ConVal Just e], and we have call (f x), then we might want
    to specialise `f (Just _)`

    For (2) it is /not/ important that the constructor arguments are work-free;
    indeed, it would be bad to insist on that. For example
       let x = Just <expensive>
       in ....(f x)...
    Here we want to specialise for `f (Just _)`, and we won't do so if we
    don't allow [x :-> ConVal Just e] into the environment.  Does this ever happen?
    Yes: see #24282.

    (Yes, the Simplifier will ANF that let-binding, but SpecConstr can
    make more: see (CV1) for an example.)

Wrinkle:

(CV1) Why is work-free-ness important for (1)?  In the example in (1) above, of `e` is
      expensive, we do /not/ want to simplify
         case x of { Just y -> ... }  ==>   let y = e in ...
      because the x-binding still exists and we've now duplicated `e`.

      This seldom happens because let-bound constructor applications are ANF-ised, but
      it can happen as a result of on-the-fly transformations in SpecConstr itself.
      Here is #7865:

              let { a'_shr =
                      case xs_af8 of _ {
                        [] -> acc_af6;
                        : ds_dgt [Dmd=<L,A>] ds_dgu [Dmd=<L,A>] ->
                          (expensive x_af7, x_af7
                      } } in
              let { ds_sht =
                      case a'_shr of _ { (p'_afd, q'_afe) ->
                      TSpecConstr_DoubleInline.recursive
                        (GHC.Types.: @ GHC.Types.Int x_af7 wild_X6) (q'_afe, p'_afd)
                      } } in

      When processed knowing that xs_af8 was bound to a cons, we simplify to
         a'_shr = (expensive x_af7, x_af7)
      and we do NOT want to inline that at the occurrence of a'_shr in ds_sht.
      (There are other occurrences of a'_shr.)  No no no.

      It would be possible to do some on-the-fly ANF-ising, so that a'_shr turned
      into a work-free value again, thus
         a1 = expensive x_af7
         a'_shr = (a1, x_af7)
      but that's more work, so until its shown to be important I'm going to
      leave it for now.

Note [Making SpecConstr keener]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this, in (perf/should_run/T9339)
   last (filter odd [1..1000])

After optimisation, including SpecConstr, we get:
   f :: Int# -> Int -> Int
   f x y = case remInt# x 2# of
             __DEFAULT -> case x of
                            __DEFAULT -> f (+# wild_Xp 1#) (I# x)
                            1000000# -> ...
             0# -> case x of
                     __DEFAULT -> f (+# wild_Xp 1#) y
                    1000000#   -> y

Not good!  We build an (I# x) box every time around the loop.
SpecConstr (as described in the paper) does not specialise f, despite
the call (f ... (I# x)) because 'y' is not scrutinised in the body.
But it is much better to specialise f for the case where the argument
is of form (I# x); then we build the box only when returning y, which
is on the cold path.

Another example:

   f x = ...(g x)....

Here 'x' is not scrutinised in f's body; but if we did specialise 'f'
then the call (g x) might allow 'g' to be specialised in turn.

So sc_keen controls whether or not we take account of whether argument is
scrutinised in the body.  True <=> ignore that, and specialise whenever
the function is applied to a data constructor.
-}

-- | Options for Specializing over constructors in Core.
data SpecConstrOpts = SpecConstrOpts
  { sc_max_args  :: !Int
  -- ^ The threshold at which a worker-wrapper transformation used as part of
  -- this pass will no longer happen, measured in the number of arguments.

  , sc_max_forced_args  :: !Int
  -- ^ The threshold at which a worker-wrapper transformation used as part of
  -- this pass will no longer happen even if a SPEC arg was used to force
  -- specialization. Measured in the number of arguments.
  -- See Note [Forcing specialisation]

  , sc_debug     :: !Bool
  -- ^ Whether to print debug information

  , sc_uf_opts   :: !UnfoldingOpts
  -- ^ Unfolding options

  , sc_module    :: !Module
  -- ^ The name of the module being processed

  , sc_size      :: !(Maybe Int)
  -- ^ Size threshold: Nothing => no limit

  , sc_count     :: !(Maybe Int)
  -- ^ Max # of specialisations for any one function. Nothing => no limit.
  -- See Note [Avoiding exponential blowup] and decreaseSpecCount

  , sc_recursive :: !Int
  -- ^ Max # of specialisations over recursive type. Stops
  -- ForceSpecConstr from diverging.

  , sc_keen      :: !Bool
  -- ^ Specialise on arguments that are known constructors, even if they are
  -- not scrutinised in the body. See Note [Making SpecConstr keener].
  }

data ScEnv = SCE { sc_opts      :: !SpecConstrOpts,
                   sc_force     :: Bool,        -- Force specialisation?
                                                -- See Note [Forcing specialisation]

                   sc_subst     :: Subst,       -- Current substitution
                                                -- Maps InIds to OutExprs

                   sc_how_bound :: HowBoundEnv,
                        -- Binds interesting non-top-level variables
                        -- Domain is OutVars (*after* applying the substitution)

                   sc_vals      :: ValueEnv,
                        -- Domain is OutIds (*after* applying the substitution)
                        -- Used even for top-level bindings (but not imported ones)

                   sc_annotations :: UniqFM Name SpecConstrAnnotation
             }

---------------------
type HowBoundEnv = VarEnv HowBound      -- Domain is OutVars

---------------------
type ValueEnv = IdEnv Value            -- Domain is OutIds

data Value = ConVal            -- Constructor application
                  Bool             -- True <=> all args are work-free
                                   --      See Note [ConVal work-free-ness]
                  AltCon           -- Never DEFAULT
                  [CoreArg]        -- Saturates the constructor
           | LambdaVal         -- Inlinable lambdas or PAPs

instance Outputable Value where
   ppr LambdaVal            = text "<Lambda>"
   ppr (ConVal wf con args)
     | null args = ppr con
     | otherwise = parens (ppr con <> braces pp_wf <+> interpp'SP args)
     where
       pp_wf | wf        = text "wf"
             | otherwise = text "not-wf"


---------------------
initScOpts :: DynFlags -> Module -> SpecConstrOpts
initScOpts dflags this_mod = SpecConstrOpts
        { sc_max_args    = maxWorkerArgs dflags,
          sc_max_forced_args = maxForcedSpecArgs dflags,
          sc_debug       = hasPprDebug dflags,
          sc_uf_opts     = unfoldingOpts dflags,
          sc_module      = this_mod,
          sc_size        = specConstrThreshold dflags,
          sc_count       = specConstrCount     dflags,
          sc_recursive   = specConstrRecursive dflags,
          sc_keen        = gopt Opt_SpecConstrKeen dflags
        }

initScEnv :: ModGuts -> CoreM ScEnv
initScEnv guts
  = do { dflags    <- getDynFlags
       ; (_, anns) <- getFirstAnnotations deserializeWithData guts
       ; this_mod  <- getModule
       ; return (SCE { sc_opts        = initScOpts dflags this_mod,
                       sc_force       = False,
                       sc_subst       = init_subst,
                       sc_how_bound   = emptyVarEnv,
                       sc_vals        = emptyVarEnv,
                       sc_annotations = anns }) }
  where
    init_subst = mkEmptySubst $ mkInScopeSetBndrs (mg_binds guts)
        -- Acccount for top-level bindings that are not in dependency order;
        -- see Note [Glomming] in GHC.Core.Opt.OccurAnal
        -- Easiest thing is to bring all the top level binders into scope at once,
        -- as if  at once, as if all the top-level decls were mutually recursive.

data HowBound = RecFun  -- These are the recursive functions for which
                        -- we seek interesting call patterns

              | RecArg  -- These are those functions' arguments, or their sub-components;
                        -- we gather occurrence information for these

instance Outputable HowBound where
  ppr RecFun = text "RecFun"
  ppr RecArg = text "RecArg"

scForce :: ScEnv -> Bool -> ScEnv
scForce env b = env { sc_force = b }

lookupHowBound :: ScEnv -> OutId -> Maybe HowBound
lookupHowBound env id = lookupVarEnv (sc_how_bound env) id

scSubstId :: ScEnv -> InId -> OutExpr
scSubstId env v = lookupIdSubst (sc_subst env) v


-- Solo is only defined in base starting from ghc-9.2
#if !(MIN_VERSION_base(4, 16, 0))
data Solo a = Solo a
#endif

-- The Solo constructor was renamed to MkSolo in ghc 9.5
#if __GLASGOW_HASKELL__ < 905
pattern MkSolo :: a -> Solo a
pattern MkSolo a = Solo a
#endif

-- The !subst ensures that we force the selection `(sc_subst env)`, which avoids
-- retaining all of `env` when we only need `subst`.  The `Solo` means that the
-- substitution itself is lazy, because that type is often discarded.
-- The callers of `scSubstTy` always force the result (to unpack the `Solo`)
-- so we get the desired effect: we leave a thunk, but retain only the subst,
-- not the whole env.
--
-- Fully forcing the result of `scSubstTy` regresses performance (#22102)
scSubstTy :: ScEnv -> InType -> Solo OutType
scSubstTy env ty =
  let !subst = sc_subst env
  in MkSolo (substTyUnchecked subst ty)

scSubstCo :: ScEnv -> Coercion -> Coercion
scSubstCo env co = substCo (sc_subst env) co

zapScSubst :: ScEnv -> ScEnv
zapScSubst env = env { sc_subst = zapSubst (sc_subst env) }

extendScInScope :: ScEnv -> [Var] -> ScEnv
        -- Bring the quantified variables into scope
extendScInScope env qvars
  = env { sc_subst = extendSubstInScopeList (sc_subst env) qvars }

        -- Extend the substitution
extendScSubst :: ScEnv -> Var -> OutExpr -> ScEnv
extendScSubst env var expr = env { sc_subst = extendSubst (sc_subst env) var expr }

extendScSubstList :: ScEnv -> [(Var,OutExpr)] -> ScEnv
extendScSubstList env prs = env { sc_subst = extendSubstList (sc_subst env) prs }

extendHowBound :: ScEnv -> [Var] -> HowBound -> ScEnv
extendHowBound env bndrs how_bound
  = env { sc_how_bound = extendVarEnvList (sc_how_bound env)
                            [(bndr,how_bound) | bndr <- bndrs] }

extendBndrsWith :: HowBound -> ScEnv -> [Var] -> (ScEnv, [Var])
extendBndrsWith how_bound env bndrs
  = (env { sc_subst = subst', sc_how_bound = hb_env' }, bndrs')
  where
    (subst', bndrs') = substBndrs (sc_subst env) bndrs
    hb_env' = sc_how_bound env `extendVarEnvList`
                    [(bndr,how_bound) | bndr <- bndrs']

extendBndrWith :: HowBound -> ScEnv -> Var -> (ScEnv, Var)
extendBndrWith how_bound env bndr
  = (env { sc_subst = subst', sc_how_bound = hb_env' }, bndr')
  where
    (subst', bndr') = substBndr (sc_subst env) bndr
    hb_env' = extendVarEnv (sc_how_bound env) bndr' how_bound

extendRecBndrs :: ScEnv -> [Var] -> (ScEnv, [Var])
extendRecBndrs env bndrs  = (env { sc_subst = subst' }, bndrs')
                      where
                        (subst', bndrs') = substRecBndrs (sc_subst env) bndrs

extendBndrs :: ScEnv -> [Var] -> (ScEnv, [Var])
extendBndrs env bndrs = mapAccumL extendBndr env bndrs

extendBndr :: ScEnv -> Var -> (ScEnv, Var)
extendBndr env bndr  = (env { sc_subst = subst' }, bndr')
                     where
                       (subst', bndr') = substBndr (sc_subst env) bndr

extendValEnv :: ScEnv -> Id -> Maybe Value -> ScEnv
extendValEnv env id mb_val
  = case mb_val of
      Nothing -> env
      Just cv -> env { sc_vals = extendVarEnv (sc_vals env) id cv }

extendCaseBndrs :: ScEnv -> OutExpr -> OutId -> AltCon -> [Var] -> (ScEnv, [Var])
-- When we encounter
--      case scrut of b
--          C x y -> ...
-- we want to bind b, to (C x y)
-- NB1: Extends only the sc_vals part of the envt
-- NB2: Kill the dead-ness info on the pattern binders x,y, since
--      they are potentially made alive by the [b -> C x y] binding
extendCaseBndrs env scrut case_bndr con alt_bndrs
   = (env2, alt_bndrs')
 where
   live_case_bndr = not (isDeadBinder case_bndr)
   env1 | DoBinderSwap v mco <- scrutOkForBinderSwap scrut
        , isReflMCo mco  = extendValEnv env v cval
        | otherwise      = env  -- See Note [Add scrutinee to ValueEnv too]
   env2 | live_case_bndr = extendValEnv env1 case_bndr cval
        | otherwise      = env1

   alt_bndrs' | case scrut of { Var {} -> True; _ -> live_case_bndr }
              = map zap alt_bndrs
              | otherwise
              = alt_bndrs

   cval = case con of
                DEFAULT    -> Nothing
                LitAlt {}  -> Just (ConVal True con [])
                DataAlt {} -> Just (ConVal True con vanilla_args)
                      where
                        vanilla_args = map Type (tyConAppArgs (idType case_bndr)) ++
                                       varsToCoreExprs alt_bndrs

   zap v | isTyVar v = v                -- See NB2 above
         | otherwise = zapIdOccInfo v


decreaseSpecCount :: ScEnv -> Int -> ScEnv
-- See Note [Avoiding exponential blowup]
decreaseSpecCount env _n_specs
  = env { sc_force = False   -- See Note [Forcing specialisation]
        , sc_opts = opts { sc_count = case sc_count opts of
                             Nothing -> Nothing
                             Just n  -> Just $! dec n
            }
        }
  where
    opts  = sc_opts env
    dec n = n `div` 2  -- See Note [Avoiding exponential blowup]

    -- Or:   n `div` (n_specs + 1)
    -- See the historical note part of Note [Avoiding exponential blowup]
    -- The "+1" takes account of the original function;

---------------------------------------------------
-- See Note [Forcing specialisation]
ignoreType    :: ScEnv -> Type   -> Bool
ignoreDataCon  :: ScEnv -> DataCon -> Bool
forceSpecBndr :: ScEnv -> Var    -> Bool

ignoreDataCon env dc = ignoreTyCon env (dataConTyCon dc)

ignoreType env ty
  = case tyConAppTyCon_maybe ty of
      Just tycon -> ignoreTyCon env tycon
      _          -> False

ignoreTyCon :: ScEnv -> TyCon -> Bool
ignoreTyCon env tycon
  = lookupUFM (sc_annotations env) (tyConName tycon) == Just NoSpecConstr

forceSpecBndr env var = forceSpecFunTy env . snd . splitForAllTyCoVars . varType $ var

forceSpecFunTy :: ScEnv -> Type -> Bool
forceSpecFunTy env = any (forceSpecArgTy env) . map scaledThing . fst . splitFunTys

forceSpecArgTy :: ScEnv -> Type -> Bool
forceSpecArgTy env ty
  | isFunTy ty
  = False

  | Just (tycon, tys) <- splitTyConApp_maybe ty
  = tycon `hasKey` specTyConKey
    || lookupUFM (sc_annotations env) (tyConName tycon) == Just ForceSpecConstr
    || any (forceSpecArgTy env) tys

forceSpecArgTy _ _ = False

{-
Note [Add scrutinee to ValueEnv too]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
   case x of y
     (a,b) -> case b of c
                I# v -> ...(f y)...
By the time we get to the call (f y), the ValueEnv
will have a binding for y, and for c
    y -> (a,b)
    c -> I# v
BUT that's not enough!  Looking at the call (f y) we
see that y is pair (a,b), but we also need to know what 'b' is.
So in extendCaseBndrs we must *also* add the binding
   b -> I# v
else we lose a useful specialisation for f.  This is necessary even
though the simplifier has systematically replaced uses of 'x' with 'y'
and 'b' with 'c' in the code.  The use of 'b' in the ValueEnv came
from outside the case.  See #4908 for the live example.

It's very like the binder-swap story, so we use scrutOkForBinderSwap
to identify suitable scrutinees -- but only if there is no cast
(isReflMCo) because that's all that the ValueEnv allows.

Note [Avoiding exponential blowup]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The sc_count field of the ScEnv says how many times we are prepared to
duplicate a single function.  But we must take care with recursive
specialisations.  Consider

        let $j1 = let $j2 = let $j3 = ...
                            in
                            ...$j3...
                  in
                  ...$j2...
        in
        ...$j1...

If we specialise $j1 then in each specialisation (as well as the original)
we can specialise $j2, and similarly $j3.  Even if we make just *one*
specialisation of each, because we also have the original we'll get 2^n
copies of $j3, which is not good.

So when recursively specialising we divide the sc_count (the maximum
number of specialisations, in the ScEnv) by two.  You might think that
gives us n*(n/2)*(n/4)... copies of the innnermost thing, which is
still exponential the depth.  But we use integer division, rounding
down, so if the starting sc_count is 3, we'll get 3 -> 1 -> 0, and
stop.  In fact, simply subtracting 1 would be good enough, for the same
reason.

Historical note: in the past we divided by (n_specs+1), where n_specs
is the number of specialisations at this level; but that gets us down
to zero jolly quickly, which I found led to some regressions.  (An
example is nofib/spectral/fibheaps, the getMin' function inside the
outer function $sfibToList, which has several interesting call
patterns.)

************************************************************************
*                                                                      *
\subsection{Usage information: flows upwards}
*                                                                      *
************************************************************************
-}

data ScUsage
   = SCU {
        scu_calls :: CallEnv,           -- Calls
                                        -- The functions are a subset of the
                                        --      RecFuns in the ScEnv

        scu_occs :: !(IdEnv ArgOcc)     -- Information on argument occurrences
     }                                  -- The domain is OutIds

type CallEnv = IdEnv [Call]  -- Domain is OutIds
data Call    = Call OutId [CoreArg] ValueEnv
        -- The arguments of the call, together with the
        -- env giving the constructor bindings at the call site
        -- We keep the function mainly for debug output
        --
        -- The call is not necessarily saturated; we just put
        -- in however many args are visible at the call site

instance Outputable ScUsage where
  ppr (SCU { scu_calls = calls, scu_occs = occs })
    = text "SCU" <+> braces (sep [ text "calls =" <+> ppr calls
                                 , text "occs =" <+> ppr occs ])

instance Outputable Call where
  ppr (Call fn args _) = ppr fn <+> fsep (map pprParendExpr args)

nullUsage :: ScUsage
nullUsage = SCU { scu_calls = emptyVarEnv, scu_occs = emptyVarEnv }

combineCalls :: CallEnv -> CallEnv -> CallEnv
combineCalls = plusVarEnv_C (++)

delCallsFor :: ScUsage -> [Var] -> ScUsage
delCallsFor env bndrs = env { scu_calls = scu_calls env `delVarEnvList` bndrs }

combineUsage :: ScUsage -> ScUsage -> ScUsage
combineUsage u1 u2 = SCU { scu_calls = combineCalls (scu_calls u1) (scu_calls u2),
                           scu_occs  = plusVarEnv_C combineOcc (scu_occs u1) (scu_occs u2) }

combineUsages :: [ScUsage] -> ScUsage
combineUsages [] = nullUsage
combineUsages us = foldr1 combineUsage us

lookupOccs :: ScUsage -> [OutVar] -> (ScUsage, [ArgOcc])
lookupOccs (SCU { scu_calls = sc_calls, scu_occs = sc_occs }) bndrs
  = (SCU {scu_calls = sc_calls, scu_occs = delVarEnvList sc_occs bndrs},
     [lookupVarEnv sc_occs b `orElse` NoOcc | b <- bndrs])

data ArgOcc = NoOcc     -- Doesn't occur at all; or a type argument
            | UnkOcc    -- Used in some unknown way

            | ScrutOcc  -- See Note [ScrutOcc]
                 (DataConEnv [ArgOcc])
                     -- [ArgOcc]: how the sub-components are used

deadArgOcc :: ArgOcc -> Bool
deadArgOcc (ScrutOcc {}) = False
deadArgOcc UnkOcc        = False
deadArgOcc NoOcc         = True

specialisableArgOcc :: ArgOcc -> Bool
-- | Does this occurrence represent one worth specializing for.
specialisableArgOcc UnkOcc        = False
specialisableArgOcc NoOcc         = False
specialisableArgOcc (ScrutOcc {}) = True


{- Note [ScrutOcc]
~~~~~~~~~~~~~~~~~~
An occurrence of ScrutOcc indicates that the thing, or a `cast` version of the thing,
is *only* taken apart or applied.

  Functions, literal: ScrutOcc emptyUFM
  Data constructors:  ScrutOcc subs,

where (subs :: UniqFM [ArgOcc]) gives usage of the *pattern-bound* components,
The domain of the UniqFM is the Unique of the data constructor

The [ArgOcc] is the occurrences of the *pattern-bound* components
of the data structure.  E.g.
        data T a = forall b. MkT a b (b->a)
A pattern binds b, x::a, y::b, z::b->a, but not 'a'!

-}

instance Outputable ArgOcc where
  ppr (ScrutOcc xs) = text "scrut-occ" <> ppr xs
  ppr UnkOcc        = text "unk-occ"
  ppr NoOcc         = text "no-occ"

evalScrutOcc :: ArgOcc
-- We use evalScrutOcc for
--   - mkVarUsage: applied functions
--   - scApp: dicts that are the argument of a classop
evalScrutOcc = ScrutOcc emptyUFM

-- Experimentally, this version of combineOcc makes ScrutOcc "win", so
-- that if the thing is scrutinised anywhere then we get to see that
-- in the overall result, even if it's also used in a boxed way
-- This might be too aggressive; see Note [Reboxing] Alternative 3
combineOcc :: ArgOcc -> ArgOcc -> ArgOcc
combineOcc NoOcc         occ           = occ
combineOcc occ           NoOcc         = occ
combineOcc (ScrutOcc xs) (ScrutOcc ys) = ScrutOcc (plusUFM_C combineOccs xs ys)
combineOcc UnkOcc        (ScrutOcc ys) = ScrutOcc ys
combineOcc (ScrutOcc xs) UnkOcc        = ScrutOcc xs
combineOcc UnkOcc        UnkOcc        = UnkOcc

combineOccs :: [ArgOcc] -> [ArgOcc] -> [ArgOcc]
combineOccs xs ys = zipWithEqual "combineOccs" combineOcc xs ys

setScrutOcc :: ScEnv -> ScUsage -> OutExpr -> ArgOcc -> ScUsage
-- _Overwrite_ the occurrence info for the scrutinee, if the scrutinee
-- is a variable, and an interesting variable
setScrutOcc env usg (Cast e _) occ      = setScrutOcc env usg e occ
setScrutOcc env usg (Tick _ e) occ      = setScrutOcc env usg e occ
setScrutOcc env usg (Var v)    occ
  | Just RecArg <- lookupHowBound env v = usg { scu_occs = extendVarEnv (scu_occs usg) v occ }
  | otherwise                           = usg
setScrutOcc _env usg _other _occ        -- Catch-all
  = usg

{-
************************************************************************
*                                                                      *
\subsection{The main recursive function}
*                                                                      *
************************************************************************

The main recursive function gathers up usage information, and
creates specialised versions of functions.
-}

scBind :: TopLevelFlag -> ScEnv -> InBind
       -> (ScEnv -> UniqSM (ScUsage, a, [SpecFailWarning]))   -- Specialise the scope of the binding
       -> UniqSM (ScUsage, [OutBind], a, [SpecFailWarning])
scBind top_lvl env (NonRec bndr rhs) do_body
  | isTyVar bndr         -- Type-lets may be created by doBeta
  = do { (final_usage, body', warnings) <- do_body (extendScSubst env bndr rhs)
       ; return (final_usage, [], body', warnings) }

  | not (isTopLevel top_lvl)  -- Nested non-recursive value binding
    -- See Note [Specialising local let bindings]
  = do  { let (body_env, bndr') = extendBndr env bndr
              -- Not necessary at top level; but here we are nested

        ; (rhs_info, rhs_ws)  <- scRecRhs env (bndr',rhs)

        ; let body_env2 = extendHowBound body_env [bndr'] RecFun
              rhs'      = ri_new_rhs rhs_info
              body_env3 = extendValEnv body_env2 bndr' (isValue (sc_vals env) rhs')

        ; (body_usg, body', warnings_body) <- do_body body_env3

          -- Now make specialised copies of the binding,
          -- based on calls in body_usg
        ; (spec_usg, specs, warnings_bnd) <- specNonRec env (scu_calls body_usg) rhs_info
          -- NB: For non-recursive bindings we inherit sc_force flag from
          -- the parent function (see Note [Forcing specialisation])

        -- Specialized + original binding
        ; let spec_bnds  = [NonRec b r | (b,r) <- ruleInfoBinds rhs_info specs]
              bind_usage = (body_usg `delCallsFor` [bndr'])
                           `combineUsage` spec_usg -- Note [spec_usg includes rhs_usg]

        ; return (bind_usage, spec_bnds, body', mconcat [warnings_bnd, warnings_body, rhs_ws])
        }

  | otherwise  -- Top-level, non-recursive value binding
    -- At top level we do not specialise non-recursive bindings; that
    -- is, we do not call specNonRec, passing the calls from the body.
    -- The original paper only specialised /recursive/ bindings, but
    -- we later started specialising nested non-recursive bindings:
    -- see Note [Specialising local let bindings]
    --
    -- I tried always specialising non-recursive top-level bindings too,
    -- but found some regressions (see !8135).  So I backed off.
  = do { (rhs_usage, rhs', ws_rhs)   <- scExpr env rhs

       -- At top level, we've already put all binders into scope; see initScEnv
       -- Hence no need to call `extendBndr`. But we still want to
       -- extend the `ValueEnv` to record the value of this binder.
       ; let body_env = extendValEnv env bndr (isValue (sc_vals env) rhs')
       ; (body_usage, body', body_warnings) <- do_body body_env

       ; return (rhs_usage `combineUsage` body_usage, [NonRec bndr rhs'], body', body_warnings ++ ws_rhs) }

scBind top_lvl env (Rec prs) do_body
  | isTopLevel top_lvl
  , Just threshold <- sc_size (sc_opts env)
  , not force_spec -- See Note [Forcing specialisation], point (FS1)
  , not (all (couldBeSmallEnoughToInline (sc_uf_opts (sc_opts env)) threshold) rhss)
  = -- Do no specialisation if the RHSs are too big
    -- ToDo: I'm honestly not sure of the rationale of this size-testing, nor
    --       why it only applies at top level. But that's the way it has been
    --       for a while. See #21456.
    do  { (body_usg, body', warnings_body) <- do_body rhs_env2
        ; (rhs_usgs, rhss', rhs_ws) <- mapAndUnzip3M (scExpr env) rhss
        ; let all_usg = (combineUsages rhs_usgs `combineUsage` body_usg)
                        `delCallsFor` bndrs'
              bind'   = Rec (bndrs' `zip` rhss')
        ; return (all_usg, [bind'], body', warnings_body ++ concat rhs_ws) }

  | otherwise
  = do  { (rhs_infos, rhs_wss) <- mapAndUnzipM (scRecRhs rhs_env2) (bndrs' `zip` rhss)
        ; let rhs_ws = mconcat rhs_wss
        ; (body_usg, body', warnings_body) <- do_body rhs_env2

        ; (spec_usg, specs, spec_ws) <- specRec (scForce rhs_env2 force_spec)
                                          (scu_calls body_usg) rhs_infos
                -- Do not unconditionally generate specialisations from rhs_usgs
                -- Instead use them only if we find an unspecialised call
                -- See Note [Seeding recursive groups]

        ; let all_usg = (spec_usg `combineUsage` body_usg)  -- Note [spec_usg includes rhs_usg]
                        `delCallsFor` bndrs'
              bind'   = Rec (concat (zipWithEqual "scExpr'" ruleInfoBinds rhs_infos specs))
                        -- zipWithEqual: length of returned [SpecInfo]
                        -- should be the same as incoming [RhsInfo]

        ; return (all_usg, [bind'], body', mconcat [warnings_body,rhs_ws,spec_ws]) }
  where
    (bndrs,rhss) = unzip prs
    force_spec   = any (forceSpecBndr env) bndrs    -- Note [Forcing specialisation]

    (rhs_env1,bndrs') | isTopLevel top_lvl = (env, bndrs)
                      | otherwise          = extendRecBndrs env bndrs
       -- At top level, we've already put all binders into scope; see initScEnv

    rhs_env2 = extendHowBound rhs_env1 bndrs' RecFun

{- Note [Specialising local let bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is not uncommon to find this

   let $j = \x. <blah> in ...$j True...$j True...

Here $j is an arbitrary let-bound function, but it often comes up for
join points.  We might like to specialise $j for its call patterns.
Notice the difference from a letrec, where we look for call patterns
in the *RHS* of the function.  Here we look for call patterns in the
*body* of the let.

At one point I predicated this on the RHS mentioning the outer
recursive function, but that's not essential and might even be
harmful.  I'm not sure.
-}

withWarnings :: SpecFailWarnings -> (ScUsage, CoreExpr, SpecFailWarnings) -> (ScUsage, CoreExpr, SpecFailWarnings)
withWarnings ws (use,expr,ws2) = (use,expr,ws ++ ws2)

------------------------
scExpr, scExpr' :: ScEnv -> CoreExpr -> UniqSM (ScUsage, CoreExpr, SpecFailWarnings)
        -- The unique supply is needed when we invent
        -- a new name for the specialised function and its args

scExpr env e = scExpr' env e

scExpr' env (Var v)      = case scSubstId env v of
                            Var v' -> return (mkVarUsage env v' [], Var v', [])
                            e'     -> scExpr (zapScSubst env) e'

scExpr' env (Type t)     =
  let !(MkSolo ty') = scSubstTy env t
  in return (nullUsage, Type ty', [])
scExpr' env (Coercion c) = return (nullUsage, Coercion (scSubstCo env c), [])
scExpr' _   e@(Lit {})   = return (nullUsage, e, [])
scExpr' env (Tick t e)   = do (usg, e', ws) <- scExpr env e
                              return (usg, Tick (scTickish env t) e', ws)
scExpr' env (Cast e co)  = do (usg, e', ws) <- scExpr env e
                              return (usg, mkCast e' (scSubstCo env co), ws)
                              -- Important to use mkCast here
                              -- See Note [SpecConstr call patterns]
scExpr' env e@(App _ _)  = scApp env (collectArgs e)
scExpr' env (Lam b e)    = do let (env', b') = extendBndr env b
                              (usg, e', ws) <- scExpr env' e
                              return (usg, Lam b' e', ws)

scExpr' env (Let bind body)
  = do { (final_usage, binds', body', ws) <- scBind NotTopLevel env bind $
                                         (\env -> scExpr env body)
       ; return (final_usage, mkLets binds' body', ws) }

scExpr' env (Case scrut b ty alts)
  = do  { (scrut_usg, scrut', ws) <- scExpr env scrut
        ; case isValue (sc_vals env) scrut' of
                Just (ConVal args_are_work_free con args)
                   | args_are_work_free -> sc_con_app con args scrut' ws
                     -- Don't duplicate work!!  #7865
                     -- See Note [ConVal work-free-ness] (1)
                _other -> sc_vanilla scrut_usg scrut' ws
        }
  where
    sc_con_app con args scrut' ws  -- Known constructor; simplify
     = do { let Alt _ bs rhs = findAlt con alts
                                  `orElse` Alt DEFAULT [] (mkImpossibleExpr ty "SpecConstr")
                alt_env'     = extendScSubstList env ((b,scrut') : bs `zip` trimConArgs con args)
          ; (use',expr',ws_new) <- scExpr alt_env' rhs
          ; return (use',expr',ws ++ ws_new) }

    sc_vanilla scrut_usg scrut' ws -- Normal case
     = do { let (alt_env,b') = extendBndrWith RecArg env b
                        -- Record RecArg for the components

          ; (alt_usgs, alt_occs, alts', ws_alts) <- mapAndUnzip4M (sc_alt alt_env scrut' b') alts

          ; let scrut_occ  = foldr combineOcc NoOcc alt_occs
                scrut_usg' = setScrutOcc env scrut_usg scrut' scrut_occ
                -- The combined usage of the scrutinee is given
                -- by scrut_occ, which is passed to setScrutOcc, which
                -- in turn treats a bare-variable scrutinee specially
          ; let !(MkSolo ty') = scSubstTy env ty

          ; return (foldr combineUsage scrut_usg' alt_usgs,
                    Case scrut' b' ty'  alts', ws ++ concat ws_alts) }

    single_alt = isSingleton alts

    sc_alt env scrut' b' (Alt con bs rhs)
     = do { let (env1, bs1) = extendBndrsWith RecArg env bs
                (env2, bs2) = extendCaseBndrs env1 scrut' b' con bs1
          ; (usg, rhs', ws) <- scExpr env2 rhs
          ; let (usg', b_occ:arg_occs) = lookupOccs usg (b':bs2)
                scrut_occ = case con of
                               DataAlt dc -- See Note [Do not specialise evals]
                                  | not (single_alt && all deadArgOcc arg_occs)
                                  -> ScrutOcc (unitUFM dc arg_occs)
                               _  -> UnkOcc
          ; return (usg', b_occ `combineOcc` scrut_occ, Alt con bs2 rhs', ws) }


-- | Substitute the free variables captured by a breakpoint.
-- Variables are dropped if they have a non-variable substitution, like in
-- 'GHC.Opt.Specialise.specTickish'.
scTickish :: ScEnv -> CoreTickish -> CoreTickish
scTickish SCE {sc_subst = subst} = substTickish subst

{- Note [Do not specialise evals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f x y = case x of I# _ ->
           if y>1 then f x (y-1) else x

Here `x` is scrutinised by a case, but only in an eval-like way; the
/component/ of the I# is unused.  We don't want to specialise this
function, even if we find a call (f (I# z)), because nothing is gained
  * No case branches are discarded
  * No allocation in removed
The specialised version would take an unboxed Int#, pass it along,
and rebox it at the end.

In fact this can cause significant regression.  In #21763 we had:
like
  f = ... case x of x' { I# n ->
          join j y = rhs
          in ...jump j x'...

Now if we specialise `j` for the argument `I# n`, we'll end up reboxing
it in `j`, without even removing an allocation from the call site.

Reboxing is always a worry.  But here we can ameliorate the problem as
follows.

* In scExpr (Case ...), for a /single-alternative/ case expression, in
  which the pattern binders are all unused, we build a UnkOcc for
  the scrutinee, not one that maps the data constructor; we don't treat
  this occurrence as a reason for specialisation.

* Conveniently, SpecConstr is doing its own occurrence analysis, so
  the "unused" bit is just looking for NoOcc

* Note that if we have
    f x = case x of { True -> e1; False -> e2 }
  then even though the pattern binders are unused (there are none), it is
  still worth specialising on x. Hence the /single-alternative/ guard.
-}

scApp :: ScEnv -> (InExpr, [InExpr]) -> UniqSM (ScUsage, CoreExpr, SpecFailWarnings)

scApp env (Var fn, args)        -- Function is a variable
  = assert (not (null args)) $
    do  { args_w_usgs <- mapM (scExpr env) args
        ; let (arg_usgs, args', arg_ws) = unzip3 args_w_usgs
              arg_usg = combineUsages arg_usgs
              arg_w = concat arg_ws
        ; case scSubstId env fn of
            fn'@(Lam {}) -> withWarnings arg_w <$> scExpr (zapScSubst env) (doBeta fn' args')
                        -- Do beta-reduction and try again

            Var fn' -> return (arg_usg' `combineUsage` mkVarUsage env fn' args',
                               mkApps (Var fn') args', arg_w )
               where
                 -- arg_usg': see Note [Specialising on dictionaries]
                 arg_usg' | Just cls <- isClassOpId_maybe fn'
                          , dict_arg : _ <- dropList (classTyVars cls) args'
                          = setScrutOcc env arg_usg dict_arg evalScrutOcc
                          | otherwise
                          = arg_usg

            other_fn' -> return (arg_usg, mkApps other_fn' args', arg_w) }
                -- NB: doing this ignores any usage info from the substituted
                --     function, but I don't think that matters.  If it does
                --     we can fix it.
  where
    doBeta :: OutExpr -> [OutExpr] -> OutExpr
    doBeta (Lam bndr body) (arg : args) = Let (NonRec bndr arg) (doBeta body args)
    doBeta fn              args         = mkApps fn args

-- The function is almost always a variable, but not always.
-- In particular, if this pass follows float-in,
-- which it may, we can get
--      (let f = ...f... in f) arg1 arg2
scApp env (other_fn, args)
  = do  { (fn_usg,   fn', fn_ws)   <- scExpr env other_fn
        ; (arg_usgs, args', arg_ws) <- mapAndUnzip3M (scExpr env) args
        ; return (combineUsages arg_usgs `combineUsage` fn_usg, mkApps fn' args', combineSpecWarning fn_ws (concat arg_ws)) }

----------------------
mkVarUsage :: ScEnv -> Id -> [CoreExpr] -> ScUsage
mkVarUsage env fn args
  = case lookupHowBound env fn of
        Just RecFun -> SCU { scu_calls = unitVarEnv fn [Call fn args (sc_vals env)]
                           , scu_occs  = emptyVarEnv }
        Just RecArg -> SCU { scu_calls = emptyVarEnv
                           , scu_occs  = unitVarEnv fn arg_occ }
        Nothing     -> nullUsage
  where
    arg_occ | null args = UnkOcc
            | otherwise = evalScrutOcc

----------------------
scRecRhs :: ScEnv -> (OutId, InExpr) -> UniqSM (RhsInfo, SpecFailWarnings)
scRecRhs env (bndr,rhs)
  = do  { let (arg_bndrs,body)       = collectBinders rhs
              (body_env, arg_bndrs') = extendBndrsWith RecArg env arg_bndrs
        ; (body_usg, body', body_ws)         <- scExpr body_env body
        ; let (rhs_usg, arg_occs)    = lookupOccs body_usg arg_bndrs'
        ; return (RI { ri_rhs_usg = rhs_usg
                     , ri_fn = bndr, ri_new_rhs = mkLams arg_bndrs' body'
                     , ri_lam_bndrs = arg_bndrs, ri_lam_body = body
                     , ri_arg_occs = arg_occs }, body_ws) }
                -- The arg_occs says how the visible,
                -- lambda-bound binders of the RHS are used
                -- (including the TyVar binders)
                -- Two pats are the same if they match both ways

----------------------
ruleInfoBinds :: RhsInfo -> SpecInfo -> [(Id,CoreExpr)]
ruleInfoBinds (RI { ri_fn = fn, ri_new_rhs = new_rhs })
              (SI { si_specs = specs })
  = [(id,rhs) | OS { os_id = id, os_rhs = rhs } <- specs] ++
              -- First the specialised bindings

    [(fn `addIdSpecialisations` rules, new_rhs)]
              -- And now the original binding
  where
    rules = [r | OS { os_rule = r } <- specs]

{-
************************************************************************
*                                                                      *
                The specialiser itself
*                                                                      *
************************************************************************
-}

data RhsInfo
  = RI { ri_fn :: OutId                 -- The binder
       , ri_new_rhs :: OutExpr          -- The specialised RHS (in current envt)
       , ri_rhs_usg :: ScUsage          -- Usage info from specialising RHS

       , ri_lam_bndrs :: [InVar]       -- The *original* RHS (\xs.body)
       , ri_lam_body  :: InExpr        --   Note [Specialise original body]
       , ri_arg_occs  :: [ArgOcc]      -- Info on how the xs occur in body
    }

data SpecInfo       -- Info about specialisations for a particular Id
  = SI { si_specs :: [OneSpec]          -- The specialisations we have
                                        -- generated for this function

       , si_n_specs :: Int              -- Length of si_specs; used for numbering them

       , si_mb_unspec :: Maybe ScUsage  -- Just cs  => we have not yet used calls in the
       }                                --             from calls in the *original* RHS as
                                        --             seeds for new specialisations;
                                        --             if you decide to do so, here is the
                                        --             RHS usage (which has not yet been
                                        --             unleashed)
                                        -- Nothing => we have
                                        -- See Note [Seeding recursive groups]
                                        -- See Note [spec_usg includes rhs_usg]

        -- One specialisation: Rule plus definition
data OneSpec =
  OS { os_pat  :: CallPat    -- Call pattern that generated this specialisation
     , os_rule :: CoreRule   -- Rule connecting original id with the specialisation
     , os_id   :: OutId      -- Spec id
     , os_rhs  :: OutExpr }  -- Spec rhs

initSpecInfo :: RhsInfo -> SpecInfo
initSpecInfo (RI { ri_rhs_usg = rhs_usg })
  = SI { si_specs = [], si_n_specs = 0, si_mb_unspec = Just rhs_usg }
    -- si_mb_unspec: add in rhs_usg if there are any boring calls,
    --               or if the bndr is exported

----------------------
specNonRec :: ScEnv
           -> CallEnv         -- Calls in body
           -> RhsInfo         -- Structure info usage info for un-specialised RHS
           -> UniqSM (ScUsage, SpecInfo, [SpecFailWarning])       -- Usage from RHSs (specialised and not)
                                               --     plus details of specialisations

specNonRec env body_calls rhs_info
  = specialise env body_calls rhs_info (initSpecInfo rhs_info)

----------------------
specRec :: ScEnv
        -> CallEnv                         -- Calls in body
        -> [RhsInfo]                       -- Structure info and usage info for un-specialised RHSs
        -> UniqSM (ScUsage, [SpecInfo], SpecFailWarnings)
                                           -- Usage from all RHSs (specialised and not)
                                           --     plus details of specialisations

specRec env body_calls rhs_infos
  = go 1 body_calls nullUsage (map initSpecInfo rhs_infos) []
    -- body_calls: see Note [Seeding recursive groups]
    -- NB: 'go' always calls 'specialise' once, which in turn unleashes
    --     si_mb_unspec if there are any boring calls in body_calls,
    --     or if any of the Id(s) are exported
  where
    opts = sc_opts env

    -- Loop, specialising, until you get no new specialisations
    go, go_again :: Int   -- Which iteration of the "until no new specialisations"
                          -- loop we are on; first iteration is 1
                 -> CallEnv   -- Seed calls
                              -- Two accumulating parameters:
                 -> ScUsage      -- Usage from earlier specialisations
                 -> [SpecInfo]   -- Details of specialisations so far
                 -> SpecFailWarnings -- Warnings so far
                 -> UniqSM (ScUsage, [SpecInfo], SpecFailWarnings)
    go n_iter seed_calls usg_so_far spec_infos ws_so_far
      = -- pprTrace "specRec3" (vcat [ text "bndrs" <+> ppr (map ri_fn rhs_infos)
        --                           , text "iteration" <+> int n_iter
        --                          , text "spec_infos" <+> ppr (map (map os_pat . si_specs) spec_infos)
        --                    ]) $
        do  { specs_w_usg <- zipWithM (specialise env seed_calls) rhs_infos spec_infos

            ; let (extra_usg_s, all_spec_infos, extra_ws ) = unzip3 specs_w_usg
                  extra_usg = combineUsages extra_usg_s
                  all_usg   = usg_so_far `combineUsage` extra_usg
                  new_calls = scu_calls extra_usg
            ; go_again n_iter new_calls all_usg all_spec_infos (ws_so_far ++ concat extra_ws) }

    -- go_again deals with termination
    go_again n_iter seed_calls usg_so_far spec_infos ws_so_far
      | isEmptyVarEnv seed_calls
      = return (usg_so_far, spec_infos, ws_so_far)

      -- Limit recursive specialisation
      -- See Note [Limit recursive specialisation]
      | n_iter > sc_recursive opts  -- Too many iterations of the 'go' loop
      , sc_force env || isNothing (sc_count opts)
           -- If both of these are false, the sc_count
           -- threshold will prevent non-termination
           -- See Note [Forcing specialisation], point (FS4) and (FS2)
      , any ((> the_limit) . si_n_specs) spec_infos
      = -- Give up on specialisation, but don't forget to include the rhs_usg
        -- for the unspecialised function, since it may now be called
        -- pprTrace "specRec2" (ppr (map (map os_pat . si_specs) spec_infos)) $
        let rhs_usgs = combineUsages (mapMaybe si_mb_unspec spec_infos)
        in return (usg_so_far `combineUsage` rhs_usgs, spec_infos, ws_so_far)

      | otherwise
      = go (n_iter + 1) seed_calls usg_so_far spec_infos ws_so_far

    -- See Note [Limit recursive specialisation]
    the_limit = case sc_count opts of
                  Nothing  -> 10    -- Ugh!
                  Just max -> max

----------------------
specialise
   :: ScEnv
   -> CallEnv                     -- Info on newly-discovered calls to this function
   -> RhsInfo
   -> SpecInfo                    -- Original RHS plus patterns dealt with
   -> UniqSM (ScUsage, SpecInfo, [SpecFailWarning])  -- New specialised versions and their usage

-- See Note [spec_usg includes rhs_usg]

-- Note: this only generates *specialised* bindings
-- The original binding is added by ruleInfoBinds
--
-- Note: the rhs here is the optimised version of the original rhs
-- So when we make a specialised copy of the RHS, we're starting
-- from an RHS whose nested functions have been optimised already.

specialise env bind_calls (RI { ri_fn = fn, ri_lam_bndrs = arg_bndrs
                              , ri_lam_body = body, ri_arg_occs = arg_occs })
               spec_info@(SI { si_specs = specs, si_n_specs = spec_count
                             , si_mb_unspec = mb_unspec })
  | isDeadEndId fn  -- Note [Do not specialise diverging functions]
                    -- /and/ do not generate specialisation seeds from its RHS
  = -- pprTrace "specialise bot" (ppr fn) $
    return (nullUsage, spec_info, [])

  | not (isNeverActive (idInlineActivation fn))
      -- See Note [Transfer activation]
      -- Don't specialise OPAQUE things, see Note [OPAQUE pragma].
      -- Since OPAQUE things are always never-active (see
      -- GHC.Parser.PostProcess.mkOpaquePragma) this guard never fires for
      -- OPAQUE things.
  , not (null arg_bndrs)                         -- Only specialise functions
  , Just all_calls <- lookupVarEnv bind_calls fn -- Some calls to it
  = -- pprTrace "specialise entry {" (ppr fn <+> ppr all_calls) $
    do  { (boring_call, pats_discarded, new_pats, warnings)
             <- callsToNewPats env fn spec_info arg_occs all_calls

        ; let n_pats = length new_pats
--        ; when (not (null new_pats) || isJust mb_unspec) $
--          pprTraceM "specialise" (vcat [ ppr fn <+> text "with" <+> int n_pats <+> text "good patterns"
--                                       , text "boring_call:" <+> ppr boring_call
--                                       , text "pats_discarded:" <+> ppr pats_discarded
--                                       , text "old spec_count" <+> ppr spec_count
--                                       , text "spec count limit" <+> ppr (sc_count (sc_opts env))
--                                       , text "mb_unspec" <+> ppr (isJust mb_unspec)
--                                       , text "arg_occs" <+> ppr arg_occs
--                                       , text "new_pats" <+> ppr new_pats])

        ; let spec_env = decreaseSpecCount env n_pats
        ; (spec_usgs, new_specs, new_wss) <- mapAndUnzip3M (spec_one spec_env fn arg_bndrs body)
                                                 (new_pats `zip` [spec_count..])
                -- See Note [Specialise original body]

        ; let spec_usg = combineUsages spec_usgs

              unspec_rhs_needed = pats_discarded || boring_call || isExportedId fn

              -- If there were any boring calls among the seeds (= all_calls), then those
              -- calls will call the un-specialised function.  So we should use the seeds
              -- from the _unspecialised_ function's RHS, which are in mb_unspec, by returning
              -- then in new_usg.
              (new_usg, mb_unspec') = case mb_unspec of
                  Just rhs_usg | unspec_rhs_needed
                               -> (spec_usg `combineUsage` rhs_usg, Nothing)
                  _            -> (spec_usg,                      mb_unspec)

--        ; pprTraceM "specialise return }" $
--          vcat [ ppr fn
--               , text "unspec_rhs_needed:" <+> ppr unspec_rhs_needed
--               , text "new calls:" <+> ppr (scu_calls new_usg)]

        ; return (new_usg, SI { si_specs     = new_specs ++ specs
                              , si_n_specs   = spec_count + n_pats
                              , si_mb_unspec = mb_unspec' }
                 ,warnings ++ concat new_wss) }

  | otherwise  -- No calls, inactive, or not a function
               -- Behave as if there was a single, boring call
  = -- pprTrace "specialise inactive" (ppr fn $$ ppr mb_unspec) $
    case mb_unspec of    -- Behave as if there was a single, boring call
      Just rhs_usg -> return (rhs_usg, spec_info { si_mb_unspec = Nothing }, [])
                         -- See Note [spec_usg includes rhs_usg]
      Nothing      -> return (nullUsage, spec_info, [])


---------------------
spec_one :: ScEnv
         -> OutId       -- Function
         -> [InVar]     -- Lambda-binders of RHS; should match patterns
         -> InExpr      -- Body of the original function
         -> (CallPat, Int)
         -> UniqSM (ScUsage, OneSpec, SpecFailWarnings)   -- Rule and binding, warnings if any

-- spec_one creates a specialised copy of the function, together
-- with a rule for using it.  I'm very proud of how short this
-- function is, considering what it does :-).

{-
  Example

     In-scope: a, x::a
     f = /\b \y::[(a,b)] -> ....f (b,c) ((:) (a,(b,c)) (x,v) (h w))...
          [c::*, v::(b,c) are presumably bound by the (...) part]
  ==>
     f_spec = /\ b c \ v::(b,c) hw::[(a,(b,c))] ->
                  (...entire body of f...) [b -> (b,c),
                                            y -> ((:) (a,(b,c)) (x,v) hw)]

     RULE:  forall b::* c::*,           -- Note, *not* forall a, x
                   v::(b,c),
                   hw::[(a,(b,c))] .

            f (b,c) ((:) (a,(b,c)) (x,v) hw) = f_spec b c v hw
-}

spec_one env fn arg_bndrs body (call_pat, rule_number)
  | CP { cp_qvars = qvars, cp_args = pats, cp_strict_args = cbv_args } <- call_pat
  = do  { -- pprTraceM "spec_one {" (ppr fn <+> ppr pats)

        ; spec_uniq <- getUniqueM
        ; let env1 = extendScSubstList (extendScInScope env qvars)
                                       (arg_bndrs `zip` pats)
              (body_env, extra_bndrs) = extendBndrs env1 (dropList pats arg_bndrs)
              -- Remember, there may be fewer pats than arg_bndrs
              -- See Note [SpecConstr call patterns]
              -- extra_bndrs will then be arguments in the specialized version
              -- which are *not* applied to arguments immediately at the call sites.
              -- e.g. let f x y = ... in map (f True) xs
              -- will result in y becoming an extra_bndr

              fn_name  = idName fn
              fn_loc   = nameSrcSpan fn_name
              fn_occ   = nameOccName fn_name
              spec_occ = mkSpecOcc fn_occ
              -- We use fn_occ rather than fn in the rule_name string
              -- as we don't want the uniq to end up in the rule, and
              -- hence in the ABI, as that can cause spurious ABI
              -- changes (#4012).
              rule_name  = mkFastString ("SC:" ++ occNameString fn_occ ++ show rule_number)
              spec_name  = mkInternalName spec_uniq spec_occ fn_loc

        -- Specialise the body
        -- ; pprTraceM "body_subst_for" $ ppr (spec_occ) $$ ppr (sc_subst body_env)
        ; (spec_usg, spec_body, body_warnings) <- scExpr body_env body

                -- And build the results
        ; (qvars', pats') <- generaliseDictPats qvars pats
        ; let spec_body_ty = exprType spec_body
              (spec_lam_args, spec_call_args, spec_sig)
                  = calcSpecInfo fn arg_bndrs call_pat extra_bndrs

              spec_arity = count isId spec_lam_args
              spec_join_arity | isJoinId fn = JoinPoint (length spec_call_args)
                              | otherwise   = NotJoinPoint
              spec_id    = asWorkerLikeId $
                           mkLocalId spec_name ManyTy
                                     (mkLamTypes spec_lam_args spec_body_ty)
                             -- See Note [Transfer strictness]
                             `setIdDmdSig`    spec_sig
                             `setIdCprSig`    topCprSig
                             `setIdArity`     spec_arity
                             `asJoinId_maybe` spec_join_arity

        -- Conditionally use result of new worker-wrapper transform
        -- mkSeqs: see Note [SpecConstr and strict fields]
              spec_rhs = mkLams spec_lam_args (mkSeqs cbv_args spec_body_ty spec_body)
              rule_rhs = mkVarApps (Var spec_id) spec_call_args
              inline_act = idInlineActivation fn
              this_mod   = sc_module $ sc_opts env
              rule       = mkRule this_mod True {- Auto -} True {- Local -}
                                  rule_name inline_act
                                  fn_name qvars' pats' rule_rhs
                           -- See Note [Transfer activation]

--        ; pprTraceM "spec_one end }" $
--          vcat [ text "function:" <+> ppr fn <+> braces (ppr (idUnique fn))
--               , text "pats:" <+> ppr pats
--               , text "call_pat:" <+> ppr call_pat
--               , text "-->" <+> ppr spec_name
--               , text "bndrs" <+> ppr arg_bndrs
--               , text "extra_bndrs" <+> ppr extra_bndrs
--               , text "cbv_args" <+> ppr cbv_args
--               , text "spec_lam_args" <+> ppr spec_lam_args
--               , text "spec_call_args" <+> ppr spec_call_args
--               , text "rule_rhs" <+> ppr rule_rhs
--               , text "adds_void_worker_arg" <+> ppr add_void_arg
----               , text "body" <+> ppr body
----               , text "spec_rhs" <+> ppr spec_rhs
----               , text "how_bound" <+> ppr (sc_how_bound env) ]
--               ]
        ; return (spec_usg, OS { os_pat = call_pat, os_rule = rule
                               , os_id = spec_id
                               , os_rhs = spec_rhs }, body_warnings) }

generaliseDictPats :: [Var] -> [CoreExpr]  -- Quantified vars and pats
                   -> UniqSM ([Var], [CoreExpr]) -- New quantified vars and pats
-- See Note [generaliseDictPats]
generaliseDictPats qvars pats
  = do { (extra_qvars, pats') <- mapAccumLM go [] pats
       ; case extra_qvars of
             [] -> return (qvars,                pats)
             _  -> return (qvars ++ extra_qvars, pats') }
  where
    qvar_set = mkVarSet qvars
    go :: [Id] -> CoreExpr -> UniqSM ([Id], CoreExpr)
    go extra_qvs pat
       | not (isTyCoArg pat)
       , let pat_ty = exprType pat
       , typeDeterminesValue pat_ty
       , exprFreeVars pat `disjointVarSet` qvar_set
       = do { id <- mkSysLocalOrCoVarM (fsLit "dict") ManyTy pat_ty
            ; return (id:extra_qvs, Var id) }
       | otherwise
       = return (extra_qvs, pat)

mkSeqs :: [Var] -> Type -> CoreExpr -> CoreExpr
-- See Note [SpecConstr and strict fields]
mkSeqs seqees res_ty rhs =
  foldr addEval rhs seqees
    where
      addEval :: Var -> CoreExpr -> CoreExpr
      addEval arg_id rhs
        -- Argument representing strict field and it's worth passing via cbv
        | shouldStrictifyIdForCbv arg_id
        = Case (Var arg_id)
               (localiseId arg_id)  -- See (SCF1) in Note [SpecConstr and strict fields]
               res_ty
               ([Alt DEFAULT [] rhs])

        | otherwise
        = rhs


{- Note [SpecConstr void argument insertion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a function
    f :: Bool -> forall t. blah
    f start @t = e
We want to specialize for a partially applied call `f True`.
See also Note [SpecConstr call patterns], second Wrinkle.
Naively we would expect to get
    $sf :: forall t. blah
    $sf @t = $se
    RULE: f True = $sf
The specialized function only takes a single type argument so we add a
void argument to prevent it from turning into a thunk. See Note
[Protecting the last value argument] for details why. Normally we
would add the void argument after the type argument giving us:

    $sf :: forall t. Void# -> bla
    $sf @t void = $se
    RULE: f True = $sf void# (wrong)

But if you look closely this wouldn't typecheck!  If we substitute `f
True` with `$sf void#` we expect the type argument to be applied first
but we apply void# first.  The easiest fix seems to be just to add the
void argument to the front of the arguments.  Now we get:

    $sf :: Void# -> forall t. bla
    $sf void @t = $se
    RULE: f True = $sf void#

And now we can substitute `f True` with `$sf void#` with everything working out nicely!

More precisely, in `calcSpecInfo`
(i)  we need the void arg to /precede/ the `extra_bndrs`, but
(ii) it must still /follow/ `qvar_bndrs`.

Example to illustrate (ii):
  f :: forall r (a :: TYPE r). Bool -> a
  f = /\r. /\(a::TYPE r). \b. body

  {- Specialise for f _ _ True -}

  $sf :: forall r (a :: TYPE r). Void# -> a
  $sf = /\r. /\(a::TYPE r). \v. body[True/b]
  RULE: forall r (a :: TYPE r). f @r @a True = $sf @r @a void#

The void argument must follow the foralls, lest the forall be
ill-kinded.  See Note [Worker/wrapper needs to add void arg last] in
GHC.Core.Opt.WorkWrap.Utils.

Note [generaliseDictPats]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider these two rules (#21831, item 2):
  RULE "SPEC:foo"  forall d1 d2. foo @Int @Integer d1 d2 = $sfoo1
  RULE "SC:foo"    forall a. foo @Int @a $fNumInteger = $sfoo2 @a
The former comes from the type class specialiser, the latter from SpecConstr.
Note that $fNumInteger is a top-level binding for Num Integer.

The trouble is that neither is more general than the other.  In a call
   (foo @Int @Integer $fNumInteger d)
it isn't clear which rule to fire.

The trouble is that the SpecConstr rule fires on a /specific/ dict, $fNumInteger,
but actually /could/ fire regardless.  That is, it could be
  RULE "SC:foo"    forall a d. foo @Int @a d = $sfoo2 @a

Now, it is clear that SPEC:foo is more specific.  But GHC can't tell
that, because SpecConstr doesn't know that dictionary arguments are
singleton types!  So generaliseDictPats teaches it this fact.  It
spots such patterns (using typeDeterminesValue), and quantifies over
the dictionary.  Now we get

  RULE "SC:foo"    forall a d. foo @Int @a d = $sfoo2 @a

And /now/ "SPEC:foo" is clearly more specific: we can instantiate the new
"SC:foo" to match the (prefix of) "SPEC:foo".
-}

calcSpecInfo :: Id           -- The original function
             -> [InVar]      -- Lambda binders of original RHS
             -> CallPat      -- Call pattern
             -> [Var]        -- Extra bndrs
             -> ( [Var]           -- Demand-decorated lambda binders
                                  --   for RHS of specialised function
                , [Var]           -- Args for call site
                , DmdSig )        -- Strictness of specialised thing
-- Calculate bits of IdInfo for the specialised function
-- See Note [Transfer strictness]
-- See Note [Strictness information in worker binders]
calcSpecInfo fn arg_bndrs (CP { cp_qvars = qvars, cp_args = pats }) extra_bndrs
  = ( spec_lam_bndrs_w_dmds
    , spec_call_args
    , zapDmdEnvSig (DmdSig (dt{dt_args = spec_fn_dmds})) )
  where
    DmdSig dt@DmdType{dt_args=fn_dmds} = idDmdSig fn
    spec_fn_dmds = [idDemandInfo b | b <- spec_lam_bndrs_w_dmds, isId b]

    val_pats   = filterOut isTypeArg pats
                 -- Value args at call sites, used to determine how many demands to drop
                 -- from the original functions demand and for setting up arg_dmd_env.
    arg_dmd_env = go emptyVarEnv fn_dmds val_pats
    qvar_dmds  = [ lookupVarEnv arg_dmd_env qv `orElse` topDmd | qv <- qvars, isId qv ]
    extra_dmds = dropList val_pats fn_dmds

    -- Annotate the variables with the strictness information from
    -- the function (see Note [Strictness information in worker binders])
    qvars_w_dmds          = set_dmds qvars       qvar_dmds
    extras_w_dmds         = set_dmds extra_bndrs extra_dmds
    spec_lam_bndrs_w_dmds = final_qvars_w_dmds ++ extras_w_dmds

    (final_qvars_w_dmds, spec_call_args)
       | needsVoidWorkerArg fn arg_bndrs (qvars ++ extra_bndrs)
         -- Usual w/w hack to avoid generating
         -- a spec_rhs of unlifted or ill-kinded type and no args.
         -- See Note [SpecConstr void argument insertion]
       = ( qvars_w_dmds ++ [voidArgId], qvars ++ [voidPrimId] )
       | otherwise
       = ( qvars_w_dmds,                qvars )

    set_dmds :: [Var] -> [Demand] -> [Var]
    set_dmds [] _   = []
    set_dmds vs  [] = vs  -- Run out of demands
    set_dmds (v:vs) ds@(d:ds') | isTyVar v = v                   : set_dmds vs ds
                               | otherwise = setIdDemandInfo v d : set_dmds vs ds'

    go :: VarEnv Demand -> [Demand] -> [CoreExpr] -> VarEnv Demand
    -- We've filtered out all the type patterns already
    go env (d:ds) (pat : pats)     = go (go_one env d pat) ds pats
    go env _      _                = env

    go_one :: VarEnv Demand -> Demand -> CoreExpr -> VarEnv Demand
    go_one env d          (Var v) = extendVarEnv_C plusDmd env v d
    go_one env (_n :* cd) e -- NB: _n does not have to be strict
      | (Var _, args) <- collectArgs e
      , Just (_b, ds) <- viewProd (length args) cd -- TODO: We may want to look at boxity _b, though...
      = go env ds args
    go_one env _  _ = env

{-
Note [spec_usg includes rhs_usg]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In calls to 'specialise', the returned ScUsage must include the rhs_usg in
the passed-in SpecInfo in si_mb_unspec, unless there are no calls at all to
the function.

The caller can, indeed must, assume this.  They should not combine in rhs_usg
themselves, or they'll get rhs_usg twice -- and that can lead to an exponential
blowup of duplicates in the CallEnv.  This is what gave rise to the massive
performance loss in #8852.

Note [Specialise original body]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The RhsInfo for a binding keeps the *original* body of the binding.  We
must specialise that, *not* the result of applying specExpr to the RHS
(which is also kept in RhsInfo). Otherwise we end up specialising a
specialised RHS, and that can lead directly to exponential behaviour.

Note [Transfer activation]
~~~~~~~~~~~~~~~~~~~~~~~~~~
  This note is for SpecConstr, but exactly the same thing
  happens in the overloading specialiser; see
  Note [Auto-specialisation and RULES] in GHC.Core.Opt.Specialise.

In which phase should the specialise-constructor rules be active?
Originally I made them always-active, but Manuel found that this
defeated some clever user-written rules.  Then I made them active only
in FinalPhase; after all, currently, the specConstr transformation is
only run after the simplifier has reached FinalPhase, but that meant
that specialisations didn't fire inside wrappers; see test
simplCore/should_compile/spec-inline.

So now I just use the inline-activation of the parent Id, as the
activation for the specialisation RULE, just like the main specialiser;

This in turn means there is no point in specialising NOINLINE things,
so we test for that.

Note [Transfer strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We must transfer strictness information from the original function to
the specialised one.  Suppose, for example

  f has strictness     SSx
        and a RULE     f (a:as) b = f_spec a as b

Now we want f_spec to have strictness  LLSx, otherwise we'll use call-by-need
when calling f_spec instead of call-by-value.  And that can result in
unbounded worsening in space (cf the classic foldl vs foldl')

See #3437 for a good example.

The function calcSpecStrictness performs the calculation.

Note [Strictness information in worker binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
After having calculated the strictness annotation for the worker (see Note
[Transfer strictness] above), we also want to have this information attached to
the worker’s arguments, for the benefit of later passes. The function
handOutStrictnessInformation decomposes the strictness annotation calculated by
calcSpecStrictness and attaches them to the variables.


************************************************************************
*                                                                      *
\subsection{Argument analysis}
*                                                                      *
************************************************************************

This code deals with analysing call-site arguments to see whether
they are constructor applications.

Note [Free type variables of the qvar types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a call (f @a x True), that we want to specialise, what variables should
we quantify over.  Clearly over 'a' and 'x', but what about any type variables
free in x's type?  In fact we don't need to worry about them because (f @a)
can only be a well-typed application if its type is compatible with x, so any
variables free in x's type must be free in (f @a), and hence either be gathered
via 'a' itself, or be in scope at f's defn.  Hence we just take
  (exprsFreeVars pats).

BUT phantom type synonyms can mess this reasoning up,
  eg   x::T b   with  type T b = Int
So we apply expandTypeSynonyms to the bound Ids.
See # 5458.  Yuk.

Note [SpecConstr call patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A "call patterns" that we collect is going to become the LHS of a RULE.

Wrinkles:

* The list of argument patterns, cp_args, is no longer than the
  visible lambdas of the binding, ri_arg_occs.  This is done via
  the zipWithM in callToPat.

* The list of argument patterns can certainly be shorter than the
  lambdas in the function definition (under-saturated).  For example
      f x y = case x of { True -> e1; False -> e2 }
      ....map (f True) e...
  We want to specialise `f` for `f True`.

* In fact we deliberately shrink the list of argument patterns,
  cp_args, by trimming off all the boring ones at the end (see
  `dropWhileEnd is_boring` in callToPat).  Since the RULE only
  applies when it is saturated, this shrinking makes the RULE more
  applicable.  But it does mean that the argument patterns do not
  necessarily saturate the lambdas of the function.

* It's important that the pattern arguments do not look like
     e |> Refl
  or
    e |> g1 |> g2
  because both of these will be optimised by Simplify.simplRule. In the
  former case such optimisation benign, because the rule will match more
  terms; but in the latter we may lose a binding of 'g1' or 'g2', and
  end up with a rule LHS that doesn't bind the template variables
  (#10602).

  The simplifier eliminates such things, but SpecConstr itself constructs
  new terms by substituting.  So the 'mkCast' in the Cast case of scExpr
  is very important!

Note [Choosing patterns]
~~~~~~~~~~~~~~~~~~~~~~~~
If we get lots of patterns we may not want to make a specialisation
for each of them (code bloat), so we choose as follows, implemented
by trim_pats.

* The flag -fspec-constr-count-N sets the sc_count field
  of the ScEnv to (Just n).  This limits the total number
  of specialisations for a given function to N.

* -fno-spec-constr-count sets the sc_count field to Nothing,
  which switches of the limit.

* The ghastly ForceSpecConstr trick also switches of the limit
  for a particular function

* Otherwise we sort the patterns to choose the most general
  ones first; more general => more widely applicable.

Note [SpecConstr and casts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#14270) a call like

    let f = e
    in ... f (K @(a |> cv)) ...

where 'cv' is a coercion variable not in scope at f's definition site.
If we aren't careful we'll get

    let $sf a cv = e (K @(a |> cv))
        RULE "SC:f" forall a cv.  f (K @(a |> cv)) = $sf a co
        f = e
    in ...

But alas, when we match the call we may fail to bind 'co', because the rule
matcher in GHC.Core.Rules cannot reliably bind coercion variables that appear
in casts (see Note [Casts in the template] in GHC.Core.Rules).

This seems intractable (see #23209). So:

* Key point: we /never/ quantify over coercion variables in a SpecConstr rule.
  If we would need to quantify over a coercion variable, we just discard the
  call pattern. See the test for `bad_covars` in callToPat.

* However (#14936) we /do/ still allow casts in call patterns. For example
     f ((e1,e2) |> sym co)
  where, say,
     f  :: Foo -> blah   -- Foo is a newtype
     f = f_rhs
     co :: Foo ~R (Int,Int)
  We want to specialise on that pair!

So for our function f, we might generate
  RULE forall x y.  f ((x,y) |> co) = $sf x y
  $sf x y = f_rhs ((x,y) |> co)

This works provided the free vars of `co` are either in-scope at the
definition of `f`, or quantified. For the latter, suppose `f` was polymorphic:

     f2  :: Foo2 a -> blah   -- Foo is a newtype
     f2 = f2_rhs
     co2 :: Foo a ~R (a,a)

Then it's fine for `co2` to mention `a`.  We'll get
  RULE forall a (x::a) (y::a).  f2 @a ((x,y) |> co2) = $sf2 a x y
  $sf2 @a x y = f2_rhs ((x,y) |> co2)
-}

data CallPat = CP { cp_qvars :: [Var]           -- Quantified variables
                  , cp_args  :: [CoreExpr]      -- Arguments
                  , cp_strict_args :: [Var] }   -- Arguments we want to pass unlifted even if they are boxed
                                                -- See Note [SpecConstr and strict fields]

     -- See Note [SpecConstr call patterns]

instance Outputable CallPat where
  ppr (CP { cp_qvars = qvars, cp_args = args, cp_strict_args =  strict })
    = text "CP" <> braces (sep [ text "cp_qvars =" <+> ppr qvars <> comma
                               , text "cp_args =" <+> ppr args
                               , text "cp_strict_args = " <> ppr strict ])

newtype SpecFailWarning = SpecFailForcedArgCount { spec_failed_fun_name :: Name }

type SpecFailWarnings = [SpecFailWarning]

instance Outputable SpecFailWarning where
  ppr (SpecFailForcedArgCount name) = ppr name <+> pprDefinedAt name

combineSpecWarning :: SpecFailWarnings -> SpecFailWarnings -> SpecFailWarnings
combineSpecWarning = (++)

data ArgCountResult = WorkerSmallEnough | WorkerTooLarge | WorkerTooLargeForced Name

callsToNewPats :: ScEnv -> Id
               -> SpecInfo
               -> [ArgOcc] -> [Call]
               -> UniqSM ( Bool        -- At least one boring call
                         , Bool        -- Patterns were discarded
                         , [CallPat]   -- Patterns to specialise
                         , [SpecFailWarning] -- Things that didn't specialise we want to warn the user about)
                         )
-- Result has no duplicate patterns,
-- nor ones mentioned in si_specs (hence "new" patterns)
-- Bool indicates that there was at least one boring pattern
-- The "New" in the name means "patterns that are not already covered
-- by an existing specialisation"
callsToNewPats env fn spec_info@(SI { si_specs = done_specs }) bndr_occs calls
  = do  { mb_pats <- mapM (callToPat env bndr_occs) calls

        ; let have_boring_call = any isNothing mb_pats

              good_pats :: [CallPat]
              good_pats = catMaybes mb_pats

              in_scope = getSubstInScope (sc_subst env)

              -- Remove patterns we have already done
              new_pats = filterOut is_done good_pats
              is_done p = any is_better done_specs
                 where
                   is_better done = betterPat in_scope (os_pat done) p

              -- Remove duplicates
              non_dups = subsumePats in_scope new_pats

              -- Remove ones that have too many worker variables
              (small_pats, arg_count_warnings) = partitionByWorkerSize too_many_worker_args non_dups

              -- too_many_worker_args :: CallPat -> Either SpecFailWarning Bool
              too_many_worker_args (CP { cp_qvars = vars, cp_args = args })
                | sc_force env
                -- See (FS5) of Note [Forcing specialisation]
                = if (isWorkerSmallEnough (sc_max_forced_args $ sc_opts env) (valArgCount args) vars)
                    then WorkerSmallEnough
                    else WorkerTooLargeForced (idName fn)
                | (isWorkerSmallEnough (sc_max_args $ sc_opts env) (valArgCount args) vars)
                = WorkerSmallEnough
                | otherwise = WorkerTooLarge
                  -- We are about to construct w/w pair in 'spec_one'.
                  -- Omit specialisation leading to high arity workers.
                  -- See Note [Limit w/w arity] in GHC.Core.Opt.WorkWrap.Utils

                -- Discard specialisations if there are too many of them
              (pats_were_discarded, trimmed_pats) = trim_pats env fn spec_info small_pats

--        ; pprTraceM "callsToPats" (vcat [ text "calls to" <+> ppr fn <> colon <+> ppr calls
--                                        , text "good_pats:" <+> ppr good_pats
--                                        , text "new_pats:" <+> ppr new_pats
--                                        , text "non_dups:" <+> ppr non_dups
--                                        , text "small_pats:" <+> ppr small_pats
--                                        , text "done_specs:" <+> ppr (map os_pat done_specs)
--                                        , text "trimmed_pats:" <+> ppr trimmed_pats ])

        ; return (have_boring_call, pats_were_discarded, trimmed_pats, arg_count_warnings) }
          -- If any of the calls does not give rise to a specialisation, either
          -- because it is boring, or because there are too many specialisations,
          -- return a flag to say so, so that we know to keep the original function.
  where
    partitionByWorkerSize worker_size pats = go pats [] []
      where
        go [] small warnings = (small, warnings)
        go (p:ps) small warnings
          | WorkerSmallEnough <- worker_size p
          = go ps (p:small) warnings
          | WorkerTooLarge <- worker_size p
          = go ps small warnings
          | WorkerTooLargeForced name <- worker_size p
          = go ps small (SpecFailForcedArgCount name : warnings)


trim_pats :: ScEnv -> Id -> SpecInfo -> [CallPat] -> (Bool, [CallPat])
-- True <=> some patterns were discarded
-- See Note [Choosing patterns]
trim_pats env fn (SI { si_n_specs = done_spec_count }) pats
  | sc_force env
    || isNothing mb_scc
    || n_remaining >= n_pats
  = -- pprTrace "trim_pats: no-trim" (ppr (sc_force env) $$ ppr mb_scc $$ ppr n_remaining $$ ppr n_pats)
    (False, pats)          -- No need to trim

  | otherwise
  = emit_trace $  -- Need to trim, so keep the best ones
    (True, take n_remaining sorted_pats)

  where
    n_pats         = length pats
    spec_count'    = n_pats + done_spec_count
    n_remaining    = max_specs - done_spec_count
    mb_scc         = sc_count $ sc_opts env
    Just max_specs = mb_scc

    sorted_pats = map fst $
                  sortBy (comparing snd) $
                  [(pat, pat_cons pat) | pat <- pats]
     -- Sort in order of increasing number of constructors
     -- (i.e. decreasing generality) and pick the initial
     -- segment of this list

    pat_cons :: CallPat -> Int
    -- How many data constructors of literals are in
    -- the pattern.  More data-cons => less general
    pat_cons (CP { cp_qvars = qs, cp_args = ps })
       = foldr ((+) . n_cons) 0 ps
       where
          q_set = mkVarSet qs
          n_cons (Var v) | v `elemVarSet` q_set = 0
                         | otherwise            = 1
          n_cons (Cast e _)  = n_cons e
          n_cons (App e1 e2) = n_cons e1 + n_cons e2
          n_cons (Lit {})    = 1
          n_cons _           = 0

    emit_trace result
       | debugIsOn || sc_debug (sc_opts env)
         -- Suppress this scary message for ordinary users!  #5125
       = pprTrace "SpecConstr" msg result
       | otherwise
       = result
    msg = vcat [ sep [ text "Function" <+> quotes (ppr fn)
                     , nest 2 (text "has" <+>
                               speakNOf spec_count' (text "call pattern") <> comma <+>
                               text "but the limit is" <+> int max_specs) ]
               , text "Use -fspec-constr-count=n to set the bound"
               , text "done_spec_count =" <+> int done_spec_count
               , text "Keeping " <+> int n_remaining <> text ", out of" <+> int n_pats
               , text "Discarding:" <+> ppr (drop n_remaining sorted_pats) ]


callToPat :: ScEnv -> [ArgOcc] -> Call -> UniqSM (Maybe CallPat)
        -- The [Var] is the variables to quantify over in the rule
        --      Type variables come first, since they may scope
        --      over the following term variables
        -- The [CoreExpr] are the argument patterns for the rule
callToPat env bndr_occs call@(Call fn args con_env)
  = do  { let in_scope = getSubstInScope (sc_subst env)

        ; arg_triples <- zipWith3M (argToPat env in_scope con_env) args bndr_occs (map (const NotMarkedStrict) args)
                   -- This zip trims the args to be no longer than
                   -- the lambdas in the function definition (bndr_occs)

          -- Drop boring patterns from the end
          -- See Note [SpecConstr call patterns]
        ; let arg_triples' | isJoinId fn     = arg_triples
                           | otherwise       = dropWhileEnd is_boring arg_triples
              is_boring (interesting, _,_)   = not interesting
              (interesting_s, pats, cbv_ids) = unzip3 arg_triples'
              interesting                    = or interesting_s

        ; let pat_fvs = exprsFreeVarsList pats
                -- To get determinism we need the list of free variables in
                -- deterministic order. Otherwise we end up creating
                -- lambdas with different argument orders. See
                -- determinism/simplCore/should_compile/spec-inline-determ.hs
                -- for an example. For explanation of determinism
                -- considerations See Note [Unique Determinism] in GHC.Types.Unique.

              in_scope_vars = getInScopeVars in_scope
              is_in_scope v = v `elemVarSet` in_scope_vars
              qvars         = filterOut is_in_scope pat_fvs
                -- Quantify over variables that are not in scope
                -- at the call site
                -- See Note [Free type variables of the qvar types]
                -- See Note [Shadowing in SpecConstr] at the top

              (qktvs, qids) = partition isTyVar qvars
              qvars'        = scopedSort qktvs ++ map sanitise qids
                -- Order into kind variables, type variables, term variables
                -- The kind of a type variable may mention a kind variable
                -- and the type of a term variable may mention a type variable

              sanitise id = updateIdTypeAndMult expandTypeSynonyms id
                -- See Note [Free type variables of the qvar types]

        -- Check for bad coercion variables: see Note [SpecConstr and casts]
        ; let bad_covars = filter isCoVar qids
        ; warnPprTrace (not (null bad_covars))
              "SpecConstr: bad covars"
              (ppr bad_covars $$ ppr call) $

          if interesting && null bad_covars
          then do { let cp_res = CP { cp_qvars = qvars', cp_args = pats
                                    , cp_strict_args = concat cbv_ids }
--                  ; pprTraceM "callToPatOut" $
--                    vcat [ text "fn:" <+> ppr fn
--                         , text "args:" <+> ppr args
--                         , text "bndr_occs:" <+> ppr bndr_occs
--                         , text "pat_fvs:" <+> ppr pat_fvs
--                         , text "cp_res:" <+> ppr cp_res ]
                  ; return (Just cp_res) }
          else return Nothing }

    -- argToPat takes an actual argument, and returns an abstracted
    -- version, consisting of just the "constructor skeleton" of the
    -- argument, with non-constructor sub-expression replaced by new
    -- placeholder variables.  For example:
    --    C a (D (f x) (g y))  ==>  C p1 (D p2 p3)

argToPat :: ScEnv
         -> InScopeSet                  -- What's in scope at the fn defn site
         -> ValueEnv                    -- ValueEnv at the call site
         -> CoreArg                     -- A call arg (or component thereof)
         -> ArgOcc
         -> StrictnessMark              -- Tells us if this argument is a strict field of a data constructor
                                        -- See Note [SpecConstr and strict fields]
         -> UniqSM (Bool, CoreArg, [Id])

-- Returns (interesting, pat),
-- where pat is the pattern derived from the argument
--            interesting=True if the pattern is non-trivial (not a variable or type)
-- E.g.         x:xs         --> (True, x:xs)
--              f xs         --> (False, w)        where w is a fresh wildcard
--              (f xs, 'c')  --> (True, (w, 'c'))  where w is a fresh wildcard
--              \x. x+y      --> (True, \x. x+y)
--              lvl7         --> (True, lvl7)      if lvl7 is bound
--                                                 somewhere further out

argToPat env in_scope val_env arg arg_occ arg_str
  = do
    -- pprTraceM "argToPatIn" (ppr arg)
    !res <- argToPat1 env in_scope val_env arg arg_occ arg_str
    -- pprTraceM "argToPatOut" (ppr res)
    return res

argToPat1 :: ScEnv
  -> InScopeSet
  -> ValueEnv
  -> Expr CoreBndr
  -> ArgOcc
  -> StrictnessMark
  -> UniqSM (Bool, Expr CoreBndr, [Id])
argToPat1 _env _in_scope _val_env arg@(Type {}) _arg_occ _arg_str
  = return (False, arg, [])

argToPat1 env in_scope val_env (Tick _ arg) arg_occ arg_str
  = argToPat env in_scope val_env arg arg_occ arg_str
        -- Note [Tick annotations in call patterns]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- Ignore Notes.  In particular, we want to ignore any InlineMe notes
        -- Perhaps we should not ignore profiling notes, but I'm going to
        -- ride roughshod over them all for now.
        --- See Note [Tick annotations in RULE matching] in GHC.Core.Rules

argToPat1 env in_scope val_env (Let _ arg) arg_occ arg_str
  = argToPat env in_scope val_env arg arg_occ arg_str
        -- See Note [Matching lets] in "GHC.Core.Rules"
        -- Look through let expressions
        -- e.g.         f (let v = rhs in (v,w))
        -- Here we can specialise for f (v,w)
        -- because the rule-matcher will look through the let.

   -- Casts: see Note [SpecConstr and casts]
argToPat1 env in_scope val_env (Cast arg co) arg_occ arg_str
  | not (ignoreType env ty2)
  = do  { (interesting, arg', strict_args) <- argToPat env in_scope val_env arg arg_occ arg_str
        ; if not interesting then
                wildCardPat ty2 arg_str
          else
                return (interesting, Cast arg' co, strict_args) }
  where
    ty2 = coercionRKind co

  -- Check for a constructor application
  -- NB: this *precedes* the Var case, so that we catch nullary constrs
argToPat1 env in_scope val_env arg arg_occ _arg_str
  | Just (ConVal _wf (DataAlt dc) args) <- isValue val_env arg
    -- Ignore `_wf` here; see Note [ConVal work-free-ness] (2)
  , not (ignoreDataCon env dc)        -- See Note [NoSpecConstr]
  , Just arg_occs <- mb_scrut dc
  = do { let (ty_args, rest_args) = splitAtList (dataConUnivTyVars dc) args
             con_str, matched_str :: [StrictnessMark]
             -- con_str corresponds 1-1 with the /value/ arguments
             -- matched_str corresponds 1-1 with /all/ arguments
             con_str = dataConRepStrictness dc
             matched_str = match_vals con_str rest_args
      --  ; pprTraceM "bangs" (ppr (length rest_args == length con_str) $$
      --       ppr dc $$
      --       ppr con_str $$
      --       ppr rest_args $$
      --       ppr (map isTypeArg rest_args))
       ; prs <- zipWith3M (argToPat env in_scope val_env) rest_args arg_occs matched_str
       ; let args' = map sndOf3 prs :: [CoreArg]
       ; assertPpr (length con_str == length (filter isRuntimeArg rest_args))
            ( ppr con_str $$ ppr rest_args $$
              ppr (length con_str) $$ ppr (length rest_args)
            ) $ return ()
       ; return (True, mkConApp dc (ty_args ++ args'), concat (map thdOf3 prs)) }
  where
    mb_scrut dc = case arg_occ of
                ScrutOcc bs | Just occs <- lookupUFM bs dc
                            -> Just (occs)  -- See Note [Reboxing]
                _other      | sc_force env || sc_keen (sc_opts env)
                            -> Just (repeat UnkOcc)
                            | otherwise
                            -> Nothing
    match_vals bangs (arg:args)
      | isTypeArg arg
      = NotMarkedStrict : match_vals bangs args
      | (b:bs) <- bangs
      = b : match_vals bs args
    match_vals [] [] = []
    match_vals as bs =
        pprPanic "spec-constr:argToPat - Bangs don't match value arguments"
            (text "arg:" <> ppr arg $$
             text "remaining args:" <> ppr as $$
             text "remaining bangs:" <> ppr bs)

  -- Check if the argument is a variable that
  --    (a) is used in an interesting way in the function body
  ---       i.e. ScrutOcc. UnkOcc and NoOcc are not interesting
  --        (NoOcc means we could drop the argument, but that's the
  --         business of absence analysis, not SpecConstr.)
  --    (b) we know what its value is
  -- In that case it counts as "interesting"
argToPat1 env in_scope val_env (Var v) arg_occ arg_str
  | sc_force env || specialisableArgOcc arg_occ  -- (a)
    -- See Note [Forcing specialisation], point (FS3)
  , is_value                                     -- (b)
       -- Ignoring sc_keen here to avoid gratuitously incurring Note [Reboxing]
       -- So sc_keen focused just on f (I# x), where we have freshly-allocated
       -- box that we can eliminate in the caller
  , not (ignoreType env (varType v))
  -- See Note [SpecConstr and strict fields]
  = return (True, Var v, if isMarkedStrict arg_str then [v] else mempty)
  where
    is_value
        | isLocalId v = v `elemInScopeSet` in_scope
                        && isJust (lookupVarEnv val_env v)
                -- Local variables have values in val_env
        | otherwise   = isValueUnfolding (idUnfolding v)
                -- Imports have unfoldings

--      I'm really not sure what this comment means
--      And by not wild-carding we tend to get forall'd
--      variables that are in scope, which in turn can
--      expose the weakness in let-matching
--      See Note [Matching lets] in GHC.Core.Rules

  -- Check for a variable bound inside the function.
  -- Don't make a wild-card, because we may usefully share
  --    e.g.  f a = let x = ... in f (x,x)
  -- NB: this case follows the lambda and con-app cases!!
-- argToPat _in_scope _val_env (Var v) _arg_occ
--   = return (False, Var v)
        -- SLPJ : disabling this to avoid proliferation of versions
        -- also works badly when thinking about seeding the loop
        -- from the body of the let
        --       f x y = letrec g z = ... in g (x,y)
        -- We don't want to specialise for that *particular* x,y


{- Disabled; see Note [Matching cases] in "GHC.Core.Rules"
argToPat env in_scope val_env (Case scrut _ _ [(_, _, rhs)]) arg_occ
  | exprOkForSpeculation scrut  -- See Note [Matching cases] in "GHC.Core.Rules"
  = argToPat env in_scope val_env rhs arg_occ
-}

{-      Disabling lambda specialisation for now
        It's fragile, and the spec_loop can be infinite
argToPat in_scope val_env arg arg_occ
  | is_value_lam arg
  = return (True, arg)
  where
    is_value_lam (Lam v e)         -- Spot a value lambda, even if
        | isId v       = True      -- it is inside a type lambda
        | otherwise    = is_value_lam e
    is_value_lam other = False
-}

  -- The default case: make a wild-card
  -- We use this for coercions too
argToPat1 _env _in_scope _val_env arg _arg_occ arg_str
  = wildCardPat (exprType arg) arg_str

-- | wildCardPats are always boring
wildCardPat :: Type -> StrictnessMark -> UniqSM (Bool, CoreArg, [Id])
wildCardPat ty str
  = do { id <- mkSysLocalOrCoVarM (fsLit "sc") ManyTy ty
       -- ; pprTraceM "wildCardPat" (ppr id' <+> ppr (idUnfolding id'))
       ; return (False, varToCoreExpr id, if isMarkedStrict str then [id] else []) }

isValue :: ValueEnv -> CoreExpr -> Maybe Value
isValue _env (Lit lit)
  | litIsLifted lit = Nothing
  | otherwise       = Just (ConVal True (LitAlt lit) [])

isValue env (Var v)
  | Just cval <- lookupVarEnv env v
  = Just cval  -- You might think we could look in the idUnfolding here
               -- but that doesn't take account of which branch of a
               -- case we are in, which is the whole point

  | not (isLocalId v) && isCheapUnfolding unf
  = isValue env (unfoldingTemplate unf)
  where
    unf = idUnfolding v
        -- However we do want to consult the unfolding
        -- as well, for let-bound constructors!

isValue env (Lam b e)
  | isTyVar b = case isValue env e of
                  Just _  -> Just LambdaVal
                  Nothing -> Nothing
  | otherwise = Just LambdaVal

isValue env (Tick t e)
  | not (tickishIsCode t)
  = isValue env e

isValue _env expr       -- Maybe it's a constructor application
  | (Var fun, args, _) <- collectArgsTicks (not . tickishIsCode) expr
  = case idDetails fun of
        DataConWorkId con | args `lengthAtLeast` dataConRepArity con
                -- Check saturated; might be > because the
                --                  arity excludes type args
                -> Just (ConVal (all exprIsWorkFree args) (DataAlt con) args)

        DFunId {} -> Just LambdaVal
        -- DFunId: see Note [Specialising on dictionaries]

        _other | valArgCount args < idArity fun
                -- Under-applied function
               -> Just LambdaVal        -- Partial application

        _other -> Nothing

isValue _env _expr = Nothing

betterPat :: InScopeSet -> CallPat -> CallPat -> Bool
-- pat1    f @a   (Just @a   (x::a))
--      is better than
-- pat2    f @Int (Just @Int (x::Int))
-- That is, we can instantiate pat1 to get pat2, using only type instantiate
-- See Note [Pattern duplicate elimination]
betterPat is (CP { cp_qvars = vs1, cp_args = as1 })
             (CP { cp_qvars = vs2, cp_args = as2 })
  | equalLength as1 as2
  = case matchExprs ise vs1 as1 as2 of
      Just (_, ms) -> all exprIsTrivial ms
      Nothing      -> False

  | otherwise -- We must handle patterns of unequal length separately (#24282)
  = False  -- For the pattern with more args, the last arg is "interesting"
           -- but the corresponding one on the other is "not interesting";
           -- So we can't get from one to the other with only exprIsTrivial
           -- instantiation.  Example nofib/spectral/ansi, function `loop`:
           --    P1: loop (I# x) (a : b)
           --    P2: loop (I# y)           -- Pattern eta-reduced
           -- Neither is better than the other, in the sense of betterPat
  where
    ise = ISE (is `extendInScopeSetList` vs2) (const noUnfolding)

subsumePats :: InScopeSet -> [CallPat] -> [CallPat]
-- Remove any patterns subsumed by others
-- See Note [Pattern duplicate elimination]
-- Other than deleting subsumed patterns, this operation is a no-op;
-- in particular it does not reverse the input.  It should not matter
-- but in #24282 it did; doing it this way keeps the existing behaviour.
subsumePats is pats = foldl add [] pats
  where
    add :: [CallPat] -> CallPat -> [CallPat]
    add []        ci                         = [ci]
    add (ci1:cis) ci2 | betterPat is ci1 ci2 = ci1 : cis
                      | betterPat is ci2 ci1 = ci2 : cis
                      | otherwise            = ci1 : add cis ci2

{-
Note [Pattern duplicate elimination]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider f :: (a,a) -> blah, and two calls
   f @Int  (x,y)
   f @Bool (p,q)

The danger is that we'll generate two *essentially identical* specialisations,
both for pairs, but with different types instantiating `a` (see #24229).

But we'll only make a `CallPat` for an argument (a,b) if `foo` scrutinises
that argument.  So SpecConstr should never need to specialise f's polymorphic
type arguments.  Even with only one of these calls we should be able to
generalise to the `CallPat`

  cp_qvars = [a, r::a, s::a], cp_args = [@a (r,s)]

Doing so isn't trivial, though.

For now we content ourselves with a simpler plan: eliminate a call pattern
if another pattern subsumes it; this is done by `subsumePats`.
For example here are two patterns

  cp_qvars = [a, r::a, s::a],  cp_args = [@a (r,s)]
  cp_qvars = [x::Int, y::Int], cp_args = [@Int (x,y)]

The first can be instantiated to the second, /by instantiating types only/.
This subsumption relationship is checked by `betterPat`.  Note that if
we have

  cp_qvars = [a, r::a, s::a], cp_args = [@a (r,s)]
  cp_qvars = [],              cp_args = [@Bool (True,False)]

the first does *not* subsume the second; the second is more specific.

In our initial example with `f @Int` and `f @Bool` neither subsumes the other,
so we will get two essentially-identical specialisations. Boo.  We rely on our
crude throttling mechanisms to stop this getting out of control -- with
polymorphic recursion we can generate an infinite number of specialisations.
Example is Data.Sequence.adjustTree, I think.
-}
