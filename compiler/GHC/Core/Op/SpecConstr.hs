{-
ToDo [Oct 2013]
~~~~~~~~~~~~~~~
1. Nuke ForceSpecConstr for good (it is subsumed by GHC.Types.SPEC in ghc-prim)
2. Nuke NoSpecConstr


(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[SpecConstr]{Specialise over constructors}
-}

{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.Core.Op.SpecConstr(
        specConstrProgram,
        SpecConstrAnnotation(..)
    ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Core
import GHC.Core.Subst
import GHC.Core.Utils
import GHC.Core.Unfold  ( couldBeSmallEnoughToInline )
import GHC.Core.FVs     ( exprsFreeVarsList )
import GHC.Core.Op.Monad
import GHC.Types.Literal ( litIsLifted )
import GHC.Driver.Types ( ModGuts(..) )
import GHC.Core.Op.WorkWrap.Lib ( isWorkerSmallEnough, mkWorkerArgs )
import GHC.Core.DataCon
import GHC.Core.Coercion hiding( substCo )
import GHC.Core.Rules
import GHC.Core.Type     hiding ( substTy )
import GHC.Core.TyCon   ( tyConName )
import GHC.Types.Id
import GHC.Core.Ppr     ( pprParendExpr )
import GHC.Core.Make    ( mkImpossibleExpr )
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Name
import GHC.Types.Basic
import GHC.Driver.Session ( DynFlags(..), GeneralFlag( Opt_SpecConstrKeen )
                          , gopt, hasPprDebug )
import Maybes           ( orElse, catMaybes, isJust, isNothing )
import GHC.Types.Demand
import GHC.Types.Cpr
import GHC.Serialized   ( deserializeWithData )
import Util
import Pair
import GHC.Types.Unique.Supply
import Outputable
import FastString
import GHC.Types.Unique.FM
import MonadUtils
import Control.Monad    ( zipWithM )
import Data.List
import PrelNames        ( specTyConName )
import GHC.Types.Module
import GHC.Core.TyCon ( TyCon )
import GHC.Exts( SpecConstrAnnotation(..) )
import Data.Ord( comparing )

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
          the fn defn.  But see Note [Shadowing] below.


NOTICE that we only abstract over variables that are not in scope,
so we're in no danger of shadowing variables used in "higher up"
in f_spec's RHS.


Note [Shadowing]
~~~~~~~~~~~~~~~~
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

Note [Local recursive groups]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a *local* recursive group, we can see all the calls to the
function, so we seed the specialisation loop from the calls in the
body, not from the calls in the RHS.  Consider:

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

However, if we find any boring calls in the body, including *unsaturated*
ones, such as
      letrec foo x y = ....foo...
      in map foo xs
then we will end up calling the un-specialised function, so then we *should*
use the calls in the un-specialised RHS as seeds.  We call these
"boring call patterns", and callsToPats reports if it finds any of these.

Note [Seeding top-level recursive groups]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This seeding is done in the binding for seed_calls in specRec.

1. If all the bindings in a top-level recursive group are local (not
   exported), then all the calls are in the rest of the top-level
   bindings.  This means we can specialise with those call patterns
   ONLY, and NOT with the RHSs of the recursive group (exactly like
   Note [Local recursive groups])

2. But if any of the bindings are exported, the function may be called
   with any old arguments, so (for lack of anything better) we specialise
   based on
     (a) the call patterns in the RHS
     (b) the call patterns in the rest of the top-level bindings
   NB: before Apr 15 we used (a) only, but Dimitrios had an example
       where (b) was crucial, so I added that.
       Adding (b) also improved nofib allocation results:
                  multiplier: 4%   better
                  minimax:    2.8% better

Actually in case (2), instead of using the calls from the RHS, it
would be better to specialise in the importing module.  We'd need to
add an INLINABLE pragma to the function, and then it can be
specialised in the importing scope, just as is done for type classes
in GHC.Core.Op.Specialise.specImports. This remains to be done (#10346).

Note [Top-level recursive groups]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To get the call usage information from "the rest of the top level
bindings" (c.f. Note [Seeding top-level recursive groups]), we work
backwards through the top-level bindings so we see the usage before we
get to the binding of the function.  Before we can collect the usage
though, we go through all the bindings and add them to the
environment. This is necessary because usage is only tracked for
functions in the environment.  These two passes are called
   'go' and 'goEnv'
in specConstrProgram.  (Looks a bit revolting to me.)

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

  * Ignore specConstrThreshold, to specialise functions of arbitrary size
        (see scTopBind)
  * Ignore specConstrCount, to make arbitrary numbers of specialisations
        (see specialise)
  * Specialise even for arguments that are not scrutinised in the loop
        (see argToPat; #4448)
  * Only specialise on recursive types a finite number of times
        (see is_too_recursive; #5550; Note [Limit recursive specialisation])

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
  = do
      dflags <- getDynFlags
      us     <- getUniqueSupplyM
      (_, annos) <- getFirstAnnotations deserializeWithData guts
      this_mod <- getModule
      let binds' = reverse $ fst $ initUs us $ do
                    -- Note [Top-level recursive groups]
                    (env, binds) <- goEnv (initScEnv dflags this_mod annos)
                                          (mg_binds guts)
                        -- binds is identical to (mg_binds guts), except that the
                        -- binders on the LHS have been replaced by extendBndr
                        --   (SPJ this seems like overkill; I don't think the binders
                        --    will change at all; and we don't substitute in the RHSs anyway!!)
                    go env nullUsage (reverse binds)

      return (guts { mg_binds = binds' })
  where
    -- See Note [Top-level recursive groups]
    goEnv env []            = return (env, [])
    goEnv env (bind:binds)  = do (env', bind')   <- scTopBindEnv env bind
                                 (env'', binds') <- goEnv env' binds
                                 return (env'', bind' : binds')

    -- Arg list of bindings is in reverse order
    go _   _   []           = return []
    go env usg (bind:binds) = do (usg', bind') <- scTopBind env usg bind
                                 binds' <- go env usg' binds
                                 return (bind' : binds')

{-
************************************************************************
*                                                                      *
\subsection{Environment: goes downwards}
*                                                                      *
************************************************************************

Note [Work-free values only in environment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The sc_vals field keeps track of in-scope value bindings, so
that if we come across (case x of Just y ->...) we can reduce the
case from knowing that x is bound to a pair.

But only *work-free* values are ok here. For example if the envt had
    x -> Just (expensive v)
then we do NOT want to expand to
     let y = expensive v in ...
because the x-binding still exists and we've now duplicated (expensive v).

This seldom happens because let-bound constructor applications are
ANF-ised, but it can happen as a result of on-the-fly transformations in
SpecConstr itself.  Here is #7865:

        let {
          a'_shr =
            case xs_af8 of _ {
              [] -> acc_af6;
              : ds_dgt [Dmd=<L,A>] ds_dgu [Dmd=<L,A>] ->
                (expensive x_af7, x_af7
            } } in
        let {
          ds_sht =
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
   f x y = case case remInt# x 2# of
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

data ScEnv = SCE { sc_dflags    :: DynFlags,
                   sc_module    :: !Module,
                   sc_size      :: Maybe Int,   -- Size threshold
                                                -- Nothing => no limit

                   sc_count     :: Maybe Int,   -- Max # of specialisations for any one fn
                                                -- Nothing => no limit
                                                -- See Note [Avoiding exponential blowup]

                   sc_recursive :: Int,         -- Max # of specialisations over recursive type.
                                                -- Stops ForceSpecConstr from diverging.

                   sc_keen     :: Bool,         -- Specialise on arguments that are known
                                                -- constructors, even if they are not
                                                -- scrutinised in the body.  See
                                                -- Note [Making SpecConstr keener]

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
                        -- The range of the ValueEnv is *work-free* values
                        -- such as (\x. blah), or (Just v)
                        -- but NOT (Just (expensive v))
                        -- See Note [Work-free values only in environment]

                   sc_annotations :: UniqFM SpecConstrAnnotation
             }

---------------------
type HowBoundEnv = VarEnv HowBound      -- Domain is OutVars

---------------------
type ValueEnv = IdEnv Value             -- Domain is OutIds
data Value    = ConVal AltCon [CoreArg] -- _Saturated_ constructors
                                        --   The AltCon is never DEFAULT
              | LambdaVal               -- Inlinable lambdas or PAPs

instance Outputable Value where
   ppr (ConVal con args) = ppr con <+> interpp'SP args
   ppr LambdaVal         = text "<Lambda>"

---------------------
initScEnv :: DynFlags -> Module -> UniqFM SpecConstrAnnotation -> ScEnv
initScEnv dflags this_mod anns
  = SCE { sc_dflags      = dflags,
          sc_module      = this_mod,
          sc_size        = specConstrThreshold dflags,
          sc_count       = specConstrCount     dflags,
          sc_recursive   = specConstrRecursive dflags,
          sc_keen        = gopt Opt_SpecConstrKeen dflags,
          sc_force       = False,
          sc_subst       = emptySubst,
          sc_how_bound   = emptyVarEnv,
          sc_vals        = emptyVarEnv,
          sc_annotations = anns }

data HowBound = RecFun  -- These are the recursive functions for which
                        -- we seek interesting call patterns

              | RecArg  -- These are those functions' arguments, or their sub-components;
                        -- we gather occurrence information for these

instance Outputable HowBound where
  ppr RecFun = text "RecFun"
  ppr RecArg = text "RecArg"

scForce :: ScEnv -> Bool -> ScEnv
scForce env b = env { sc_force = b }

lookupHowBound :: ScEnv -> Id -> Maybe HowBound
lookupHowBound env id = lookupVarEnv (sc_how_bound env) id

scSubstId :: ScEnv -> Id -> CoreExpr
scSubstId env v = lookupIdSubst (text "scSubstId") (sc_subst env) v

scSubstTy :: ScEnv -> Type -> Type
scSubstTy env ty = substTy (sc_subst env) ty

scSubstCo :: ScEnv -> Coercion -> Coercion
scSubstCo env co = substCo (sc_subst env) co

zapScSubst :: ScEnv -> ScEnv
zapScSubst env = env { sc_subst = zapSubstEnv (sc_subst env) }

extendScInScope :: ScEnv -> [Var] -> ScEnv
        -- Bring the quantified variables into scope
extendScInScope env qvars = env { sc_subst = extendInScopeList (sc_subst env) qvars }

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

extendBndr :: ScEnv -> Var -> (ScEnv, Var)
extendBndr  env bndr  = (env { sc_subst = subst' }, bndr')
                      where
                        (subst', bndr') = substBndr (sc_subst env) bndr

extendValEnv :: ScEnv -> Id -> Maybe Value -> ScEnv
extendValEnv env _  Nothing   = env
extendValEnv env id (Just cv)
 | valueIsWorkFree cv      -- Don't duplicate work!!  #7865
 = env { sc_vals = extendVarEnv (sc_vals env) id cv }
extendValEnv env _ _ = env

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
   env1 | Var v <- stripTicksTopE (const True) scrut
                         = extendValEnv env v cval
        | otherwise      = env  -- See Note [Add scrutinee to ValueEnv too]
   env2 | live_case_bndr = extendValEnv env1 case_bndr cval
        | otherwise      = env1

   alt_bndrs' | case scrut of { Var {} -> True; _ -> live_case_bndr }
              = map zap alt_bndrs
              | otherwise
              = alt_bndrs

   cval = case con of
                DEFAULT    -> Nothing
                LitAlt {}  -> Just (ConVal con [])
                DataAlt {} -> Just (ConVal con vanilla_args)
                      where
                        vanilla_args = map Type (tyConAppArgs (idType case_bndr)) ++
                                       varsToCoreExprs alt_bndrs

   zap v | isTyVar v = v                -- See NB2 above
         | otherwise = zapIdOccInfo v


decreaseSpecCount :: ScEnv -> Int -> ScEnv
-- See Note [Avoiding exponential blowup]
decreaseSpecCount env n_specs
  = env { sc_force = False   -- See Note [Forcing specialisation]
        , sc_count = case sc_count env of
                       Nothing -> Nothing
                       Just n  -> Just (n `div` (n_specs + 1)) }
        -- The "+1" takes account of the original function;
        -- See Note [Avoiding exponential blowup]

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
  = lookupUFM (sc_annotations env) tycon == Just NoSpecConstr

forceSpecBndr env var = forceSpecFunTy env . snd . splitForAllTys . varType $ var

forceSpecFunTy :: ScEnv -> Type -> Bool
forceSpecFunTy env = any (forceSpecArgTy env) . fst . splitFunTys

forceSpecArgTy :: ScEnv -> Type -> Bool
forceSpecArgTy env ty
  | Just ty' <- coreView ty = forceSpecArgTy env ty'

forceSpecArgTy env ty
  | Just (tycon, tys) <- splitTyConApp_maybe ty
  , tycon /= funTyCon
      = tyConName tycon == specTyConName
        || lookupUFM (sc_annotations env) tycon == Just ForceSpecConstr
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

So when recursively specialising we divide the sc_count by the number of
copies we are making at this level, including the original.


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

type CallEnv = IdEnv [Call]
data Call = Call Id [CoreArg] ValueEnv
        -- The arguments of the call, together with the
        -- env giving the constructor bindings at the call site
        -- We keep the function mainly for debug output

instance Outputable ScUsage where
  ppr (SCU { scu_calls = calls, scu_occs = occs })
    = text "SCU" <+> braces (sep [ ptext (sLit "calls =") <+> ppr calls
                                         , text "occs =" <+> ppr occs ])

instance Outputable Call where
  ppr (Call fn args _) = ppr fn <+> fsep (map pprParendExpr args)

nullUsage :: ScUsage
nullUsage = SCU { scu_calls = emptyVarEnv, scu_occs = emptyVarEnv }

combineCalls :: CallEnv -> CallEnv -> CallEnv
combineCalls = plusVarEnv_C (++)
  where
--    plus cs ds | length res > 1
--               = pprTrace "combineCalls" (vcat [ text "cs:" <+> ppr cs
--                                               , text "ds:" <+> ppr ds])
--                 res
--               | otherwise = res
--       where
--          res = cs ++ ds

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
                 (DataConEnv [ArgOcc])   -- How the sub-components are used

type DataConEnv a = UniqFM a     -- Keyed by DataCon

{- Note  [ScrutOcc]
~~~~~~~~~~~~~~~~~~~
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

scExpr, scExpr' :: ScEnv -> CoreExpr -> UniqSM (ScUsage, CoreExpr)
        -- The unique supply is needed when we invent
        -- a new name for the specialised function and its args

scExpr env e = scExpr' env e

scExpr' env (Var v)      = case scSubstId env v of
                            Var v' -> return (mkVarUsage env v' [], Var v')
                            e'     -> scExpr (zapScSubst env) e'

scExpr' env (Type t)     = return (nullUsage, Type (scSubstTy env t))
scExpr' env (Coercion c) = return (nullUsage, Coercion (scSubstCo env c))
scExpr' _   e@(Lit {})   = return (nullUsage, e)
scExpr' env (Tick t e)   = do (usg, e') <- scExpr env e
                              return (usg, Tick t e')
scExpr' env (Cast e co)  = do (usg, e') <- scExpr env e
                              return (usg, mkCast e' (scSubstCo env co))
                              -- Important to use mkCast here
                              -- See Note [SpecConstr call patterns]
scExpr' env e@(App _ _)  = scApp env (collectArgs e)
scExpr' env (Lam b e)    = do let (env', b') = extendBndr env b
                              (usg, e') <- scExpr env' e
                              return (usg, Lam b' e')

scExpr' env (Case scrut b ty alts)
  = do  { (scrut_usg, scrut') <- scExpr env scrut
        ; case isValue (sc_vals env) scrut' of
                Just (ConVal con args) -> sc_con_app con args scrut'
                _other                 -> sc_vanilla scrut_usg scrut'
        }
  where
    sc_con_app con args scrut'  -- Known constructor; simplify
     = do { let (_, bs, rhs) = findAlt con alts
                                  `orElse` (DEFAULT, [], mkImpossibleExpr ty)
                alt_env'     = extendScSubstList env ((b,scrut') : bs `zip` trimConArgs con args)
          ; scExpr alt_env' rhs }

    sc_vanilla scrut_usg scrut' -- Normal case
     = do { let (alt_env,b') = extendBndrWith RecArg env b
                        -- Record RecArg for the components

          ; (alt_usgs, alt_occs, alts')
                <- mapAndUnzip3M (sc_alt alt_env scrut' b') alts

          ; let scrut_occ  = foldr combineOcc NoOcc alt_occs
                scrut_usg' = setScrutOcc env scrut_usg scrut' scrut_occ
                -- The combined usage of the scrutinee is given
                -- by scrut_occ, which is passed to scScrut, which
                -- in turn treats a bare-variable scrutinee specially

          ; return (foldr combineUsage scrut_usg' alt_usgs,
                    Case scrut' b' (scSubstTy env ty) alts') }

    sc_alt env scrut' b' (con,bs,rhs)
     = do { let (env1, bs1) = extendBndrsWith RecArg env bs
                (env2, bs2) = extendCaseBndrs env1 scrut' b' con bs1
          ; (usg, rhs') <- scExpr env2 rhs
          ; let (usg', b_occ:arg_occs) = lookupOccs usg (b':bs2)
                scrut_occ = case con of
                               DataAlt dc -> ScrutOcc (unitUFM dc arg_occs)
                               _          -> ScrutOcc emptyUFM
          ; return (usg', b_occ `combineOcc` scrut_occ, (con, bs2, rhs')) }

scExpr' env (Let (NonRec bndr rhs) body)
  | isTyVar bndr        -- Type-lets may be created by doBeta
  = scExpr' (extendScSubst env bndr rhs) body

  | otherwise
  = do  { let (body_env, bndr') = extendBndr env bndr
        ; rhs_info  <- scRecRhs env (bndr',rhs)

        ; let body_env2 = extendHowBound body_env [bndr'] RecFun
                           -- Note [Local let bindings]
              rhs'      = ri_new_rhs rhs_info
              body_env3 = extendValEnv body_env2 bndr' (isValue (sc_vals env) rhs')

        ; (body_usg, body') <- scExpr body_env3 body

          -- NB: For non-recursive bindings we inherit sc_force flag from
          -- the parent function (see Note [Forcing specialisation])
        ; (spec_usg, specs) <- specNonRec env body_usg rhs_info

        ; return (body_usg { scu_calls = scu_calls body_usg `delVarEnv` bndr' }
                    `combineUsage` spec_usg,  -- Note [spec_usg includes rhs_usg]
                  mkLets [NonRec b r | (b,r) <- ruleInfoBinds rhs_info specs] body')
        }


-- A *local* recursive group: see Note [Local recursive groups]
scExpr' env (Let (Rec prs) body)
  = do  { let (bndrs,rhss)      = unzip prs
              (rhs_env1,bndrs') = extendRecBndrs env bndrs
              rhs_env2          = extendHowBound rhs_env1 bndrs' RecFun
              force_spec        = any (forceSpecBndr env) bndrs'
                -- Note [Forcing specialisation]

        ; rhs_infos <- mapM (scRecRhs rhs_env2) (bndrs' `zip` rhss)
        ; (body_usg, body')     <- scExpr rhs_env2 body

        -- NB: start specLoop from body_usg
        ; (spec_usg, specs) <- specRec NotTopLevel (scForce rhs_env2 force_spec)
                                       body_usg rhs_infos
                -- Do not unconditionally generate specialisations from rhs_usgs
                -- Instead use them only if we find an unspecialised call
                -- See Note [Local recursive groups]

        ; let all_usg = spec_usg `combineUsage` body_usg  -- Note [spec_usg includes rhs_usg]
              bind'   = Rec (concat (zipWith ruleInfoBinds rhs_infos specs))

        ; return (all_usg { scu_calls = scu_calls all_usg `delVarEnvList` bndrs' },
                  Let bind' body') }

{-
Note [Local let bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~
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

scApp :: ScEnv -> (InExpr, [InExpr]) -> UniqSM (ScUsage, CoreExpr)

scApp env (Var fn, args)        -- Function is a variable
  = ASSERT( not (null args) )
    do  { args_w_usgs <- mapM (scExpr env) args
        ; let (arg_usgs, args') = unzip args_w_usgs
              arg_usg = combineUsages arg_usgs
        ; case scSubstId env fn of
            fn'@(Lam {}) -> scExpr (zapScSubst env) (doBeta fn' args')
                        -- Do beta-reduction and try again

            Var fn' -> return (arg_usg `combineUsage` mkVarUsage env fn' args',
                               mkApps (Var fn') args')

            other_fn' -> return (arg_usg, mkApps other_fn' args') }
                -- NB: doing this ignores any usage info from the substituted
                --     function, but I don't think that matters.  If it does
                --     we can fix it.
  where
    doBeta :: OutExpr -> [OutExpr] -> OutExpr
    -- ToDo: adjust for System IF
    doBeta (Lam bndr body) (arg : args) = Let (NonRec bndr arg) (doBeta body args)
    doBeta fn              args         = mkApps fn args

-- The function is almost always a variable, but not always.
-- In particular, if this pass follows float-in,
-- which it may, we can get
--      (let f = ...f... in f) arg1 arg2
scApp env (other_fn, args)
  = do  { (fn_usg,   fn')   <- scExpr env other_fn
        ; (arg_usgs, args') <- mapAndUnzipM (scExpr env) args
        ; return (combineUsages arg_usgs `combineUsage` fn_usg, mkApps fn' args') }

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
    -- I rather think we could use UnkOcc all the time
    arg_occ | null args = UnkOcc
            | otherwise = evalScrutOcc

----------------------
scTopBindEnv :: ScEnv -> CoreBind -> UniqSM (ScEnv, CoreBind)
scTopBindEnv env (Rec prs)
  = do  { let (rhs_env1,bndrs') = extendRecBndrs env bndrs
              rhs_env2          = extendHowBound rhs_env1 bndrs RecFun

              prs'              = zip bndrs' rhss
        ; return (rhs_env2, Rec prs') }
  where
    (bndrs,rhss) = unzip prs

scTopBindEnv env (NonRec bndr rhs)
  = do  { let (env1, bndr') = extendBndr env bndr
              env2          = extendValEnv env1 bndr' (isValue (sc_vals env) rhs)
        ; return (env2, NonRec bndr' rhs) }

----------------------
scTopBind :: ScEnv -> ScUsage -> CoreBind -> UniqSM (ScUsage, CoreBind)

{-
scTopBind _ usage _
  | pprTrace "scTopBind_usage" (ppr (scu_calls usage)) False
  = error "false"
-}

scTopBind env body_usage (Rec prs)
  | Just threshold <- sc_size env
  , not force_spec
  , not (all (couldBeSmallEnoughToInline (sc_dflags env) threshold) rhss)
                -- No specialisation
  = -- pprTrace "scTopBind: nospec" (ppr bndrs) $
    do  { (rhs_usgs, rhss')   <- mapAndUnzipM (scExpr env) rhss
        ; return (body_usage `combineUsage` combineUsages rhs_usgs, Rec (bndrs `zip` rhss')) }

  | otherwise   -- Do specialisation
  = do  { rhs_infos <- mapM (scRecRhs env) prs

        ; (spec_usage, specs) <- specRec TopLevel (scForce env force_spec)
                                         body_usage rhs_infos

        ; return (body_usage `combineUsage` spec_usage,
                  Rec (concat (zipWith ruleInfoBinds rhs_infos specs))) }
  where
    (bndrs,rhss) = unzip prs
    force_spec   = any (forceSpecBndr env) bndrs
      -- Note [Forcing specialisation]

scTopBind env usage (NonRec bndr rhs)   -- Oddly, we don't seem to specialise top-level non-rec functions
  = do  { (rhs_usg', rhs') <- scExpr env rhs
        ; return (usage `combineUsage` rhs_usg', NonRec bndr rhs') }

----------------------
scRecRhs :: ScEnv -> (OutId, InExpr) -> UniqSM RhsInfo
scRecRhs env (bndr,rhs)
  = do  { let (arg_bndrs,body)       = collectBinders rhs
              (body_env, arg_bndrs') = extendBndrsWith RecArg env arg_bndrs
        ; (body_usg, body')         <- scExpr body_env body
        ; let (rhs_usg, arg_occs)    = lookupOccs body_usg arg_bndrs'
        ; return (RI { ri_rhs_usg = rhs_usg
                     , ri_fn = bndr, ri_new_rhs = mkLams arg_bndrs' body'
                     , ri_lam_bndrs = arg_bndrs, ri_lam_body = body
                     , ri_arg_occs = arg_occs }) }
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
  = SI { si_specs :: [OneSpec]          -- The specialisations we have generated

       , si_n_specs :: Int              -- Length of si_specs; used for numbering them

       , si_mb_unspec :: Maybe ScUsage  -- Just cs  => we have not yet used calls in the
       }                                --             from calls in the *original* RHS as
                                        --             seeds for new specialisations;
                                        --             if you decide to do so, here is the
                                        --             RHS usage (which has not yet been
                                        --             unleashed)
                                        -- Nothing => we have
                                        -- See Note [Local recursive groups]
                                        -- See Note [spec_usg includes rhs_usg]

        -- One specialisation: Rule plus definition
data OneSpec =
  OS { os_pat  :: CallPat    -- Call pattern that generated this specialisation
     , os_rule :: CoreRule   -- Rule connecting original id with the specialisation
     , os_id   :: OutId      -- Spec id
     , os_rhs  :: OutExpr }  -- Spec rhs

noSpecInfo :: SpecInfo
noSpecInfo = SI { si_specs = [], si_n_specs = 0, si_mb_unspec = Nothing }

----------------------
specNonRec :: ScEnv
           -> ScUsage         -- Body usage
           -> RhsInfo         -- Structure info usage info for un-specialised RHS
           -> UniqSM (ScUsage, SpecInfo)       -- Usage from RHSs (specialised and not)
                                               --     plus details of specialisations

specNonRec env body_usg rhs_info
  = specialise env (scu_calls body_usg) rhs_info
               (noSpecInfo { si_mb_unspec = Just (ri_rhs_usg rhs_info) })

----------------------
specRec :: TopLevelFlag -> ScEnv
        -> ScUsage                         -- Body usage
        -> [RhsInfo]                       -- Structure info and usage info for un-specialised RHSs
        -> UniqSM (ScUsage, [SpecInfo])    -- Usage from all RHSs (specialised and not)
                                           --     plus details of specialisations

specRec top_lvl env body_usg rhs_infos
  = go 1 seed_calls nullUsage init_spec_infos
  where
    (seed_calls, init_spec_infos)    -- Note [Seeding top-level recursive groups]
       | isTopLevel top_lvl
       , any (isExportedId . ri_fn) rhs_infos   -- Seed from body and RHSs
       = (all_calls,     [noSpecInfo | _ <- rhs_infos])
       | otherwise                              -- Seed from body only
       = (calls_in_body, [noSpecInfo { si_mb_unspec = Just (ri_rhs_usg ri) }
                         | ri <- rhs_infos])

    calls_in_body = scu_calls body_usg
    calls_in_rhss = foldr (combineCalls . scu_calls . ri_rhs_usg) emptyVarEnv rhs_infos
    all_calls = calls_in_rhss `combineCalls` calls_in_body

    -- Loop, specialising, until you get no new specialisations
    go :: Int   -- Which iteration of the "until no new specialisations"
                -- loop we are on; first iteration is 1
       -> CallEnv   -- Seed calls
                    -- Two accumulating parameters:
       -> ScUsage      -- Usage from earlier specialisations
       -> [SpecInfo]   -- Details of specialisations so far
       -> UniqSM (ScUsage, [SpecInfo])
    go n_iter seed_calls usg_so_far spec_infos
      | isEmptyVarEnv seed_calls
      = -- pprTrace "specRec1" (vcat [ ppr (map ri_fn rhs_infos)
        --                           , ppr seed_calls
        --                           , ppr body_usg ]) $
        return (usg_so_far, spec_infos)

      -- Limit recursive specialisation
      -- See Note [Limit recursive specialisation]
      | n_iter > sc_recursive env  -- Too many iterations of the 'go' loop
      , sc_force env || isNothing (sc_count env)
           -- If both of these are false, the sc_count
           -- threshold will prevent non-termination
      , any ((> the_limit) . si_n_specs) spec_infos
      = -- pprTrace "specRec2" (ppr (map (map os_pat . si_specs) spec_infos)) $
        return (usg_so_far, spec_infos)

      | otherwise
      = -- pprTrace "specRec3" (vcat [ text "bndrs" <+> ppr (map ri_fn rhs_infos)
        --                           , text "iteration" <+> int n_iter
        --                          , text "spec_infos" <+> ppr (map (map os_pat . si_specs) spec_infos)
        --                    ]) $
        do  { specs_w_usg <- zipWithM (specialise env seed_calls) rhs_infos spec_infos
            ; let (extra_usg_s, new_spec_infos) = unzip specs_w_usg
                  extra_usg = combineUsages extra_usg_s
                  all_usg   = usg_so_far `combineUsage` extra_usg
            ; go (n_iter + 1) (scu_calls extra_usg) all_usg new_spec_infos }

    -- See Note [Limit recursive specialisation]
    the_limit = case sc_count env of
                  Nothing  -> 10    -- Ugh!
                  Just max -> max


----------------------
specialise
   :: ScEnv
   -> CallEnv                     -- Info on newly-discovered calls to this function
   -> RhsInfo
   -> SpecInfo                    -- Original RHS plus patterns dealt with
   -> UniqSM (ScUsage, SpecInfo)  -- New specialised versions and their usage

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
                    -- and do not generate specialisation seeds from its RHS
  = -- pprTrace "specialise bot" (ppr fn) $
    return (nullUsage, spec_info)

  | isNeverActive (idInlineActivation fn) -- See Note [Transfer activation]
    || null arg_bndrs                     -- Only specialise functions
  = -- pprTrace "specialise inactive" (ppr fn) $
    case mb_unspec of    -- Behave as if there was a single, boring call
      Just rhs_usg -> return (rhs_usg, spec_info { si_mb_unspec = Nothing })
                         -- See Note [spec_usg includes rhs_usg]
      Nothing      -> return (nullUsage, spec_info)

  | Just all_calls <- lookupVarEnv bind_calls fn
  = -- pprTrace "specialise entry {" (ppr fn <+> ppr all_calls) $
    do  { (boring_call, new_pats) <- callsToNewPats env fn spec_info arg_occs all_calls

        ; let n_pats = length new_pats
--        ; if (not (null new_pats) || isJust mb_unspec) then
--            pprTrace "specialise" (vcat [ ppr fn <+> text "with" <+> int n_pats <+> text "good patterns"
--                                        , text "mb_unspec" <+> ppr (isJust mb_unspec)
--                                        , text "arg_occs" <+> ppr arg_occs
--                                        , text "good pats" <+> ppr new_pats])  $
--               return ()
--          else return ()

        ; let spec_env = decreaseSpecCount env n_pats
        ; (spec_usgs, new_specs) <- mapAndUnzipM (spec_one spec_env fn arg_bndrs body)
                                                 (new_pats `zip` [spec_count..])
                -- See Note [Specialise original body]

        ; let spec_usg = combineUsages spec_usgs

              -- If there were any boring calls among the seeds (= all_calls), then those
              -- calls will call the un-specialised function.  So we should use the seeds
              -- from the _unspecialised_ function's RHS, which are in mb_unspec, by returning
              -- then in new_usg.
              (new_usg, mb_unspec')
                  = case mb_unspec of
                      Just rhs_usg | boring_call -> (spec_usg `combineUsage` rhs_usg, Nothing)
                      _                          -> (spec_usg,                      mb_unspec)

--        ; pprTrace "specialise return }"
--             (vcat [ ppr fn
--                   , text "boring_call:" <+> ppr boring_call
--                   , text "new calls:" <+> ppr (scu_calls new_usg)]) $
--          return ()

          ; return (new_usg, SI { si_specs = new_specs ++ specs
                                , si_n_specs = spec_count + n_pats
                                , si_mb_unspec = mb_unspec' }) }

  | otherwise  -- No new seeds, so return nullUsage
  = return (nullUsage, spec_info)




---------------------
spec_one :: ScEnv
         -> OutId       -- Function
         -> [InVar]     -- Lambda-binders of RHS; should match patterns
         -> InExpr      -- Body of the original function
         -> (CallPat, Int)
         -> UniqSM (ScUsage, OneSpec)   -- Rule and binding

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

spec_one env fn arg_bndrs body (call_pat@(qvars, pats), rule_number)
  = do  { spec_uniq <- getUniqueM
        ; let spec_env   = extendScSubstList (extendScInScope env qvars)
                                             (arg_bndrs `zip` pats)
              fn_name    = idName fn
              fn_loc     = nameSrcSpan fn_name
              fn_occ     = nameOccName fn_name
              spec_occ   = mkSpecOcc fn_occ
              -- We use fn_occ rather than fn in the rule_name string
              -- as we don't want the uniq to end up in the rule, and
              -- hence in the ABI, as that can cause spurious ABI
              -- changes (#4012).
              rule_name  = mkFastString ("SC:" ++ occNameString fn_occ ++ show rule_number)
              spec_name  = mkInternalName spec_uniq spec_occ fn_loc
--      ; pprTrace "{spec_one" (ppr (sc_count env) <+> ppr fn
--                              <+> ppr pats <+> text "-->" <+> ppr spec_name) $
--        return ()

        -- Specialise the body
        ; (spec_usg, spec_body) <- scExpr spec_env body

--      ; pprTrace "done spec_one}" (ppr fn) $
--        return ()

                -- And build the results
        ; let (spec_lam_args, spec_call_args) = mkWorkerArgs (sc_dflags env)
                                                             qvars body_ty
                -- Usual w/w hack to avoid generating
                -- a spec_rhs of unlifted type and no args

              spec_lam_args_str = handOutStrictnessInformation (fst (splitStrictSig spec_str)) spec_lam_args
                -- Annotate the variables with the strictness information from
                -- the function (see Note [Strictness information in worker binders])

              spec_join_arity | isJoinId fn = Just (length spec_lam_args)
                              | otherwise   = Nothing
              spec_id    = mkLocalId spec_name
                                     (mkLamTypes spec_lam_args body_ty)
                             -- See Note [Transfer strictness]
                             `setIdStrictness` spec_str
                             `setIdCprInfo` topCprSig
                             `setIdArity` count isId spec_lam_args
                             `asJoinId_maybe` spec_join_arity
              spec_str   = calcSpecStrictness fn spec_lam_args pats


                -- Conditionally use result of new worker-wrapper transform
              spec_rhs   = mkLams spec_lam_args_str spec_body
              body_ty    = exprType spec_body
              rule_rhs   = mkVarApps (Var spec_id) spec_call_args
              inline_act = idInlineActivation fn
              this_mod   = sc_module spec_env
              rule       = mkRule this_mod True {- Auto -} True {- Local -}
                                  rule_name inline_act fn_name qvars pats rule_rhs
                           -- See Note [Transfer activation]
        ; return (spec_usg, OS { os_pat = call_pat, os_rule = rule
                               , os_id = spec_id
                               , os_rhs = spec_rhs }) }


-- See Note [Strictness information in worker binders]
handOutStrictnessInformation :: [Demand] -> [Var] -> [Var]
handOutStrictnessInformation = go
  where
    go _ [] = []
    go [] vs = vs
    go (d:dmds) (v:vs) | isId v = setIdDemandInfo v d : go dmds vs
    go dmds (v:vs) = v : go dmds vs

calcSpecStrictness :: Id                     -- The original function
                   -> [Var] -> [CoreExpr]    -- Call pattern
                   -> StrictSig              -- Strictness of specialised thing
-- See Note [Transfer strictness]
calcSpecStrictness fn qvars pats
  = mkClosedStrictSig spec_dmds div
  where
    spec_dmds = [ lookupVarEnv dmd_env qv `orElse` topDmd | qv <- qvars, isId qv ]
    StrictSig (DmdType _ dmds div) = idStrictness fn

    dmd_env = go emptyVarEnv dmds pats

    go :: DmdEnv -> [Demand] -> [CoreExpr] -> DmdEnv
    go env ds (Type {} : pats)     = go env ds pats
    go env ds (Coercion {} : pats) = go env ds pats
    go env (d:ds) (pat : pats)     = go (go_one env d pat) ds pats
    go env _      _                = env

    go_one :: DmdEnv -> Demand -> CoreExpr -> DmdEnv
    go_one env d   (Var v) = extendVarEnv_C bothDmd env v d
    go_one env d e
           | Just ds <- splitProdDmd_maybe d  -- NB: d does not have to be strict
           , (Var _, args) <- collectArgs e = go env ds args
    go_one env _         _ = env

{-
Note [spec_usg includes rhs_usg]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In calls to 'specialise', the returned ScUsage must include the rhs_usg in
the passed-in SpecInfo, unless there are no calls at all to the function.

The caller can, indeed must, assume this.  He should not combine in rhs_usg
himself, or he'll get rhs_usg twice -- and that can lead to an exponential
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
  Note [Auto-specialisation and RULES] in GHC.Core.Op.Specialise.

In which phase should the specialise-constructor rules be active?
Originally I made them always-active, but Manuel found that this
defeated some clever user-written rules.  Then I made them active only
in Phase 0; after all, currently, the specConstr transformation is
only run after the simplifier has reached Phase 0, but that meant
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
It's important that it doesn't have
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
    in ... f (K @(a |> co)) ...

where 'co' is a coercion variable not in scope at f's definition site.
If we aren't caereful we'll get

    let $sf a co = e (K @(a |> co))
        RULE "SC:f" forall a co.  f (K @(a |> co)) = $sf a co
        f = e
    in ...

But alas, when we match the call we won't bind 'co', because type-matching
(for good reasons) discards casts).

I don't know how to solve this, so for now I'm just discarding any
call patterns that
  * Mentions a coercion variable in a type argument
  * That is not in scope at the binding of the function

I think this is very rare.

It is important (e.g. #14936) that this /only/ applies to
coercions mentioned in casts.  We don't want to be discombobulated
by casts in terms!  For example, consider
   f ((e1,e2) |> sym co)
where, say,
   f  :: Foo -> blah
   co :: Foo ~R (Int,Int)

Here we definitely do want to specialise for that pair!  We do not
match on the structure of the coercion; instead we just match on a
coercion variable, so the RULE looks like

   forall (x::Int, y::Int, co :: (Int,Int) ~R Foo)
     f ((x,y) |> co) = $sf x y co

Often the body of f looks like
   f arg = ...(case arg |> co' of
                (x,y) -> blah)...

so that the specialised f will turn into
   $sf x y co = let arg = (x,y) |> co
                in ...(case arg>| co' of
                         (x,y) -> blah)....

which will simplify to not use 'co' at all.  But we can't guarantee
that co will end up unused, so we still pass it.  Absence analysis
may remove it later.

Note that this /also/ discards the call pattern if we have a cast in a
/term/, although in fact Rules.match does make a very flaky and
fragile attempt to match coercions.  e.g. a call like
    f (Maybe Age) (Nothing |> co) blah
    where co :: Maybe Int ~ Maybe Age
will be discarded.  It's extremely fragile to match on the form of a
coercion, so I think it's better just not to try.  A more complicated
alternative would be to discard calls that mention coercion variables
only in kind-casts, but I'm doing the simple thing for now.
-}

type CallPat = ([Var], [CoreExpr])      -- Quantified variables and arguments
                                        -- See Note [SpecConstr call patterns]

callsToNewPats :: ScEnv -> Id
               -> SpecInfo
               -> [ArgOcc] -> [Call]
               -> UniqSM (Bool, [CallPat])
        -- Result has no duplicate patterns,
        -- nor ones mentioned in done_pats
        -- Bool indicates that there was at least one boring pattern
callsToNewPats env fn spec_info@(SI { si_specs = done_specs }) bndr_occs calls
  = do  { mb_pats <- mapM (callToPats env bndr_occs) calls

        ; let have_boring_call = any isNothing mb_pats

              good_pats :: [CallPat]
              good_pats = catMaybes mb_pats

              -- Remove patterns we have already done
              new_pats = filterOut is_done good_pats
              is_done p = any (samePat p . os_pat) done_specs

              -- Remove duplicates
              non_dups = nubBy samePat new_pats

              -- Remove ones that have too many worker variables
              small_pats = filterOut too_big non_dups
              too_big (vars,_) = not (isWorkerSmallEnough (sc_dflags env) vars)
                  -- We are about to construct w/w pair in 'spec_one'.
                  -- Omit specialisation leading to high arity workers.
                  -- See Note [Limit w/w arity] in GHC.Core.Op.WorkWrap.Lib

                -- Discard specialisations if there are too many of them
              trimmed_pats = trim_pats env fn spec_info small_pats

--        ; pprTrace "callsToPats" (vcat [ text "calls to" <+> ppr fn <> colon <+> ppr calls
--                                       , text "done_specs:" <+> ppr (map os_pat done_specs)
--                                       , text "good_pats:" <+> ppr good_pats ]) $
--          return ()

        ; return (have_boring_call, trimmed_pats) }


trim_pats :: ScEnv -> Id -> SpecInfo -> [CallPat] -> [CallPat]
-- See Note [Choosing patterns]
trim_pats env fn (SI { si_n_specs = done_spec_count }) pats
  | sc_force env
    || isNothing mb_scc
    || n_remaining >= n_pats
  = -- pprTrace "trim_pats: no-trim" (ppr (sc_force env) $$ ppr mb_scc $$ ppr n_remaining $$ ppr n_pats)
    pats          -- No need to trim

  | otherwise
  = emit_trace $  -- Need to trim, so keep the best ones
    take n_remaining sorted_pats

  where
    n_pats         = length pats
    spec_count'    = n_pats + done_spec_count
    n_remaining    = max_specs - done_spec_count
    mb_scc         = sc_count env
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
    pat_cons (qs, ps) = foldr ((+) . n_cons) 0 ps
       where
          q_set = mkVarSet qs
          n_cons (Var v) | v `elemVarSet` q_set = 0
                         | otherwise            = 1
          n_cons (Cast e _)  = n_cons e
          n_cons (App e1 e2) = n_cons e1 + n_cons e2
          n_cons (Lit {})    = 1
          n_cons _           = 0

    emit_trace result
       | debugIsOn || hasPprDebug (sc_dflags env)
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


callToPats :: ScEnv -> [ArgOcc] -> Call -> UniqSM (Maybe CallPat)
        -- The [Var] is the variables to quantify over in the rule
        --      Type variables come first, since they may scope
        --      over the following term variables
        -- The [CoreExpr] are the argument patterns for the rule
callToPats env bndr_occs call@(Call _ args con_env)
  | args `ltLength` bndr_occs      -- Check saturated
  = return Nothing
  | otherwise
  = do  { let in_scope = substInScope (sc_subst env)
        ; (interesting, pats) <- argsToPats env in_scope con_env args bndr_occs
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
                -- See Note [Shadowing] at the top

              (ktvs, ids)   = partition isTyVar qvars
              qvars'        = scopedSort ktvs ++ map sanitise ids
                -- Order into kind variables, type variables, term variables
                -- The kind of a type variable may mention a kind variable
                -- and the type of a term variable may mention a type variable

              sanitise id   = id `setIdType` expandTypeSynonyms (idType id)
                -- See Note [Free type variables of the qvar types]

              -- Bad coercion variables: see Note [SpecConstr and casts]
              bad_covars :: CoVarSet
              bad_covars = mapUnionVarSet get_bad_covars pats
              get_bad_covars :: CoreArg -> CoVarSet
              get_bad_covars (Type ty)
                = filterVarSet (\v -> isId v && not (is_in_scope v)) $
                  tyCoVarsOfType ty
              get_bad_covars _
                = emptyVarSet

        ; -- pprTrace "callToPats"  (ppr args $$ ppr bndr_occs) $
          WARN( not (isEmptyVarSet bad_covars)
              , text "SpecConstr: bad covars:" <+> ppr bad_covars
                $$ ppr call )
          if interesting && isEmptyVarSet bad_covars
          then return (Just (qvars', pats))
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
         -> UniqSM (Bool, CoreArg)

-- Returns (interesting, pat),
-- where pat is the pattern derived from the argument
--            interesting=True if the pattern is non-trivial (not a variable or type)
-- E.g.         x:xs         --> (True, x:xs)
--              f xs         --> (False, w)        where w is a fresh wildcard
--              (f xs, 'c')  --> (True, (w, 'c'))  where w is a fresh wildcard
--              \x. x+y      --> (True, \x. x+y)
--              lvl7         --> (True, lvl7)      if lvl7 is bound
--                                                 somewhere further out

argToPat _env _in_scope _val_env arg@(Type {}) _arg_occ
  = return (False, arg)

argToPat env in_scope val_env (Tick _ arg) arg_occ
  = argToPat env in_scope val_env arg arg_occ
        -- Note [Notes in call patterns]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- Ignore Notes.  In particular, we want to ignore any InlineMe notes
        -- Perhaps we should not ignore profiling notes, but I'm going to
        -- ride roughshod over them all for now.
        --- See Note [Notes in RULE matching] in GHC.Core.Rules

argToPat env in_scope val_env (Let _ arg) arg_occ
  = argToPat env in_scope val_env arg arg_occ
        -- See Note [Matching lets] in Rule.hs
        -- Look through let expressions
        -- e.g.         f (let v = rhs in (v,w))
        -- Here we can specialise for f (v,w)
        -- because the rule-matcher will look through the let.

{- Disabled; see Note [Matching cases] in Rule.hs
argToPat env in_scope val_env (Case scrut _ _ [(_, _, rhs)]) arg_occ
  | exprOkForSpeculation scrut  -- See Note [Matching cases] in Rule.hhs
  = argToPat env in_scope val_env rhs arg_occ
-}

argToPat env in_scope val_env (Cast arg co) arg_occ
  | not (ignoreType env ty2)
  = do  { (interesting, arg') <- argToPat env in_scope val_env arg arg_occ
        ; if not interesting then
                wildCardPat ty2
          else do
        { -- Make a wild-card pattern for the coercion
          uniq <- getUniqueM
        ; let co_name = mkSysTvName uniq (fsLit "sg")
              co_var  = mkCoVar co_name (mkCoercionType Representational ty1 ty2)
        ; return (interesting, Cast arg' (mkCoVarCo co_var)) } }
  where
    Pair ty1 ty2 = coercionKind co



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

  -- Check for a constructor application
  -- NB: this *precedes* the Var case, so that we catch nullary constrs
argToPat env in_scope val_env arg arg_occ
  | Just (ConVal (DataAlt dc) args) <- isValue val_env arg
  , not (ignoreDataCon env dc)        -- See Note [NoSpecConstr]
  , Just arg_occs <- mb_scrut dc
  = do  { let (ty_args, rest_args) = splitAtList (dataConUnivTyVars dc) args
        ; (_, args') <- argsToPats env in_scope val_env rest_args arg_occs
        ; return (True,
                  mkConApp dc (ty_args ++ args')) }
  where
    mb_scrut dc = case arg_occ of
                    ScrutOcc bs | Just occs <- lookupUFM bs dc
                                -> Just (occs)  -- See Note [Reboxing]
                    _other      | sc_force env || sc_keen env
                                -> Just (repeat UnkOcc)
                                | otherwise
                                -> Nothing

  -- Check if the argument is a variable that
  --    (a) is used in an interesting way in the function body
  --    (b) we know what its value is
  -- In that case it counts as "interesting"
argToPat env in_scope val_env (Var v) arg_occ
  | sc_force env || case arg_occ of { UnkOcc -> False; _other -> True }, -- (a)
    is_value,                                                            -- (b)
       -- Ignoring sc_keen here to avoid gratuitously incurring Note [Reboxing]
       -- So sc_keen focused just on f (I# x), where we have freshly-allocated
       -- box that we can eliminate in the caller
    not (ignoreType env (varType v))
  = return (True, Var v)
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

  -- The default case: make a wild-card
  -- We use this for coercions too
argToPat _env _in_scope _val_env arg _arg_occ
  = wildCardPat (exprType arg)

wildCardPat :: Type -> UniqSM (Bool, CoreArg)
wildCardPat ty
  = do { uniq <- getUniqueM
       ; let id = mkSysLocalOrCoVar (fsLit "sc") uniq ty
       ; return (False, varToCoreExpr id) }

argsToPats :: ScEnv -> InScopeSet -> ValueEnv
           -> [CoreArg] -> [ArgOcc]  -- Should be same length
           -> UniqSM (Bool, [CoreArg])
argsToPats env in_scope val_env args occs
  = do { stuff <- zipWithM (argToPat env in_scope val_env) args occs
       ; let (interesting_s, args') = unzip stuff
       ; return (or interesting_s, args') }

isValue :: ValueEnv -> CoreExpr -> Maybe Value
isValue _env (Lit lit)
  | litIsLifted lit = Nothing
  | otherwise       = Just (ConVal (LitAlt lit) [])

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
  = case isDataConWorkId_maybe fun of

        Just con | args `lengthAtLeast` dataConRepArity con
                -- Check saturated; might be > because the
                --                  arity excludes type args
                -> Just (ConVal (DataAlt con) args)

        _other | valArgCount args < idArity fun
                -- Under-applied function
               -> Just LambdaVal        -- Partial application

        _other -> Nothing

isValue _env _expr = Nothing

valueIsWorkFree :: Value -> Bool
valueIsWorkFree LambdaVal       = True
valueIsWorkFree (ConVal _ args) = all exprIsWorkFree args

samePat :: CallPat -> CallPat -> Bool
samePat (vs1, as1) (vs2, as2)
  = all2 same as1 as2
  where
    same (Var v1) (Var v2)
        | v1 `elem` vs1 = v2 `elem` vs2
        | v2 `elem` vs2 = False
        | otherwise     = v1 == v2

    same (Lit l1)    (Lit l2)    = l1==l2
    same (App f1 a1) (App f2 a2) = same f1 f2 && same a1 a2

    same (Type {}) (Type {}) = True     -- Note [Ignore type differences]
    same (Coercion {}) (Coercion {}) = True
    same (Tick _ e1) e2 = same e1 e2  -- Ignore casts and notes
    same (Cast e1 _) e2 = same e1 e2
    same e1 (Tick _ e2) = same e1 e2
    same e1 (Cast e2 _) = same e1 e2

    same e1 e2 = WARN( bad e1 || bad e2, ppr e1 $$ ppr e2)
                 False  -- Let, lambda, case should not occur
    bad (Case {}) = True
    bad (Let {})  = True
    bad (Lam {})  = True
    bad _other    = False

{-
Note [Ignore type differences]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not want to generate specialisations where the call patterns
differ only in their type arguments!  Not only is it utterly useless,
but it also means that (with polymorphic recursion) we can generate
an infinite number of specialisations. Example is Data.Sequence.adjustTree,
I think.
-}
