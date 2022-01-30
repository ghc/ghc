{-# LANGUAGE PatternSynonyms    #-}

{-|
This module defines the semi-ring of multiplicities, and associated functions.
Multiplicities annotate arrow types to indicate the linearity of the
arrow (in the sense of linear types).

Mult is a type synonym for Type, used only when its kind is Multiplicity.
To simplify dealing with multiplicities, functions such as
mkMultMul perform simplifications such as Many * x = Many on the fly.
-}
module GHC.Core.Multiplicity
  ( Mult
  , pattern One
  , pattern Many
  , isMultMul
  , mkMultAdd
  , mkMultMul
  , mkMultSup
  , Scaled(..)
  , scaledMult
  , scaledThing
  , unrestricted
  , linear
  , tymult
  , irrelevantMult
  , mkScaled
  , scaledSet
  , scaleScaled
  , IsSubmult(..)
  , submult
  , mapScaledType) where

import GHC.Prelude

import GHC.Utils.Outputable
import GHC.Core.TyCo.Rep
import {-# SOURCE #-} GHC.Builtin.Types ( multMulTyCon )
import GHC.Core.Type
import GHC.Builtin.Names (multMulTyConKey)
import GHC.Types.Unique (hasKey)

{-
Note [Linear types]
~~~~~~~~~~~~~~~~~~~
This module is the entry point for linear types.

The detailed design is in the _Linear Haskell_ article
[https://arxiv.org/abs/1710.09756]. Other important resources in the linear
types implementation wiki page
[https://gitlab.haskell.org/ghc/ghc/wikis/linear-types/implementation], and the
proposal [https://github.com/ghc-proposals/ghc-proposals/pull/111] which
describes the concrete design at length.

For the busy developer, though, here is a high-level view of linear types is the following:

- Function arrows are annotated with a multiplicity (as defined by type `Mult`
  and its smart constructors in this module)
    - Because, as a type constructor, the type of function now has an extra
      argument, the notation (->) is no longer suitable. We named the function
      type constructor `FUN`.
    - (->) retains its backward compatible meaning: `(->) a b = a -> b`. To
      achieve this, `(->)` is defined as a type synonym to `FUN Many` (see
      below).
- Multiplicities can be reified in Haskell as types of kind
  `GHC.Types.Multiplicity`
- Ground multiplicity (that is, without a variable) can be `One` or `Many`
  (`Many` is generally rendered as ω in the scientific literature).
  Functions whose type is annotated with `One` are linear functions, functions whose
  type is annotated with `Many` are regular functions, often called “unrestricted”
  to contrast them with linear functions.
- A linear function is defined as a function such that *if* its result is
  consumed exactly once, *then* its argument is consumed exactly once. You can
  think of “consuming exactly once” as evaluating a value in normal form exactly
  once (though not necessarily in one go). The _Linear Haskell_ article (see
  infra) has a more precise definition of “consuming exactly once”.
- Data types can have unrestricted fields (the canonical example being the
  `Unrestricted` data type), then these don't need to be consumed for a value to
  be consumed exactly once. So consuming a value of type `Unrestricted` exactly
  once means forcing it at least once.
- Why “at least once”? Because if `case u of { C x y -> f (C x y) }` is linear
  (provided `f` is a linear function). So we might as well have done `case u of
  { !z -> f z }`. So, we can observe constructors as many times as we want, and
  we are actually allowed to force the same thing several times because laziness
  means that we are really forcing a the value once, and observing its
  constructor several times. The type checker and the linter recognise some (but
  not all) of these multiple forces as indeed linear. Mostly just enough to
  support variable patterns.
- Multiplicities form a semiring.
- Multiplicities can also be variables and we can universally quantify over
  these variables. This is referred to as “multiplicity
  polymorphism”. Furthermore, multiplicity can be formal semiring expressions
  combining variables.
- Contrary to the paper, the sum of two multiplicities is always `Many`. This
  will have to change, however, if we want to add a multiplicity for 0. Whether
  we want to is still debated.
- Case expressions have a multiplicity annotation too. A case expression with
  multiplicity `One`, consumes its scrutinee exactly once (provided the entire
  case expression is consumed exactly once); whereas a case expression with
  multiplicity `Many` can consume its scrutinee as many time as it wishes (no
  matter how much the case expression is consumed).

Note [Usages]
~~~~~~~~~~~~~
In the _Linear Haskell_ paper, you'll find typing rules such as these:

    Γ ⊢ f : A #π-> B  Δ ⊢ u : A
    ---------------------------
        Γ + kΔ ⊢ f u : B

If you read this as a type-checking algorithm going from the bottom up, this
reads as: the algorithm has to find a split of some input context Ξ into an
appropriate Γ and a Δ such as Ξ = Γ + kΔ, *and the multiplicities are chosen to
make f and u typecheck*.

This could be achieved by letting the typechecking of `f` use exactly the
variable it needs, then passing the remainder, as `Delta` to the typechecking of
u. But what does that mean if `x` is bound with multiplicity `p` (a variable)
and `f` consumes `x` once? `Delta` would have to contain `x` with multiplicity
`p-1`. It's not really clear how to make that works. In summary: bottom-up
multiplicity checking forgoes addition and multiplication in favour of
subtraction and division. And variables make the latter hard.

The alternative is to read multiplicities from the top down: as an *output* from
the typechecking algorithm, rather than an input. We call these output
multiplicities Usages, to distinguish them from the multiplicities which come,
as input, from the types of functions. Usages are checked for compatibility with
multiplicity annotations using an ordering relation. In other words, the usage
of x in the expression u is the smallest multiplicity which can be ascribed to x
for u to typecheck.

Usages are usually group in a UsageEnv, as defined in the UsageEnv module.

So, in our function application example, the typechecking algorithm would
receive usage environements f_ue from the typechecking of f, and u_ue from the
typechecking of u. Then the output would be f_ue + (k * u_ue). Addition and
scaling of usage environment is the pointwise extension of the semiring
operations on multiplicities.

Note [Zero as a usage]
~~~~~~~~~~~~~~~~~~~~~~
In the current presentation usages are not exactly multiplicities, because they
can contain 0, and multiplicities can't.

Why do we need a 0 usage? A function which doesn't use its argument will be
required to annotate it with `Many`:

    \(x % Many) -> 0

However, we cannot replace absence with Many when computing usages
compositionally: in

    (x, True)

We expect x to have usage 1. But when computing the usage of x in True we would
find that x is absent, hence has multiplicity Many. The final multiplicity would
be One+Many = Many. Oops!

Hence there is a usage Zero for absent variables. Zero is characterised by being
the neutral element to usage addition.

We may decide to add Zero as a multiplicity in the future. In which case, this
distinction will go away.

Note [Joining usages]
~~~~~~~~~~~~~~~~~~~~~
The usage of a variable is defined, in Note [Usages], as the minimum usage which
can be ascribed to a variable.

So what is the usage of x in

    case … of
      { p1 -> u   -- usage env: u_ue
      ; p2 -> v } -- usage env: v_ue

It must be the least upper bound, or _join_, of u_ue(x) and v_ue(x).

So, contrary to a declarative presentation where the correct usage of x can be
conjured out of thin air, we need to be able to compute the join of two
multiplicities. Join is extended pointwise on usage environments.

Note [Bottom as a usage]
~~~~~~~~~~~~~~~~~~~~~~
What is the usage of x in

   case … of {}

Per usual linear logic, as well as the _Linear Haskell_ article, x can have
every multiplicity.

So we need a minimum usage _bottom_, which is also the neutral element for join.

In fact, this is not such as nice solution, because it is not clear how to
define sum and multiplication with bottom. We give reasonable definitions, but
they are not complete (they don't respect the semiring laws, and it's possible
to come up with examples of Core transformation which are not well-typed)

A better solution would probably be to annotate case expressions with a usage
environment, just like they are annotated with a type. Which, probably not
coincidentally, is also primarily for empty cases.

A side benefit of this approach is that the linter would not need to join
multiplicities, anymore; hence would be closer to the presentation in the
article. That's because it could use the annotation as the multiplicity for each
branch.

Note [Data constructors are linear by default]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Data constructors defined without -XLinearTypes (as well as data constructors
defined with the Haskell 98 in all circumstances) have all their fields linear.

That is, in

    data Maybe a = Nothing | Just a

We have

    Just :: a %1 -> Just a

The goal is to maximise reuse of types between linear code and traditional
code. This is argued at length in the proposal and the article (links in Note
[Linear types]).

Note [Polymorphisation of linear fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The choice in Note [Data constructors are linear by default] has an impact on
backwards compatibility. Consider

    map Just

We have

    map :: (a -> b) -> f a -> f b
    Just :: a %1 -> Just a

Types don't match, we should get a type error. But this is legal Haskell 98
code! Bad! Bad! Bad!

It could be solved with subtyping, but subtyping doesn't combine well with
polymorphism. Instead, we generalise the type of Just, when used as term:

   Just :: forall {p}. a %p-> Just a

This is solely a concern for higher-order code like this: when called fully
applied linear constructors are more general than constructors with unrestricted
fields. In particular, linear constructors can always be eta-expanded to their
Haskell 98 type. This is explained in the paper (but there, we had a different
strategy to resolve this type mismatch in higher-order code. It turned out to be
insufficient, which is explained in the wiki page as well as the proposal).

We only generalise linear fields this way: fields with multiplicity Many, or
other multiplicity expressions are exclusive to -XLinearTypes, hence don't have
backward compatibility implications.

The implementation is described in Note [Typechecking data constructors]
in GHC.Tc.Gen.Head.

More details in the proposal.
-}

{-
Note [Adding new multiplicities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To add a new multiplicity, you need to:
* Add the new type with Multiplicity kind
* Update cases in mkMultAdd, mkMultMul, mkMultSup, submult, tcSubMult
* Check supUE function that computes sup of a multiplicity
  and Zero
-}

isMultMul :: Mult -> Maybe (Mult, Mult)
isMultMul ty | Just (tc, [x, y]) <- splitTyConApp_maybe ty
             , tc `hasKey` multMulTyConKey = Just (x, y)
             | otherwise = Nothing

{-
Note [Overapproximating multiplicities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The functions mkMultAdd, mkMultMul, mkMultSup perform operations
on multiplicities. They can return overapproximations: their result
is merely guaranteed to be a submultiplicity of the actual value.

They should be used only when an upper bound is acceptable.
In most cases, they are used in usage environments (UsageEnv);
in usage environments, replacing a usage with a larger one can only
cause more programs to fail to typecheck.

In future work, instead of approximating we might add type families
and allow users to write types involving operations on multiplicities.
In this case, we could enforce more invariants in Mult, for example,
enforce that it is in the form of a sum of products, and even
that the summands and factors are ordered somehow, to have more equalities.
-}

-- With only two multiplicities One and Many, we can always replace
-- p + q by Many. See Note [Overapproximating multiplicities].
mkMultAdd :: Mult -> Mult -> Mult
mkMultAdd _ _ = Many

mkMultMul :: Mult -> Mult -> Mult
mkMultMul One p = p
mkMultMul p One = p
mkMultMul Many _ = Many
mkMultMul _ Many = Many
mkMultMul p q = mkTyConApp multMulTyCon [p, q]

scaleScaled :: Mult -> Scaled a -> Scaled a
scaleScaled m' (Scaled m t) = Scaled (m' `mkMultMul` m) t

-- See Note [Joining usages]
-- | @mkMultSup w1 w2@ returns a multiplicity such that @mkMultSup w1
-- w2 >= w1@ and @mkMultSup w1 w2 >= w2@. See Note [Overapproximating multiplicities].
mkMultSup :: Mult -> Mult -> Mult
mkMultSup = mkMultMul
-- Note: If you are changing this logic, check 'supUE' in UsageEnv as well.

--
-- * Multiplicity ordering
--

data IsSubmult = Submult     -- Definitely a submult
               | Unknown     -- Could be a submult, need to ask the typechecker
               deriving (Show, Eq)

instance Outputable IsSubmult where
  ppr = text . show

-- | @submult w1 w2@ check whether a value of multiplicity @w1@ is allowed where a
-- value of multiplicity @w2@ is expected. This is a partial order.

submult :: Mult -> Mult -> IsSubmult
submult _     Many = Submult
submult One   One  = Submult
-- The 1 <= p rule
submult One   _    = Submult
submult _     _    = Unknown
