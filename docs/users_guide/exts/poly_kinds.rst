.. _type-in-type:
.. _kind-polymorphism:

Kind polymorphism
==================================

.. extension:: TypeInType
    :shortdesc: Deprecated. Enable kind polymorphism and datatype promotion.

    :implies: :extension:`PolyKinds`, :extension:`DataKinds`, :extension:`KindSignatures`
    :since: 8.0.1

    The extension :extension:`TypeInType` is now deprecated: its sole effect is
    to switch on :extension:`PolyKinds`
    (and hence :extension:`KindSignatures`) and :extension:`DataKinds`.

.. extension:: PolyKinds
    :shortdesc: Enable kind polymorphism.
        Implies :extension:`KindSignatures`.

    :implies: :extension:`KindSignatures`
    :since: 7.4.1

    Allow kind polymorphic types.

This section describes GHC's kind system, as it appears in version 8.0 and beyond.
The kind system as described here is always in effect, with or without extensions,
although it is a conservative extension beyond standard Haskell. The extensions
above simply enable syntax and tweak the inference algorithm to allow users to
take advantage of the extra expressiveness of GHC's kind system.

Overview of kind polymorphism
-----------------------------

Consider inferring the kind for ::

  data App f a = MkApp (f a)

In Haskell 98, the inferred kind for ``App`` is ``(Type -> Type) -> Type ->
Type``. But this is overly specific, because another suitable Haskell 98 kind
for ``App`` is ``((Type -> Type) -> Type) -> (Type -> Type) -> Type``, where the
kind assigned to ``a`` is ``Type -> Type``. Indeed, without kind signatures
(:extension:`KindSignatures`), it is necessary to use a dummy constructor to get
a Haskell compiler to infer the second kind. With kind polymorphism
(:extension:`PolyKinds`), GHC infers the kind ``forall k. (k -> Type) -> k ->
Type`` for ``App``, which is its most general kind.

Thus, the chief benefit of kind polymorphism is that we can now infer these
most general kinds and use ``App`` at a variety of kinds: ::

  App Maybe Int   -- `k` is instantiated to Type

  data T a = MkT (a Int)    -- `a` is inferred to have kind (Type -> Type)
  App T Maybe     -- `k` is instantiated to (Type -> Type)

Overview of Type-in-Type
------------------------

GHC 8 extends the idea of kind polymorphism by declaring that types and kinds
are indeed one and the same. Nothing within GHC distinguishes between types
and kinds. Another way of thinking about this is that the type ``Bool`` and
the "promoted kind" ``Bool`` are actually identical. (Note that term
``True`` and the type ``'True`` are still distinct, because the former can
be used in expressions and the latter in types.) This lack of distinction
between types and kinds is a hallmark of dependently typed languages.
Full dependently typed languages also remove the difference between expressions
and types, but doing that in GHC is a story for another day.

One simplification allowed by combining types and kinds is that the type of
``Type`` is just ``Type``. It is true that the ``Type :: Type`` axiom can lead
to non-termination, but this is not a problem in GHC, as we already have other
means of non-terminating programs in both types and expressions. This decision
(among many, many others) *does* mean that despite the expressiveness of GHC's
type system, a "proof" you write in Haskell is not an irrefutable mathematical
proof. GHC promises only partial correctness, that if your programs compile and
run to completion, their results indeed have the types assigned. It makes no
claim about programs that do not finish in a finite amount of time.

To learn more about this decision and the design of GHC under the hood
please see the `paper <http://www.seas.upenn.edu/~sweirich/papers/fckinds.pdf>`__
introducing this kind system to GHC/Haskell.

Principles of kind inference
----------------------------

Generally speaking, when :extension:`PolyKinds` is on, GHC tries to infer the
most general kind for a declaration.
In many cases (for example, in a datatype declaration)
the definition has a right-hand side to inform kind
inference. But that is not always the case. Consider ::

    type family F a

Type family declarations have no right-hand side, but GHC must still
infer a kind for ``F``. Since there are no constraints, it could infer
``F :: forall k1 k2. k1 -> k2``, but that seems *too* polymorphic. So
GHC defaults those entirely-unconstrained kind variables to ``Type`` and we
get ``F :: Type -> Type``. You can still declare ``F`` to be kind-polymorphic
using kind signatures: ::

    type family F1 a                -- F1 :: Type -> Type
    type family F2 (a :: k)         -- F2 :: forall k. k -> Type
    type family F3 a :: k           -- F3 :: forall k. Type -> k
    type family F4 (a :: k1) :: k2  -- F4 :: forall k1 k2. k1 -> k2

The general principle is this:

-  When there is a right-hand side, GHC infers the most polymorphic
   kind consistent with the right-hand side. Examples: ordinary data
   type and GADT declarations, class declarations. In the case of a
   class declaration the role of "right hand side" is played by the
   class method signatures.

-  When there is no right hand side, GHC defaults argument and result
   kinds to ``Type``, except when directed otherwise by a kind signature.
   Examples: data and open type family declarations.

This rule has occasionally-surprising consequences (see
:ghc-ticket:`10132`). ::

    class C a where    -- Class declarations are generalised
                       -- so C :: forall k. k -> Constraint
      data D1 a        -- No right hand side for these two family
      type F1 a        -- declarations, but the class forces (a :: k)
                       -- so   D1, F1 :: forall k. k -> Type

    data D2 a   -- No right-hand side so D2 :: Type -> Type
    type F2 a   -- No right-hand side so F2 :: Type -> Type

The kind-polymorphism from the class declaration makes ``D1``
kind-polymorphic, but not so ``D2``; and similarly ``F1``, ``F2``.

Kind inference in type signatures
---------------------------------

When kind-checking a type, GHC considers only what is written in that
type when figuring out how to generalise the type's kind.

For example,
consider these definitions (with :extension:`ScopedTypeVariables`): ::

  data Proxy a    -- Proxy :: forall k. k -> Type
  p :: forall a. Proxy a
  p = Proxy :: Proxy (a :: Type)

GHC reports an error, saying that the kind of ``a`` should be a kind variable
``k``, not ``Type``. This is because, by looking at the type signature
``forall a. Proxy a``, GHC assumes ``a``'s kind should be generalised, not
restricted to be ``Type``. The function definition is then rejected for being
more specific than its type signature.

.. _explicit-kind-quantification:

Explicit kind quantification
----------------------------

Enabled by :extension:`PolyKinds`, GHC supports explicit kind quantification,
as in these examples: ::

  data Proxy :: forall k. k -> Type
  f :: (forall k (a :: k). Proxy a -> ()) -> Int

Note that the second example has a ``forall`` that binds both a kind ``k`` and
a type variable ``a`` of kind ``k``. In general, there is no limit to how
deeply nested this sort of dependency can work. However, the dependency must
be well-scoped: ``forall (a :: k) k. ...`` is an error.


.. _inferring-variable-order:


Inferring the order of variables in a type/class declaration
------------------------------------------------------------

It is possible to get intricate dependencies among the type variables
introduced in a type or class declaration. Here is an example::

  data T a (b :: k) c = MkT (a c)

After analysing this declaration, GHC will discover that ``a`` and
``c`` can be kind-polymorphic, with ``a :: k2 -> Type`` and
``c :: k2``. We thus infer the following kind::

  T :: forall {k2 :: Type} (k :: Type). (k2 -> Type) -> k -> k2 -> Type

Note that ``k2`` is placed *before* ``k``, and that ``k`` is placed *before*
``a``. Also, note that ``k2`` is written here in braces. As explained with
:extension:`TypeApplications` (:ref:`inferred-vs-specified`),
type and kind variables that GHC generalises
over, but not written in the original program, are not available for visible
type application. (These are called *inferred* variables.)
Such variables are written in braces.

The general principle is this:

  * Variables not available for type application come first.

  * Then come variables the user has written, implicitly brought into scope
    in a type variable's kind.

  * Lastly come the normal type variables of a declaration.

  * Variables not given an explicit ordering by the user are sorted according
    to ScopedSort (:ref:`ScopedSort`).

With the ``T`` example above, we could bind ``k`` *after* ``a``; doing so
would not violate dependency concerns. However, it would violate our general
principle, and so ``k`` comes first.

Sometimes, this ordering does not respect dependency. For example::

  data T2 k (a :: k) (c :: Proxy '[a, b])

It must be that ``a`` and ``b`` have the same kind. Note also that ``b``
is implicitly declared in ``c``\'s kind. Thus, according to our general
principle, ``b`` must come *before* ``k``. However, ``b`` *depends on*
``k``. We thus reject ``T2`` with a suitable error message.

In associated types, we order the type variables as if the type family was a
top-level declaration, ignoring the visibilities of the class's type variable
binders. Here is an example: ::

  class C (a :: k) b where
    type F (c :: j) (d :: Proxy m) a b

We infer these kinds::

  C :: forall {k1 :: Type} (k :: Type). k -> k1 -> Constraint
  F :: forall {k1 :: Type} {k2 :: Type} {k3 :: Type} j (m :: k1).
       j -> Proxy m -> k2 -> k3 -> Type

Note that the kind of ``a`` is specified in the kind of ``C`` but inferred in
the kind of ``F``.

The "general principle" described here is meant to make all this more
predictable for users. It would not be hard to extend GHC to relax
this principle. If you should want a change here, consider writing
a `proposal <https://github.com/ghc-proposals/ghc-proposals/>`_ to
do so.

.. index::
   single: CUSK
   single: complete user-supplied kind signature

.. _complete-kind-signatures:

Complete user-supplied kind signatures and polymorphic recursion
----------------------------------------------------------------

.. extension:: CUSKs
    :shortdesc: Enable detection of complete user-supplied kind signatures.

    :since: 8.10.1

NB! This is a legacy feature, see :extension:`StandaloneKindSignatures` for the
modern replacement.

Just as in type inference, kind inference for recursive types can only
use *monomorphic* recursion. Consider this (contrived) example: ::

    data T m a = MkT (m a) (T Maybe (m a))
    -- GHC infers kind  T :: (Type -> Type) -> Type -> Type

The recursive use of ``T`` forced the second argument to have kind
``Type``. However, just as in type inference, you can achieve polymorphic
recursion by giving a *complete user-supplied kind signature* (or CUSK)
for ``T``. A CUSK is present when all argument kinds and the result kind
are known, without any need for inference. For example: ::

    data T (m :: k -> Type) :: k -> Type where
      MkT :: m a -> T Maybe (m a) -> T m a

The complete user-supplied kind signature specifies the polymorphic kind
for ``T``, and this signature is used for all the calls to ``T``
including the recursive ones. In particular, the recursive use of ``T``
is at kind ``Type``.

What exactly is considered to be a "complete user-supplied kind
signature" for a type constructor? These are the forms:

-  For a datatype, every type variable must be annotated with a kind. In
   a GADT-style declaration, there may also be a kind signature (with a
   top-level ``::`` in the header), but the presence or absence of this
   annotation does not affect whether or not the declaration has a
   complete signature. ::

       data T1 :: (k -> Type) -> k -> Type       where ...
       -- Yes;  T1 :: forall k. (k->Type) -> k -> Type

       data T2 (a :: k -> Type) :: k -> Type     where ...
       -- Yes;  T2 :: forall k. (k->Type) -> k -> Type

       data T3 (a :: k -> Type) (b :: k) :: Type where ...
       -- Yes;  T3 :: forall k. (k->Type) -> k -> Type

       data T4 (a :: k -> Type) (b :: k)      where ...
       -- Yes;  T4 :: forall k. (k->Type) -> k -> Type

       data T5 a (b :: k) :: Type             where ...
       -- No;  kind is inferred

       data T6 a b                         where ...
       -- No;  kind is inferred

-  For a datatype with a top-level ``::``: all kind variables introduced after
   the ``::`` must be explicitly quantified. ::

     data T1 :: k -> Type            -- No CUSK: `k` is not explicitly quantified
     data T2 :: forall k. k -> Type  -- CUSK: `k` is bound explicitly
     data T3 :: forall (k :: Type). k -> Type   -- still a CUSK

-  For a newtype, the rules are the same as they are for a data type
   unless :extension:`UnliftedNewtypes` is enabled.
   With :extension:`UnliftedNewtypes`, the type constructor
   only has a CUSK if a kind signature is present. As with a datatype
   with a top-level ``::``, all kind variables introduced after
   the ``::`` must be explicitly quantified ::

       {-# LANGUAGE UnliftedNewtypes #-}
       newtype N1 where                 -- No; missing kind signature
       newtype N2 :: TYPE 'IntRep where -- Yes; kind signature present
       newtype N3 (a :: Type) where     -- No; missing kind signature
       newtype N4 :: k -> Type where    -- No; `k` is not explicitly quantified
       newtype N5 :: forall (k :: Type). k -> Type where -- Yes; good signature

-  For a class, every type variable must be annotated with a kind.

-  For a type synonym, every type variable and the result type must all
   be annotated with kinds: ::

       type S1 (a :: k) = (a :: k)    -- Yes   S1 :: forall k. k -> k
       type S2 (a :: k) = a           -- No    kind is inferred
       type S3 (a :: k) = Proxy a     -- No    kind is inferred

   Note that in ``S2`` and ``S3``, the kind of the right-hand side is
   rather apparent, but it is still not considered to have a complete
   signature -- no inference can be done before detecting the signature.

-  An un-associated open type or data family declaration *always* has a CUSK;
   un-annotated type variables default to kind ``Type``: ::

       data family D1 a                  -- D1 :: Type -> Type
       data family D2 (a :: k)           -- D2 :: forall k. k -> Type
       data family D3 (a :: k) :: Type   -- D3 :: forall k. k -> Type
       type family S1 a :: k -> Type     -- S1 :: forall k. Type -> k -> Type

-  An associated type or data family declaration has a CUSK precisely if
   its enclosing class has a CUSK. ::

       class C a where                -- no CUSK
         type AT a b                  -- no CUSK, b is defaulted

       class D (a :: k) where         -- yes CUSK
         type AT2 a b                 -- yes CUSK, b is defaulted

-  A closed type family has a complete signature when all of its type
   variables are annotated and a return kind (with a top-level ``::``)
   is supplied.

It is possible to write a datatype that syntactically has a CUSK (according to
the rules above) but actually requires some inference. As a very contrived
example, consider ::

  data Proxy a           -- Proxy :: forall k. k -> Type
  data X (a :: Proxy k)

According to the rules above ``X`` has a CUSK. Yet, the kind of ``k`` is undetermined.
It is thus quantified over, giving ``X`` the kind ``forall k1 (k :: k1). Proxy k -> Type``.

The detection of CUSKs is enabled by the :extension:`CUSKs` flag, which is
switched on by default. This extension is scheduled for deprecation to be
replaced with :extension:`StandaloneKindSignatures`.

.. index::
   single: standalone kind signature

.. _standalone-kind-signatures:

Standalone kind signatures and polymorphic recursion
----------------------------------------------------

.. extension:: StandaloneKindSignatures
    :shortdesc: Allow the use of standalone kind signatures.

    :implies: :extension:`NoCUSKs`
    :since: 8.10.1

Just as in type inference, kind inference for recursive types can only
use *monomorphic* recursion. Consider this (contrived) example: ::

    data T m a = MkT (m a) (T Maybe (m a))
    -- GHC infers kind  T :: (Type -> Type) -> Type -> Type

The recursive use of ``T`` forced the second argument to have kind
``Type``. However, just as in type inference, you can achieve polymorphic
recursion by giving a *standalone kind signature* for ``T``: ::

    type T :: (k -> Type) -> k -> Type
    data T m a = MkT (m a) (T Maybe (m a))

The standalone kind signature specifies the polymorphic kind
for ``T``, and this signature is used for all the calls to ``T``
including the recursive ones. In particular, the recursive use of ``T``
is at kind ``Type``.

While a standalone kind signature determines the kind of a type constructor, it
does not determine its arity. This is of particular importance for type
families and type synonyms, as they cannot be partially applied. See
:ref:`type-family-declarations` for more information about arity.

The arity can be specified using explicit binders and inline kind annotations::

    -- arity F0 = 0
    type F0 :: forall k. k -> Type
    type family F0 :: forall k. k -> Type

    -- arity F1 = 1
    type F1 :: forall k. k -> Type
    type family F1 :: k -> Type

    -- arity F2 = 2
    type F2 :: forall k. k -> Type
    type family F2 a :: Type

In absence of an inline kind annotation, the inferred arity includes all
explicitly bound parameters and all immediately following invisible
parameters::

    -- arity FD1 = 1
    type FD1 :: forall k. k -> Type
    type FD1

    -- arity FD2 = 2
    type FD2 :: forall k. k -> Type
    type FD2 a

Note that ``F0``, ``F1``, ``F2``, ``FD1``, and ``FD2`` all have identical
standalone kind signatures. The arity is inferred from the type family header.

The kind variables bound by an outermost ``forall`` in a standalone kind
signature scope only over the kind in that signature. Unlike term-level type
signatures (see :ref:`decl-type-sigs`), the outermost kind variables do *not*
scope over the corresponding declaration. For example, given this class
declaration: ::

    class C (a :: k) where
      m :: Proxy k -> Proxy a -> String

The following would *not* be an equivalent definition of ``C``: ::

    type C :: forall k. k -> Constraint
    class C a where
      m :: Proxy k -> Proxy a -> String

Because the ``k`` from the standalone kind signature does not scope over
``C``'s definition, the ``k`` in ``m``'s type signature is no longer the kind
of ``a``, but rather a completely distinct kind. It's as if you had written
this: ::

    type C :: forall k. k -> Constraint
    class C (a :: kindOfA) where
      m :: forall k. Proxy k -> Proxy (a :: kindOfA) -> String

To avoid this issue, ``C``'s definition must be given an inline kind annotation
like so: ::

    type C :: forall k. k -> Constraint
    class C (a :: k) where
      m :: Proxy k -> Proxy a -> String

Standalone kind signatures and declaration headers
--------------------------------------------------

GHC requires that in the presence of a standalone kind signature, data
declarations must bind all their inputs. For example: ::

    type Prox1 :: k -> Type
    data Prox1 a = MkProx1
      -- OK.

    type Prox2 :: k -> Type
    data Prox2 = MkProx2
      -- Error:
      --   • Expected a type, but found something with kind ‘k -> Type’
      --   • In the data type declaration for ‘Prox2’


GADT-style data declarations may either bind their inputs or use an inline
signature in addition to the standalone kind signature: ::

    type GProx1 :: k -> Type
    data GProx1 a where MkGProx1 :: GProx1 a
      -- OK.

    type GProx2 :: k -> Type
    data GProx2 where MkGProx2 :: GProx2 a
      -- Error:
      --   • Expected a type, but found something with kind ‘k -> Type’
      --   • In the data type declaration for ‘GProx2’

    type GProx3 :: k -> Type
    data GProx3 :: k -> Type where MkGProx3 :: GProx3 a
      -- OK.

    type GProx4 :: k -> Type
    data GProx4 :: w where MkGProx4 :: GProx4 a
      -- OK, w ~ (k -> Type)

Classes are subject to the same rules: ::

    type C1 :: Type -> Constraint
    class C1 a
      -- OK.

    type C2 :: Type -> Constraint
    class C2
      -- Error:
      --   • Couldn't match expected kind ‘Constraint’
      --                 with actual kind ‘Type -> Constraint’
      --   • In the class declaration for ‘C2’

On the other hand, type families are exempt from this rule: ::

    type F :: Type -> Type
    type family F
      -- OK.

Data families are tricky territory. Their headers are exempt from this rule,
but their instances are not: ::

    type T :: k -> Type
    data family T
      -- OK.

    data instance T Int = MkT1
      -- OK.

    data instance T = MkT3
      -- Error:
      --   • Expecting one more argument to ‘T’
      --     Expected a type, but ‘T’ has kind ‘k0 -> Type’
      --   • In the data instance declaration for ‘T’

This also applies to GADT-style data instances: ::

    data instance T (a :: Nat) where MkN4 :: T 4
                                     MKN9 :: T 9
      -- OK.

    data instance T :: Symbol -> Type where MkSN :: T "Neptune"
                                            MkSJ :: T "Jupiter"
      -- OK.

    data instance T where MkT4 :: T x
      -- Error:
      --   • Expecting one more argument to ‘T’
      --     Expected a type, but ‘T’ has kind ‘k0 -> Type’
      --   • In the data instance declaration for ‘T’


Kind inference in data type declarations
----------------------------------------

Consider the declaration ::

  data T1 f a = MkT1 (f a)
  data T2 f a where
    MkT2 :: f a -> T f a

In both cases GHC looks at the data constructor declarations to
give constraints on the kind of ``T``, yielding ::

  T1, T2 :: forall k. (k -> Type) -> k -> Type

Consider the type ::

  type G :: forall k. k -> Type
  data G (a :: k) where
    GInt    :: G Int
    GMaybe  :: G Maybe

This datatype ``G`` is GADT-like in both its kind and its type. Suppose you
have ``g :: G a``, where ``a :: k``. Then pattern matching to discover that
``g`` is in fact ``GMaybe`` tells you both that ``k ~ (Type -> Type)`` and
``a ~ Maybe``. The definition for ``G`` requires that :extension:`PolyKinds`
be in effect, but pattern-matching on ``G`` requires no extension beyond
:extension:`GADTs`. That this works is actually a straightforward extension
of regular GADTs and a consequence of the fact that kinds and types are the
same.

Note that the datatype ``G`` is used at different kinds in its body, and
therefore that kind-indexed GADTs use a form of polymorphic recursion.
It is thus only possible to use this feature if you have provided a
complete user-supplied kind signature (CUSK)
for the datatype (:ref:`complete-kind-signatures`), or a standalone
kind signature (:ref:`standalone-kind-signatures`);
in the case of ``G`` we both.
If you wish to see the kind indexing explicitly, you can do so by enabling :ghc-flag:`-fprint-explicit-kinds` and querying ``G`` with GHCi's :ghci-cmd:`:info` command: ::

  > :set -fprint-explicit-kinds
  > :info G
  type role G nominal nominal
  type G :: forall k. k -> Type
  data G @k a where
    GInt   :: G @Type Int
    GMaybe :: G @(Type -> Type) Maybe

where you can see the GADT-like nature of the two constructors.

.. _kind-inference-data-family-instances:

Kind inference for data/newtype instance declarations
-----------------------------------------------------

Consider these declarations ::

   data family T :: forall k. (k->Type) -> k -> Type

   data instance T p q where
      MkT :: forall r. r Int -> T r Int

Here ``T`` has an invisible kind argument; and perhaps it is instantiated
to ``Type`` in the instance, thus::

   data instance T @Type (p :: Type->Type) (q :: Type) where
      MkT :: forall r. r Int -> T r Int

Or perhaps we intended the specialisation to be in the GADT data
constructor, thus::

   data instance T @k (p :: k->Type) (q :: k) where
      MkT :: forall r. r Int -> T @Type r Int

It gets more complicated if there are multiple constructors.  In
general, there is no principled way to tell which type specialisation
comes from the data instance, and which from the individual GADT data
constructors.

So GHC implements this rule: in data/newtype instance declararations
(unlike ordinary data/newtype declarations) we do *not* look at the
constructor declarations when inferring the shape of the instance
header.  The principle is that *the instantiation of the data instance
should be apparent from the header alone*.  This principle makes the
program easier to understand, and avoids the swamp of complexity
indicated above.


Kind inference in class instance declarations
---------------------------------------------

Consider the following example of a poly-kinded class and an instance
for it: ::

    class C a where
      type F a

    instance C b where
      type F b = b -> b

In the class declaration, nothing constrains the kind of the type ``a``,
so it becomes a poly-kinded type variable ``(a :: k)``. Yet, in the
instance declaration, the right-hand side of the associated type
instance ``b -> b`` says that ``b`` must be of kind ``Type``. GHC could
theoretically propagate this information back into the instance head,
and make that instance declaration apply only to type of kind ``Type``, as
opposed to types of any kind. However, GHC does *not* do this.

In short: GHC does *not* propagate kind information from the members of
a class instance declaration into the instance declaration head.

This lack of kind inference is simply an engineering problem within GHC,
but getting it to work would make a substantial change to the inference
infrastructure, and it's not clear the payoff is worth it. If you want
to restrict ``b``\ 's kind in the instance above, just use a kind
signature in the instance head.

Kind inference in type synonyms and type family instances
---------------------------------------------------------

Consider the scoping rules for type synonyms and type family instances, such as
these::

   type          TS a (b :: k) = <rhs>
   type instance TF a (b :: k) = <rhs>

The basic principle is that all variables mentioned on the right hand side
``<rhs>`` must be bound on the left hand side::

  type TS a (b :: k) = (k, a, Proxy b)    -- accepted
  type TS a (b :: k) = (k, a, Proxy b, z) -- rejected: z not in scope

But there is one exception: free variables mentioned in the outermost kind
signature on the right hand side are quantified implicitly. Thus, in the
following example the variables ``a``, ``b``, and ``k`` are all in scope on the
right hand side of ``S``::

  type S a b = <rhs> :: k -> k

The reason for this exception is that there may be no other way to bind ``k``.
For example, suppose we wanted ``S`` to have the following kind with an
*invisible* parameter ``k``::

  S :: forall k. Type -> Type -> k -> k

In this case, we could not simply bind ``k`` on the left-hand side, as ``k``
would become a *visible* parameter::

  type S k a b = <rhs> :: k -> k
  S :: forall k -> Type -> Type -> k -> k

Note that we only look at the *outermost* kind signature to decide which
variables to quantify implicitly. As a counter-example, consider ``M1``: ::

  type M1 = 'Just ('Nothing :: Maybe k)    -- rejected: k not in scope

Here, the kind signature is hidden inside ``'Just``, and there is no outermost
kind signature. We can fix this example by providing an outermost kind signature: ::

  type M2 = 'Just ('Nothing :: Maybe k) :: Maybe (Maybe k)

Here, ``k`` is brought into scope by ``:: Maybe (Maybe k)``.

A kind signature is considered to be outermost regardless of redundant
parentheses: ::

  type P =    'Nothing :: Maybe a    -- accepted
  type P = ((('Nothing :: Maybe a))) -- accepted

Closed type family instances are subject to the same rules: ::

  type family F where
    F = 'Nothing :: Maybe k            -- accepted

  type family F where
    F = 'Just ('Nothing :: Maybe k)    -- rejected: k not in scope

  type family F where
    F = 'Just ('Nothing :: Maybe k) :: Maybe (Maybe k)  -- accepted

  type family F :: Maybe (Maybe k) where
    F = 'Just ('Nothing :: Maybe k)    -- rejected: k not in scope

  type family F :: Maybe (Maybe k) where
    F @k = 'Just ('Nothing :: Maybe k) -- accepted

Kind variables can also be quantified in *visible* positions. Consider the
following two examples: ::

  data ProxyKInvis (a :: k)
  data ProxyKVis k (a :: k)

In the first example, the kind variable ``k`` is an *invisible* argument to
``ProxyKInvis``. In other words, a user does not need to instantiate ``k``
explicitly, as kind inference automatically determines what ``k`` should be.
For instance, in ``ProxyKInvis True``, ``k`` is inferred to be ``Bool``.
This is reflected in the kind of ``ProxyKInvis``: ::

  ProxyKInvis :: forall k. k -> Type

In the second example, ``k`` is a *visible* argument to ``ProxyKVis``. That is
to say, ``k`` is an argument that users must provide explicitly when applying
``ProxyKVis``. For example, ``ProxyKVis Bool True`` is a well formed type.

What is the kind of ``ProxyKVis``? One might say
``forall k. Type -> k -> Type``, but this isn't quite right, since this would
allow incorrect things like ``ProxyKVis Bool Int``, which should be rejected
due to the fact that ``Int`` is not of kind ``Bool``. The key observation is that
the kind of the second argument *depends* on the first argument. GHC indicates
this dependency in the syntax that it gives for the kind of ``ProxyKVis``: ::

  ProxyKVis :: forall k -> k -> Type

This kind is similar to the kind of ``ProxyKInvis``, but with a key difference:
the type variables quantified by the ``forall`` are followed by an arrow
(``->``), not a dot (``.``). This is a visible, dependent quantifier. It is
visible in that the user must pass in a type for ``k`` explicitly, and it is
dependent in the sense that ``k`` appears later in the kind of ``ProxyKVis``.
As a counterpart, the ``k`` binder in ``forall k. k -> Type`` can be thought
of as an *invisible*, dependent quantifier.

GHC permits writing kinds with this syntax, provided that the
``ExplicitForAll`` and ``PolyKinds`` language extensions are enabled. Just
like the invisible ``forall``, one can put explicit kind signatures on visibly
bound kind variables, so the following is syntactically valid: ::

  ProxyKVis :: forall (k :: Type) -> k -> Type

Currently, the ability to write visible, dependent quantifiers is limited to
kinds. Consequently, visible dependent quantifiers are rejected in any context
that is unambiguously the type of a term. They are also rejected in the types
of data constructors.

Kind inference in closed type families
--------------------------------------

Although all open type families are considered to have a complete
user-supplied kind signature (:ref:`complete-kind-signatures`),
we can relax this condition for closed
type families, where we have equations on which to perform kind
inference. GHC will infer kinds for the arguments and result types of a
closed type family.

GHC supports *kind-indexed* type families, where the family matches both
on the kind and type. GHC will *not* infer this behaviour without a
complete user-supplied kind signature or standalone kind
signature (see :ref:`standalone-kind-signatures`),
because doing so would sometimes infer
non-principal types. Indeed, we can see kind-indexing as a form
of polymorphic recursion, where a type is used at a kind other than
its most general in its own definition.

For example: ::

    type family F1 a where
      F1 True  = False
      F1 False = True
      F1 x     = x
    -- F1 fails to compile: kind-indexing is not inferred

    type family F2 (a :: k) where
      F2 True  = False
      F2 False = True
      F2 x     = x
    -- F2 fails to compile: no complete signature

    type family F3 (a :: k) :: k where
      F3 True  = False
      F3 False = True
      F3 x     = x
    -- OK

Higher-rank kinds
-----------------

In concert with :extension:`RankNTypes`, GHC supports higher-rank kinds.
Here is an example::

  -- Heterogeneous propositional equality
  data (a :: k1) :~~: (b :: k2) where
    HRefl :: a :~~: a

  class HTestEquality (t :: forall k. k -> Type) where
    hTestEquality :: forall k1 k2 (a :: k1) (b :: k2). t a -> t b -> Maybe (a :~~: b)

Note that ``hTestEquality`` takes two arguments where the type variable ``t`` is applied
to types of different kinds. That type variable must then be polykinded. Accordingly,
the kind of ``HTestEquality`` (the class) is ``(forall k. k -> Type) -> Constraint``,
a higher-rank kind.

A big difference with higher-rank kinds as compared with higher-rank types is that
``forall``\s in kinds *cannot* be moved. This is best illustrated by example.
Suppose we want to have an instance of ``HTestEquality`` for ``(:~~:)``. ::

  instance HTestEquality ((:~~:) a) where
    hTestEquality HRefl HRefl = Just HRefl

With the declaration of ``(:~~:)`` above, it gets kind ``forall k1 k2. k1 -> k2 -> Type``.
Thus, the type ``(:~~:) a`` has kind ``k2 -> Type`` for some ``k2``. GHC cannot
then *regeneralize* this kind to become ``forall k2. k2 -> Type`` as desired. Thus, the
instance is rejected as ill-kinded.

To allow for such an instance, we would have to define ``(:~~:)`` as follows::

  data (:~~:) :: forall k1. k1 -> forall k2. k2 -> Type where
    HRefl :: a :~~: a

In this redefinition, we give an explicit kind for ``(:~~:)``, deferring the choice
of ``k2`` until after the first argument (``a``) has been given. With this declaration
for ``(:~~:)``, the instance for ``HTestEquality`` is accepted.

The kind ``Type``
-----------------

.. extension:: StarIsType
    :shortdesc: Treat ``*`` as ``Data.Kind.Type``.

    :since: 8.6.1

    Treat the unqualified uses of the ``*`` type operator as nullary and desugar
    to ``Data.Kind.Type``.

The kind ``Type`` (imported from ``Data.Kind``) classifies ordinary types. With
:extension:`StarIsType` (currently enabled by default), ``*`` is desugared to
``Type``, but using this legacy syntax is not recommended due to conflicts with
:extension:`TypeOperators`. This also applies to ``★``, the Unicode variant of
``*``.

Inferring dependency in datatype declarations
---------------------------------------------

If a type variable ``a`` in a datatype, class, or type family declaration
depends on another such variable ``k`` in the same declaration, two properties
must hold:

-  ``a`` must appear after ``k`` in the declaration, and

-  ``k`` must appear explicitly in the kind of *some* type variable in that
   declaration.

The first bullet simply means that the dependency must be well-scoped. The
second bullet concerns GHC's ability to infer dependency. Inferring this
dependency is difficult, and GHC currently requires the dependency to be
made explicit, meaning that ``k`` must appear in the kind of a type variable,
making it obvious to GHC that dependency is intended. For example: ::

  data Proxy k (a :: k)            -- OK: dependency is "obvious"
  data Proxy2 k a = P (Proxy k a)  -- ERROR: dependency is unclear

In the second declaration, GHC cannot immediately tell that ``k`` should
be a dependent variable, and so the declaration is rejected.

It is conceivable that this restriction will be relaxed in the future,
but it is (at the time of writing) unclear if the difficulties around this
scenario are theoretical (inferring this dependency would mean our type
system does not have principal types) or merely practical (inferring this
dependency is hard, given GHC's implementation). So, GHC takes the easy
way out and requires a little help from the user.

Inferring dependency in user-written ``forall``\s
-------------------------------------------------

A programmer may use ``forall`` in a type to introduce new quantified type
variables. These variables may depend on each other, even in the same
``forall``. However, GHC requires that the dependency be inferrable from
the body of the ``forall``. Here are some examples::

  data Proxy k (a :: k) = MkProxy   -- just to use below

  f :: forall k a. Proxy k a        -- This is just fine. We see that (a :: k).
  f = undefined

  g :: Proxy k a -> ()              -- This is to use below.
  g = undefined

  data Sing a
  h :: forall k a. Sing k -> Sing a -> ()  -- No obvious relationship between k and a
  h _ _ = g (MkProxy :: Proxy k a)  -- This fails. We didn't know that a should have kind k.

Note that in the last example, it's impossible to learn that ``a`` depends on ``k`` in the
body of the ``forall`` (that is, the ``Sing k -> Sing a -> ()``). And so GHC rejects
the program.

Kind defaulting without :extension:`PolyKinds`
-----------------------------------------------

Without :extension:`PolyKinds`, GHC refuses to generalise over kind variables.
It thus defaults kind variables to ``Type`` when possible; when this is not
possible, an error is issued.

Here is an example of this in action: ::

  {-# LANGUAGE PolyKinds #-}
  import Data.Kind (Type)
  data Proxy a = P   -- inferred kind: Proxy :: k -> Type
  data Compose f g x = MkCompose (f (g x))
    -- inferred kind: Compose :: (b -> Type) -> (a -> b) -> a -> Type

  -- separate module having imported the first
  {-# LANGUAGE NoPolyKinds, DataKinds #-}
  z = Proxy :: Proxy 'MkCompose

In the last line, we use the promoted constructor ``'MkCompose``, which has
kind ::

  forall (a :: Type) (b :: Type) (f :: b -> Type) (g :: a -> b) (x :: a).
    f (g x) -> Compose f g x

Now we must infer a type for ``z``. To do so without generalising over kind
variables, we must default the kind variables of ``'MkCompose``. We can easily
default ``a`` and ``b`` to ``Type``, but ``f`` and ``g`` would be ill-kinded if
defaulted. The definition for ``z`` is thus an error.

Pretty-printing in the presence of kind polymorphism
----------------------------------------------------

With kind polymorphism, there is quite a bit going on behind the scenes that
may be invisible to a Haskell programmer. GHC supports several flags that
control how types are printed in error messages and at the GHCi prompt.
See the :ref:`discussion of type pretty-printing options <pretty-printing-types>`
for further details. If you are using kind polymorphism and are confused as to
why GHC is rejecting (or accepting) your program, we encourage you to turn on
these flags, especially :ghc-flag:`-fprint-explicit-kinds`.

Datatype return kinds
---------------------

With :extension:`KindSignatures`, we can give the kind of a datatype written
in GADT-syntax (see :extension:`GADTSyntax`). For example::

  data T :: Type -> Type where ...

There are a number of restrictions around these *return kinds*. The text below
considers :extension:`UnliftedNewtypes` and data families (enabled by :extension:`TypeFamilies`).
The discussion also assumes familiarity with :ref:`levity polymorphism <runtime-rep>`.

1. ``data`` and ``data instance`` declarations must have return kinds that
   end in ``TYPE LiftedRep``. (Recall that ``Type`` is just a synonym for
   ``TYPE LiftedRep``.) By "end in", we refer to the kind left over after
   all arguments (introduced either by ``forall`` or ``->``) are stripped
   off and type synonyms expanded. Note that no type family expansion
   is done when performing this check.

2. If :extension:`UnliftedNewtypes` is enabled, then ``newtype`` and
   ``newtype instance`` declarations must have return kinds that end
   in ``TYPE rep`` for some ``rep``. The ``rep`` may mention type families,
   but the ``TYPE`` must be apparent without type family expansion.
   (Type synonym expansion is acceptable.)

   If :extension:`UnliftedNewtypes` is not enabled, then ``newtype`` and
   ``newtype instance`` declarations have the same restrictions as ``data``
   declarations.

3. A ``data`` or ``newtype`` instance actually can have *two* return kinds.
   The first is the kind derived by applying the data family to the
   patterns provided in the instance declaration. The second is given by
   a kind annotation. Both return kinds must satisfy the restrictions
   above.

Examples::

  data T1 :: Type             -- good: Type expands to TYPE LiftedRep
  data T2 :: TYPE LiftedRep   -- good
  data T3 :: forall k. k -> Type -> Type  -- good: arguments are dropped

  type LR = LiftedRep
  data T3 :: TYPE LR          -- good: we look through type synonyms

  type family F a where
    F Int = LiftedRep

  data T4 :: TYPE (F Int)     -- bad: we do not look through type families

  type family G a where
    G Int = Type

  data T5 :: G Int            -- bad: we do not look through type families

  -- assume -XUnliftedNewtypes
  newtype T6 :: Type where ...             -- good
  newtype T7 :: TYPE (F Int) where ...     -- good
  newtype T8 :: G Int where ...            -- bad

  data family DF a :: Type
  data instance DF Int :: Type             -- good
  data instance DF Bool :: TYPE LiftedRep  -- good
  data instance DF Char :: G Int           -- bad

  data family DF2 k :: k                   -- good
  data family DF2 Type                     -- good
  data family DF2 Bool                     -- bad
  data family DF2 (G Int)                  -- bad for 2 reasons:
                                           --  a type family can't be in a pattern, and
                                           --  the kind fails the restrictions here

.. index::
   single: TYPE
   single: levity polymorphism


