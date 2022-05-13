.. _arbitrary-rank-polymorphism:

Arbitrary-rank polymorphism
===========================

.. extension:: RankNTypes
    :shortdesc: Enable rank-N types.
        Implied by :extension:`ImpredicativeTypes`.

    :implies: :extension:`ExplicitForAll`
    :since: 6.8.1

    Allow types of arbitrary rank.

.. extension:: Rank2Types
    :shortdesc: Enable rank-2 types.
        Synonym for :extension:`RankNTypes`.

    :since: 6.8.1

    A deprecated alias of :extension:`RankNTypes`.

GHC's type system supports *arbitrary-rank* explicit universal
quantification in types. For example, all the following types are legal: ::

        f1 :: forall a b. a -> b -> a
        g1 :: forall a b. (Ord a, Eq  b) => a -> b -> a

        f2 :: (forall a. a->a) -> Int -> Int
        g2 :: (forall a. Eq a => [a] -> a -> Bool) -> Int -> Int

        f3 :: ((forall a. a->a) -> Int) -> Bool -> Bool

        f4 :: Int -> (forall a. a -> a)

Here, ``f1`` and ``g1`` are rank-1 types, and can be written in standard
Haskell (e.g. ``f1 :: a->b->a``). The ``forall`` makes explicit the
universal quantification that is implicitly added by Haskell.

The functions ``f2`` and ``g2`` have rank-2 types; the ``forall`` is on
the left of a function arrow. As ``g2`` shows, the polymorphic type on
the left of the function arrow can be overloaded.

The function ``f3`` has a rank-3 type; it has rank-2 types on the left
of a function arrow.

The language option :extension:`RankNTypes` (which implies
:extension:`ExplicitForAll`) enables higher-rank
types. That is, you can nest ``forall``\ s arbitrarily deep in function
arrows. For example, a forall-type (also called a "type scheme"),
including a type-class context, is legal:

-  On the left or right (see ``f4``, for example) of a function arrow

-  As the argument of a constructor, or type of a field, in a data type
   declaration. For example, any of the ``f1, f2, f3, g1, g2`` above would
   be valid field type signatures.

-  As the type of an implicit parameter

-  In a pattern type signature (see :ref:`scoped-type-variables`)

The :extension:`RankNTypes` option is also required for any type with a
``forall`` or context to the right of an arrow (e.g.
``f :: Int -> forall a. a->a``, or ``g :: Int -> Ord a => a -> a``).
Such types are technically rank 1, but are clearly not Haskell-98, and
an extra extension did not seem worth the bother.

In particular, in ``data`` and ``newtype`` declarations the constructor
arguments may be polymorphic types of any rank; see examples in
:ref:`univ`. Note that the declared types are nevertheless always
monomorphic. This is important because by default GHC will not
instantiate type variables to a polymorphic type
(:ref:`impredicative-polymorphism`).

The obsolete language option :extension:`Rank2Types` is a synonym for
:extension:`RankNTypes`. They used to specify finer distinctions that GHC no
longer makes.

.. _univ:

Examples
--------

These are examples of ``data`` and ``newtype`` declarations whose data
constructors have polymorphic argument types: ::

    data T a = T1 (forall b. b -> b -> b) a

    data MonadT m = MkMonad { return :: forall a. a -> m a,
                              bind   :: forall a b. m a -> (a -> m b) -> m b
                            }

    newtype Swizzle = MkSwizzle (forall a. Ord a => [a] -> [a])

The constructors have rank-2 types: ::

    T1 :: forall a. (forall b. b -> b -> b) -> a -> T a

    MkMonad :: forall m. (forall a. a -> m a)
                      -> (forall a b. m a -> (a -> m b) -> m b)
                      -> MonadT m

    MkSwizzle :: (forall a. Ord a => [a] -> [a]) -> Swizzle

In earlier versions of GHC, it was possible to omit the ``forall`` in
the type of the constructor if there was an explicit context. For
example: ::

    newtype Swizzle' = MkSwizzle' (Ord a => [a] -> [a])

Since GHC 8.0 declarations such as ``MkSwizzle'`` will cause an out-of-scope
error.

You construct values of types ``T1, MonadT, Swizzle`` by applying the
constructor to suitable values, just as usual. For example, ::

        a1 :: T Int
        a1 = T1 (\x y->x) 3

        a2, a3 :: Swizzle
        a2 = MkSwizzle sort
        a3 = MkSwizzle reverse

        a4 :: MonadT Maybe
        a4 = let r x = Just x
             b m k = case m of
                   Just y -> k y
                   Nothing -> Nothing
             in
             MkMonad r b

        mkTs :: (forall b. b -> b -> b) -> a -> a -> [T a]
        mkTs f x y = [T1 f x, T1 f y]

The type of the argument can, as usual, be more general than the type
required, as ``(MkSwizzle reverse)`` shows. (``reverse`` does not need
the ``Ord`` constraint.)

When you use pattern matching, the bound variables may now have
polymorphic types. For example: ::

        f :: T a -> a -> (a, Char)
        f (T1 w k) x = (w k x, w 'c' 'd')

        g :: (Ord a, Ord b) => Swizzle -> [a] -> (a -> b) -> [b]
        g (MkSwizzle s) xs f = s (map f (s xs))

        h :: MonadT m -> [m a] -> m [a]
        h m [] = return m []
        h m (x:xs) = bind m x          $ \y ->
                     bind m (h m xs)   $ \ys ->
                     return m (y:ys)

In the function ``h`` we use the record selectors ``return`` and
``bind`` to extract the polymorphic bind and return functions from the
``MonadT`` data structure, rather than using pattern matching.


.. _subsumption:

Subsumption
-------------

Suppose: ::

  f1 :: (forall a b. Int -> a -> b -> b) -> Bool
  g1 :: forall x y. Int -> y -> x -> x

  f2 :: (forall a. (Eq a, Show a) => a -> a) -> Bool
  g2 :: forall x. (Show x, Eq x) => x -> x

then ``f1 g1`` and ``f2 g2`` are both well typed, despite the
different order of type variables and constraints.  What happens is that the
argument is instantiated, and then re-generalised to match the type expected
by the function.

But this instantiation and re-generalisation happens only at the top level
of a type. In particular, none of this happens if the foralls are underneath an arrow.
For example: ::

  f3 :: (Int -> forall a b. a -> b -> b) -> Bool
  g3a :: Int -> forall x y. x -> y -> y
  g3b :: forall x. Int -> forall y. x -> y -> y
  g3c :: Int -> forall x y. y -> x -> x

  f4 :: (Int -> forall a. (Eq a, Show a) => a -> a) -> Bool
  g4 ::  Int -> forall x. (Show x, Eq x) => x -> x) -> Bool

Then the application ``f3 g3a`` is well-typed, because ``g3a`` has a type that matches the type
expected by ``f3``.  But ``f3 g3b`` is not well typed, because the foralls are in different places.
Nor is ``f3 g3c``, where the foralls are in the same place but the variables are in a different order.
Similarly ``f4 g4`` is not well typed, because the constraints appear in a different order.

These examples can be made to typecheck by eta-expansion. For example ``f3 (\x -> g3b x)``
is well typed, and similarly ``f3 (\x -> g3c x)`` and ``f4 (\x -> g4 x)``.

A similar phenomenon occurs for operator sections. For example,
``(\`g3a\` "hello")`` is not well typed, but it can be made to typecheck by eta
expanding it to ``\x -> x \`g3a\` "hello"``.

.. extension:: DeepSubsumption
    :shortdesc: Enable deep subsumption

    :since: 9.2.4

    Relax the simple subsumption rules, implicitly inserting eta-expansions
    when matching up function types with different quantification structures.

The :extension:`DeepSubsumption` extension relaxes the aforementioned requirement that
foralls must appear in the same place. GHC will instead automatically rewrite expressions
like ``f x`` of type ``ty1 -> ty2`` to become ``(\ (y :: ty1) -> f x y)``; this is called eta-expansion.
See Section 4.6 of
`Practical type inference for arbitrary-rank types <https://www.microsoft.com/en-us/research/publication/practical-type-inference-for-arbitrary-rank-types/>`__,
where this process is called "deep skolemisation".

Note that these eta-expansions may silently change the semantics of the user's program: ::

  h1 :: Int -> forall a. a -> a
  h1 = undefined
  h2 :: forall b. Int -> b -> b
  h2 = h1

With :extension:`DeepSubsumption`, GHC will accept these definitions,
inserting an implicit eta-expansion: ::

  h2 = \ i -> h1 i

This means that ``h2 `seq` ()`` will not crash, even though ``h1 `seq` ()`` does crash.

Historical note: Deep skolemisation was initially removed from the language by
`GHC Proposal #287 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0287-simplify-subsumption.rst>`__,
but was re-introduced as part of the :extension:`DeepSubsumption` extension following
`GHC Proposal #511 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0511-deep-subsumption.rst>`__.

.. _higher-rank-type-inference:

Type inference
--------------

In general, type inference for arbitrary-rank types is undecidable. GHC
uses an algorithm proposed by Odersky and Laufer ("Putting type
annotations to work", POPL'96) to get a decidable algorithm by requiring
some help from the programmer. We do not yet have a formal specification
of "some help" but the rule is this:

    For a lambda-bound or case-bound variable, x, either the programmer
    provides an explicit polymorphic type for x, or GHC's type inference
    will assume that x's type has no foralls in it.

What does it mean to "provide" an explicit type for x? You can do that
by giving a type signature for x directly, using a pattern type
signature (:ref:`scoped-type-variables`), thus: ::

    \ f :: (forall a. a->a) -> (f True, f 'c')

Alternatively, you can give a type signature to the enclosing context,
which GHC can "push down" to find the type for the variable: ::

    (\ f -> (f True, f 'c')) :: (forall a. a->a) -> (Bool,Char)

Here the type signature on the expression can be pushed inwards to give
a type signature for f. Similarly, and more commonly, one can give a
type signature for the function itself: ::

    h :: (forall a. a->a) -> (Bool,Char)
    h f = (f True, f 'c')

You don't need to give a type signature if the lambda bound variable is
a constructor argument. Here is an example we saw earlier: ::

    f :: T a -> a -> (a, Char)
    f (T1 w k) x = (w k x, w 'c' 'd')

Here we do not need to give a type signature to ``w``, because it is an
argument of constructor ``T1`` and that tells GHC all it needs to know.


.. _implicit-quantification:

Implicit quantification
-----------------------

GHC performs implicit quantification as follows. At the outermost level
(only) of user-written types, if and only if there is no explicit
``forall``, GHC finds all the type variables mentioned in the type that
are not already in scope, and universally quantifies them. For example,
the following pairs are equivalent: ::

      f :: a -> a
      f :: forall a. a -> a

      g (x::a) = let
                    h :: a -> b -> b
                    h x y = y
                 in ...
      g (x::a) = let
                    h :: forall b. a -> b -> b
                    h x y = y
                 in ...

Notice that GHC always adds implicit quantifiers *at the outermost level*
of a user-written type; it
does *not* find the inner-most possible quantification
point. For example: ::

      f :: (a -> a) -> Int
               -- MEANS
      f :: forall a. (a -> a) -> Int
               -- NOT
      f :: (forall a. a -> a) -> Int


      g :: (Ord a => a -> a) -> Int
               -- MEANS
      g :: forall a. (Ord a => a -> a) -> Int
               -- NOT
      g :: (forall a. Ord a => a -> a) -> Int

If you want the latter type, you can write
your ``forall``\s explicitly. Indeed, doing so is strongly advised for
rank-2 types.

Sometimes there *is* no "outermost level", in which case no
implicit quantification happens: ::

      data PackMap a b s t = PackMap (Monad f => (a -> f b) -> s -> f t)

This is rejected because there is no "outermost level" for the types on the RHS
(it would obviously be terrible to add extra parameters to ``PackMap``),
so no implicit quantification happens, and the declaration is rejected
(with "``f`` is out of scope").  Solution: use an explicit ``forall``: ::

      data PackMap a b s t = PackMap (forall f. Monad f => (a -> f b) -> s -> f t)


