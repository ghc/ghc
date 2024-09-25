.. _or-patterns:

Or-Patterns
-------------

.. extension:: OrPatterns
    :shortdesc: Enable or-patterns.

    :since: 9.12.1

    Allow use of or-pattern syntax.

Or-patterns are enabled by the language extension :extension:`OrPatterns`.

They allow condensing multiple patterns into a single one.

Suppose we have some sum type and code matching on it: ::

    data Sweet = Cupcake | Liquorice | Cookie | Raisins

    tasty Cupcake = True
    tasty Cookie = True
    tasty _ = False

Let us say we need to add another constructor to our type, like ``Cheesecake``.
Because of the wildcard pattern we used when defining ``tasty``, the compiler
doesn't warn us that the pattern match might need adjustment, resulting in
cheesecake incorrectly being characterised as untasty.

If we want the compiler to aid us in Haskell2010, we must write out all cases
explicitly, vertically bloating the code.
This is where Or-patterns help. With :extension:`OrPatterns` we can write: ::

  tasty (Cupcake; Cookie) = True
  tasty (Liquorice; Raisins) = False

If we extend ``Sweet`` by another constructor, we'll now get a warning
about a non-exhaustive pattern match -– given we compile with
:ghc-flag:`-Wincomplete-patterns`.

Or-patterns are particularly useful in pattern matches that need to handle a
high number of constructors. It is not uncommon to see pattern matches that deal
with dozens of constructors, e.g. in GHC's own source code
(`Pat.hs <https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Hs/Pat.hs>`_).
In such cases, the only options are:

- to use a wildcard and at the expense of clarity, and risking bugs when adding new constructors

- to enumerate each constructor, at the expense of duplicating the code of the RHS

- to use an Or-pattern

Specification
~~~~~~~~~~~~~

An or-pattern looks like this: ::

  (pat_1; ...; pat_n)

where ``pat_1``, ..., ``pat_n`` are patterns themselves. Or-Patterns are
ordinary patterns and can be used wherever other patterns can be used.

The result of matching a value ``x`` against this pattern is:

- the result of matching ``x`` against ``pat_1`` if it is not a failure

- the result of matching ``x`` against ``(pat_2; ...; pat_n)`` otherwise.


The current main restriction on or-patterns is that **they may not bind any
variables or constraints**. This prohibits code like ::

  value :: Either a a -> a
  value (Left x; Right x) = x -- binds a variable

or ::

  data G a where
    G1 :: Num a => G a
    G2 :: Num a => G a

  bar :: G a -> a
  bar (G1; G2) = 3 -- cannot solve constraint `Num a`

  data GADT a where
    IsInt1 :: GADT Int
    IsInt2 :: GADT Int

  foo :: a -> GADT a -> a
  foo x (IsInt1; IsInt2) = x + 1 -- cannot solve constraint `Num a`

This is so simply because we have not proposed yet a more general static
semantics for such or-patterns.

So what *can* or-patterns do?

Apart from reducing code size and duplication, they compose with all forms of
existing patterns, like view patterns and pattern synonyms: ::

  f :: (Eq a, Show a) => a -> a -> Bool
  f a ((== a) -> True; show -> "yes") = True
  f _ _ = False

  small (abs -> (0; 1; 2); 3) = True -- -3 is not small
  small _ = False

  type Coll a = Either [a] (Set a)
  pattern None <- (Left []; Right (toList -> []))

  empty None = False
  empty _ = True

Or-patterns do not employ backtracking when given guarded right hand sides, i.e.
when one alternative of the or-pattern matches, the others are not tried when
the guard fails. The following code yields ``"no backtracking"``: ::

  case error "backtracking" of
    (_; True) | False -> error "inaccessible"
    _ -> error "no backtracking"

(The exact syntax and semantics of or-patterns are found
`here <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0522-or-patterns.rst#22static-semantics-of-or-pattern-matching>`_.)
