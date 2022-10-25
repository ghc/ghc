.. _or-patterns:

Or-Patterns
-------------

.. extension:: OrPatterns
    :shortdesc: Enable or-patterns.

    :since: 9.8.1

    Allow use of or-pattern syntax.

Or-patterns are enabled by the language extension :extension:`OrPatterns`.

They allow condensing multiple patterns into a single one. Suppose you had some sum type: ::

    data Sweet = Cupcake | Liquorice | Cookie | Raisins
    
    tasty Cupcake = True
    tasty Cookie = True
    tasty _ = False

All well and good, but what if we add another constructor to our type, like ``Cheesecake``?
Because of the wildcard pattern we used when defining ``tasty``, the compiler doesn't warn us that the pattern match is incomplete,
resulting in cheesecake incorrectly being characterised as untasty. What a shame!

So if we want the compiler to aid us, we should have written out all cases explicitly, vertically bloating the code.
Now or-patterns come in quite handy: With ``-XOrPatterns``, we can write: ::

  tasty (one of Cupcake, Cookie) = True
  tasty (one of Liquorice, Raisins) = False
  
If we extend ``Sweet`` by another constructor, we'll now get a warning about a non-exhaustive pattern match – given we compile with ``-Wincomplete-patterns``.

While this may seem like a pointless example, it isn't: there are lots of places in GHC where constructor pattern matches either use a closing wildcard, or where all patterns are explicitly matched at the expense of code duplication. Just look at `Pat.hs <https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Hs/Pat.hs>`_.


Specification
~~~~~~~~~~~~~

An or-pattern looks like this: ::

  (one of pat_1, ..., pat_n)

where ``pat_1``, ..., ``pat_n`` are patterns themselves. Or-Patterns are ordinary patterns and can be used wherever other patterns can be used.

The result of matching a value ``x`` against this pattern is:

- the result of matching ``x`` against ``pat_1`` if it is not a failure

- the result of matching ``x`` against ``(one of pat_2, ..., pat_n)`` otherwise.


The current main restriction on or-patterns is that **they may not bind any variables or constraints**. This prohibits code like ::

  value :: Either a a -> a
  value (one of Left x, Right x) = x -- binds a variable

or ::

  data G a where
    G1 :: Num a => G a
    G2 :: Num a => G a

  bar :: G a -> a
  bar (one of G1, G2) = 3 -- induces `Num a`

  data GADT a where
    IsInt1 :: GADT Int
    IsInt2 :: GADT Int

  foo :: a -> GADT a -> a
  foo x (one of IsInt1 {}, IsInt2 {}) = x + 1 -- induces `a ~ Int`

This is so because it is hard to specify good and correct static semantics for such or-patterns, but this could still be done sometime in the future.


So what *can* or-patterns do?

Apart from reducing code size and duplication, they interact with all forms of existing patterns, like view patterns and pattern synonyms: ::

  f :: (Eq a, Show a) => a -> a -> Bool
  f a (one of (== a) -> True, show -> "yes") = True
  f _ _ = False

  small (one of abs -> (one of 0, 1, 2), 3) = True -- -3 is not small
  small _ = False

  type Coll a = Either [a] (Set a)
  pattern None <- (one of (Left []), Right (toList -> []))

  empty None = False
  empty _ = True

Or-patterns do not employ backtracking when given guarded right hand sides, i.e. when one alternative of the or-pattern matches, the others are not tried when the guard fails. The following code yields ``"no backtracking"``: ::

  case (True, error "backtracking") of
    (one of (True, _), (_, True)) | False -> error "inaccessible"
    _ -> error "no backtracking"


(The exact syntax and semantics of or-patterns are found `here <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0522-or-patterns.rst#22static-semantics-of-or-pattern-matching>`_.)