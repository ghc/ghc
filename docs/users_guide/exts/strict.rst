.. _bang-patterns:

.. _strict-haskell:

Bang patterns and Strict Haskell
================================

.. index::
   single: strict haskell

.. index::
   single: Bang patterns

In high-performance Haskell code (e.g. numeric code) eliminating
thunks from an inner loop can be a huge win.
GHC supports three extensions to allow the programmer to specify
use of strict (call-by-value) evaluation rather than lazy (call-by-need)
evaluation.

- Bang patterns (:extension:`BangPatterns`) makes pattern matching and
  let bindings stricter.

- Strict data types (:extension:`StrictData`) makes constructor fields
  strict by default, on a per-module basis.

- Strict pattern (:extension:`Strict`) makes all patterns and let bindings
  strict by default, on a per-module basis.

The latter two extensions are simply a way to avoid littering high-performance
code with bang patterns, making it harder to read.

Bang patterns and strict matching do not affect the type system in any way.

.. _bang-patterns-informal:

Bang patterns
-------------

.. extension:: BangPatterns
    :shortdesc: Enable bang patterns.

    :since: 6.8.1

    Allow use of bang pattern syntax.

GHC supports an extension of pattern matching called *bang patterns*,
written ``!pat``. Bang patterns are under consideration for Haskell
Prime. The `Haskell prime feature
description <https://gitlab.haskell.org/haskell/prime/-/wikis/BangPatterns>`__
contains more discussion and examples than the material below.

The main idea is to add a single new production to the syntax of
patterns: ::

      pat ::= !pat

Matching an expression ``e`` against a pattern ``!p`` is done by first
evaluating ``e`` (to WHNF) and then matching the result against ``p``.
Example: ::

    f1 !x = True

This definition makes ``f1`` is strict in ``x``, whereas without the
bang it would be lazy. Bang patterns can be nested of course: ::

    f2 (!x, y) = [x,y]

Here, ``f2`` is strict in ``x`` but not in ``y``.

Note the following points:

- A bang only really has
  an effect if it precedes a variable or wild-card pattern: ::

    f3 !(x,y) = [x,y]
    f4 (x,y)  = [x,y]

  Here, ``f3`` and ``f4`` are identical; putting a bang before a pattern
  that forces evaluation anyway does nothing.

- A bang pattern is allowed in a let or where clause, and makes the binding
  strict.  For example: ::

    let !x = e in body
    let !(p,q) = e in body

  In both cases ``e`` is evaluated before starting to evaluate ``body``.

  However, *nested* bangs in a let/where pattern binding behave uniformly with all
  other forms of pattern matching. For example ::

    let (!x,[y]) = e in b

  is equivalent to this: ::

    let { t = case e of (x,[y]) -> x `seq` (x,y)
          x = fst t
          y = snd t }
    in b

  The binding is lazy, but when either ``x`` or ``y`` is evaluated by
  ``b`` the entire pattern is matched, including forcing the evaluation of
  ``x``.

  See :ref:`Semantics of let bindings with bang patterns <recursive-and-polymorphic-let-bindings>` for
  the detailed semantics.

- A pattern with a bang at the outermost level is not allowed at the top
  level of a module.

- Bang patterns work in ``case`` expressions too, of course: ::

    g5 x = let y = f x in body
    g6 x = case f x of { y -> body }
    g7 x = case f x of { !y -> body }

  The functions ``g5`` and ``g6`` mean exactly the same thing. But ``g7``
  evaluates ``(f x)``, binds ``y`` to the result, and then evaluates
  ``body``.

- There is one problem with syntactic ambiguity. Consider: ::

    f !x = 3

  Is this a definition of the infix function "``(!)``", or of the "``f``" with
  a bang pattern? GHC resolves this ambiguity by looking at the surrounding
  whitespace: ::

    a ! b = ...   -- infix operator
    a !b = ...    -- bang pattern

  See `GHC Proposal #229 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0229-whitespace-bang-patterns.rst>`__
  for the precise rules.


.. _strict-data:

Strict-by-default data types
----------------------------

.. extension:: StrictData
    :shortdesc: Enable default strict datatype fields.

    :since: 8.0.1

    Make fields of data types defined in the current module strict by default.

Informally the ``StrictData`` language extension switches data type
declarations to be strict by default allowing fields to be lazy by
adding a ``~`` in front of the field.

When the user writes ::

          data T = C a
          data T' = C' ~a

we interpret it as if they had written ::

          data T = C !a
          data T' = C' a

The extension only affects definitions in this module.

The ``~`` annotation must be written in prefix form::

   data T = MkT ~Int   -- valid
   data T = MkT ~ Int  -- invalid

See `GHC Proposal #229 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0229-whitespace-bang-patterns.rst>`__
for the precise rules.

.. _strict:

Strict-by-default pattern bindings
----------------------------------

.. extension:: Strict
    :shortdesc: Make bindings in the current module strict by default.

    :implies: :extension:`StrictData`
    :since: 8.0.1

    Make bindings in the current module strict by default.

Informally the ``Strict`` language extension switches functions, data
types, and bindings to be strict by default, allowing optional laziness
by adding ``~`` in front of a variable. This essentially reverses the
present situation where laziness is default and strictness can be
optionally had by adding ``!`` in front of a variable.

``Strict`` implies :ref:`StrictData <strict-data>`.

-  **Function definitions**

   When the user writes ::

       f x = ...

   we interpret it as if they had written ::

       f !x = ...

   Adding ``~`` in front of ``x`` gives the regular lazy behavior.

   Turning patterns into irrefutable ones requires ``~(~p)`` when ``Strict`` is enabled.



-  **Let/where bindings**

   When the user writes ::

     let x = ...
     let pat = ...

   we interpret it as if they had written ::

     let !x = ...
     let !pat = ...

   Adding ``~`` in front of ``x`` gives the regular lazy
   behavior.
   The general rule is that we add an implicit bang on the outermost pattern,
   unless disabled with ``~``.

-  **Pattern matching in case expressions, lambdas, do-notation, etc**

   The outermost pattern of all pattern matches gets an implicit bang,
   unless disabled with ``~``.
   This applies to case expressions, patterns in lambda, do-notation,
   list comprehension, and so on.
   For example ::

       case x of (a,b) -> rhs

   is interpreted as ::

       case x of !(a,b) -> rhs

   Since the semantics of pattern matching in case expressions is
   strict, this usually has no effect whatsoever. But it does make a
   difference in the degenerate case of variables and newtypes. So ::

       case x of y -> rhs

   is lazy in Haskell, but with ``Strict`` is interpreted as ::

       case x of !y -> rhs

   which evaluates ``x``. Similarly, if ``newtype Age = MkAge Int``, then ::

       case x of MkAge i -> rhs

   is lazy in Haskell; but with ``Strict`` the added bang makes it
   strict.

   Similarly ::

      \ x -> body
      do { x <- rhs; blah }
      [ e | x <- rhs; blah }

   all get implicit bangs on the ``x`` pattern.

-  **Nested patterns**

   Notice that we do *not* put bangs on nested patterns. For
   example ::

     let (p,q) = if flob then (undefined, undefined) else (True, False)
     in ...

   will behave like ::

     let !(p,q) = if flob then (undefined, undefined) else (True,False)
     in ...

   which will strictly evaluate the right hand side, and bind ``p``
   and ``q`` to the components of the pair. But the pair itself is
   lazy (unless we also compile the ``Prelude`` with ``Strict``; see
   :ref:`strict-modularity` below). So ``p`` and ``q`` may end up bound to
   undefined. See also :ref:`recursive-and-polymorphic-let-bindings` below.

-  **Top level bindings**

   are unaffected by ``Strict``. For example: ::

       x = factorial 20
       (y,z) = if x > 10 then True else False

   Here ``x`` and the pattern binding ``(y,z)`` remain lazy. Reason:
   there is no good moment to force them, until first use.

-  **Newtypes**

   There is no effect on newtypes, which simply rename existing types.
   For example: ::

       newtype T = C a
       f (C x)  = rhs1
       g !(C x) = rhs2

   In ordinary Haskell, ``f`` is lazy in its argument and hence in
   ``x``; and ``g`` is strict in its argument and hence also strict in
   ``x``. With ``Strict``, both become strict because ``f``'s argument
   gets an implicit bang.


.. _strict-modularity:

Modularity
----------

``Strict`` and ``StrictData`` only affects definitions in the module
they are used in. Functions and data types imported from other modules
are unaffected. For example, we won't evaluate the argument to
``Just`` before applying the constructor.  Similarly we won't evaluate
the first argument to ``Data.Map.findWithDefault`` before applying the
function.

This is crucial to preserve correctness. Entities defined in other
modules might rely on laziness for correctness (whether functional or
performance).

Tuples, lists, ``Maybe``, and all the other types from ``Prelude``
continue to have their existing, lazy, semantics.

.. _bang-patterns-sem:
.. _recursive-and-polymorphic-let-bindings:

Dynamic semantics of bang patterns
----------------------------------

The semantics of Haskell pattern matching is described in `Section
3.17.2 <http://www.haskell.org/onlinereport/exps.html#sect3.17.2>`__ of
the Haskell Report. To this description add one extra item 10, saying:

-  Matching the pattern ``!pat`` against a value ``v`` behaves as
   follows:

   -  if ``v`` is bottom, the match diverges

   -  otherwise, ``pat`` is matched against ``v``

Similarly, in Figure 4 of `Section
3.17.3 <http://www.haskell.org/onlinereport/exps.html#sect3.17.3>`__,
add a new case (t): ::

    case v of { !pat -> e; _ -> e' }
       = v `seq` case v of { pat -> e; _ -> e' }

That leaves let expressions, whose translation is given in `Section
3.12 <http://www.haskell.org/onlinereport/exps.html#sect3.12>`__ of the
Haskell Report.
Replace the "Translation" there with the following one.  Given
``let { bind1 ... bindn } in body``:

.. admonition:: FORCE

    Replace any binding ``!p = e`` with ``v = case e of p -> (x1, ..., xn); (x1, ..., xn) = v`` and replace
    ``body`` with ``v seq body``, where ``v`` is fresh. This translation works fine if
    ``p`` is already a variable ``x``, but can obviously be optimised by not
    introducing a fresh variable ``v``.

.. admonition:: SPLIT

    Replace any binding ``p = e``, where ``p`` is not a variable, with
    ``v = e; x1 = case v of p -> x1; ...; xn = case v of p -> xn``, where
    ``v`` is fresh and ``x1``.. ``xn`` are the bound variables of ``p``.
    Again if ``e`` is a variable, this can be optimised by not introducing a
    fresh variable.

The result will be a (possibly) recursive set of bindings, binding
only simple variables on the left hand side. (One could go one step
further, as in the Haskell Report and make the recursive bindings
non-recursive using ``fix``, but we do not do so in Core, and it only
obfuscates matters, so we do not do so here.)

The translation is carefully crafted to make bang patterns meaningful
for recursive and polymorphic bindings as well as straightforward
non-recursive bindings.

Here are some examples of how this translation works. The first
expression of each sequence is Haskell source; the subsequent ones are
Core.

Here is a simple non-recursive case: ::

    let x :: Int     -- Non-recursive
        !x = factorial y
    in body

    ===> (FORCE)
        let x = factorial y in x `seq` body

    ===> (inline seq)
        let x = factorial y in case x of x -> body

    ===> (inline x)
        case factorial y of x -> body

Same again, only with a pattern binding: ::

    let !(Just x, Left y) = e in body

    ===> (FORCE)
        let v = case e of (Just x, Left y) -> (x,y)
            (x,y) = v
        in v `seq` body

    ===> (SPLIT)
        let v = case e of (Just x, Left y) -> (x,y)
            x = case v of (x,y) -> x
            y = case v of (x,y) -> y
        in v `seq` body

    ===> (inline seq, float x,y bindings inwards)
        let v = case e of (Just x, Left y) -> (x,y)
        in case v of v -> let x = case v of (x,y) -> x
                              y = case v of (x,y) -> y
                          in body

    ===> (fluff up v's pattern; this is a standard Core optimisation)
        let v = case e of (Just x, Left y) -> (x,y)
        in case v of v@(p,q) -> let x = case v of (x,y) -> x
                                    y = case v of (x,y) -> y
                                in body

    ===> (case of known constructor)
        let v = case e of (Just x, Left y) -> (x,y)
        in case v of v@(p,q) -> let x = p
                                    y = q
                                in body

    ===> (inline x,y, v)
        case (case e of (Just x, Left y) -> (x,y) of
            (p,q) -> body[p/x, q/y]

    ===> (case of case)
        case e of (Just x, Left y) -> body[p/x, q/y]

The final form is just what we want: a simple case expression.

Here is a recursive case ::

    letrec xs :: [Int]  -- Recursive
            !xs = factorial y : xs
    in body

    ===> (FORCE)
        letrec xs = factorial y : xs in xs `seq` body

    ===> (inline seq)
        letrec xs = factorial y : xs in case xs of xs -> body

    ===> (eliminate case of value)
        letrec xs = factorial y : xs in body

and a polymorphic one: ::

    let f :: forall a. [a] -> [a]    -- Polymorphic
        !f = fst (reverse, True)
    in body

    ===> (FORCE)
        let f = /\a. fst (reverse a, True) in f `seq` body
    ===> (inline seq, inline f)
        case (/\a. fst (reverse a, True)) of f -> body

Notice that the ``seq`` is added only in the translation to Core
If we did it in Haskell source, thus ::

   let f = ... in f `seq` body

then ``f``\ 's polymorphic type would get instantiated, so the Core
translation would be ::

   let f = ... in f Any `seq` body


When overloading is involved, the results might be slightly counter
intuitive: ::

    let f :: forall a. Eq a => a -> [a] -> Bool    -- Overloaded
        !f = fst (member, True)
    in body

    ===> (FORCE)
        let f = /\a \(d::Eq a). fst (member, True) in f `seq` body

    ===> (inline seq, case of value)
        let f = /\a \(d::Eq a). fst (member, True) in body

Note that the bang has no effect at all in this case



