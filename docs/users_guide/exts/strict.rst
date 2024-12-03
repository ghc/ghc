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
    :shortdesc: Allow bang pattern syntax.

    :since: 6.8.1

    :status: Included in :extension:`GHC2024`, :extension:`GHC2021`

    Allow use of bang pattern syntax.

GHC supports an extension of pattern matching called *bang patterns*,
written ``!pat``. Bang patterns are available by default as a part
of :extension:`GHC2021`.

The main idea is to add a single new production to the syntax of
patterns: ::

      pat ::= !pat

Matching an expression ``e`` against a pattern ``!p`` is done by first
evaluating ``e`` (to WHNF) and then matching the result against ``p``.
Example: ::

    f1 !x = True

This definition makes ``f1`` is strict in ``x``, whereas without the
bang it would be lazy.

Note the following points:

- Bang patterns can be nested: ::

      f2 (!x, y) = [x,y]

  Here, ``f2`` is strict in ``x`` but not in ``y``.

- Bang patterns can be used in ``case`` expressions too: ::

    g1 x = let y = f x in body
    g2 x = case f x of { y -> body }
    g3 x = case f x of { !y -> body }

  The functions ``g1`` and ``g2`` mean exactly the same thing. But ``g3``
  evaluates ``(f x)``, binds ``y`` to the result, and then evaluates
  ``body``.

- Bang patterns do not have any effect with constructor patterns: ::

    f3 !(x,y) = [x,y]
    f4 (x,y)  = [x,y]

  Here, ``f3`` and ``f4`` are identical; putting a bang before a pattern
  that forces evaluation anyway does nothing. However, see the caveat below.

- There is one problem with syntactic ambiguity. Consider: ::

    f !x = 3

  Is this a definition of the infix function "``(!)``", or of the "``f``" with
  a bang pattern? GHC resolves this ambiguity by looking at the surrounding
  whitespace: ::

    a ! b = ...   -- infix operator
    a !b = ...    -- bang pattern

  See `GHC Proposal #229 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0229-whitespace-bang-patterns.rst>`__
  for the precise rules.

Strict bindings
~~~~~~~~~~~~~~~

The ``BangPatterns`` extension furthermore enables syntax for strict
``let`` or ``where`` bindings with ``!pat = expr``. For example, ::

    let !x = e in body
    let !(p,q) = e in body

In both cases ``e`` is evaluated before starting to evaluate ``body``.

Note the following points:

- A strict binding (with a top level ``!``) should not be thought of as a regular
  pattern binding that happens to have a bang pattern (:ref:`bang-patterns-informal`) on the LHS.
  Rather, the top level ``!`` should be considered part of the let-binding, rather than
  part of the pattern.  This makes a difference when we come to the rules in :ref:`bang-patterns-sem`.

- Only a top-level bang (perhaps under parentheses) makes the binding strict; otherwise,
  it is considered a normal bang pattern. For example, ::

      let (!x,[y]) = e in b

  is equivalent to this: ::

    let { t = case e of (x,[y]) -> x `seq` (x,y)
          x = fst t
          y = snd t }
    in b

  The binding is lazy, but when either ``x`` or ``y`` is evaluated by
  ``b`` the entire pattern is matched, including forcing the evaluation of
  ``x``.

- Because the ``!`` in a strict binding is not a bang pattern, it must
  be visible without looking through pattern synonyms ::

      pattern Bang x <- !x
      f1 = let Bang x = y in ...
      f2 = let !x     = y in ...  -- not equivalent to f1

- Strict bindings are not allowed at the top level of a module.

- See :ref:`Semantics of let bindings with bang patterns <recursive-and-polymorphic-let-bindings>` for
  the detailed semantics, and the `Haskell prime feature
  description <https://gitlab.haskell.org/haskell/prime/-/wikis/BangPatterns>`__
  for more discussion and examples.


.. _strict-data:

Strict-by-default data types
----------------------------

.. extension:: StrictData
    :shortdesc: Treat datatype fields as strict by default.

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
3.17.2 <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-610003.17.2>`__ of
the Haskell Report. To this description add one extra item 9, saying:

-  Matching the pattern ``!pat`` against a value ``v`` behaves as
   follows:

   -  if ``v`` is bottom, the match diverges

   -  otherwise, ``pat`` is matched against ``v``

Similarly, in Figure 4 of `Section
3.17.3 <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-440003.12>`__,
add a new case (w): ::

    case v of { !pat -> e; _ -> e' }
       = v `seq` case v of { pat -> e; _ -> e' }

That leaves let expressions, whose translation is given in `Section
3.12 <https://www.haskell.org/onlinereport/exps.html#sect3.12>`__ of the
Haskell Report.
Replace the "Translation" there with the following one.  Given
``let { bind1 ... bindn } in body``:

.. admonition:: SPLIT-LAZY

    Given a lazy pattern binding ``p = e``, where ``p`` is not a variable,
    and ``x1...xn`` are the variables bound by ``p``,
    and all these binders have lifted type,
    replace the binding with this (where ``v`` is fresh)::

       v = case e of { p -> (x1, ..., xn) }
       x1 = case v of { (x1, ..., xn) -> x1 }
       ...
       xn = case v of { (x1, ..., xn) -> xn }``

    If n=1 (i.e. exactly one variable is bound),
    the desugaring uses the ``Solo`` type to make a 1-tuple.

.. admonition:: SPLIT-STRICT

    Given a strict pattern binding ``!p = e``, where
    ``x1...xn`` are the variables bound by ``p``,
    and all these binders have lifted type:

    1. Replace the binding with this (where ``v`` is fresh)::

          v = case e of { !p -> (x1, ..., xn) }
          (x1, ..., xn) = v

    2. Replace ``body`` with ``v `seq` body``.

    As in SPLIT-LAZY, if n=1 the desugaring uses the ``Solo`` type to make a 1-tuple.

    This transformation is illegal at the top
    level of a module (since there is no ``body``), so strict bindings are illegal at top level.

    The transformation is correct when ``p`` is a variable ``x``, but can be optimised to::

       let !x = e in body  ==>   let x = e in x `seq` body

.. admonition:: CASE

    Given a non-recursive strict pattern binding ``!p = e``,
    where ``x1...xn`` are the variables bound by ``p``,
    and any of the binders has unlifted type:
    replace the binding with nothing at all, and replace
    ``body`` with ``case e of p -> body``.

    This transformation is illegal at the top
    level of a module, so such bindings are rejected.

    The result of this transformation is ill-scoped if any of the binders
    ``x1...xn`` appears in ``e``; hence the restriction to non-recursive pattern bindings.

    Exactly the same transformation applies to a non-recursive lazy pattern binding
    (i.e. one lacking a top-level ``!``) that binds any unlifted variables; but
    such a binding emits a warning :ghc-flag:`-Wunbanged-strict-patterns`. The
    warning encourages the programmer to make visible the fact that this binding
    is necessarily strict.

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

    ===> (SPLIT-STRICT)
         let x = factorial y in x `seq` body

    ===> (inline seq)
         let x = factorial y in case x of !x -> body

    ===> (inline x)
         case factorial y of !x -> body

Same again, only with a pattern binding: ::

    let !(Just x) = e in body

    ===> (SPLIT-STRICT)
         let v = case e of !(Just x) -> Solo x
             Solo x = v
         in v `seq` body

    ===> (SPLIT-LAZY, drop redundant bang)
         let v = case e of Just x -> Solo x
             x = case v of Solo x -> x
         in v `seq` body

    ===> (inline seq, float x,y bindings inwards)
         let v = case e of Just x -> Solo x
         in case v of !v -> let x = case v of Solo x -> x
                            in body

    ===> (fluff up v's pattern; this is a standard Core optimisation)
         let v = case e of Just x -> Solo x
         in case v of v@(Solo p) -> let x = case v of Solo x -> x
                                    in body

    ===> (case of known constructor)
         let v = case e of Just x -> Solo x
         in case v of v@(Solo p) -> let x = p
                                    in body

    ===> (inline x, v)
         case (case e of Just x -> Solo x) of
            Solo p -> body[p/x]

    ===> (case of case)
         case e of Just x -> body[p/x]

The final form is just what we want: a simple case expression.  Notice, crucially,
that that *pattern* ``Just x`` is forced eagerly, but ``x`` itself is not evaluated
unless and until ``body`` does so.  Note also that this example uses a pattern
that binds exactly one variable, and illustrates the use of the ``Solo`` 1-tuple.

Rule (SPLIT-STRICT) applies even if the pattern binds no variables::

    let !(True,False) = e in body

    ===> (SPLIT-STRICT)
         let v = case e of !(True,False) -> (); () = v in v `seq` body

    ===> (inline, simplify, drop redundant bang)
         case e of (True,False) -> body

That is, we force ``e`` and check that it has the right form before proceeding with ``body``.
This happens even if the pattern is itself vacuous::

    let !_ = e in body

    ===> (SPLIT-STRICT)
         let v = case e of !_ -> (); () = v in v `seq` body

    ===> (inline, simplify)
         case e of !_ -> body

Again, ``e`` is forced before evaluating ``body``.  This (along with ``!x = e``) is the reason
that (SPLIT-STRICT) uses a bang-pattern in the ``case`` in the desugared right-hand side.

Note that rule (CASE) applies only when any of the *binders* is unlifted;
it is irrelevant whether the binding *itself* is unlifted (see
`GHC Proposal #35 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0035-unbanged-strict-patterns.rst>`__).
For example (see :ref:`primitives`)::

    let (# a::Int, b::Bool #) = e in body
    ===> (SPLIT-LAZY)
        let v = case e of (# a,b #) -> (a,b)
            a = case v of (a,b) -> a
            b = case v of (a,b) -> b
        in body

Even though the tuple pattern is unboxed, it is matched only when ``a`` or ``b`` are evaluated in ``body``.

Here is an example with an unlifted data type::

    type T :: UnliftedType
    data T = MkT Int
    f1 x = let MkT y  = blah in body1
    f2 x = let z :: T = blah in body2
    f3 x = let _ :: T = blah in body3

In ``f1``, even though ``T`` is an unlifted type, the pattern ``MkT y`` binds a lifted
variable ``y``, so (SPLIT-LAZY) applies, and ``blah`` is not evaluated until ``body1`` evaluates ``y``.
In contrast, in ``f2`` the pattern ``z :: T`` binds a variable ``z`` of unlifted type, so (CASE) applies
and the let-binding is strict.  In ``f3`` the pattern binds no variables, so again it is lazy like ``f1``.

Here is a recursive case ::

    letrec xs :: [Int]  -- Recursive
            !xs = factorial y : xs
    in body

    ===> (SPLIT-STRICT)
         letrec xs = factorial y : xs in xs `seq` body

    ===> (inline seq)
         letrec xs = factorial y : xs in case xs of xs -> body

    ===> (eliminate case of value)
         letrec xs = factorial y : xs in body

and a polymorphic one: ::

    let f :: forall a. [a] -> [a]    -- Polymorphic
        !f = fst (reverse, True)
    in body

    ===> (SPLIT-STRICT)
         let f = /\a. fst (reverse a, True) in f `seq` body

    ===> (inline seq, inline f)
         case (/\a. fst (reverse a, True)) of !f -> body

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

    ===> (SPLIT-STRICT)
         let f = /\a \(d::Eq a). fst (member, True) in f `seq` body

    ===> (inline seq, case of value)
         let f = /\a \(d::Eq a). fst (member, True) in body

Note that the bang has no effect at all in this case
