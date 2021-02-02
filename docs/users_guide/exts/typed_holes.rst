.. _typed-holes:

Typed Holes
===========

Typed holes are a feature of GHC that allows special placeholders
written with a leading underscore (e.g., "``_``", "``_foo``",
"``_bar``"), to be used as expressions. During compilation these holes
will generate an error message that describes which type is expected at
the hole's location, information about the origin of any free type
variables, and a list of local bindings that might help fill the hole
and bindings in scope that fit the type of the hole that might help fill
the hole with actual code. Typed holes are always enabled in GHC.

The goal of typed holes is to help with writing Haskell code rather than
to change the type system. Typed holes can be used to obtain extra
information from the type checker, which might otherwise be hard to get.
Normally, using GHCi, users can inspect the (inferred) type signatures
of all top-level bindings. However, this method is less convenient with
terms that are not defined on top-level or inside complex expressions.
Holes allow the user to check the type of the term they are about to
write.

For example, compiling the following module with GHC: ::

    f :: a -> a
    f x = _

will fail with the following error: ::

    hole.hs:2:7:
        Found hole `_' with type: a
        Where: `a' is a rigid type variable bound by
                   the type signature for f :: a -> a at hole.hs:1:6
        In the expression: _
        In an equation for `f': f x = _
        Relevant bindings include
          x :: a (bound at hole.hs:2:3)
          f :: a -> a (bound at hole.hs:2:1)
        Valid hole fits include x :: a (bound at hole.hs:2:3)

Here are some more details:

-  A "``Found hole``" error usually terminates compilation, like any
   other type error. After all, you have omitted some code from your
   program. Nevertheless, you can run and test a piece of code
   containing holes, by using the :ghc-flag:`-fdefer-typed-holes` flag. This
   flag defers errors produced by typed holes until runtime, and
   converts them into compile-time warnings. These warnings can in turn
   be suppressed entirely by :ghc-flag:`-Wno-typed-holes <-Wtyped-holes>`.

   The same behaviour for "``Variable out of scope``" errors, it terminates
   compilation by default. You can defer such errors by using the
   :ghc-flag:`-fdefer-out-of-scope-variables` flag. This flag defers errors
   produced by out of scope variables until runtime, and
   converts them into compile-time warnings. These warnings can in turn
   be suppressed entirely by :ghc-flag:`-Wno-deferred-out-of-scope-variables
   <-Wdeferred-out-of-scope-variables>`.

   The result is that a hole or a variable will behave like ``undefined``, but with
   the added benefits that it shows a warning at compile time, and will
   show the same message if it gets evaluated at runtime. This behaviour
   follows that of the :ghc-flag:`-fdefer-type-errors` option, which implies
   :ghc-flag:`-fdefer-typed-holes` and :ghc-flag:`-fdefer-out-of-scope-variables`.
   See :ref:`defer-type-errors`.

-  All unbound identifiers are treated as typed holes, *whether or not
   they start with an underscore*. The only difference is in the error
   message: ::

       cons z = z : True : _x : y

   yields the errors

   .. code-block:: none

       Foo.hs:3:21: error:
          Found hole: _x :: Bool
          Or perhaps ‘_x’ is mis-spelled, or not in scope
          In the first argument of ‘(:)’, namely ‘_x’
          In the second argument of ‘(:)’, namely ‘_x : y’
          In the second argument of ‘(:)’, namely ‘True : _x : y’
          Relevant bindings include
            z :: Bool (bound at Foo.hs:3:6)
          cons :: Bool -> [Bool] (bound at Foo.hs:3:1)
          Valid hole fits include
            z :: Bool (bound at mpt.hs:2:6)
            otherwise :: Bool
              (imported from ‘Prelude’ at mpt.hs:1:8-10
              (and originally defined in ‘GHC.Base’))
            False :: Bool
              (imported from ‘Prelude’ at mpt.hs:1:8-10
              (and originally defined in ‘GHC.Types’))
            True :: Bool
              (imported from ‘Prelude’ at mpt.hs:1:8-10
              (and originally defined in ‘GHC.Types’))
            maxBound :: forall a. Bounded a => a
              with maxBound @Bool
              (imported from ‘Prelude’ at mpt.hs:1:8-10
              (and originally defined in ‘GHC.Enum’))
            minBound :: forall a. Bounded a => a
              with minBound @Bool
              (imported from ‘Prelude’ at mpt.hs:1:8-10
              (and originally defined in ‘GHC.Enum’))

       Foo.hs:3:26: error:
           Variable not in scope: y :: [Bool]

   More information is given for explicit holes (i.e. ones that start
   with an underscore), than for out-of-scope variables, because the
   latter are often unintended typos, so the extra information is
   distracting. If you want the detailed information, use a leading
   underscore to make explicit your intent to use a hole.

-  Unbound identifiers with the same name are never unified, even within
   the same function, but shown individually. For example: ::

       cons = _x : _x

   results in the following errors:

   .. code-block:: none

      unbound.hs:1:8:
          Found hole '_x' with type: a
          Where: `a' is a rigid type variable bound by
                     the inferred type of cons :: [a] at unbound.hs:1:1
          In the first argument of `(:)', namely `_x'
          In the expression: _x : _x
          In an equation for `cons': cons = _x : _x
          Relevant bindings include cons :: [a] (bound at unbound.hs:1:1)

      unbound.hs:1:13:
          Found hole: _x :: [a]
          Where: ‘a’ is a rigid type variable bound by
                  the inferred type of cons :: [a]
                  at unbound.hs:3:1-12
          Or perhaps ‘_x’ is mis-spelled, or not in scope
          In the second argument of ‘(:)’, namely ‘_x’
          In the expression: _x : _x
          In an equation for ‘cons’: cons = _x : _x
          Relevant bindings include cons :: [a] (bound at unbound.hs:3:1)
          Valid hole fits include
            cons :: forall a. [a]
              with cons @a
              (defined at mpt.hs:3:1)
            mempty :: forall a. Monoid a => a
              with mempty @[a]
              (imported from ‘Prelude’ at mpt.hs:1:8-10
              (and originally defined in ‘GHC.Base’))

   Notice the two different types reported for the two different
   occurrences of ``_x``.

-  No language extension is required to use typed holes. The lexeme
   "``_``" was previously illegal in Haskell, but now has a more
   informative error message. The lexeme "``_x``" is a perfectly legal
   variable, and its behaviour is unchanged when it is in scope. For
   example ::

       f _x = _x + 1

   does not elicit any errors. Only a variable *that is not in scope*
   (whether or not it starts with an underscore) is treated as an error
   (which it always was), albeit now with a more informative error
   message.

-  Unbound data constructors used in expressions behave exactly as
   above. However, unbound data constructors used in *patterns* cannot
   be deferred, and instead bring compilation to a halt. (In
   implementation terms, they are reported by the renamer rather than
   the type checker.)

- The list of valid hole fits is found by checking which bindings in scope
  would fit into the hole. As an example, compiling the following module with
  GHC: ::

      import Data.List (inits)

      g :: [String]
      g = _ "hello, world"

  yields the errors:


  .. code-block:: none


    • Found hole: _ :: [Char] -> [String]
    • In the expression: _
      In the expression: _ "hello, world"
      In an equation for ‘g’: g = _ "hello, world"
    • Relevant bindings include g :: [String] (bound at mpt.hs:6:1)
      Valid hole fits include
        lines :: String -> [String]
          (imported from ‘Prelude’ at mpt.hs:3:8-9
           (and originally defined in ‘base-4.11.0.0:Data.OldList’))
        words :: String -> [String]
          (imported from ‘Prelude’ at mpt.hs:3:8-9
           (and originally defined in ‘base-4.11.0.0:Data.OldList’))
        inits :: forall a. [a] -> [[a]]
          with inits @Char
          (imported from ‘Data.List’ at mpt.hs:4:19-23
           (and originally defined in ‘base-4.11.0.0:Data.OldList’))
        repeat :: forall a. a -> [a]
          with repeat @String
          (imported from ‘Prelude’ at mpt.hs:3:8-9
           (and originally defined in ‘GHC.List’))
        fail :: forall (m :: * -> *). Monad m => forall a. String -> m a
          with fail @[] @String
          (imported from ‘Prelude’ at mpt.hs:3:8-9
           (and originally defined in ‘GHC.Base’))
        return :: forall (m :: * -> *). Monad m => forall a. a -> m a
          with return @[] @String
          (imported from ‘Prelude’ at mpt.hs:3:8-9
           (and originally defined in ‘GHC.Base’))
        pure :: forall (f :: * -> *). Applicative f => forall a. a -> f a
          with pure @[] @String
          (imported from ‘Prelude’ at mpt.hs:3:8-9
           (and originally defined in ‘GHC.Base’))
        read :: forall a. Read a => String -> a
          with read @[String]
          (imported from ‘Prelude’ at mpt.hs:3:8-9
           (and originally defined in ‘Text.Read’))
        mempty :: forall a. Monoid a => a
          with mempty @([Char] -> [String])
          (imported from ‘Prelude’ at mpt.hs:3:8-9
           (and originally defined in ‘GHC.Base’))

There are a few flags for controlling the amount of context information shown
for typed holes:

.. ghc-flag:: -fshow-hole-constraints
    :shortdesc: Show constraints when reporting typed holes.
    :type: dynamic
    :category: verbosity

    When reporting typed holes, also print constraints that are in scope.
    Example: ::

        f :: Eq a => a -> Bool
        f x = _

    results in the following message:

    .. code-block:: none

      show_constraints.hs:4:7: error:
          • Found hole: _ :: Bool
          • In the expression: _
            In an equation for ‘f’: f x = _
          • Relevant bindings include
              x :: a (bound at show_constraints.hs:4:3)
              f :: a -> Bool (bound at show_constraints.hs:4:1)
            Constraints include Eq a (from show_constraints.hs:3:1-22)
            Valid hole fits include
              otherwise :: Bool
              False :: Bool
              True :: Bool
              maxBound :: forall a. Bounded a => a
                with maxBound @Bool
              minBound :: forall a. Bounded a => a
                with minBound @Bool

.. _typed-hole-valid-hole-fits:

Valid Hole Fits
-------------------
GHC sometimes suggests valid hole fits for typed holes, which is
configurable by a few flags.

.. ghc-flag:: -fno-show-valid-hole-fits
    :shortdesc: Disables showing a list of valid hole fits for typed holes
        in type error messages.
    :type: dynamic
    :category: verbosity

    :default: off

    This flag can be toggled to turn off the display of valid hole fits
    entirely.

.. ghc-flag:: -fmax-valid-hole-fits=⟨n⟩
    :shortdesc: *default: 6.* Set the maximum number of valid hole fits for
        typed holes to display in type error messages.
    :type: dynamic
    :reverse: -fno-max-valid-hole-fits
    :category: verbosity

    :default: 6

    The list of valid hole fits is limited by displaying up to 6
    hole fits per hole. The number of hole fits shown can be set by this
    flag. Turning the limit off with :ghc-flag:`-fno-max-valid-hole-fits`
    displays all found hole fits.


.. ghc-flag:: -fshow-type-of-hole-fits
    :shortdesc: Toggles whether to show the type of the valid hole fits
       in the output.
    :type: dynamic
    :category: verbosity
    :reverse: -fno-show-type-of-hole-fits

    :default: on

    By default, the hole fits show the type of the hole fit.
    This can be turned off by the reverse of this flag.



.. ghc-flag:: -fshow-type-app-of-hole-fits
    :shortdesc: Toggles whether to show the type application of the valid
       hole fits in the output.
    :type: dynamic
    :category: verbosity
    :reverse: -fno-show-type-app-of-hole-fits

    :default: on

    By default, the hole fits show the type application needed to make
    this hole fit fit the type of the hole, e.g. for the hole
    ``(_ :: Int -> [Int])``, ``mempty`` is a hole fit with
    ``mempty @(Int -> [Int])``. This can be toggled off with
    the reverse of this flag.

.. ghc-flag:: -fshow-docs-of-hole-fits
    :shortdesc: Toggles whether to show the documentation of the valid
       hole fits in the output.
    :type: dynamic
    :category: verbosity
    :reverse: -fno-show-docs-of-hole-fits

    :default: off

    It can sometime be the case that the name and type of a valid hole
    fit is not enough to realize what the fit stands for. This flag
    adds the documentation of the fit to the message, if the
    documentation is available (and the module from which the function
    comes was compiled with the ``-haddock`` flag).

.. ghc-flag:: -fshow-type-app-vars-of-hole-fits
    :shortdesc: Toggles whether to show what type each quantified
       variable takes in a valid hole fit.
    :type: dynamic
    :category: verbosity
    :reverse: -fno-show-type-app-vars-of-hole-fits

    :default: on

    By default, the hole fits show the type application needed to make
    this hole fit fit the type of the hole, e.g. for the hole
    ``(_ :: Int -> [Int])``, ``mempty :: Monoid a => a`` is a hole fit
    with ``mempty @(Int -> [Int])``. This flag toggles whether to show
    ``a ~ (Int -> [Int])`` instead of ``mempty @(Int -> [Int])`` in the where
    clause of the valid hole fit message.

.. ghc-flag:: -fshow-provenance-of-hole-fits
    :shortdesc: Toggles whether to show the provenance of the valid hole fits
       in the output.
    :type: dynamic
    :category: verbosity
    :reverse: -fno-show-provenance-of-hole-fits

    :default: on

    By default, each hole fit shows the provenance information of its
    hole fit, i.e. where it was bound or defined, and what module
    it was originally defined in if it was imported. This can be toggled
    off using the reverse of this flag.


.. ghc-flag:: -funclutter-valid-hole-fits
    :shortdesc: Unclutter the list of valid hole fits by not showing
        provenance nor type applications of suggestions.
    :type: dynamic
    :category: verbosity

    :default: off

    This flag can be toggled to decrease the verbosity of the valid hole fit
    suggestions by not showing the provenance nor type application of the
    suggestions.



.. _typed-holes-refinement-hole-fits:

Refinement Hole Fits
~~~~~~~~~~~~~~~~~~~~~~~~

When the flag :ghc-flag:`-frefinement-level-hole-fits=⟨n⟩` is set to an
``n`` larger than ``0``, GHC will offer up a list of valid refinement
hole fits, which are valid hole fits that need up to ``n`` levels of
additional refinement to be complete, where each level represents an additional
hole in the hole fit that requires filling in.  As an example, consider the
hole in ::

  f :: [Integer] -> Integer
  f = _

When the refinement level is not set, it will only offer valid hole fits
suggestions: ::

  Valid hole fits include
    f :: [Integer] -> Integer
    head :: forall a. [a] -> a
      with head @Integer
    last :: forall a. [a] -> a
      with last @Integer
    maximum :: forall (t :: * -> *).
                Foldable t =>
                forall a. Ord a => t a -> a
      with maximum @[] @Integer
    minimum :: forall (t :: * -> *).
                Foldable t =>
                forall a. Ord a => t a -> a
      with minimum @[] @Integer
    product :: forall (t :: * -> *).
                Foldable t =>
                forall a. Num a => t a -> a
      with product @[] @Integer
    sum :: forall (t :: * -> *).
            Foldable t =>
            forall a. Num a => t a -> a
      with sum @[] @Integer

However, with :ghc-flag:`-frefinement-level-hole-fits=⟨n⟩` set to e.g. `1`,
it will additionally offer up a list of refinement hole fits, in this case: ::

  Valid refinement hole fits include
    foldl1 (_ :: Integer -> Integer -> Integer)
      with foldl1 @[] @Integer
      where foldl1 :: forall (t :: * -> *).
                      Foldable t =>
                      forall a. (a -> a -> a) -> t a -> a
    foldr1 (_ :: Integer -> Integer -> Integer)
      with foldr1 @[] @Integer
      where foldr1 :: forall (t :: * -> *).
                      Foldable t =>
                      forall a. (a -> a -> a) -> t a -> a
    const (_ :: Integer)
      with const @Integer @[Integer]
      where const :: forall a b. a -> b -> a
    ($) (_ :: [Integer] -> Integer)
      with ($) @GHC.Types.LiftedRep @[Integer] @Integer
      where ($) :: forall a b. (a -> b) -> a -> b
    fail (_ :: String)
      with fail @((->) [Integer]) @Integer
      where fail :: forall (m :: * -> *).
                    Monad m =>
                    forall a. String -> m a
    return (_ :: Integer)
      with return @((->) [Integer]) @Integer
      where return :: forall (m :: * -> *). Monad m => forall a. a -> m a
    (Some refinement hole fits suppressed;
      use -fmax-refinement-hole-fits=N or -fno-max-refinement-hole-fits)

Which shows that the hole could be replaced with e.g. ``foldl1 _``. While not
fixing the hole, this can help users understand what options they have.

.. ghc-flag:: -frefinement-level-hole-fits=⟨n⟩
    :shortdesc: *default: off.* Sets the level of refinement of the
         refinement hole fits, where level ``n`` means that hole fits
         of up to ``n`` holes will be considered.
    :type: dynamic
    :reverse: -fno-refinement-level-hole-fits
    :category: verbosity

    :default: off

    The list of valid refinement hole fits is generated by considering
    hole fits with a varying amount of additional holes. The amount of
    holes in a refinement can be set by this flag. If the flag is set to 0
    or not set at all, no valid refinement hole fits will be suggested.

.. ghc-flag:: -fabstract-refinement-hole-fits
    :shortdesc: *default: off.* Toggles whether refinements where one or more
         of the holes are abstract are reported.
    :type: dynamic
    :reverse: -fno-abstract-refinement-hole-fits
    :category: verbosity

    :default: off

    Valid list of valid refinement hole fits can often grow large when
    the refinement level is ``>= 2``, with holes like ``head _ _`` or
    ``fst _ _``, which are valid refinements, but which are unlikely to be
    relevant since one or more of the holes are still completely open, in that
    neither the type nor kind of those holes are constrained by the proposed
    identifier at all. By default, such holes are not reported. By turning this
    flag on, such holes are included in the list of valid refinement hole fits.

.. ghc-flag:: -fmax-refinement-hole-fits=⟨n⟩
    :shortdesc: *default: 6.* Set the maximum number of refinement hole fits
         for typed holes to display in type error messages.
    :type: dynamic
    :reverse: -fno-max-refinement-hole-fits
    :category: verbosity

    :default: 6

    The list of valid refinement hole fits is limited by displaying up to 6
    hole fits per hole. The number of hole fits shown can be set by this
    flag. Turning the limit off with :ghc-flag:`-fno-max-refinement-hole-fits`
    displays all found hole fits.

.. ghc-flag:: -fshow-hole-matches-of-hole-fits
    :shortdesc: Toggles whether to show the type of the additional holes
       in refinement hole fits.
    :type: dynamic
    :category: verbosity
    :reverse: -fno-show-hole-matches-of-hole-fits

    :default: on

    The types of the additional holes in refinement hole fits are displayed
    in the output, e.g. ``foldl1 (_ :: a -> a -> a)`` is a refinement
    for the hole ``_ :: [a] -> a``. If this flag is toggled off, the output
    will display only ``foldl1 _``, which can be used as a direct replacement
    for the hole, without requiring ``-XScopedTypeVariables``.




Sorting Valid Hole Fits
~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are currently two ways to sort valid hole fits.
Sorting can be toggled with :ghc-flag:`-fsort-valid-hole-fits`

.. ghc-flag:: -fno-sort-valid-hole-fits
    :shortdesc: Disables the sorting of the list of valid hole fits for typed holes
        in type error messages.
    :type: dynamic
    :reverse: -fsort-valid-hole-fits
    :category: verbosity

    :default: off

    By default the valid hole fits are sorted to show the most relevant
    hole fits at the top of the list of valid hole fits. This can be
    toggled off with this flag.

.. ghc-flag:: -fsort-by-size-hole-fits
    :shortdesc: Sort valid hole fits by size.
    :type: dynamic
    :reverse: -fno-sort-by-size-hole-fits
    :category: verbosity

    :default: on

    Sorts by how big the types the quantified type variables in the type of the
    function would have to be in order to match the type of the hole.


.. ghc-flag:: -fsort-by-subsumption-hole-fits
    :shortdesc: Sort valid hole fits by subsumption.
    :type: dynamic
    :reverse: -fno-sort-by-subsumption-hole-fits
    :category: verbosity

    :default: off

    An alternative sort. Sorts by checking which hole fits subsume other
    hole fits, such that if hole fit a could be used as hole fits for
    hole fit b, then b appears before a in the output. It is more precise than
    the default sort, but also a lot slower, since a subsumption check has to be
    run for each pair of valid hole fits.


