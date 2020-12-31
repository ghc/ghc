.. _defer-type-errors:

Deferring type errors to runtime
================================

While developing, sometimes it is desirable to allow compilation to
succeed even if there are type errors in the code. Consider the
following case: ::

    module Main where

    a :: Int
    a = 'a'

    main = print "b"

Even though ``a`` is ill-typed, it is not used in the end, so if all
that we're interested in is ``main`` it can be useful to be able to
ignore the problems in ``a``.

For more motivation and details please refer to the
:ghc-wiki:`Wiki <defer-errors-to-runtime>` page or the `original
paper <http://dreixel.net/research/pdf/epdtecp.pdf>`__.

Enabling deferring of type errors
---------------------------------

The flag :ghc-flag:`-fdefer-type-errors` controls whether type errors are
deferred to runtime. Type errors will still be emitted as warnings, but
will not prevent compilation. You can use :ghc-flag:`-Wno-deferred-type-errors`
to suppress these warnings.

This flag implies the :ghc-flag:`-fdefer-typed-holes` and
:ghc-flag:`-fdefer-out-of-scope-variables` flags, which enables this behaviour
for `typed holes <#typed-holes>`__ and variables. Should you so wish, it is
possible to enable :ghc-flag:`-fdefer-type-errors` without enabling
:ghc-flag:`-fdefer-typed-holes` or :ghc-flag:`-fdefer-out-of-scope-variables`,
by explicitly specifying :ghc-flag:`-fno-defer-typed-holes
<-fdefer-typed-holes>` or :ghc-flag:`-fno-defer-out-of-scope-variables
<-fdefer-out-of-scope-variables>` on the command-line after the
:ghc-flag:`-fdefer-type-errors` flag.

At runtime, whenever a term containing a type error would need to be
evaluated, the error is converted into a runtime exception of type
``TypeError``. Note that type errors are deferred as much as possible
during runtime, but invalid coercions are never performed, even when
they would ultimately result in a value of the correct type. For
example, given the following code: ::

    x :: Int
    x = 0

    y :: Char
    y = x

    z :: Int
    z = y

evaluating ``z`` will result in a runtime ``TypeError``.

Deferred type errors in GHCi
----------------------------

The flag :ghc-flag:`-fdefer-type-errors` works in GHCi as well, with one
exception: for "naked" expressions typed at the prompt, type errors
don't get delayed, so for example: ::

    Prelude> fst (True, 1 == 'a')

    <interactive>:2:12:
        No instance for (Num Char) arising from the literal `1'
        Possible fix: add an instance declaration for (Num Char)
        In the first argument of `(==)', namely `1'
        In the expression: 1 == 'a'
        In the first argument of `fst', namely `(True, 1 == 'a')'

Otherwise, in the common case of a simple type error such as typing
``reverse True`` at the prompt, you would get a warning and then an
immediately-following type error when the expression is evaluated.

This exception doesn't apply to statements, as the following example
demonstrates:

.. code-block:: none

    Prelude> let x = (True, 1 == 'a')

    <interactive>:3:16: Warning:
        No instance for (Num Char) arising from the literal `1'
        Possible fix: add an instance declaration for (Num Char)
        In the first argument of `(==)', namely `1'
        In the expression: 1 == 'a'
        In the expression: (True, 1 == 'a')
    Prelude> fst x
    True

Limitations of deferred type errors
-----------------------------------
The errors that can be deferred are:

- Out of scope term variables
- Equality constraints; e.g. ``ord True`` gives rise to an insoluble equality constraint ``Char ~ Bool``, which can be deferred.
- Type-class and implicit-parameter constraints

All other type errors are reported immediately, and cannot be deferred; for
example, an ill-kinded type signature, an instance declaration that is
non-terminating or ill-formed, a type-family instance that does not
obey the declared injectivity constraints, etc etc.

In a few cases, equality constraints cannot be deferred.  Specifically:

- Kind errors in a type or kind signature, partial type signatures, or pattern signature.
  e.g. ::

    f :: Int Bool -> Char

  This type signature contains a kind error which cannot be deferred.

- Type equalities under a forall (c.f. `#14605
  <https://gitlab.haskell.org/ghc/ghc/issues/14605>`_).

- Kind errors in a visible type application. e.g. ::

    reverse @Maybe xs

- Kind errors in a ``default`` declaration.  e.g. ::

    default( Double, Int Int )
