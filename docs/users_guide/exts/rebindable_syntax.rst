.. _rebindable-syntax:

Rebindable syntax and the implicit Prelude import
-------------------------------------------------

.. extension:: ImplicitPrelude
    :shortdesc: Implicitly import ``Prelude``.

    :implied by: :extension:`RebindableSyntax` implies :extension:`NoImplicitPrelude`.
    :since: 6.8.1

    Implicitly import the ``Prelude`` module by default.

    The implicit import can be refined in a module by explicitly writing an
    import of the form::

      import Prelude (foo)

    This will only import ``foo`` from ``Prelude`` rather than the whole module
    as the implicit import.

GHC normally imports the ``Prelude`` module for
you. If you'd rather it didn't, then give it a ``-XNoImplicitPrelude``
option. The idea is that you can then import a Prelude of your own.

.. extension:: RebindableSyntax
    :shortdesc: Allow rebinding of builtin syntax.

    :implies: :extension:`NoImplicitPrelude`
    :since: 7.0.1

    Enable rebinding of a variety of usually-built-in operations.

Suppose you are importing a Prelude of your own in order to define your
own numeric class hierarchy. It completely defeats that purpose if the
literal "1" means "``Prelude.fromInteger 1``", which is what the Haskell
Report specifies. So the :extension:`RebindableSyntax` extension causes the
following pieces of built-in syntax to refer to *whatever is in scope*,
not the Prelude versions:

-  An integer literal ``368`` means "``fromInteger (368::Integer)``",
   rather than "``Prelude.fromInteger (368::Integer)``".

-  Fractional literals are handled in just the same way, except that the
   translation is ``fromRational (3.68::Rational)``.

-  String literals are also handled the same way, except that the
   translation is ``fromString ("368"::String)``.

-  The equality test in an overloaded numeric pattern uses whatever
   ``(==)`` is in scope.

-  The subtraction operation, and the greater-than-or-equal test, in
   ``n+k`` patterns use whatever ``(-)`` and ``(>=)`` are in scope.

-  Negation (e.g. "``- (f x)``") means "``negate (f x)``", both in
   numeric patterns, and expressions.

-  Conditionals (e.g. "``if`` e1 ``then`` e2 ``else`` e3") means
   "``ifThenElse`` e1 e2 e3". However ``case`` expressions are
   unaffected.

-  "Do" notation is translated using whatever functions ``(>>=)``,
   ``(>>)``, and ``fail``, are in scope (not the Prelude versions). List
   comprehensions, ``mdo`` (:ref:`recursive-do-notation`), and parallel
   array comprehensions, are unaffected.

-  Arrow notation (see :ref:`arrow-notation`) uses whatever ``arr``,
   ``(>>>)``, ``first``, ``app``, ``(|||)`` and ``loop`` functions are
   in scope. But unlike the other constructs, the types of these
   functions must match the Prelude types very closely. Details are in
   flux; if you want to use this, ask!

-  List notation, such as ``[x,y]`` or ``[m..n]`` can also be treated
   via rebindable syntax if you use `-XOverloadedLists`;
   see :ref:`overloaded-lists`.

-  An overloaded label "``#foo``" means "``fromLabel @"foo"``", rather than
   "``GHC.OverloadedLabels.fromLabel @"foo"``" (see :ref:`overloaded-labels`).

:extension:`RebindableSyntax` implies :extension:`NoImplicitPrelude`.

In all cases (apart from arrow notation), the static semantics should be
that of the desugared form, even if that is a little unexpected. For
example, the static semantics of the literal ``368`` is exactly that of
``fromInteger (368::Integer)``; it's fine for ``fromInteger`` to have
any of the types: ::

    fromInteger :: Integer -> Integer
    fromInteger :: forall a. Foo a => Integer -> a
    fromInteger :: Num a => a -> Integer
    fromInteger :: Integer -> Bool -> Bool

Be warned: this is an experimental facility, with fewer checks than
usual. Use ``-dcore-lint`` to typecheck the desugared program. If Core
Lint is happy you should be all right.

Custom Prelude modules named ``Prelude``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you call your custom Prelude module ``Prelude`` and place it in a file called
``Prelude.hs``, then your custom Prelude will be implicitly imported instead of
the default Prelude.

Here is an example that compiles: ::

    $ cat Prelude.hs
    module Prelude where

    a = ()

    $ cat B.hs
    module B where

    foo = a

    $ ghc Prelude.hs B.hs
    [1 of 2] Compiling Prelude          ( Prelude.hs, Prelude.o )
    [2 of 2] Compiling B                ( B.hs, B.o )

The new ``Prelude`` is implicitly imported in ``B.hs``.

Here is an example that does not compile::

    $ cat Prelude.hs
    module Prelude where

    foo = True

    $ ghc Prelude.hs
    [1 of 1] Compiling Prelude          ( Prelude.hs, Prelude.o )

    Prelude.hs:3:7: error: Data constructor not in scope: True

The original ``Prelude`` module is shadowed by the custom Prelude in this case.
To include the original Prelude in your custom Prelude, you can explicitly
import it with the ``-XPackageImports`` option and ``import "base" Prelude``.

Writing an explicit import of ``Prelude`` will suppress the implicit import. This
allows you to refine the implicit import::

    $ cat Prelude.hs
    module Prelude where

    a = ()

    b = ()

    $ cat B.hs
    module B where

    import Prelude (b)

    -- a is now not in scope, there is no implicit Prelude import
    foo = a
    qux = b

    $ ghc Prelude.hs B.hs
    [1 of 2] Compiling Prelude          ( Prelude.hs, Prelude.o )
    [2 of 2] Compiling B                ( B.hs, B.o )
      B.hs:5:7: error: [GHC-88464]
          Variable not in scope: a
          Suggested fix:
            Add 'a' to the import list in the import of 'Prelude'
            (at B.hs:3:1-18).
        |
      5 | foo = a
        |


.. note::
  Importing a module named ``Prelude`` with the :extension:`PackageImports` extension will
  not affect the implicit ``Prelude`` import::

    > cat Prelude.hs
    module Prelude where

    a = ()

    > cat B.hs
    {-# LANGUAGE PackageImports #-}
    module B where

    import "base" Prelude

    -- This definition comes from the implicit prelude import
    foo = a

    -- These definitions come from the package import
    baz :: Int -> Int -> Int
    baz = (+)

    > ghc B.hs
    [1 of 2] Compiling Prelude          ( Prelude.hs, Prelude.o )
    [2 of 2] Compiling B                ( B.hs, B.o )

  If you want to use package imports then you should explicitly disable the import
  of the implicit prelude module by enabling :extension:`NoImplicitPrelude`.


Things unaffected by :extension:`RebindableSyntax`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:extension:`RebindableSyntax` does not apply to any code generated from a
``deriving`` clause or declaration. To see why, consider the following code: ::

    {-# LANGUAGE RebindableSyntax, OverloadedStrings #-}
    newtype Text = Text String

    fromString :: String -> Text
    fromString = Text

    data Foo = Foo deriving Show

This will generate code to the effect of: ::

    instance Show Foo where
      showsPrec _ Foo = showString "Foo"

But because :extension:`RebindableSyntax` and :extension:`OverloadedStrings`
are enabled, the ``"Foo"`` string literal would now be of type ``Text``, not
``String``, which ``showString`` doesn't accept! This causes the generated
``Show`` instance to fail to typecheck. It's hard to imagine any scenario where
it would be desirable have :extension:`RebindableSyntax` behavior within
derived code, so GHC simply ignores :extension:`RebindableSyntax` entirely
when checking derived code.

.. _postfix-operators:

Postfix operators
-----------------

.. extension:: PostfixOperators
    :shortdesc: Allow the use of postfix operators.

    :since: 7.10.1
    :status: Included in :extension:`GHC2024`, :extension:`GHC2021`

    Allow the use of post-fix operators

The :extension:`PostfixOperators` extension enables a small extension to the syntax
of left operator sections, which allows you to define postfix operators.
The extension is this: for any expression ``e`` and operator ``(!)``, the left
section ::

      (e !)

is equivalent (from the point of view of both type checking and
execution) to the expression ::

      ((!) e)

The strict Haskell 98 interpretation is that the section is equivalent to ::

      (\y -> (!) e y)

That is, the operator must be a function of two arguments. GHC allows it
to take only one argument, and that in turn allows you to write the
function postfix.

The extension does not extend to the left-hand side of function
definitions; you must define such a function in prefix form.
