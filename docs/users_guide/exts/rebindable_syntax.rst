.. _rebindable-syntax:

Rebindable syntax and the implicit Prelude import
-------------------------------------------------

.. extension:: NoImplicitPrelude
    :shortdesc: Don't implicitly ``import Prelude``.
        Implied by :extension:`RebindableSyntax`.

    :since: 6.8.1

    Don't import ``Prelude`` by default.

GHC normally imports ``Prelude.hi`` files for
you. If you'd rather it didn't, then give it a ``-XNoImplicitPrelude``
option. The idea is that you can then import a Prelude of your own. (But
don't call it ``Prelude``; the Haskell module namespace is flat, and you
must not conflict with any Prelude module.)

.. extension:: RebindableSyntax
    :shortdesc: Employ rebindable syntax.
        Implies :extension:`NoImplicitPrelude`.

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
    :shortdesc: Enable postfix operators.

    :since: 7.10.1

    Allow the use of post-fix operators

The :extension:`PostfixOperators` extension enables a small extension to the syntax
of left operator sections, which allows you to define postfix operators.
The extension is this: the left section ::

      (e !)

is equivalent (from the point of view of both type checking and
execution) to the expression ::

      ((!) e)

(for any expression ``e`` and operator ``(!)``. The strict Haskell 98
interpretation is that the section is equivalent to ::

      (\y -> (!) e y)

That is, the operator must be a function of two arguments. GHC allows it
to take only one argument, and that in turn allows you to write the
function postfix.

The extension does not extend to the left-hand side of function
definitions; you must define such a function in prefix form.


