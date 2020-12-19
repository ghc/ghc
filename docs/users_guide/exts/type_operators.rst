.. _type-operators:

Type operators
--------------

.. extension:: TypeOperators
    :shortdesc: Enable type operators.
        Implies :extension:`ExplicitNamespaces`.

    :implies: :extension:`ExplicitNamespaces`
    :since: 6.8.1

    Allow the use and definition of types with operator names.

The language :extension:`TypeOperators` allows you to use infix operators
in types.

-  Operator symbols are *constructors* rather than type
   *variables* (as they are in terms).

-  Operator symbols in types can be written infix, both in definitions
   and uses. For example: ::

       data a + b = Plus a b
       type Foo = Int + Bool

-  Alphanumeric type constructors can now be written infix, using backquote
   syntax::

     x :: Int `Either` Bool
     x = Left 5
       
-  There is now some potential ambiguity in import and export lists; for
   example if you write ``import M( (+) )`` do you mean the *function*
   ``(+)`` or the *type constructor* ``(+)``? The default is the former,
   but with :extension:`ExplicitNamespaces` (which is implied by
   :extension:`TypeOperators`) GHC allows you to specify the latter by
   preceding it with the keyword ``type``, thus: ::

       import M( type (+) )

   See :ref:`explicit-namespaces`.

-  The fixity of a type operator may be set using the usual fixity
   declarations but, as in :ref:`infix-tycons`, the function and type
   constructor share a single fixity.

-  There is now potential ambiguity in the traditional syntax for
   data constructor declarations. For example::

     type a :+: b = Either a b
     data X = Int :+: Bool :+: Char

   This code wants to declare both a type-level ``:+:`` and a term-level
   ``:+:`` (which is, generally, allowed). But we cannot tell how to
   parenthesize the data constructor declaration in ``X``: either way
   makes sense. We might
   imagine that a fixity declaration could help us, but it is awkward
   to apply the fixity declaration to the very definition of a new
   data constructor. Instead of declaring delicate rules around this
   issue, GHC simply rejects if the top level of a traditional-syntax
   data constructor declaration uses two operators without parenthesizing.
