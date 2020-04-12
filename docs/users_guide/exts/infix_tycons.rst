.. _infix-tycons:

Infix type constructors, classes, and type variables
----------------------------------------------------

GHC allows type constructors, classes, and type variables to be
operators, and to be written infix, very much like expressions. More
specifically:

-  A type constructor or class can be any non-reserved operator.
   Symbols used in types are always like capitalized identifiers; they
   are never variables. Note that this is different from the lexical
   syntax of data constructors, which are required to begin with a
   ``:``.

-  Data type and type-synonym declarations can be written infix,
   parenthesised if you want further arguments. E.g. ::

         data a :*: b = Foo a b
         type a :+: b = Either a b
         class a :=: b where ...

         data (a :**: b) x = Baz a b x
         type (a :++: b) y = Either (a,b) y

-  Types, and class constraints, can be written infix. For example ::

         x :: Int :*: Bool
         f :: (a :=: b) => a -> b

-  Back-quotes work as for expressions, both for type constructors and
   type variables; e.g. ``Int `Either` Bool``, or ``Int `a` Bool``.
   Similarly, parentheses work the same; e.g. ``(:*:) Int Bool``.

-  Fixities may be declared for type constructors, or classes, just as
   for data constructors. However, one cannot distinguish between the
   two in a fixity declaration; a fixity declaration sets the fixity for
   a data constructor and the corresponding type constructor. For
   example: ::

         infixl 7 T, :*:

   sets the fixity for both type constructor ``T`` and data constructor
   ``T``, and similarly for ``:*:``. ``Int `a` Bool``.

-  The function arrow ``->`` is ``infixr`` with fixity -1.


