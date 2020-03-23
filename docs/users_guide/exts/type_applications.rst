.. _visible-type-application:

Visible type application
========================

.. extension:: TypeApplications
    :shortdesc: Enable type application syntax in terms and types.

    :since: 8.0.1

    Allow the use of type application syntax.

The :extension:`TypeApplications` extension allows you to use
*visible type application* in expressions. Here is an
example: ``show (read @Int "5")``. The ``@Int``
is the visible type application; it specifies the value of the type variable
in ``read``'s type.

A visible type application is preceded with an ``@``
sign. (To disambiguate the syntax, the ``@`` must be
preceded with a non-identifier letter, usually a space. For example,
``read@Int 5`` would not parse.) It can be used whenever
the full polymorphic type of the function is known. If the function
is an identifier (the common case), its type is considered known only when
the identifier has been given a type signature. If the identifier does
not have a type signature, visible type application cannot be used.

GHC also permits visible kind application, where users can declare the kind
arguments to be instantiated in kind-polymorphic cases. Its usage parallels
visible type application in the term level, as specified above.

.. _inferred-vs-specified:

Inferred vs. specified type variables
-------------------------------------

.. index::
   single: type variable; inferred vs. specified

GHC tracks a distinction between what we call *inferred* and *specified*
type variables. Only specified type variables are available for instantiation
with visible type application. An example illustrates this well::

  f :: (Eq b, Eq a) => a -> b -> Bool
  f x y = (x == x) && (y == y)

  g x y = (x == x) && (y == y)

The functions ``f`` and ``g`` have the same body, but only ``f`` is given
a type signature. When GHC is figuring out how to process a visible type application,
it must know what variable to instantiate. It thus must be able to provide
an ordering to the type variables in a function's type.

If the user has supplied a type signature, as in ``f``, then this is easy:
we just take the ordering from the type signature, going left to right and
using the first occurrence of a variable to choose its position within the
ordering. Thus, the variables in ``f`` will be ``b``, then ``a``.

In contrast, there is no reliable way to do this for ``g``; we will not know
whether ``Eq a`` or ``Eq b`` will be listed first in the constraint in ``g``\'s
type. In order to have visible type application be robust between releases of
GHC, we thus forbid its use with ``g``.

We say that the type variables in ``f`` are *specified*, while those in
``g`` are *inferred*. The general rule is this: if the user has written
a type variable in the source program, it is *specified*; if not, it is
*inferred*.

This rule applies in datatype declarations, too. For example, if we have
``data Proxy a = Proxy`` (and :extension:`PolyKinds` is enabled), then
``a`` will be assigned kind ``k``, where ``k`` is a fresh kind variable.
Because ``k`` was not written by the user, it will be unavailable for
type application in the type of the constructor ``Proxy``; only the ``a``
will be available.

Inferred variables are printed in braces. Thus, the type of the data
constructor ``Proxy`` from the previous example is
``forall {k} (a :: k). Proxy a``.
We can observe this behavior in a GHCi session: ::

  > :set -XTypeApplications -fprint-explicit-foralls
  > let myLength1 :: Foldable f => f a -> Int; myLength1 = length
  > :type +v myLength1
  myLength1 :: forall (f :: * -> *) a. Foldable f => f a -> Int
  > let myLength2 = length
  > :type +v myLength2
  myLength2 :: forall {a} {t :: * -> *}. Foldable t => t a -> Int
  > :type +v myLength2 @[]

  <interactive>:1:1: error:
      • Cannot apply expression of type ‘t0 a0 -> Int’
        to a visible type argument ‘[]’
      • In the expression: myLength2 @[]

Notice that since ``myLength1`` was defined with an explicit type signature,
:ghci-cmd:`:type +v` reports that all of its type variables are available
for type application. On the other hand, ``myLength2`` was not given a type
signature. As a result, all of its type variables are surrounded with braces,
and trying to use visible type application with ``myLength2`` fails.

Also note the use of :ghci-cmd:`:type +v` in the GHCi session above instead
of :ghci-cmd:`:type`. This is because :ghci-cmd:`:type` gives you the type
that would be inferred for a variable assigned to the expression provided
(that is, the type of ``x`` in ``let x = <expr>``). As we saw above with
``myLength2``, this type will have no variables available to visible type
application. On the other hand, :ghci-cmd:`:type +v` gives you the actual
type of the expression provided. To illustrate this: ::

  > :type myLength1
  myLength1 :: forall {a} {f :: * -> *}. Foldable f => f a -> Int
  > :type myLength2
  myLength2 :: forall {a} {t :: * -> *}. Foldable t => t a -> Int

Using :ghci-cmd:`:type` might lead one to conclude that none of the type
variables in ``myLength1``'s type signature are available for type
application. This isn't true, however! Be sure to use :ghci-cmd:`:type +v`
if you want the most accurate information with respect to visible type
application properties.

.. index::
   single: ScopedSort

.. _ScopedSort:

Ordering of specified variables
-------------------------------

In the simple case of the previous section, we can say that specified variables
appear in left-to-right order. However, not all cases are so simple. Here are
the rules in the subtler cases:

- If an identifier's type has a ``forall``, then the order of type variables
  as written in the ``forall`` is retained.

- If any of the variables depend on other variables (that is, if some
  of the variables are *kind* variables), the variables are reordered
  so that kind variables come before type variables, preserving the
  left-to-right order as much as possible. That is, GHC performs a
  stable topological sort on the variables. Example::

    h :: Proxy (a :: (j, k)) -> Proxy (b :: Proxy a) -> ()
      -- as if h :: forall j k a b. ...

  In this example, ``a`` depends on ``j`` and ``k``, and ``b`` depends on ``a``.
  Even though ``a`` appears lexically before ``j`` and ``k``, ``j`` and ``k``
  are quantified first, because ``a`` depends on ``j`` and ``k``. Note further
  that ``j`` and ``k`` are not reordered with respect to each other, even
  though doing so would not violate dependency conditions.

  A "stable topological sort" here, we mean that we perform this algorithm
  (which we call *ScopedSort*):

  * Work left-to-right through the input list of type variables, with a cursor.
  * If variable ``v`` at the cursor is depended on by any earlier variable ``w``,
    move ``v`` immediately before the leftmost such ``w``.

- Class methods' type arguments include the class type
  variables, followed by any variables an individual method is polymorphic
  in. So, ``class Monad m where return :: a -> m a`` means
  that ``return``'s type arguments are ``m, a``.

- With the :extension:`RankNTypes` extension
  (:ref:`universal-quantification`), it is possible to declare
  type arguments somewhere other than the beginning of a type. For example,
  we can have ``pair :: forall a. a -> forall b. b -> (a, b)``
  and then say ``pair @Bool True @Char`` which would have
  type ``Char -> (Bool, Char)``.

- Partial type signatures (:ref:`partial-type-signatures`)
  work nicely with visible type
  application. If you want to specify only the second type argument to
  ``wurble``, then you can say ``wurble @_ @Int``.
  The first argument is a wildcard, just like in a partial type signature.
  However, if used in a visible type application/visible kind application,
  it is *not* necessary to specify :extension:`PartialTypeSignatures` and your
  code will not generate a warning informing you of the omitted type.

The section in this manual on kind polymorphism describes how variables
in type and class declarations are ordered (:ref:`inferring-variable-order`).

.. _Manually-defining-inferred-variables:

Manually defining inferred variables
------------------------------------

While user-written type or kind variables are specified by default, GHC permits
labelling these variables as inferred. By writing the type variable binder in
braces as ``{tyvar}`` or ``{tyvar :: kind}``, the new variable will be
classified as inferred, not specified. Doing so gives the programmer control
over which variables can be manually instantiated and which can't.
Note that the braces do not influence scoping: variables in braces are still
brought into scope just the same.
Consider for example::

  myConst :: forall {a} b. a -> b -> a
  myConst x _ = x

In this example, despite both variables appearing in a type signature, ``a`` is
an inferred variable while ``b`` is specified. This means that the expression
``myConst @Int`` has type ``forall {a}. a -> Int -> a``.

The braces are allowed in the following places:

- In the type signatures of functions, variables, class methods, as well as type
  annotations on expressions. Consider the example above.

- In data constructor declarations, using the GADT syntax. Consider::

    data T a where MkT :: forall {k} (a :: k). Proxy a -> T a

  The constructor ``MkT`` defined in this example is kind polymorphic, which is
  emphasized to the reader by explicitly abstracting over the ``k`` variable.
  As this variable is marked as inferred, it can not be manually instantiated.

- In existential variable quantifications, e.g.::

    data HList = HNil
               | forall {a}. HCons a HList

- In pattern synonym signatures. Consider for instance::

    data T a where MkT :: forall a b. a -> b -> T a

    pattern Pat :: forall {c}. () => forall {d}. c -> d -> T c
    pattern Pat x y = MkT x y

  Note that in this example, ``a`` is a universal variable in the data type
  ``T``, where ``b`` is existential. When writing the pattern synonym, both
  types are allowed to be specified or inferred.

- On the right-hand side of a type synonym, e.g.::

    type Foo = forall a {b}. Either a b

- In type signatures on variables bound in RULES, e.g.::

    {-# RULES "parametricity" forall (f :: forall {a}. a -> a). map f = id #-}

The braces are *not* allowed in the following places:

- In visible dependent quantifiers. Consider::

    data T :: forall {k} -> k -> Type

  This example is rejected, as a visible argument should by definition be
  explicitly applied. Making them inferred (and thus not appliable) would be
  conflicting.

- In default type signatures for class methods, in SPECIALISE pragmas or in
  instance declaration heads, e.g.::

    instance forall {a}. Eq (Maybe a) where ...

  The reason for this is, essentially, that none of these define a new
  construct. This means that no new type is being defined where specificity
  could play a role.

- On the left-hand sides of type declarations, such as classes, data types, etc.
