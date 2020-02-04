.. applicative-comprehensions:

Applicative comprehensions
--------------------

.. index::
   single: applicative comprehensions

.. extension:: ApplicativeComprehensions
    :shortdesc: Enable applicative comprehensions.

    :since: 8.10.1

    Enable list comprehension syntax for arbitrary applicative functors.

Applicative comprehensions generalise the list comprehension notation,
with some restrictions, to work for any applicative functor.

Applicative comprehensions support:

-  Bindings: ::

       [ x + y | x <- Just 1, y <- Just 2 ]

   Bindings are translated with the ``(<*>)`` and ``pure`` functions::

       (\ x y -> x + y) <$> Just 1 <*> Just 2

   Note, however, if the RHS of any binding has a free variable which is bound
   in the same comprehension, the comprehension will incur a ``Monad`` constraint.

-  Guards: ::

       [ x | x <- Just 1, y <- Just 2 () == () ]

   Guards are translated with the ``guard`` function, which requires an
   ``Alternative`` instance.

       (\ x y -> x + y) <$> Just 1 <*> Just 2 <* guard (() == ())

All these features are enabled by default if the :extension:`ApplicativeComprehensions`
extension is enabled. The types and more detailed examples on how to use
comprehensions are explained in the previous chapter
:ref:`generalised-list-comprehensions`. In general you just have to replace
the type ``[a]`` with the type ``c p => p a`` for applicative comprehensions,
where ``c`` is ``Functor``, Applicaitve``, or ``Monad``.

.. note::
    Even though most of these examples are using the list applicative functor,
    applicative comprehensions work for any applicative functor. The ``base``
    package offers all necessary instances for lists, which make
    :extension:`ApplicativeComprehensions` backward compatible to built-in list
    comprehensions.

More formally, the desugaring is as follows. We write ``D[ e | Q]`` to
mean the desugaring of the monad comprehension ``[ e | Q]``:

.. code-block:: none

    Expressions: a,b
    Declarations: d
    Lists of qualifiers: Q,R,S

    -- Basic forms
    D[ a | ]                = pure a
    D[ a | p <- b ]         = (\ p -> a) <$> b
    D[ a | p <- b, q <- c ] = (\ p q -> a) <$> b <*> c
    -- etc.

    Operator     Standard binding       Expected type
    --------------------------------------------------------------------
    pure         GHC.Base               s -> p t
    (<$>)        GHC.Base               forall a b. (a -> b) -> f a -> f b
    (<*>)        GHC.Base               p (s1 -> t1) -> q t2 -> r t3

The comprehension should typecheck when its desugaring would typecheck.

Applicative comprehensions support rebindable syntax
(:ref:`rebindable-syntax`). Without rebindable syntax, the operators
from the "standard binding" module are used; with rebindable syntax, the
operators are looked up in the current lexical scope.

The rebindable operators must have the "Expected type" given in the
table above. These types are surprisingly general. For example, you can
use an apply operator with the type

::

    (<*>) :: T x y (a -> b) -> T y z a -> T x z b

