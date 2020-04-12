.. _superclass-rules:

Flexible contexts
-----------------

.. extension:: FlexibleContexts
    :shortdesc: Enable flexible contexts.

    :since: 6.8.1

    Allow the use of complex constraints in class declaration contexts.

In Haskell 98 the context of a class declaration (which introduces
superclasses) must be simple; that is, each predicate must consist of a
class applied to type variables. The extension :extension:`FlexibleContexts`
(:ref:`flexible-contexts`) lifts this restriction, so that the only
restriction on the context in a class declaration is that the class
hierarchy must be acyclic. So these class declarations are OK: ::

      class Functor (m k) => FiniteMap m k where
        ...

      class (Monad m, Monad (t m)) => Transform t m where
        lift :: m a -> (t m) a

As in Haskell 98, the class hierarchy must be acyclic. However, the
definition of "acyclic" involves only the superclass relationships. For
example, this is okay: ::

      class C a where
        op :: D b => a -> b -> b

      class C a => D a where ...

Here, ``C`` is a superclass of ``D``, but it's OK for a class operation
``op`` of ``C`` to mention ``D``. (It would not be OK for ``D`` to be a
superclass of ``C``.)

With the extension that adds a :ref:`kind of
constraints <constraint-kind>`, you can write more exotic superclass
definitions. The superclass cycle check is even more liberal in these
case. For example, this is OK: ::

      class A cls c where
        meth :: cls c => c -> c

      class A B c => B c where

A superclass context for a class ``C`` is allowed if, after expanding
type synonyms to their right-hand-sides, and uses of classes (other than
``C``) to their superclasses, ``C`` does not occur syntactically in the
context.


