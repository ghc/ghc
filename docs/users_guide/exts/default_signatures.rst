.. _class-default-signatures:

Default method signatures
~~~~~~~~~~~~~~~~~~~~~~~~~

.. extension:: DefaultSignatures
    :shortdesc: Enable default signatures.

    :since: 7.2.1

    Allows the definition of default method signatures in class definitions.

Haskell 98 allows you to define a default implementation when declaring
a class: ::

      class Enum a where
        enum :: [a]
        enum = []

The type of the ``enum`` method is ``[a]``, and this is also the type of
the default method. You can change the type of the default method by
requiring a different context using the extension
:extension:`DefaultSignatures`. For instance, if you have written a
generic implementation of enumeration in a class ``GEnum`` with method
``genum``, you can specify a default method that uses that generic
implementation. But your default implementation can only be used if the
constraints are satisfied, therefore you need to change the type of the
default method ::

      class Enum a where
        enum :: [a]
        default enum :: (Generic a, GEnum (Rep a)) => [a]
        enum = map to genum

We reuse the keyword ``default`` to signal that a signature applies to
the default method only; when defining instances of the ``Enum`` class,
the original type ``[a]`` of ``enum`` still applies. When giving an
empty instance, however, the default implementation ``(map to genum)`` is
filled-in, and type-checked with the type
``(Generic a, GEnum (Rep a)) => [a]``.

The type signature for a default method of a type class must take on the same
form as the corresponding main method's type signature. Otherwise, the
typechecker will reject that class's definition. By "take on the same form", we
mean that the default type signature should differ from the main type signature
only in their contexts. Therefore, if you have a method ``bar``: ::

      class Foo a where
        bar :: forall b. C => a -> b -> b

Then a default method for ``bar`` must take on the form: ::

      default bar :: forall b. C' => a -> b -> b

``C`` is allowed to be different from ``C'``, but the right-hand sides of the
type signatures must coincide. We require this because when you declare an
empty instance for a class that uses :extension:`DefaultSignatures`, GHC
implicitly fills in the default implementation like this: ::

      instance Foo Int where
        bar = default_bar @Int

Where ``@Int`` utilizes visible type application
(:ref:`visible-type-application`) to instantiate the ``b`` in
``default bar :: forall b. C' => a -> b -> b``. In order for this type
application to work, the default type signature for ``bar`` must have the same
type variable order as the non-default signature! But there is no obligation
for ``C`` and ``C'`` to be the same (see, for instance, the ``Enum`` example
above, which relies on this).

To further explain this example, the right-hand side of the default
type signature for ``bar`` must be something that is alpha-equivalent to
``forall b. a -> b -> b`` (where ``a`` is bound by the class itself, and is
thus free in the methods' type signatures). So this would also be an acceptable
default type signature: ::

      default bar :: forall x. C' => a -> x -> x

But not this (since the free variable ``a`` is in the wrong place): ::

      default bar :: forall b. C' => b -> a -> b

Nor this, since we can't match the type variable ``b`` with the concrete type
``Int``: ::

      default bar :: C' => a -> Int -> Int

That last one deserves a special mention, however, since ``a -> Int -> Int`` is
a straightforward instantiation of ``forall b. a -> b -> b``. You can still
write such a default type signature, but you now must use type equalities to
do so: ::

      default bar :: forall b. (C', b ~ Int) => a -> b -> b

We use default signatures to simplify generic programming in GHC
(:ref:`generic-programming`).


