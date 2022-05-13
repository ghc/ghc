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
only in their outermost contexts. Therefore, if you have a method ``bar``: ::

      class Foo a where
        bar :: forall b. C => a -> b -> b

Then a default method for ``bar`` must take on the form: ::

      default bar :: forall b. C' => a -> b -> b
      bar = ...

``C`` is allowed to be different from ``C'``, but the right-hand sides of the
type signatures must coincide. We require this because when you declare an
empty instance for a class that uses :extension:`DefaultSignatures`, GHC
implicitly fills in the default implementation like this: ::

      instance Foo Int where
        bar = default_bar

Where ``default_bar`` is a top-level function based on the default type
signature and implementation for ``bar``: ::

      default_bar :: forall a b. (Foo a, C') => a -> b -> b
      default_bar = ...

In order for this approach to work, the default type signature for ``bar``
should be the same as the non-default signature, modulo the outermost context
(with some caveats—see
:ref:`class-default-signatures-detailed-requirements`). There is no obligation
for ``C`` and ``C'`` to be the same, and indeed, the ``Enum`` example above
relies on ``enum``'s default type signature having a more specific context than
the original type signature.

We use default signatures to simplify generic programming in GHC
(:ref:`generic-programming`).

.. _class-default-signatures-detailed-requirements:

Detailed requirements for default type signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The rest of this section gives further details about what constitutes valid
default type signatures.

- Ignoring outermost contexts, a default type signature must match the original
  type signature according to
  :ref:`GHC's subsumption rules <subsumption>`. As a result, the order
  of type variables in the default signature is important. Recall the ``Foo``
  example from the previous section: ::

    class Foo a where
      bar :: forall b. C => a -> b -> b

      default bar :: forall b. C' => a -> b -> b
      bar = ...

  This is legal because if you remove the outermost contexts ``C`` and ``C'``,
  then the two type signatures are the same. It is not necessarily the case
  that the default signature has to be *exactly* the same, however. For
  instance, this would also be an acceptable default type signature, as it is
  alpha-equivalent to the original type signature: ::

      default bar :: forall x. C' => a -> x -> x

  On the other hand, this is *not* an acceptable default type signature, since
  the type variable ``a`` is in the wrong place: ::

      default bar :: forall b. C' => b -> a -> b

- The only place where a default type signature is allowed to more precise than
  the original type signature is in the outermost context. For example, this
  would *not* be an acceptable default type signature, since we can't match the
  type variable ``b`` with the concrete type ``Int``: ::

      default bar :: C' => a -> Int -> Int

  You can, however, use type equalities to achieve the same result: ::

      default bar :: forall b. (C', b ~ Int) => a -> b -> b

- Because of :ref:`GHC's subsumption rules <subsumption>` rules, there
  are relatively tight restrictions concerning nested or higher-rank
  ``forall``\ s (see :ref:`arbitrary-rank-polymorphism`). Consider this
  class: ::

    class C x where
      m :: x -> forall a b. a -> b

  GHC would *not* permit the following default type signature for ``m``: ::

      default m :: x -> forall b a. a -> b

  This is because the default signature quantifies the nested ``forall``\ s
  in a different order than the original type signature. In order for this to
  typecheck, the default signature must preserve the original order: ::

      default m :: x -> forall a b. a -> b

  Note that unlike nested or higher-rank ``forall``\ s, outermost
  ``forall``\ s have more flexibility in how they are ordered. As a result, GHC
  will permit the following: ::

    class C' x where
      m'         :: forall a b. x -> a -> b
      default m' :: forall b a. x -> a -> b
      m' = ...

- Just as the order of nested or higher-rank ``forall``\ s is restricted, a
  similar restriction applies to the order in which nested or higher-rank
  contexts appear. As a result, GHC will not permit the following: ::

    class D a where
      n         :: a -> forall b. (Eq b, Show b) => b -> String
      default n :: a -> forall b. (Show b, Eq b) => b -> String
      n = ...

  GHC will permit reordering constraints within an outermost context, however,
  as demonstrated by the fact that GHC accepts the following: ::

    class D' a where
      n'         :: (Eq b, Show b) => a -> b -> String
      default n' :: (Show b, Eq b) => a -> b -> String
      n' = ...

- Because a default signature is only ever allowed to differ from its original
  type signature in the outermost context, not in nested or higher-rank
  contexts, there are certain defaults that cannot be written without
  reordering ``forall`` \s. Consider this example: ::

    class E a where
      p :: Int -> forall b. b -> String

  Suppose one wishes to write a default signature for ``p`` where the context
  must mention both ``a`` and ``b``. While the natural thing to do would be to
  write this default: ::

      default p :: Int -> forall b. DefaultClass a b => b -> String

  This will not typecheck, since the default type signature now differs from
  the original type signature in its use of nested contexts. The only way to
  make such a default signature work is to change the order in which ``b``
  is quantified: ::

      default p :: forall b. DefaultClass a b => Int -> b -> String

  This works, but at the expense of changing ``p``'s behavior with respect to
  :ref:`visible-type-application`.
