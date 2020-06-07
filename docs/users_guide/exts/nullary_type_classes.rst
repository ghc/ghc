.. _nullary-type-classes:

Nullary type classes
~~~~~~~~~~~~~~~~~~~~

.. extension:: NullaryTypeClasses
    :shortdesc: Deprecated, does nothing. nullary (no parameter) type
        classes are now enabled using :extension:`MultiParamTypeClasses`.

    :since: 7.8.1

    Allow use and definition of type classes with no parameters. This extension
    has been replaced by :extension:`MultiParamTypeClasses`.


Nullary (no parameter) type classes are enabled with
:extension:`MultiParamTypeClasses`; historically, they were enabled with the
(now deprecated) :extension:`NullaryTypeClasses`. Since there are no available
parameters, there can be at most one instance of a nullary class. A nullary type
class might be used to document some assumption in a type signature (such as
reliance on the Riemann hypothesis) or add some globally configurable settings
in a program. For example, ::

      class RiemannHypothesis where
        assumeRH :: a -> a

      -- Deterministic version of the Miller test
      -- correctness depends on the generalised Riemann hypothesis
      isPrime :: RiemannHypothesis => Integer -> Bool
      isPrime n = assumeRH (...)

The type signature of ``isPrime`` informs users that its correctness depends on
an unproven conjecture. If the function is used, the user has to acknowledge the
dependence with: ::

      instance RiemannHypothesis where
        assumeRH = id


