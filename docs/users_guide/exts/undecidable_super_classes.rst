.. _superclass-rules:

Undecidable (or recursive) superclasses
---------------------------------------

.. extension:: UndecidableSuperClasses
    :shortdesc: Allow all superclass constraints, including those that may
        result in non-termination of the typechecker.

    :since: 8.0.1

    Allow all superclass constraints, including those that may result in
    non-termination of the typechecker.

The language extension :extension:`UndecidableSuperClasses` allows much more flexible
constraints in superclasses.

A class cannot generally have itself as a superclass. So this is illegal ::

    class C a => D a where ...
    class D a => C a where ...

GHC implements this test conservatively when type functions, or type variables,
are involved. For example ::

    type family F a :: Constraint
    class F a => C a where ...

GHC will complain about this, because you might later add ::

    type instance F Int = C Int

and now we'd be in a superclass loop.  Here's an example involving a type variable ::

   class f (C f) => C f
   class c       => Id c

If we expanded the superclasses of ``C Id`` we'd get first ``Id (C Id)`` and
thence ``C Id`` again.

But superclass constraints like these are sometimes useful, and the conservative
check is annoying where no actual recursion is involved.

Moreover genuinely-recursive superclasses are sometimes useful. Here's a real-life
example (#10318) ::

     class (Frac (Frac a) ~ Frac a,
            Fractional (Frac a),
            IntegralDomain (Frac a))
         => IntegralDomain a where
      type Frac a :: Type

Here the superclass cycle does terminate but it's not entirely straightforward
to see that it does.

With the language extension :extension:`UndecidableSuperClasses` GHC lifts all restrictions
on superclass constraints. If there really *is* a loop, GHC will only
expand it to finite depth.
