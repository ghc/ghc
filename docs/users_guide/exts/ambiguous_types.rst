.. _ambiguity:

Ambiguous types and the ambiguity check
---------------------------------------

.. extension:: AllowAmbiguousTypes
    :shortdesc: Allow the user to write ambiguous types, and
        the type inference engine to infer them.

    :since: 7.8.1

    Allow type signatures which appear that they would result in
    an unusable binding.

Each user-written type signature is subjected to an *ambiguity check*.
The ambiguity check rejects functions that can never be called. For
example: ::

       f :: C a => Int

The idea is there can be no legal calls to ``f`` because every call will
give rise to an ambiguous constraint. Indeed, the *only* purpose of the
ambiguity check is to report functions that cannot possibly be called.
We could soundly omit the ambiguity check on type signatures entirely,
at the expense of delaying ambiguity errors to call sites. Indeed, the
language extension :extension:`AllowAmbiguousTypes` switches off the ambiguity
check.

Ambiguity can be subtle. Consider this example which uses functional
dependencies: ::

       class D a b | a -> b where ..
       h :: D Int b => Int

The ``Int`` may well fix ``b`` at the call site, so that signature
should not be rejected. Moreover, the dependencies might be hidden.
Consider ::

       class X a b where ...
       class D a b | a -> b where ...
       instance D a b => X [a] b where...
       h :: X a b => a -> a

Here ``h``\'s type looks ambiguous in ``b``, but here's a legal call: ::

       ...(h [True])...

That gives rise to a ``(X [Bool] beta)`` constraint, and using the
instance means we need ``(D Bool beta)`` and that fixes ``beta`` via
``D``\'s fundep!

Behind all these special cases there is a simple guiding principle.
Consider ::

      f :: type
      f = ...blah...

      g :: type
      g = f

You would think that the definition of ``g`` would surely typecheck!
After all ``f`` has exactly the same type, and ``g=f``. But in fact
``f``\'s type is instantiated and the instantiated constraints are solved
against the constraints bound by ``g``\ 's signature. So, in the case an
ambiguous type, solving will fail. For example, consider the earlier
definition ``f :: C a => Int``: ::

      f :: C a => Int
      f = ...blah...

      g :: C a => Int
      g = f

In ``g``\'s definition, we'll instantiate to ``(C alpha)`` and try to
deduce ``(C alpha)`` from ``(C a)``, and fail.

So in fact we use this as our *definition* of ambiguity: a type ``ty``
is ambiguous if and only if ``((undefined :: ty) :: ty)`` would fail to
typecheck. We use a very similar test for *inferred* types, to ensure
that they too are unambiguous.

*Switching off the ambiguity check.* Even if a function has an
ambiguous type according to the "guiding principle", it is possible that
the function is callable. For example: ::

      class D a b where ...
      instance D Bool b where ...

      strange :: D a b => a -> a
      strange = ...blah...

      foo = strange True

Here ``strange``\'s type is ambiguous, but the call in ``foo`` is OK
because it gives rise to a constraint ``(D Bool beta)``, which is
soluble by the ``(D Bool b)`` instance.

Another way of getting rid of the ambiguity at the call site is to use
the :extension:`TypeApplications` extension to specify the types. For example: ::

      class D a b where
        h :: b
      instance D Int Int where ...

      main = print (h @Int @Int)

Here ``a`` is ambiguous in the definition of ``D`` but later specified
to be `Int` using type applications.

:extension:`AllowAmbiguousTypes` allows you to switch off the ambiguity check.
However, even with ambiguity checking switched off, GHC will complain about a
function that can *never* be called, such as this one: ::

      f :: (Int ~ Bool) => a -> a

Sometimes :extension:`AllowAmbiguousTypes` does not mix well with :extension:`RankNTypes`.
For example: ::

      foo :: forall r. (forall i. (KnownNat i) => r) -> r
      foo f = f @1

      boo :: forall j. (KnownNat j) => Int
      boo = ....

      h :: Int
      h = foo boo

This program will be rejected as ambiguous because GHC will not unify
the type variables `j` and `i`.

Unlike the previous examples, it is not currently possible
to resolve the ambiguity manually by using :extension:`TypeApplications`.


.. note::
    *A historical note.* GHC used to impose some more restrictive and less
    principled conditions on type signatures. For type
    ``forall tv1..tvn (c1, ...,cn) => type`` GHC used to require

     a. that each universally quantified type variable ``tvi`` must be "reachable"
        from ``type``, and

     b. that every constraint ``ci`` mentions at least one of the universally
        quantified type variables ``tvi``. These ad-hoc restrictions are
        completely subsumed by the new ambiguity check.


