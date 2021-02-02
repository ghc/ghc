.. _runtime-rep:

Levity polymorphism
===================

In order to allow full flexibility in how kinds are used, it is necessary
to use the kind system to differentiate between boxed, lifted types
(normal, everyday types like ``Int`` and ``[Bool]``) and unboxed, primitive
types (:ref:`primitives`) like ``Int#``. We thus have so-called levity
polymorphism.

Here are the key definitions, all available from ``GHC.Exts``: ::

  TYPE :: RuntimeRep -> Type   -- highly magical, built into GHC

  data Levity = Lifted    -- for things like `Int`
              | Unlifted  -- for things like `Array#`

  data RuntimeRep = BoxedRep Levity  -- for anything represented by a GC-managed pointer
                  | IntRep           -- for `Int#`
                  | TupleRep [RuntimeRep]  -- unboxed tuples, indexed by the representations of the elements
                  | SumRep [RuntimeRep]    -- unboxed sums, indexed by the representations of the disjuncts
                  | ...

  type LiftedRep = BoxedRep Lifted

  type Type = TYPE LiftedRep    -- Type is just an ordinary type synonym

The idea is that we have a new fundamental type constant ``TYPE``, which
is parameterised by a ``RuntimeRep``. We thus get ``Int# :: TYPE 'IntRep``
and ``Bool :: TYPE LiftedRep``. Anything with a type of the form
``TYPE x`` can appear to either side of a function arrow ``->``. We can
thus say that ``->`` has type
``TYPE r1 -> TYPE r2 -> TYPE LiftedRep``. The result is always lifted
because all functions are lifted in GHC.

.. _levity-polymorphic-restrictions:

No levity-polymorphic variables or arguments
--------------------------------------------

If GHC didn't have to compile programs that run in the real world, that
would be the end of the story. But representation polymorphism can cause
quite a bit of trouble for GHC's code generator. Consider ::

  bad :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                (a :: TYPE r1) (b :: TYPE r2).
         (a -> b) -> a -> b
  bad f x = f x

This seems like a generalisation of the standard ``$`` operator. If we
think about compiling this to runnable code, though, problems appear.
In particular, when we call ``bad``, we must somehow pass ``x`` into
``bad``. How wide (that is, how many bits) is ``x``? Is it a pointer?
What kind of register (floating-point or integral) should ``x`` go in?
It's all impossible to say, because ``x``'s type, ``a :: TYPE r1`` is
levity polymorphic. We thus forbid such constructions, via the
following straightforward rule:

    No variable may have a levity-polymorphic type.

This eliminates ``bad`` because the variable ``x`` would have a
representation-polymorphic type.

However, not all is lost. We can still do this: ::

  ($) :: forall r (a :: Type) (b :: TYPE r).
         (a -> b) -> a -> b
  f $ x = f x

Here, only ``b`` is levity polymorphic. There are no variables
with a levity-polymorphic type. And the code generator has no
trouble with this. Indeed, this is the true type of GHC's ``$`` operator,
slightly more general than the Haskell 98 version.

Because the code generator must store and move arguments as well
as variables, the logic above applies equally well to function arguments,
which may not be levity-polymorphic.


Levity-polymorphic bottoms
--------------------------

We can use levity polymorphism to good effect with ``error``
and ``undefined``, whose types are given here: ::

  undefined :: forall (r :: RuntimeRep) (a :: TYPE r).
               HasCallStack => a
  error :: forall (r :: RuntimeRep) (a :: TYPE r).
           HasCallStack => String -> a

These functions do not bind a levity-polymorphic variable, and
so are accepted. Their polymorphism allows users to use these to conveniently
stub out functions that return unboxed types.

.. _printing-levity-polymorphic-types:

Printing levity-polymorphic types
---------------------------------

.. ghc-flag:: -fprint-explicit-runtime-reps
    :shortdesc: Print ``RuntimeRep`` variables in types which are
        runtime-representation polymorphic.
    :type: dynamic
    :reverse: -fno-print-explicit-runtime-reps
    :category: verbosity

    Print ``RuntimeRep`` parameters as they appear; otherwise, they are
    defaulted to ``LiftedRep``.

Most GHC users will not need to worry about levity polymorphism
or unboxed types. For these users, seeing the levity polymorphism
in the type of ``$`` is unhelpful. And thus, by default, it is suppressed,
by supposing all type variables of type ``RuntimeRep`` to be ``LiftedRep``
when printing, and printing ``TYPE LiftedRep`` as ``Type`` (or ``*`` when
:extension:`StarIsType` is on).

Should you wish to see levity polymorphism in your types, enable
the flag :ghc-flag:`-fprint-explicit-runtime-reps`. For example,

    .. code-block:: none

        ghci> :t ($)
        ($) :: (a -> b) -> a -> b
        ghci> :set -fprint-explicit-runtime-reps
        ghci> :t ($)
        ($)
          :: forall (r :: GHC.Types.RuntimeRep) a (b :: TYPE r).
             (a -> b) -> a -> b


