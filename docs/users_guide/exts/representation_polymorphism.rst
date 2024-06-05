.. _runtime-rep:

Representation polymorphism
===========================

In order to allow full flexibility in how kinds are used, it is necessary
to use the kind system to differentiate between boxed, lifted types
(normal, everyday types like ``Int`` and ``[Bool]``) and unboxed, primitive
types (:ref:`primitives`) like ``Int#``. We thus have so-called representation
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
is parameterised by a ``RuntimeRep``. We thus get ``Int# :: TYPE IntRep``
and ``Bool :: TYPE LiftedRep``. Anything with a type of the form
``TYPE x`` can appear to either side of a function arrow ``->``. We can
thus say that ``->`` has type
``TYPE r1 -> TYPE r2 -> TYPE LiftedRep``. The result is always lifted
because all functions are lifted in GHC.

Levity polymorphism
-------------------

A special case of representation polymorphism is levity polymorphism,
where we abstract over a variable of kind ``Levity``, such as: ::

  example :: forall (l :: Levity) (a :: TYPE (BoxedRep l)). (Int -> a) -> a
  example f = f 42

With :extension:`UnliftedDatatypes`, we can even declare levity-polymorphic
data types: ::

  type PEither :: Type -> Type -> TYPE (BoxedRep l)
  data PEither l r = PLeft l | PRight r

.. _representation-polymorphism-restrictions:

No representation-polymorphic variables or arguments
----------------------------------------------------

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
representation-polymorphic. We thus forbid such constructions, via the
following straightforward rule:

    No variable may have a representation-polymorphic type.

This eliminates ``bad`` because the variable ``x`` would have a
representation-polymorphic type.

However, not all is lost. We can still do this: ::

  good :: forall r (a :: Type) (b :: TYPE r).
         (a -> b) -> a -> b
  good f x = f x

Here, only ``b`` is representation-polymorphic. There are no variables
with a representation-polymorphic type. And the code generator has no
trouble with this. Nonetheless, there is a way to write a definition with
``bad``'s type: ::


  ($) :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                (a :: TYPE r1) (b :: TYPE r2).
         (a -> b) -> a -> b
  ($) f = f

By eta-reducing, we got rid of ``x``, and thus have no variable with a
representation-polymorphic type.  Indeed, this is the true type of GHC's ``$``
operator, slightly more general than the Haskell 98 version. However, its
strictness properties are different: ``(good undefined) `seq` ()`` is equivalent
to ``()``, whereas ``(($) undefined) `seq` ()`` diverges.

Because the code generator must store and move arguments as well
as variables, the logic above applies equally well to function arguments,
which may not be representation-polymorphic.


Representation-polymorphic bottoms
----------------------------------

We can use representation polymorphism to good effect with ``error``
and ``undefined``, whose types are given here: ::

  undefined :: forall (r :: RuntimeRep) (a :: TYPE r).
               HasCallStack => a
  error :: forall (r :: RuntimeRep) (a :: TYPE r).
           HasCallStack => String -> a

These functions do not bind a representation-polymorphic variable, and
so are accepted. Their polymorphism allows users to use these to conveniently
stub out functions that return unboxed types.

.. _representation-polymorphism-defaulting:

Inference and defaulting
------------------------

GHC does not infer representation-polymorphic types.
If the representation of a variable is not specified, it will be assumed
to be ``LiftedRep``.
For example, if you write ``f a b = a b``, the inferred type of ``f``
will be ::

  f :: forall {a :: Type} {b :: Type}. (a -> b) -> a -> b

even though ::

  f :: forall {rep} {a :: Type} {b :: TYPE rep}. (a -> b) -> a -> b

would also be legal, as described above.

Likewise, in a user-written signature ``f :: forall a b. (a -> b) -> a -> b``
GHC will assume that both ``a`` and ``b`` have kind ``Type``. To use
a different representation, you have to specify the kinds of ``a`` and ``b``.

During type inference, GHC does not quantify over variables of kind
``RuntimeRep`` nor ``Levity``.
Instead, they are defaulted to ``LiftedRep`` and ``Lifted`` respectively.
Likewise, ``Multiplicity`` variables (:ref:`linear-types`) are defaulted
to ``Many``.

.. _printing-representation-polymorphic-types:

Printing representation-polymorphic types
-----------------------------------------

.. ghc-flag:: -fprint-explicit-runtime-reps
    :shortdesc: Print ``RuntimeRep`` and ``Levity`` variables in types which are
        runtime-representation polymorphic.
    :type: dynamic
    :reverse: -fno-print-explicit-runtime-reps
    :category: verbosity

    Print ``RuntimeRep`` and ``Levity`` parameters as they appear;
    otherwise, they are defaulted to ``LiftedRep`` and ``Lifted``, respectively.

Most GHC users will not need to worry about representation polymorphism
or unboxed types. For these users, seeing the representation polymorphism
in the type of ``$`` is unhelpful. And thus, by default, it is suppressed,
by supposing all type variables of type ``RuntimeRep`` to be ``LiftedRep``
when printing, and printing ``TYPE LiftedRep`` as ``Type`` (or ``*`` when
:extension:`StarIsType` is on).

Should you wish to see representation polymorphism in your types, enable
the flag :ghc-flag:`-fprint-explicit-runtime-reps`. For example,

    .. code-block:: none

        ghci> :t ($)
        ($) :: (a -> b) -> a -> b
        ghci> :set -fprint-explicit-runtime-reps
        ghci> :t ($)
        ($)
          :: forall (r :: GHC.Types.RuntimeRep) a (b :: TYPE r).
             (a -> b) -> a -> b


