.. _constraint-kind:

The ``Constraint`` kind
-----------------------

.. extension:: ConstraintKinds
    :shortdesc: Enable a kind of constraints.

    :since: 7.4.1

    Allow types of kind ``Constraint`` to be used in contexts.

Normally, *constraints* (which appear in types to the left of the ``=>``
arrow) have a very restricted syntax. They can only be:

-  Class constraints, e.g. ``Show a``

-  :ghc-flag:`Implicit parameter <-XImplicitParams>` constraints, e.g.
   ``?x::Int`` (with the :extension:`ImplicitParams` extension)

-  :ref:`Equality constraints <equality-constraints>`, e.g. ``a ~ Int``
   (with the :extension:`TypeFamilies` or :extension:`GADTs` extensions)

With the :extension:`ConstraintKinds` extension, GHC becomes more liberal in what it
accepts as constraints in your program. To be precise, with this flag
any *type* of the new kind ``Constraint`` can be used as a constraint.
The following things have kind ``Constraint``:

-  Anything which is already valid as a constraint without the flag:
   saturated applications to type classes, implicit parameter and
   equality constraints.

- Tuples, all of whose component types have kind ``Constraint``. So for example
  the type ``(Show a, Ord a)`` is of kind ``Constraint``.

-  Anything whose form is not yet known, but the user has declared to
   have kind ``Constraint`` (for which they need to import it from
   ``Data.Kind``). So for example
   ``type Foo (f :: Type -> Constraint) = forall b. f b => b -> b``
   is allowed, as well as examples involving type families: ::

       type family Typ a b :: Constraint
       type instance Typ Int  b = Show b
       type instance Typ Bool b = Num b

       func :: Typ a b => a -> b -> b
       func = ...

Note that because constraints are just handled as types of a particular
kind, this extension allows type constraint synonyms: ::

    type Stringy a = (Read a, Show a)
    foo :: Stringy a => a -> (String, String -> a)
    foo x = (show x, read)

Presently, only standard constraints, tuples and type synonyms for those
two sorts of constraint are permitted in instance contexts and
superclasses (without extra flags). The reason is that permitting more
general constraints can cause type checking to loop, as it would with
these two programs:

::

    type family Clsish u a
    type instance Clsish () a = Cls a
    class Clsish () a => Cls a where

::

    class OkCls a where

    type family OkClsish u a
    type instance OkClsish () a = OkCls a
    instance OkClsish () a => OkCls a where

You may write programs that use exotic sorts of constraints in instance
contexts and superclasses, but to do so you must use
:extension:`UndecidableInstances` to signal that you don't mind if the type
checker fails to terminate.


