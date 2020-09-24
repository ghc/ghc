.. _record-field-selector-polymorphism:

Record field selector polymorphism
----------------------------------

The module :base-ref:`GHC.Records.` defines the following: ::

  class HasField (x :: k) r a | x r -> a where
    hasField :: r -> (a -> r, a)

A ``HasField x r a`` constraint represents the fact that ``x`` is a
field of type ``a`` belonging to a record type ``r``.  The
``hasField`` method gives the ability to select and update the field.

This module also defines: ::

  getField :: forall x r a . HasField x r a => r -> a
  getField = snd . hasField @x

  setField :: forall x r a . HasField x r a => r -> a -> r
  setField = fst . hasField @x

These make it possible to write functions that are polymorphic over record types
with a specified field.  For example, the following works with any record type
that has a field ``name :: String``: ::

  foo :: HasField "name" r String => r -> String
  foo r = reverse (getField @"name" r)

``HasField`` is a magic built-in typeclass (similar to ``Coercible``, for
example).  It is given special treatment by the constraint solver (see
:ref:`solving-hasfield-constraints`).  Users may define their own instances of
``HasField`` also (see :ref:`virtual-record-fields`).

.. _solving-hasfield-constraints:

Solving HasField constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the constraint solver encounters a constraint ``HasField x r a``
where ``r`` is a concrete datatype with a field ``x`` in scope, it
will automatically solve the constraint by generating a suitable
dictionary, unifying ``a`` with the type of the field if
necessary.  This happens irrespective of which extensions are enabled.

For example, if the following datatype is in scope ::

  data Person = Person { name :: String }

the end result is rather like having an instance ::

  instance HasField "name" Person String where
    getField = name

except that this instance is not actually generated anywhere, rather
the constraint is solved directly by the constraint solver.

A field must be in scope for the corresponding ``HasField`` constraint
to be solved.  This retains the existing representation hiding
mechanism, whereby a module may choose not to export a field,
preventing client modules from accessing or updating it directly.

Solving ``HasField`` constraints depends on the type of the field:

-  If a record field has a type containing an existential variable, it cannot
   have a selector function, and the corresponding ``HasField`` constraint will
   not be solved, because this would allow the existential variable to escape.
   For example, ::

     {-# LANGUAGE ExistentialQuantification #-}
     data Exists t = forall x . MkExists { unExists :: t x }

   does not give rise to a selector ``unExists :: Exists t -> t x`` and we will not
   solve ``HasField "unExists" (Exists t) a`` automatically.

-  If a record field has a polymorphic type (and hence the selector function is
   higher-rank), the corresponding ``HasField`` constraint will not be solved,
   because doing so would violate the functional dependency on ``HasField``
   and/or require an impredicative constraint (which is not allowed even with
   :extension:`ImpredicativeTypes`).  For example, ::

     {-# LANGUAGE RankNTypes #-}
     data Higher = MkHigher { unHigher :: forall t . t -> t }

   gives rise to a selector ``unHigher :: Higher -> (forall t . t -> t)`` but does
   not lead to solution of the constraint ``HasField "unHigher" Higher a`` (which
   would require an impredicative instantiation of ``a`` with ``forall t . t -> t``).

-  A record GADT may have a restricted type for a selector function, which may lead
   to additional unification when solving ``HasField`` constraints.  For example, ::

     {-# LANGUAGE GADTs #-}
     data Gadt t where
       MkGadt :: { unGadt :: Maybe v } -> Gadt [v]

   gives rise to a selector ``unGadt :: Gadt [v] -> Maybe v``, so the solver will reduce
   the constraint ``HasField "unGadt" (Gadt t) b`` by unifying ``t ~ [v]`` and
   ``b ~ Maybe v`` for some fresh metavariable ``v``, rather as if we had an instance ::

     instance (t ~ [v], b ~ Maybe v) => HasField "unGadt" (Gadt t) b

-  If a record type has an old-fashioned datatype context, the ``HasField``
   constraint will be reduced to solving the constraints from the context.
   For example, ::

     {-# LANGUAGE DatatypeContexts #-}
     data Eq a => Silly a = MkSilly { unSilly :: a }

   gives rise to a selector ``unSilly :: Eq a => Silly a -> a``, so
   the solver will reduce the constraint ``HasField "unSilly" (Silly a) b`` to
   ``Eq a`` (and unify ``a`` with ``b``), rather as if we had an instance ::

     instance (Eq a, a ~ b) => HasField "unSilly" (Silly a) b

.. _virtual-record-fields:

Virtual record fields
~~~~~~~~~~~~~~~~~~~~~

Users may define their own instances of ``HasField``, provided they do
not conflict with the built-in constraint solving behaviour.  This
allows "virtual" record fields to be defined for datatypes that do not
otherwise have them.

For example, this instance would make the ``name`` field of ``Person``
accessible using ``#fullname`` as well: ::

  instance HasField "fullname" Person String where
    hasField r = (\n -> r { name = n }, name r)

More substantially, an anonymous records library could provide
``HasField`` instances for its anonymous records, and thus be
compatible with the polymorphic record selectors introduced by this
proposal.  For example, something like this makes it possible to use
``getField`` to access ``Record`` values with the appropriate
string in the type-level list of fields: ::

  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE FlexibleContexts #-}
  {-# LANGUAGE GADTs #-}
  {-# LANGUAGE PolyKinds #-}
  {-# LANGUAGE ScopedTypeVariables #-}
  {-# LANGUAGE TypeApplications #-}
  {-# LANGUAGE TypeOperators #-}
  {-# LANGUAGE UndecidableInstances #-}

  import Data.Kind (Type)
  import Data.Proxy (Proxy(..))
  import GHC.Records

  data Record (xs :: [(k, Type)]) where
    Nil  :: Record '[]
    Cons :: Proxy x -> a -> Record xs -> Record ('(x, a) ': xs)

  instance {-# OVERLAPS #-} HasField x (Record ('(x, a) ': xs)) a where
    hasField (Cons p v r) = (\v' -> Cons p v' r, v)
  instance HasField x (Record xs) a => HasField x (Record ('(y, b) ': xs)) a where
    hasField (Cons p v r) = (\v' -> Cons p v (set v'), a)
      where
        (set,a) = hasField @x r

  r :: Record '[ '("name", String) ]
  r = Cons Proxy "R" Nil

  x = getField @"name" (setField @"name" r "S")

Since representations such as this can support field labels with kinds other
than ``Symbol``, the ``HasField`` class is poly-kinded (even though the built-in
constraint solving works only at kind ``Symbol``).  In particular, this allows
users to declare scoped field labels such as in the following example: ::

  data PersonFields = Name

  s :: Record '[ '(Name, String) ]
  s = Cons Proxy "S" Nil

  y = getField @Name s

In order to avoid conflicting with the built-in constraint solving,
the following user-defined ``HasField`` instances are prohibited (in
addition to the usual rules, such as the prohibition on type
families appearing in instance heads):

-  ``HasField _ r _`` where ``r`` is a variable;

-  ``HasField _ (T ...) _`` if ``T`` is a data family (because it
   might have fields introduced later, using data instance declarations);

-  ``HasField x (T ...) _`` if ``x`` is a variable and ``T`` has any
   fields at all (but this instance is permitted if ``T`` has no fields);

-  ``HasField "foo" (T ...) _`` if ``T`` has a field ``foo`` (but this
   instance is permitted if it does not).

If a field has a higher-rank or existential type, the corresponding ``HasField``
constraint will not be solved automatically (as described above), but in the
interests of simplicity we do not permit users to define their own instances
either.  If a field is not in scope, the corresponding instance is still
prohibited, to avoid conflicts in downstream modules.

.. _compatibility-notes:

Compatibility notes
~~~~~~~~~~~~~~~~~~~

``HasField`` was introduced in GHC 8.2.

In versions of GHC prior to 9.2, the ``HasField`` class provided only a
``getField`` method, so it was not possibly to update fields in a polymorphic
way.  This means:

- Code using ``hasField`` or ``setField`` will require at least GHC 9.2.

- Code using ``getField`` only may support GHC 8.2 and later, and should use
  ``import GHC.Records (HasField, getField)`` which works regardless of whether
  ``getField`` is a class method (prior to 9.2) or a function (9.2 and later).

- User-defined ``HasField`` instances must use :extension:`CPP` to support GHC
  versions before and after 9.2.

:ref:`record-patsyn` do not lead to automatic solution of ``HasField`` instances
for their fields, so if you replace a datatype with a pattern synonym where
``HasField`` is in use, you may need to define :ref:`virtual-record-fields`
manually.
