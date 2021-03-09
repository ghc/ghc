.. _record-field-selector-polymorphism:

Record field selector polymorphism
----------------------------------

The module :base-ref:`GHC.Records.` defines the following: ::

  class HasField (x :: k) r a | x r -> a where
    getField :: r -> a

A ``HasField x r a`` constraint represents the fact that ``x`` is a
field of type ``a`` belonging to a record type ``r``.  The
``getField`` method gives the record selector function.

This allows definitions that are polymorphic over record types with a specified
field.  For example, the following works with any record type that has a field
``name :: String``: ::

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
will automatically solve the constraint using the field selector as
the dictionary, unifying ``a`` with the type of the field if
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

Solving ``HasField`` constraints depends on the field selector functions that
are generated for each datatype definition:

-  If a record field does not have a selector function because its type would allow
   an existential variable to escape, the corresponding ``HasField`` constraint
   will not be solved.  For example, ::

     {-# LANGUAGE ExistentialQuantification #-}
     data Exists t = forall x . MkExists { unExists :: t x }

   does not give rise to a selector ``unExists :: Exists t -> t x`` and we will not
   solve ``HasField "unExists" (Exists t) a`` automatically.

-  If a record field has a polymorphic type (and hence the selector function is
   higher-rank), the corresponding ``HasField`` constraint will not be solved,
   because doing so would violate the functional dependency on ``HasField`` and/or
   require impredicativity.  For example, ::

     {-# LANGUAGE RankNTypes #-}
     data Higher = MkHigher { unHigher :: forall t . t -> t }

   gives rise to a selector ``unHigher :: Higher -> (forall t . t -> t)`` but does
   not lead to solution of the constraint ``HasField "unHigher" Higher a``.

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

See :ref:`overloaded-record-dot` for an application of solving ``HasField`` constraints to implementing "record dot syntax".

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
    getField = name

More substantially, an anonymous records library could provide
``HasField`` instances for its anonymous records, and thus be
compatible with the polymorphic record selectors introduced by this
proposal.  For example, something like this makes it possible to use
``getField`` to access ``Record`` values with the appropriate
string in the type-level list of fields: ::

  data Record (xs :: [(k, Type)]) where
    Nil  :: Record '[]
    Cons :: Proxy x -> a -> Record xs -> Record ('(x, a) ': xs)

  instance HasField x (Record ('(x, a) ': xs)) a where
    getField (Cons _ v _) = v
  instance HasField x (Record xs) a => HasField x (Record ('(y, b) ': xs)) a where
    getField (Cons _ _ r) = getField @x r

  r :: Record '[ '("name", String) ]
  r = Cons Proxy "R" Nil)

  x = getField @"name" r

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
