.. _roles:

Roles
=====

.. index::
   single: roles

Using :extension:`GeneralizedNewtypeDeriving`
(:ref:`newtype-deriving`), a programmer can take existing
instances of classes and "lift" these into instances of that class for a
newtype. However, this is not always safe. For example, consider the
following:

::

      newtype Age = MkAge { unAge :: Int }

      type family Inspect x
      type instance Inspect Age = Int
      type instance Inspect Int = Bool

      class BadIdea a where
        bad :: a -> Inspect a

      instance BadIdea Int where
        bad = (> 0)

      deriving instance BadIdea Age    -- not allowed!

If the derived instance were allowed, what would the type of its method
``bad`` be? It would seem to be ``Age -> Inspect Age``, which is
equivalent to ``Age -> Int``, according to the type family ``Inspect``.
Yet, if we simply adapt the implementation from the instance for
``Int``, the implementation for ``bad`` produces a ``Bool``, and we have
trouble.

The way to identify such situations is to have *roles* assigned to type
variables of datatypes, classes, and type synonyms.

Roles as implemented in GHC are a from a simplified version of the work
described in `Generative type abstraction and type-level
computation <http://www.seas.upenn.edu/~sweirich/papers/popl163af-weirich.pdf>`__,
published at POPL 2011.

.. _nominal-representational-and-phantom:

Nominal, Representational, and Phantom
--------------------------------------

.. index::
   single: representational; role
   single: nominal; role
   single: phantom; role

The goal of the roles system is to track when two types have the same
underlying representation. In the example above, ``Age`` and ``Int``
have the same representation. But, the corresponding instances of
``BadIdea`` would *not* have the same representation, because the types
of the implementations of ``bad`` would be different.

Suppose we have two uses of a type constructor, each applied to the same
parameters except for one difference. (For example, ``T Age Bool c`` and
``T Int Bool c`` for some type ``T``.) The role of a type parameter says
what we need to know about the two differing type arguments in order to
know that the two outer types have the same representation (in the
example, what must be true about ``Age`` and ``Int`` in order to show
that ``T Age Bool c`` has the same representation as ``T Int Bool c``).

GHC supports three different roles for type parameters: nominal,
representational, and phantom. If a type parameter has a nominal role,
then the two types that differ must not actually differ at all: they
must be identical (after type family reduction). If a type parameter has
a representational role, then the two types must have the same
representation. (If ``T``\'s first parameter's role is representational,
then ``T Age Bool c`` and ``T Int Bool c`` would have the same
representation, because ``Age`` and ``Int`` have the same
representation.) If a type parameter has a phantom role, then we need no
further information.

Here are some examples: ::

      data Simple a = MkSimple a          -- a has role representational

      type family F
      type instance F Int = Bool
      type instance F Age = Char

      data Complex a = MkComplex (F a)    -- a has role nominal

      data Phant a = MkPhant Bool         -- a has role phantom

The type ``Simple`` has its parameter at role representational, which is
generally the most common case. ``Simple Age`` would have the same
representation as ``Simple Int``. The type ``Complex``, on the other
hand, has its parameter at role nominal, because ``Complex Age`` and
``Complex Int`` are *not* the same. Lastly, ``Phant Age`` and
``Phant Bool`` have the same representation, even though ``Age`` and
``Bool`` are unrelated.

.. _role-inference:

Role inference
--------------

What role should a given type parameter should have? GHC performs role
inference to determine the correct role for every parameter. It starts
with a few base facts: ``(->)`` has two representational parameters;
``(~)`` has two nominal parameters; all type families' parameters are
nominal; and all GADT-like parameters are nominal. Then, these facts are
propagated to all places where these types are used. The default role
for datatypes and synonyms is phantom; the default role for classes is
nominal. Thus, for datatypes and synonyms, any parameters unused in the
right-hand side (or used only in other types in phantom positions) will
be phantom. Whenever a parameter is used in a representational position
(that is, used as a type argument to a constructor whose corresponding
variable is at role representational), we raise its role from phantom to
representational. Similarly, when a parameter is used in a nominal
position, its role is upgraded to nominal. We never downgrade a role
from nominal to phantom or representational, or from representational to
phantom. In this way, we infer the most-general role for each parameter.

Classes have their roles default to nominal to promote coherence of
class instances. If a ``C Int`` were stored in a datatype, it would be
quite bad if that were somehow changed into a ``C Age`` somewhere,
especially if another ``C Age`` had been declared!

There is one particularly tricky case that should be explained: ::

      data Tricky a b = MkTricky (a b)

What should ``Tricky``'s roles be? At first blush, it would seem that
both ``a`` and ``b`` should be at role representational, since both are
used in the right-hand side and neither is involved in a type family.
However, this would be wrong, as the following example shows: ::

      data Nom a = MkNom (F a)   -- type family F from example above

Is ``Tricky Nom Age`` representationally equal to ``Tricky Nom Int``?
No! The former stores a ``Char`` and the latter stores a ``Bool``. The
solution to this is to require all parameters to type variables to have
role nominal. Thus, GHC would infer role representational for ``a`` but
role nominal for ``b``.

.. _role-annotations:

Role annotations
----------------

.. extension:: RoleAnnotations
    :shortdesc: Enable role annotations.

    :since: 7.8.1

    Allow role annotation syntax.

Sometimes the programmer wants to constrain the inference process. For
example, the base library contains the following definition: ::

      data Ptr a = Ptr Addr#

The idea is that ``a`` should really be a representational parameter,
but role inference assigns it to phantom. This makes some level of
sense: a pointer to an ``Int`` really is representationally the same as
a pointer to a ``Bool``. But, that's not at all how we want to use
``Ptr``\ s! So, we want to be able to say ::

      type role Ptr representational
      data Ptr a = Ptr Addr#

The ``type role`` (enabled with :extension:`RoleAnnotations`) declaration
forces the parameter ``a`` to be at role representational, not role
phantom. GHC then checks the user-supplied roles to make sure they don't
break any promises. It would be bad, for example, if the user could make
``BadIdea``\'s role be representational.

As another example, we can consider a type ``Set a`` that represents a
set of data, ordered according to ``a``\'s ``Ord`` instance. While it
would generally be type-safe to consider ``a`` to be at role
representational, it is possible that a ``newtype`` and its base type
have *different* orderings encoded in their respective ``Ord``
instances. This would lead to misbehavior at runtime. So, the author of
the ``Set`` datatype would like its parameter to be at role nominal.
This would be done with a declaration ::

      type role Set nominal

Role annotations can also be used should a programmer wish to write a
class with a representational (or phantom) role. However, as a class
with non-nominal roles can quickly lead to class instance incoherence,
it is necessary to also specify :extension:`IncoherentInstances` to allow
non-nominal roles for classes.

The other place where role annotations may be necessary are in
``hs-boot`` files (:ref:`mutual-recursion`), where the right-hand sides
of definitions can be omitted. As usual, the types/classes declared in
an ``hs-boot`` file must match up with the definitions in the ``hs``
file, including down to the roles. The default role for datatypes is
representational in ``hs-boot`` files, corresponding to the common use
case.

Role annotations are allowed on data, newtype, and class declarations. A
role annotation declaration starts with ``type role`` and is followed by
one role listing for each parameter of the type. (This parameter count
includes parameters implicitly specified by a kind signature in a
GADT-style data or newtype declaration.) Each role listing is a role
(``nominal``, ``representational``, or ``phantom``) or a ``_``. Using a
``_`` says that GHC should infer that role. The role annotation may go
anywhere in the same module as the datatype or class definition (much
like a value-level type signature). Here are some examples: ::

      type role T1 _ phantom
      data T1 a b = MkT1 a     -- b is not used; annotation is fine but unnecessary

      type role T2 _ phantom
      data T2 a b = MkT2 b     -- ERROR: b is used and cannot be phantom

      type role T3 _ nominal
      data T3 a b = MkT3 a     -- OK: nominal is higher than necessary, but safe

      type role T4 nominal
      data T4 a = MkT4 (a Int) -- OK, but nominal is higher than necessary

      type role C representational _   -- OK, with -XIncoherentInstances
      class C a b where ...    -- OK, b will get a nominal role

      type role X nominal
      type X a = ...           -- ERROR: role annotations not allowed for type synonyms
