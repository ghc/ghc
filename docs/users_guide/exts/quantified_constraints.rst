.. _quantified-constraints:

Quantified constraints
======================

.. extension:: QuantifiedConstraints
    :shortdesc: Allow ``forall`` quantifiers in constraints.

    :since: 8.6.1

    Allow constraints to quantify over types.

The extension :extension:`QuantifiedConstraints` introduces **quantified constraints**,
which give a new level of expressiveness in constraints. For example, consider ::

 data Rose f a = Branch a (f (Rose f a))

 instance (Eq a, ???) => Eq (Rose f a)
   where
     (Branch x1 c1) == (Branch x2 c2)
        = x1==x1 && c1==c2

From the ``x1==x2`` we need ``Eq a``, which is fine.  From ``c1==c2`` we need ``Eq (f (Rose f a))`` which
is *not* fine in Haskell today; we have no way to solve such a constraint.

:extension:`QuantifiedConstraints` lets us write this ::

 instance (Eq a, forall b. (Eq b) => Eq (f b))
        => Eq (Rose f a)
   where
     (Branch x1 c1) == (Branch x2 c2)
        = x1==x1 && c1==c2

Here, the quantified constraint ``forall b. (Eq b) => Eq (f b)`` behaves
a bit like a local instance declaration, and makes the instance typeable.

The paper `Quantified class constraints
<https://homepages.inf.ed.ac.uk/wadler/papers/quantcc/quantcc.pdf>`_ (by Bottu, Karachalias,
Schrijvers, Oliveira, Wadler, Haskell Symposium 2017) describes this feature in
technical detail, with examples, and so is a primary reference source for this
feature.

Motivation
----------------
Introducing quantified constraints offers two main benefits:

- Firstly, they enable terminating resolution where this was not possible before.  Consider for instance the following instance declaration for the general rose datatype ::

   data Rose f x = Rose x (f (Rose f x))

   instance (Eq a, forall b. Eq b => Eq (f b)) => Eq (Rose f a) where
     (Rose x1 rs1) == (Rose x2 rs2) = x1 == x2 && rs1 == rs2

  This extension allows us to write constraints of the form ``forall b. Eq b =>
  Eq (f b)``, which is needed to solve the ``Eq (f (Rose f x))`` constraint
  arising from the second usage of the ``(==)`` method.

- Secondly, quantified constraints allow for more concise and precise specifications. As an example, consider the MTL type class for monad transformers::

   class Trans t where
     lift :: Monad m => m a -> (t m) a

  The developer knows that a monad transformer takes a monad ``m`` into a new monad ``t m``.
  But this property is not formally specified in the above declaration.
  This omission becomes an issue when defining monad transformer composition::

    newtype (t1 * t2) m a = C { runC :: t1 (t2 m) a }

    instance (Trans t1, Trans t2) => Trans (t1 * t2) where
      lift = C . lift . lift

  The goal here is to ``lift`` from monad ``m`` to ``t2 m`` and
  then ``lift`` this again into ``t1 (t2 m)``.
  However, this second ``lift`` can only be accepted when ``(t2 m)`` is a monad
  and there is no way of establishing that this fact universally holds.

  Quantified constraints enable this property to be made explicit in the ``Trans``
  class declaration::

    class (forall m. Monad m => Monad (t m)) => Trans t where
      lift :: Monad m => m a -> (t m) a

This idea is very old; see Section 7 of `Derivable type classes <https://www.microsoft.com/en-us/research/publication/derivable-type-classes/>`_.

Syntax changes
----------------

`Haskell 2010 <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5>`_ defines a ``context`` (the bit to the left of ``=>`` in a type) like this

.. code-block:: none

    context ::= class
            |   ( class1, ..., classn )

    class ::= qtycls tyvar
            |  qtycls (tyvar atype1 ... atypen)

We extend ``class`` (warning: this is a rather confusingly named non-terminal symbol) with two extra forms, namely precisely what can appear in an instance declaration

.. code-block:: none

    class ::= ...
          | [context =>] qtycls inst
          | [context =>] tyvar inst

The definition of ``inst`` is unchanged from the Haskell Report (roughly, just a type).
The ``context =>`` part is optional.  That is the only syntactic change to the language.

Notes:

- Where GHC allows extensions in instance declarations we allow exactly the same extensions to this new form of ``class``.  Specifically, with :extension:`ExplicitForAll` and :extension:`MultiParamTypeClasses` the syntax becomes

  .. code-block:: none

    class ::= ...
           | [forall tyvars .] [context =>] qtycls inst1 ... instn
           | [forall tyvars .] [context =>] tyvar inst1 ... instn

  Note that an explicit ``forall`` is often absolutely essential. Consider the rose-tree example ::

    instance (Eq a, forall b. Eq b => Eq (f b)) => Eq (Rose f a) where ...

  Without the ``forall b``, the type variable ``b`` would be quantified over the whole instance declaration, which is not what is intended.

- One of these new quantified constraints can appear anywhere that any other constraint can, not just in instance declarations.  Notably, it can appear in a type signature for a value binding, data constructor, or expression.  For example ::

   f :: (Eq a, forall b. Eq b => Eq (f b)) => Rose f a -> Rose f a -> Bool
   f t1 t2 = not (t1 == t2)

- The form with a type variable at the head allows this::

   instance (forall xx. c (Free c xx)) => Monad (Free c) where
       Free f >>= g = f g

  See `Iceland Jack's summary <https://gitlab.haskell.org/ghc/ghc/issues/14733#note_148352>`_.  The key point is that the bit to the right of the ``=>`` may be headed by a type *variable* (``c`` in this case), rather than a class.  It should not be one of the forall'd variables, though.

  (NB: this goes beyond what is described in `the paper <http://i.cs.hku.hk/~bruno//papers/hs2017.pdf>`_, but does not seem to introduce any new technical difficulties.)


Typing changes
----------------

See `the paper <http://i.cs.hku.hk/~bruno//papers/hs2017.pdf>`_.

Superclasses
----------------

Suppose we have::

     f :: forall m. (forall a. Ord a => Ord (m a)) => m Int -> Bool
     f x = x == x

From the ``x==x`` we need an ``Eq (m Int)`` constraint, but the context only gives us a way to figure out ``Ord (m a)`` constraints.  But from the given constraint ``forall a. Ord a => Ord (m a)`` we derive a second given constraint ``forall a. Ord a => Eq (m a)``, and from that we can readily solve ``Eq (m Int)``.  This process is very similar to the way that superclasses already work: given an ``Ord a`` constraint we derive a second given ``Eq a`` constraint.

NB: This treatment of superclasses goes beyond `the paper <http://i.cs.hku.hk/~bruno//papers/hs2017.pdf>`_, but is specifically desired by users.

Overlap
-------------

Quantified constraints can potentially lead to overlapping local axioms.
Consider for instance the following example::

 class A a where {}
 class B a where {}
 class C a where {}
 class (A a => C a) => D a where {}
 class (B a => C a) => E a where {}

 class C a => F a where {}
 instance (B a, D a, E a) => F a where {}

When type checking the instance declaration for ``F a``,
we need to check that the superclass ``C`` of ``F`` holds.
We thus try to entail the constraint ``C a`` under the theory containing:

- The instance axioms : ``(B a, D a, E a) => F a``
- The local axioms from the instance context : ``B a``, ``D a`` and ``E a``
- The closure of the superclass relation over these local axioms : ``A a => C a`` and ``B a => C a``

However, the ``A a => C a`` and ``B a => C a`` axioms both match the wanted constraint ``C a``.
There are several possible approaches for handling these overlapping local axioms:

- **Pick first**.  We can simply select the **first matching axiom** we encounter.
  In the above example, this would be ``A a => C a``.
  We'd then need to entail ``A a``, for which we have no matching axioms available, causing the above program to be rejected.

  But suppose we made a slight adjustment to the order of the instance context, putting ``E a`` before ``D a``::

   instance (B a, E a, D a) => F a where {}

  The first matching axiom we encounter while entailing ``C a``, is ``B a => C a``.
  We have a local axiom ``B a`` available, so now the program is suddenly accepted.
  This behaviour, where the ordering of an instance context determines
  whether or not the program is accepted, seems rather confusing for the developer.

- **Reject if in doubt**.  An alternative approach would be to check for overlapping axioms,
  when solving a constraint.
  When multiple matching axioms are discovered, we **reject the program**.
  This approach is a bit conservative, in that it may reject working programs.
  But it seem much more transparent towards the developer, who
  can be presented with a clear message, explaining why the program is rejected.

- **Backtracking**.  Lastly, a simple form of **backtracking** could be introduced.
  We simply select the first matching axiom we encounter and when the entailment fails,
  we backtrack and look for other axioms that might match the wanted constraint.

  This seems the most intuitive and transparent approach towards the developer,
  who no longer needs to concern himself with the fact that his code might contain
  overlapping axioms or with the ordering of his instance contexts.  But backtracking
  would apply equally to ordinary instance selection (in the presence of overlapping
  instances), so it is a much more pervasive change, with substantial consequences
  for the type inference engine.

GHC adopts **Reject if in doubt** for now.  We can see how painful it
is in practice, and try something more ambitious if necessary.

Instance lookup
-------------------

In the light of the overlap decision, instance lookup works like this when
trying to solve a class constraint ``C t``

1. First see if there is a given un-quantified constraint ``C t``.  If so, use it to solve the constraint.

2. If not, look at all the available given quantified constraints; if exactly one matches ``C t``, choose it; if more than one matches, report an error.

3. If no quantified constraints match, look up in the global instances, as described in :ref:`instance-resolution` and :ref:`instance-overlap`.

Termination
---------------

GHC uses the :ref:`Paterson Conditions <instance-termination>` to ensure
that instance resolution terminates. How are those rules modified for quantified
constraints? In two ways.

- Each quantified constraint, taken by itself, must satisfy the termination rules for an instance declaration.

- After "for each class constraint ``(C t1 ... tn)``", add "or each quantified constraint ``(forall as. context => C t1 .. tn)``"

Note that the second item only at the *head* of the quantified constraint, not its context.  Reason: the head is the new goal that has to be solved if we use the instance declaration.

Of course, ``UndecidableInstances`` lifts the Paterson Conditions, as now.

Coherence
-----------

Although quantified constraints are a little like local instance declarations, they differ
in one big way: the local instances are written by the compiler, not the user, and hence
cannot introduce incoherence.  Consider ::

  f :: (forall a. Eq a => Eq (f a)) => f b -> f Bool
  f x = ...rhs...

In ``...rhs...`` there is, in effect a local instance for ``Eq (f a)`` for any ``a``.  But
at a call site for ``f`` the compiler itself produces evidence to pass to ``f``. For example,
if we called ``f Nothing``, then ``f`` is ``Maybe`` and the compiler must prove (at the
call site) that ``forall a. Eq a => Eq (Maybe a)`` holds.  It can do this easily, by
appealing to the existing instance declaration for ``Eq (Maybe a)``.

In short, quantified constraints do not introduce incoherence.



