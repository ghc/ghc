.. _instance-decls:
.. _instance-resolution:

Instance declarations and resolution
------------------------------------

An instance declaration has the form ::

      instance (assertion1, ..., assertionn) => class type1 ... typem where ...

The part before the "``=>``" is the *context*, while the part after the
"``=>``" is the *head* of the instance declaration.

When GHC tries to resolve, say, the constraint ``C Int Bool``, it tries
to match every instance declaration against the constraint, by
instantiating the head of the instance declaration. Consider these
declarations: ::

      instance context1 => C Int a     where ...  -- (A)
      instance context2 => C a   Bool  where ...  -- (B)

GHC's default behaviour is that *exactly one instance must match the
constraint it is trying to resolve*. For example, the constraint
``C Int Bool`` matches instances (A) and (B), and hence would be
rejected; while ``C Int Char`` matches only (A) and hence (A) is chosen.

Notice that

-  When matching, GHC takes no account of the context of the instance
   declaration (``context1`` etc).

-  It is fine for there to be a *potential* of overlap (by including
   both declarations (A) and (B), say); an error is only reported if a
   particular constraint matches more than one.

See also :ref:`instance-overlap` for flags that loosen the instance
resolution rules.

.. _flexible-instance-head:

Relaxed rules for the instance head
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. extension:: TypeSynonymInstances
    :shortdesc: Allow type synonyms to be mentioned in instance heads.

    :implied by: :extension:`FlexibleInstances`
    :since: 6.8.1

    :status: Included in :extension:`GHC2024`, :extension:`GHC2021`

    Allow definition of type class instances for type synonyms.

.. extension:: FlexibleInstances
    :shortdesc: Allow instance heads to mention arbitrary nested types.

    :implies: :extension:`TypeSynonymInstances`

    :since: 6.8.1

    :status: Included in :extension:`GHC2024`, :extension:`GHC2021`

    Allow definition of type class instances with arbitrary nested types in the
    instance head.

In Haskell 2010 the head of an instance declaration must be of the form
``C (T a1 ... an)``, where ``C`` is the class, ``T`` is a data type
constructor, and the ``a1 ... an`` are distinct type variables. In the
case of multi-parameter type classes, this rule applies to each
parameter of the instance head (Arguably it should be okay if just one
has this form and the others are type variables, but that's the rules at
the moment).

GHC relaxes this rule in two ways:

-  With the :extension:`TypeSynonymInstances` extension, instance heads may use type
   synonyms. As always, using a type synonym is just shorthand for
   writing the RHS of the type synonym definition. For example: ::

         type Point a = (a,a)
         instance C (Point a) where ...

   is legal. The instance declaration is equivalent to ::

         instance C (a,a) where ...

   As always, type synonyms must be fully applied. You cannot, for
   example, write: ::

         instance Monad Point where ...

-  The :extension:`FlexibleInstances` extension allows the head of the instance
   declaration to mention arbitrary nested types. For example, this
   becomes a legal instance declaration ::

         instance C (Maybe Int) where ...

   See also the `rules on overlap <#instance-overlap>`__.

   The :extension:`FlexibleInstances` extension implies
   :extension:`TypeSynonymInstances`.

However, the instance declaration must still conform to the rules for
instance termination: see :ref:`instance-termination`.

.. _formal-instance-syntax:

Formal syntax for instance declaration types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The top of an instance declaration only permits very specific forms of types.
To make more precise what forms of types are or are not permitted, we provide a
BNF-style grammar for the tops of instance declarations below.

.. code-block:: none

  inst_top ::= 'instance' opt_forall opt_ctxt inst_head opt_where

  opt_forall ::= <empty>
              |  'forall' tv_bndrs '.'

  tv_bndrs ::= <empty>
            |  tv_bndr tv_bndrs

  tv_bndr ::= tyvar
           |  '(' tyvar '::' ctype ')'

  opt_ctxt ::= <empty>
            |  btype '=>'
            |  '(' ctxt ')' '=>'

  ctxt ::= ctype
        |  ctype ',' ctxt

  inst_head ::= '(' inst_head ')'
             |  prefix_cls_tycon arg_types
             |  arg_type infix_cls_tycon arg_type
             |  '(' arg_type infix_cls_tycon arg_type ')' arg_types

  arg_types ::= <empty>
             |  arg_type arg_types

  opt_where ::= <empty>
             |  'where'

Where:

- ``btype`` is a type that is not allowed to have an outermost
  ``forall``/``=>`` unless it is surrounded by parentheses. For example,
  ``forall a. a`` and ``Eq a => a`` are not legal ``btype``\s, but
  ``(forall a. a)`` and ``(Eq a => a)`` are legal.
- ``ctype`` is a ``btype`` that has no restrictions on an outermost
  ``forall``/``=>``, so ``forall a. a`` and ``Eq a => a`` are legal ``ctype``\s.
- ``arg_type`` is a type that is not allowed to have ``forall``\s or ``=>``\s
- ``prefix_cls_tycon`` is a class type constructor written prefix (e.g.,
  ``Show`` or ``(&&&)``), while ``infix_cls_tycon`` is a class type constructor
  written infix (e.g., ``\`Show\``` or ``&&&``).

This is a simplified grammar that does not fully delve into all of the
implementation details of GHC's parser (such as the placement of Haddock
comments), but it is sufficient to attain an understanding of what is
syntactically allowed. Some further various observations about this grammar:

- Instance declarations are not allowed to be declared with nested ``forall``\s
  or ``=>``\s. For example, this would be rejected: ::

    instance forall a. forall b. C (Either a b) where ...

  As a result, ``inst_top`` puts all of its quantification and constraints up
  front with ``opt_forall`` and ``opt_context``.
- Furthermore, instance declarations types do not permit outermost parentheses
  that surround the ``opt_forall`` or ``opt_ctxt``, if at least one of them are
  used. For example, ``instance (forall a. C a)`` would be rejected, since GHC
  would treat the ``forall`` as being nested.

  Note that it is acceptable to use parentheses in a ``inst_head``. For
  instance, ``instance (C a)`` is accepted, as is ``instance forall a. (C a)``.

.. _instance-rules:
.. _instance-termination:

Instance termination rules
~~~~~~~~~~~~~~~~~~~~~~~~~~

Regardless of :extension:`FlexibleInstances` and :extension:`FlexibleContexts`,
instance declarations must conform to some rules that ensure that
instance resolution will terminate. The restrictions can be lifted with
:extension:`UndecidableInstances` (see :ref:`undecidable-instances`).

The rules are these:

1. The Paterson Conditions: for each class constraint ``(C t1 ... tn)``
   in the context

   1. No type variable has more occurrences in the constraint than in
      the head

   2. The constraint has fewer constructors and variables (taken
      together and counting repetitions) than the head

   3. The constraint mentions no type functions. A type function
      application can in principle expand to a type of arbitrary size,
      and so are rejected out of hand

   If these three conditions hold we say that the constraint ``(C t1 ... tn)`` is
   **Paterson-smaller** than the instance head.

2. The Coverage Condition. For each functional dependency,
   ⟨tvs⟩\ :sub:`left` ``->`` ⟨tvs⟩\ :sub:`right`, of the class, every
   type variable in S(⟨tvs⟩\ :sub:`right`) must appear in
   S(⟨tvs⟩\ :sub:`left`), where S is the substitution mapping each type
   variable in the class declaration to the corresponding type in the
   instance head.

These restrictions ensure that instance resolution terminates: each
reduction step makes the problem smaller by at least one constructor.
You can find lots of background material about the reason for these
restrictions in the paper `Understanding functional dependencies via
Constraint Handling
Rules <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp06.pdf>`__.

For example, these are okay:

::

      instance C Int [a]          -- Multiple parameters
      instance Eq (S [a])         -- Structured type in head

          -- Repeated type variable in head
      instance C4 a a => C4 [a] [a]
      instance Stateful (ST s) (MutVar s)

          -- Head can consist of type variables only
      instance C a
      instance (Eq a, Show b) => C2 a b

          -- Non-type variables in context
      instance Show (s a) => Show (Sized s a)
      instance C2 Int a => C3 Bool [a]
      instance C2 Int a => C3 [a] b

But these are not:

::

          -- Context assertion no smaller than head
      instance C a => C a where ...
          -- (C b b) has more occurrences of b than the head
      instance C b b => Foo [b] where ...

The same restrictions apply to instances generated by ``deriving``
clauses. Thus the following is accepted:

::

      data MinHeap h a = H a (h a)
        deriving (Show)

because the derived instance

::

      instance (Show a, Show (h a)) => Show (MinHeap h a)

conforms to the above rules.

The restrictions on functional dependencies
(:ref:`functional-dependencies`) are particularly troublesome. It is
tempting to introduce type variables in the context that do not appear
in the head, something that is excluded by the normal rules. For
example:

::

      class HasConverter a b | a -> b where
         convert :: a -> b

      data Foo a = MkFoo a

      instance (HasConverter a b,Show b) => Show (Foo a) where
         show (MkFoo value) = show (convert value)

This is dangerous territory, however. Here, for example, is a program
that would make the typechecker loop:

::

      class D a
      class F a b | a->b
      instance F [a] [[a]]
      instance (D c, F a c) => D [a]   -- 'c' is not mentioned in the head

Similarly, it can be tempting to lift the coverage condition:

::

      class Mul a b c | a b -> c where
        (.*.) :: a -> b -> c

      instance Mul Int Int Int where (.*.) = (*)
      instance Mul Int Float Float where x .*. y = fromIntegral x * y
      instance Mul a b c => Mul a [b] [c] where x .*. v = map (x.*.) v

The third instance declaration does not obey the coverage condition; and
indeed the (somewhat strange) definition:

::

      f = \ b x y -> if b then x .*. [y] else y

makes instance inference go into a loop, because it requires the
constraint ``(Mul a [b] b)``.

.. _undecidable-instances:

Undecidable instances and loopy superclasses
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. extension:: UndecidableInstances
    :shortdesc: Allow definition of instances which may make solving undecidable.

    :since: 6.8.1

    Permit definition of instances which may lead to type-checker non-termination.

The :extension:`UndecidableInstances` extension  lifts the restrictions on
on instance declarations described in :ref:`instance-termination`.
The :extension:`UndecidableInstances` extension also lifts some of the
restrictions imposed on type family instances; see
:ref:`type-family-decidability`.


With :extension:`UndecidableInstances` it is possible to create a superclass cycle,
which leads to the program failing to terminate.  To avoid this, GHC imposes
rules on the way in which superclass constraints are satisfied in an instance
declaration.  These rules apply even when :extension:`UndecidableInstances` is enabled.
Consider::

  class C a => D a where ...

  instance Wombat [b] => D [b] where ...

When typechecking this ``instance`` declaration, GHC must ensure that ``D``'s superclass,
``(C [b])`` is satisfied. We say that ``(C [b])`` is a **Wanted superclass constraint** of the
instance declaration.

If there is an ``instance blah => C [b]``, which is often the
case, GHC can use the instance declaration and all is well.  But suppose there is no
such instance, so GHC can only satisfy the Wanted ``(C [b])`` from the context of the instance,
namely the Given constraint ``(Wombat [b])``.  Perhaps the declaration of ``Wombat`` looks like this::

  class C a => Wombat a

So the Given ``(Wombat [b])`` has a superclass ``(C [b])``, and it looks as if we can satisfy the
Wanted ``(C [b])`` constraint from this superclass of ``Wombat``.  But it turns out that
allowing this can lead to subtle looping dictionaries, and GHC prevents it.

The rule is this: **a Wanted superclass constraint can only be satisfied in one of these three ways:**

.. rst-class:: open

1. *Directly from the context of the instance declaration*.  For example, if the declaration looked like this::

      instance (Wombat [b], C [b]) => D [b] where ...

   we could satisfy the Wanted ``(C [b])`` from the Given ``(C [b])``.

2. *Using another instance declaration*. For example, if we had::

      instance C b => C [b] where ...

   we can satisfy the Wanted superclass constraint ``(C [b])`` using this instance,
   reducing it to the Wanted constraint ``(C b)`` (which still has to be solved).

3. *Using the immediate superclass of a Given constraint X that is Paterson-smaller than the head of the instance declaration.*
   The rules for Paterson-smaller are precisely those described in :ref:`instance-rules`:

     - No type variable can occur more often in X than in the instance head.

     - X must have fewer type constructors and variables (taken together and counting repetitions) than the instance head.

     - X must mention no type functions.

Rule (3) is the tricky one.  Here is an example, taken from GHC's own source code::

           class Ord r => UserOfRegs r a where ...
    (i1)   instance UserOfRegs r a => UserOfRegs r (Maybe a) where
    (i2)   instance (Ord r, UserOfRegs r CmmReg) => UserOfRegs r CmmExpr where

For ``(i1)`` we can get the ``(Ord r)`` superclass by selection from
``(UserOfRegs r a)``, since it (i.e. ``UserOfRegs r a``) is Paterson-smaller than the
head of the instance declaration, namely ``(UserOfRegs r (Maybe a))``.

But for ``(i2)`` that isn't the case: ``(UserOfRegs r CmmReg)`` is not Paterson-smaller
than the head of the instance ``(UserOfRegs r CmmExpr)``, so we can't use
the superclasses of the former.  Hence we must instead add an explicit,
and perhaps surprising, ``(Ord r)`` argument to the instance declaration.

This fix, of simply adding an apparently-redundant constraint to the context
of an instance declaration, is robust: it always fixes the problem.
(We considered adding it automatically, but decided that it was better be explicit.)

Fixing this subtle superclass cycle has a long history; if you are interested, read
``Note [Recursive superclasses]`` and ``Note [Solving superclass constraints]``
in ``GHC.Tc.TyCl.Instance``.

.. _instance-overlap:

Overlapping instances
~~~~~~~~~~~~~~~~~~~~~

.. extension:: OverlappingInstances
    :shortdesc: Allow definition of overlapping instances.

    :since: 6.8.1

    :status: Deprecated

    Deprecated extension to weaken checks intended to ensure instance resolution
    termination. Use ``OVERLAPPING``, ``OVERLAPPABLE`` or ``OVERLAPS`` pragmas instead.

.. extension:: IncoherentInstances
    :shortdesc: Allow definitions of instances that may result in incoherence.

    :implies: :extension:`OverlappingInstances`.

    :since: 6.8.1

    :status: Deprecated

    Deprecated extension to weaken checks intended to ensure instance resolution
    termination. Use ``INCOHERENT`` pragmas instead. Also permits classes to
    have non-nominal roles, and affects the instance resolution algorithm for
    in-scope given constraints.

In general, as discussed in :ref:`instance-resolution`, *GHC requires
that it be unambiguous which instance declaration should be used to
resolve a type-class constraint*. GHC also provides a way to loosen
the instance resolution, by allowing more than one instance to match,
*provided there is a most specific one*. Moreover, it can be loosened
further, by allowing more than one instance to match irrespective of
whether there is a most specific one. This section gives the details.

To control the choice of instance, it is possible to specify the overlap
behavior for individual instances with a pragma, written immediately
after the ``instance`` keyword. The pragma may be one of:
``{-# OVERLAPPING #-}``, ``{-# OVERLAPPABLE #-}``, ``{-# OVERLAPS #-}``,
or ``{-# INCOHERENT #-}``.

The matching behaviour is also influenced by two module-level language
extension flags: :extension:`OverlappingInstances` and
:extension:`IncoherentInstances`. These extensions are now
deprecated (since GHC 7.10) in favour of the fine-grained per-instance
pragmas.

A more precise specification is as follows. The willingness to be
overlapped or incoherent is a property of the *instance declaration*
itself, controlled as follows:

-  An instance is *incoherent* if: it has an ``INCOHERENT`` pragma; or
   if the instance has no pragma and it appears in a module compiled
   with :extension:`IncoherentInstances`.

-  An instance is *overlappable* if: it has an ``OVERLAPPABLE`` or
   ``OVERLAPS`` pragma; or if the instance has no pragma and it appears
   in a module compiled with :extension:`OverlappingInstances`; or if the
   instance is incoherent.

-  An instance is *overlapping* if: it has an ``OVERLAPPING`` or
   ``OVERLAPS`` pragma; or if the instance has no pragma and it appears
   in a module compiled with :extension:`OverlappingInstances`; or if the
   instance is incoherent.

Now suppose that, in some client module, we are searching for an
instance of the *target constraint* ``(C ty1 .. tyn)``. The search works
like this:

-  If there are any in-scope given constraints that might match the target
   constraint (after unifying any metavariables), and
   :extension:`IncoherentInstances` is not enabled, the search fails.

-  Find all instances :math:`I` that *match* the target constraint; that is, the
   target constraint is a substitution instance of :math:`I`. These instance
   declarations are the *candidates*.

-  If there are no candidates, the search fails.

-  Eliminate any candidate :math:`IX` for which there is another candidate
   :math:`IY` such that both of the following hold:

   -  :math:`IY` is strictly more specific than :math:`IX`.  That
      is, :math:`IY` is a substitution instance of :math:`IX` but not vice versa.

   -  :math:`IX` is *overlappable* or :math:`IY` is *overlapping*. (This
      "or" design, rather than an "and" design, allows a
      client to deliberately override an instance from a library,
      without requiring a change to the library.)

-  If all the remaining candidates are incoherent, the search succeeds, returning
   an arbitrary surviving candidate.

-  If more than one non-incoherent candidate remains, the search fails.

-  Otherwise there is exactly one non-incoherent candidate; call it the
   "prime candidate".

-  Now find all instances that *unify* with
   the target constraint,
   but do not *match* it. Such non-candidate instances might match when
   the target constraint is further instantiated. If all of them are
   incoherent top-level instances, the search succeeds, returning the prime candidate.
   Otherwise the search fails.

Notice that these rules are not influenced by flag settings in the
client module, where the instances are *used*. These rules make it
possible for a library author to design a library that relies on
overlapping instances without the client having to know.

Errors are reported *lazily* (when attempting to solve a constraint),
rather than *eagerly* (when the instances themselves are defined).
Consider, for example ::

      instance C Int  b where ..
      instance C a Bool where ..

These potentially overlap, but GHC will not complain about the instance
declarations themselves, regardless of flag settings. If we later try to
solve the constraint ``(C Int Char)`` then only the first instance
matches, and all is well. Similarly with ``(C Bool Bool)``. But if we
try to solve ``(C Int Bool)``, both instances match and an error is
reported.

As a more substantial example of the rules in action, consider ::

      instance {-# OVERLAPPABLE #-} context1 => C Int b     where ...  -- (A)
      instance {-# OVERLAPPABLE #-} context2 => C a   Bool  where ...  -- (B)
      instance {-# OVERLAPPABLE #-} context3 => C a   [b]   where ...  -- (C)
      instance {-# OVERLAPPING  #-} context4 => C Int [Int] where ...  -- (D)

(These all need :extension:`FlexibleInstances`.)
Now suppose that the type inference engine needs to solve the constraint
``C Int [Int]``. This constraint matches instances (A), (C) and (D), but
the last is more specific, and hence is chosen.

If (D) did not exist then (A) and (C) would still be matched, but
neither is most specific. In that case, the program would be rejected,
unless :extension:`IncoherentInstances` is enabled, in which case it would be
accepted and (A) or (C) would be chosen arbitrarily.

An instance declaration is *more specific* than another iff the head of
former is a substitution instance of the latter. For example (D) is
"more specific" than (C) because you can get from (C) to (D) by
substituting ``a := Int`` and ``b := Int``.

The final bullet (about unifying instances)
makes GHC conservative about committing to an
overlapping instance. For example: ::

      f :: [b] -> [b]
      f x = ...

Suppose that from the RHS of ``f`` we get the constraint ``C b [b]``.
But GHC does not commit to instance (C), because in a particular call of
``f``, ``b`` might be instantiated to ``Int``, in which case instance (D)
would be more specific still. So GHC rejects the program.

If, however, you enable the extension :extension:`IncoherentInstances` when compiling
the module that contains (D), GHC will instead pick (C), without
complaining about the problem of subsequent instantiations.

Notice that we gave a type signature to ``f``, so GHC had to *check*
that ``f`` has the specified type. Suppose instead we do not give a type
signature, asking GHC to *infer* it instead. In this case, GHC will
refrain from simplifying the constraint ``C Int [b]`` (for the same
reason as before) but, rather than rejecting the program, it will infer
the type ::

      f :: C b [b] => [b] -> [b]

That postpones the question of which instance to pick to the call site
for ``f`` by which time more is known about the type ``b``. You
will need the
:extension:`FlexibleContexts` extension.

Exactly the same situation can arise in instance declarations
themselves. Suppose we have ::

      class Foo a where
         f :: a -> a
      instance Foo [b] where
         f x = ...

and, as before, the constraint ``C Int [b]`` arises from ``f``'s right
hand side. GHC will reject the instance, complaining as before that it
does not know how to resolve the constraint ``C Int [b]``, because it
matches more than one instance declaration. The solution is to postpone
the choice by adding the constraint to the context of the instance
declaration, thus: ::

      instance C Int [b] => Foo [b] where
         f x = ...

(You need :extension:`FlexibleContexts` to do this.)

As an example of the "in-scope given constraints" in the first bullet,
consider ::

   instance C a Int

   g :: forall b c. C b Int => blah
   g = ...needs (C c Int)...

Here GHC will not solve the constraint ``(C c Int)`` from the
top-level instance, because a particular call of ``g`` might
instantiate both ``b`` and ``c`` to the same type, which would
allow the constraint to be solved in a different way.  This latter
restriction is principally to make the constraint-solver complete.
(Interested folk can read ``Note [Instance and Given overlap]`` in ``GHC.Tc.Solver.Dict``.)
It is easy to avoid: in a type signature avoid a constraint that
matches a top-level instance.  The flag :ghc-flag:`-Wsimplifiable-class-constraints` warns about such signatures.

.. warning::
    Overlapping instances must be used with care. They can give
    rise to incoherence (i.e. different instance choices are made in
    different parts of the program) even without :extension:`IncoherentInstances`.
    Consider: ::

        {-# LANGUAGE OverlappingInstances #-}
        module Help where

            class MyShow a where
            myshow :: a -> String

            instance MyShow a => MyShow [a] where
            myshow xs = concatMap myshow xs

            showHelp :: MyShow a => [a] -> String
            showHelp xs = myshow xs

        {-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
        module Main where
            import Help

            data T = MkT

            instance MyShow T where
            myshow x = "Used generic instance"

            instance MyShow [T] where
            myshow xs = "Used more specific instance"

            main = do { print (myshow [MkT]); print (showHelp [MkT]) }

    In function ``showHelp`` GHC sees no overlapping instances, and so uses
    the ``MyShow [a]`` instance without complaint. In the call to ``myshow``
    in ``main``, GHC resolves the ``MyShow [T]`` constraint using the
    overlapping instance declaration in module ``Main``. As a result, the
    program prints

    .. code-block:: none

        "Used more specific instance"
        "Used generic instance"

    (An alternative possible behaviour, not currently implemented, would be
    to reject module ``Help`` on the grounds that a later instance
    declaration might overlap the local one.)

.. warning::
     GHC's optimiser (in particular, the :ghc-flag:`-fspecialise` option)
     assumes that type-classes are coherent, and hence it may replace
     any type-class dictionary argument with another dictionary of the same
     type.

     This may cause unexpected results if incoherence occurs due to incoherent
     or overlapping instances, and there is an observable difference between the
     instances (see :ghc-ticket:`22448` and :ghc-ticket:`24924` for examples).

     The :ghc-flag:`-fno-specialise-incoherents <-fspecialise-incoherents>` will
     inhibit specialisation in the presence of some incoherent instance matches,
     which may help avoid this issue at the cost of runtime performance.
     Alternatively, specialisation can be disabled entirely with
     :ghc-flag:`-fno-specialise <-fspecialise>`.

.. _instance-sigs:

Instance signatures: type signatures in instance declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. extension:: InstanceSigs
    :shortdesc: Allow type signatures to be written for instance methods.

    :since: 7.6.1

    :status: Included in :extension:`GHC2024`, :extension:`GHC2021`

    Allow type signatures for members in instance definitions.

The :extension:`InstanceSigs` extension allows users to give type signatures
to the class methods in a class instance declaration. For example: ::

      data T a = MkT a a
      instance Eq a => Eq (T a) where
        (==) :: T a -> T a -> Bool   -- The instance signature
        (==) (MkT x1 x2) (MkTy y1 y2) = x1==y1 && x2==y2

Some details:

-  The type signature in the instance declaration must be more
   polymorphic than (or the same as) the one in the class declaration,
   instantiated with the instance type. For example, this is fine: ::

         instance Eq a => Eq (T a) where
            (==) :: forall b. b -> b -> Bool
            (==) x y = True

   Here the signature in the instance declaration is more polymorphic
   than that required by the instantiated class method.

   Note that, to check that the instance signature is more polymorphic,
   GHC performs a sub-type check, which can solve constraints using available
   top-level instances.
   This means that the following instance signature is accepted: ::

      instance Eq (T Int) where
        (==) :: Eq Int => T Int -> T Int -> Bool
        (==) (MkT x1 _) (MkT y1 _) = x1 == y1

   The ``Eq Int`` constraint in the instance signature will be solved
   by the top-level ``Eq Int`` instance, from which it follows that the
   instance signature is indeed as general as the instantiated class
   method type ``T Int -> T Int -> Bool``.

-  The code for the method in the instance declaration is typechecked
   against the type signature supplied in the instance declaration, as
   you would expect. So if the instance signature is more polymorphic
   than required, the code must be too.

-  The instance signature is purely local to the class instance
   declaration. It only affects the typechecking of the method in
   the instance; it does not affect anything outside the class
   instance. In this way, it is similar to an inline type signature: ::

       instance Eq a => Eq (T a) where
           (==) = (\ x y -> True) :: forall b. b -> b -> Bool

   In particular, adding constraints such as `HasCallStack` to the
   instance signature will not have an effect; they need to be added
   to the class instead.

-  One stylistic reason for wanting to write a type signature is simple
   documentation. Another is that you may want to bring scoped type
   variables into scope. For example: ::

       class C a where
         foo :: b -> a -> (a, [b])

       instance C a => C (T a) where
         foo :: forall b. b -> T a -> (T a, [b])
         foo x (T y) = (T y, xs)
            where
              xs :: [b]
              xs = [x,x,x]

   Provided that you also specify :extension:`ScopedTypeVariables`
   (:ref:`scoped-type-variables`), the ``forall b`` scopes over the
   definition of ``foo``, and in particular over the type signature for
   ``xs``.
