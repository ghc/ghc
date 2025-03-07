.. _type-defaulting:

Type variable defaulting
=========================

.. index::
   single: default declarations

During type-checking, the typechecker will create metavariables that stand for
as-yet unknown types. If the typechecker cannot infer a specific type for a
metavariable (nor quantify over it), it will **default** the type variable
to a specific type, such as ``Int`` or ``String``.

The main principle of defaulting is that it is used as a **best guess** for
type variables that remain undetermined at the very end of type inference.

For example: ::

  -- Ambiguity:
  u :: String
  u = show 4.12
    -- The type of 4.12 is ambiguous; default it to Double

  -- Monomorphism restriction:
  k = 6
    -- Due to the monomorphism restriction, we must infer a monomorphic type, not:
    --
    --   k :: forall a. Num a => a
    --
    -- GHC defaults the type of k to Integer.

  -- NoPolyKinds:
  f Proxy = ()
    -- With -XNoPolyKinds, we cannot infer the poly-kinded type
    --
    --   forall {k} (a :: k). Proxy a -> ()
    --
    -- GHC defaults (k := Type), thus inferring
    --
    --   f :: forall (a :: Type). Proxy a -> ()

  -- Representation polymorphism:
  g x = x
    -- Should not quantify over the representation, as we cannot infer
    -- a representation-polymorphic type for 'x':
    --
    --   forall {r} (a :: TYPE r). a -> a
    --
    -- Default r := LiftedRep, thus inferring
    --
    --   g :: forall (a :: Type). a -> a

  -- Multiplicity polymorphism
  {-# LANGUAGE LinearTypes #-}
  h x = x
    -- Do not infer multiplicity polymorphism:
    --
    -- h :: forall {m} a. a %m -> a
    --
    -- GHC defaults (m := Many), thus inferring
    --
    --   h :: forall (a :: Type). a %Many -> a

The rest of this section will explain how defaulting assignment choices
are made by GHC.

.. _class_defaulting:

Type class defaulting
---------------------
The principal defaulting mechanism is type class defaulting. It is used in
situations such as: ::

  u :: String
  u = show 4.12
    -- (4.12 :: a), with constraints (Fractional a, Show a).

  v :: [Int] -> [Char] -> Bool
  v xs ys = genericLength xs > genericLength ys
    -- Recall that (genericLength :: Num i => [x] -> i). Here, the return type
    -- of the two calls to genericLength are ambiguous; we get
    -- (genericLength xs :: a) with constraints (Num a, Ord a).

The idea is that, because the type variable ``a`` appears as the argument to
an unsolved numeric constraint such as ``Num a`` or ``Fractional a``, it is
natural to default ``a`` to a numeric type such as ``Double`` or ``Integer``,
respectively.

The original rules from the (`Haskell Report, Section 4.3.4 <https://www.haskell.org/onlinereport/decls.html#sect4.3.4>`__) for
defaulting an unfilled metavariable ``a`` are as follows:

  1. The type variable ``a`` appears only in constraints of the form ``C v``
     for a class ``C``.
     (Small asterisk: ignoring invisible type parameters, e.g. the kind
     parameter for ``Typeable``.)

  2. All the classes that ``a`` appears in are standard (i.e. they are defined
     in the Prelude or a standard library).

  3. At least one of the classes is numeric (e.g. is ``Num``, or implies ``Num``).

In such a situation, GHC will attempt to default ``a`` by looking at which
default declarations are in scope in the module, e.g.: ::

  default (Int, Integer, Double)

means: first try to default ``a`` to ``Int``. If that fails, try ``Integer``
instead, and if that fails try ``Double``. The Haskell report requires that
all the types listed in the ``default`` declaration must be instances of
``Num``.

If the module contains no ``default`` declaration, then GHC behaves as if
the following fallback default declaration is present: ::

  default (Integer, Double)

.. _extended-class-defaulting:

Extensions to type class defaulting
-----------------------------------
Several extensions to the rules are available. The basic mechanism described
above dictates that the only **defaultable classes** are the numeric classes,
with the extra constraint that all classes must be standard.

Overloaded strings
^^^^^^^^^^^^^^^^^^

:extension:`OverloadedStrings` extends both the defaultable and standard classes
to include ``IsString``. It allows types that are instances of ``IsString`` in
default declarations, and adds ``String`` to the fallback default declaration
when no default declaration is provided.

Named defaults
^^^^^^^^^^^^^^

:extension:`NamedDefaults` allows per-class default declarations, e.g.
``default Show (Int, Bool)``, with the requirement that the types are instances
of the class. The sets of defaultable and standard classes are both extended to
include any class with a named default declaration.

Extended default rules
^^^^^^^^^^^^^^^^^^^^^^

:extension:`ExtendedDefaultRules`, which is enabled by default in the GHCi
prompt, extends the rules more significantly:

  1. It extends the set of defautable classes to include **interactive classes**,
     adding ``Show``, ``Eq``, ``Ord``, ``Foldable``, ``Traversable``. Types that
     are instances of any of these classes are then allowed in default declarations.

  2. It adds the unit type ``()`` and the list type ``[]`` to the start of the
     fallback default declaration.

  3. The rules for defaulting are changed to the following:

    a. Find the constraints that are of form ``C a`` where ``a`` is a type
       variable, and partition those constraints into groups that share a
       common type variable ``a``.

    b. Keep only the groups in which at least one of the classes is a
       defaultable class. (As explained above, these include the interactive
       classes, ``IsString`` with :extension:`OverloadedStrings`, and any
       class with a named default declaration with :extension:`NamedDefaults`.)

    c. For each remaining group G, try each type ``ty`` from the default-type
       list in turn. If setting ``a = ty`` would allow the constraints in G to
       be completely solved, default ``a`` to ``ty``; otherwise try the next
       type in the list.

To rephrase, (3) means that we filter out all the constraints that are not of
the form ``C a`` for a defaultable class ``C``. Such constraints thus do not
participate in the process (either to help or to hinder); but they must
of course be soluble once the defaulting process is complete. In particular,
this includes all multi-parameter constraints such as ``D a b`` or ``E [a] Int``.

Other defaulting mechanisms
---------------------------
Besides the main type-class defaulting mechanism described in :ref:`class_defaulting`,
GHC also contains a several other (more specialised) mechanisms for defaulting.

.. _kind-based-defaulting:

Kind-based defaulting
^^^^^^^^^^^^^^^^^^^^^
For type variables of kind ``RuntimeRep`` or ``Levity``, GHC will default them
to ``LiftedRep`` and ``Lifted``, respectively. This is explained in
:ref:`representation-polymorphism-defaulting`.

For type variables of kind ``Multiplicity``, GHC will default to ``Many``, as
explained in :ref:`linear-types`: linear and multiplicity-polymorphic
arrows are *always declared*, never inferred.

For kind variables (of kind ``Type``), GHC will default to ``Type``
when :extension:`PolyKinds` is not enabled; see :ref:`kind-defaulting`.

.. _callstack_defaulting:

Callstack defaulting
^^^^^^^^^^^^^^^^^^^^^
To implement the ``HasCallStack`` and ``ExceptionContext`` mechanisms, GHC
will default any unsolved ``HasCallStack`` constraints to ``EmptyCallStack``,
and any unsolved ``ExceptionContext`` constraints to ``emptyExceptionContext``,
respectively.

.. _equality_defaulting:

Equality defaulting
^^^^^^^^^^^^^^^^^^^^^
Usually, when GHC encounters an equality constraint ``alpha ~ ty`` in which
``alpha`` is an unfilled metavariable, GHC will immediately unify ``alpha``
with ``ty``. However, in certain circumstances, GHC will hold off from eagerly
unifying ``alpha := ty`` in order to preserve the ability to infer principal
types. For example: ::

  {-# LANGUAGE GADTs #-}
  data T a where
    MkT :: T Bool
  h x = case x of { MkT -> True }

Within the case match, GHC creates a fresh metavariable ``beta`` for the return
type, but this metavariable is "stuck" inside the equality constraint
introduced by the GADT pattern match:

  forall a. a ~ Bool => beta ~ Bool

Here GHC could either unify ``beta := a`` or ``beta := Bool``, which would
result in the following type signatures for ``h``, respectively: ::

  forall a. T a -> a
  T Bool -> Bool

Neither of these is more general than the other, so GHC holds off from unifying
``beta := Bool`` – even though there is an unsolved equality constraint
``beta ~ Bool``. Because we are not at the top-level (we are inside a GADT
pattern match), this will cause GHC to reject this program (fix: add a type
signature to ``h``).

However, in cases where typechecking does manage to proceed to the top-level,
it makes sense to default unfilled metavariables that appear in unsolved nominal
equality constraints. For example: ::

  f :: forall a. (forall t. (F t ~ Int) => a -> Int) -> Int

  g :: Int
  g = f id

In this case, when instantiating ``f`` in the body of ``g``, GHC will create
a fresh metavariable ``alpha`` for the outer forall of ``f``, which again
appears in an implication below another equality constraint:

  forall t. (F t ~ Int) => alpha ~ Int

However, unlike in the previous GADT example, here we are at the top-level and
we can't quantify over ``alpha``. So GHC goes ahead and defaults ``alpha``
to ``Int``; this does not threaten principal types.
