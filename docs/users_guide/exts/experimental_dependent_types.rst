.. _experimental-dependent-types:

Experimental dependent types
============================

.. extension:: ExperimentalDependentTypes
    :shortdesc: Check restricted retained algebraic quantification.

    :since: 10.1
    :status: Experimental

    Enable retained algebraic quantification for an internal experiment.

This flag is local to the native dependent Haskell experiment.
This extension is disabled by default. It enables ``foreach`` in types and
checks a restricted subset of explicit value signatures. The experiment uses a
native dependent Core representation. The restricted module path supports
native top-level signatures and local retained functions. Interfaces encode
retained binders with scoped local Id names. Native module unfoldings remain
unsupported. The regression harness also
executes checked local declarations through a native bytecode entry.

The syntax retains a value argument whose identity can occur in the
result type. For example::

    {-# LANGUAGE ExperimentalDependentTypes #-}

    keep :: foreach (b :: Bool) -> Select b -> Select b
    keep b x = x

Here ``Select`` denotes a type family indexed by ``Bool``. The argument ``b`` is
an ordinary runtime Boolean whose identity can occur in the result type.
Applications retain that value argument. They accept supported constructors or
named local values. They do not construct singleton witnesses or
evaluate an index to determine its type.

Constructor equations can specialise the result type::

    choose :: foreach (b :: Bool) -> Select b
    choose True = 42
    choose False = False

The checker substitutes the constructor into later argument types and the
result type. A consumer can therefore receive the selected value::

    observe :: foreach (b :: Bool) -> Select b -> String
    observe True n = show (n :: Int)
    observe False p = if p then "True" else "False"

The domain can also be a user-defined closed algebraic type::

    {-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies #-}

    data Nat = Z | S Nat

    data Vec a (n :: Nat) where
      VNil :: Vec a Z
      VCons :: a -> Vec a n -> Vec a (S n)

    type family Add (n :: Nat) (m :: Nat) where
      Add Z m = m
      Add (S n) m = S (Add n m)

    replicateNative :: forall a. foreach (n :: Nat) -> a -> Vec a n
    replicateNative Z x = VNil
    replicateNative (S k) x = VCons x (replicateNative k x)

    headNative :: forall a. foreach (n :: Nat) -> Vec a (S n) -> a
    headNative n (VCons x _) = x

    successorHead :: foreach (n :: Nat) -> Int
    successorHead n = headNative n (replicateNative (S n) 42)

    appendNative :: forall a. foreach (n :: Nat) (m :: Nat)
                 -> Vec a n -> Vec a m -> Vec a (Add n m)
    appendNative n m xs ys = appendVec xs ys

    appendVec :: Vec a n -> Vec a m -> Vec a (Add n m)
    appendVec VNil ys = ys
    appendVec (VCons x xs) ys = VCons x (appendVec xs ys)

The input Nat remains a runtime value. ``headNative`` does not inspect its
Nat argument. ``appendNative`` uses its vector arguments to select equations.
Nat constructor indices use ordinary DataKinds promotion.

The current subset requires a complete value signature. It permits outer
erased ``forall`` binders and multiple annotated retained binders. The domain
must be a closed, lifted, first-order algebraic type. Parameterised domains,
newtypes, existential or constrained constructors, unboxed fields, and
constructor wrappers remain unsupported.

One variable equation can bind the complete retained telescope and match an
ordinary GADT payload, as in ``headNative``. Constructor equations split one
leading retained index. They require one unguarded equation per constructor.
Constructor fields and later arguments require bare variable patterns and
unrestricted multiplicities. The result must have lifted kind, and the outer
match wrapper must be identity. Ordinary rank restrictions still apply.
An erased ``forall`` inside a retained binder's scope remains unsupported.
Index-dependent constraints are limited to reflexive
Boolean equalities. Quick Look inference and optimisation pragmas for retained
functions remain restricted. Dedicated rejection checks report GHC-83605.
Ordinary payload mismatches use the existing type error. Unsupported type
positions report GHC-43582.

Refinement in source ``case`` expressions remains unimplemented. The constructor
rule currently applies to function equations.
Calls can pass constructor arguments with fields, including ``S n`` and
``S (S n)``. These calls do not force ``n`` to check its index. CorePrep checks
transport of later payloads and the result when it names a constructor argument.
Transport through a result that still contains a dependent function type remains
unsupported. A partial application with a constructed retained argument reports
the named CorePrep restriction. Strict wildcard matching on a partial function
with named indices preserves ordinary forcing behavior. It does not force an
unused index or the function body. An empty match on a function type remains
non-exhaustive. Producers and consumers that use local names must use the same
named index. The checker does not equate a name with its definition.

To compile an ordinary executable, use
``-O0 -fomit-interface-pragmas -fno-write-if-simplified-core``. The restricted
module pipeline forces Core Lint and STG Lint. Profiling, plugins, non-object
module backends, and optimized module options remain unsupported.
Native-containing ``INLINE``, ``INLINABLE``, and class-default unfoldings also
remain unsupported. Their current builders run ordinary optimization during
desugaring.

This fork uses interface version ``1001202611131`` from the configured compiler
version ``10.1.20261113.1``. Rebuild matching libraries and packages with this
compiler. The whole-file version check rejects interfaces from the old format.

The extension implies no other extensions. In particular, it does not imply
:extension:`KindSignatures`. The ``:: Bool`` annotation specifies the
type of a value binder. It does not make that binder a type variable.

When the extension is disabled, ``foreach`` remains an ordinary identifier::

    foreach :: a -> a
    foreach x = x

When the extension is enabled, ``foreach`` is a reserved keyword. Programs that
use it as an identifier must leave the extension disabled. The extension does
not enable general dependent Haskell, native module unfoldings, or the
ordinary optimizer for dependent Core.
