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

    {-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeApplications, TypeFamilies #-}

    data Nat = Z | S Nat

    data Vec a (n :: Nat) where
      VNil :: Vec a Z
      VCons :: a -> Vec a n -> Vec a (S n)

    type family Add (n :: Nat) (m :: Nat) where
      Add Z m = m
      Add (S n) m = S (Add n m)

    replicateNative :: forall a. foreach (n :: Nat) -> a -> Vec a n
    replicateNative n x = case n of
      Z -> VNil
      S k -> VCons x (replicateNative k x)

    headNative :: forall a. foreach (n :: Nat) -> Vec a (S n) -> a
    headNative n (VCons x _) = x

    successorHead :: foreach (n :: Nat) -> Int
    successorHead n = headNative n (replicateNative (S n) 42)

    appendNative :: forall a. foreach (n :: Nat) (m :: Nat)
                 -> Vec a n -> Vec a m -> Vec a (Add n m)
    appendNative n m xs ys = appendVec xs ys

    appendAfterSuccessor :: foreach (n :: Nat) (m :: Nat)
                         -> Vec Int (S n) -> Vec Int m -> Vec Int (Add (S n) m)
    appendAfterSuccessor n = appendNative @Int (S n)

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
ordinary GADT payload, as in ``headNative``. An equation with a complete signature
can also stop before the remaining retained arguments, as in
``appendAfterSuccessor``.
Unsigned aliases of retained functions still require a complete signature and
report GHC-83605. Constructor equations split one
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

Source ``case`` expressions can refine a named local retained index when the
result type is checked. The rule also refines referenced local values whose
types depend on that index. It requires one unguarded branch per constructor
and bare variable fields. The result must have lifted kind. Parentheses around
the scrutinee are accepted. Generalized locals require unrestricted
multiplicities. Guards, defaults, nested constructor patterns,
Template Haskell quotations, and general motive inference remain unsupported.
The case inspects its index even when the result is a function. Captured payloads
remain lazy until the selected branch uses them.

Calls can pass constructor arguments with fields, including ``S n`` and
``S (S n)``. These calls do not force ``n`` to check its index. CorePrep checks
transport of later payloads and the result when it names a constructor argument.
Transport through a remaining retained function is supported when its closed
algebraic domain and unrestricted multiplicity stay unchanged. This includes
partial applications such as ``appendNative @Int (S n)``. Later applications
retain the original index relationships. Structural Pi unification,
normalization with ambient Given evidence or plugins,
changed domains or multiplicities, and dependent erased binder kinds remain
unsupported. Strict wildcard matching on a partial function with named indices
preserves ordinary forcing behavior. It does not force an
unused index or the function body. An empty match on a function type remains
non-exhaustive. Producers and consumers that use local names must use the same
named index. The checker does not equate a name with its definition.

Global type-family equations can normalize beneath a retained binder with an
unchanged closed algebraic domain and unrestricted multiplicity. Complete
signatures can therefore check aliases with no source argument patterns::

    nativeId :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
    nativeId n xs = xs

    leftUnit :: foreach (n :: Nat) -> Vec Int (Add Z n) -> Vec Int n
    leftUnit = nativeId

    leftUnitAppend :: foreach (n :: Nat) (m :: Nat)
                   -> Vec Int (Add Z n) -> Vec Int m -> Vec Int (Add n m)
    leftUnitAppend = appendNative @Int

A constant family can remove the retained dependency::

    type family ConstantNat (n :: Nat) where
      ConstantNat n = Int

    ordinaryNat :: Nat -> Int
    ordinaryNat _ = 42

    constantAlias :: foreach (n :: Nat) -> ConstantNat n
    constantAlias = ordinaryNat

The normalized signature of ``constantAlias`` is an ordinary ``Nat -> Int``
arrow. This normalization does not evaluate a runtime index. A family that needs
an unknown constructor remains stuck. Solver normalization under the retained
binder excludes ambient Given evidence and plugin rewriting. It does not enable
unsigned aliases, structural retained unification, or a function eta law.
After normalization, identical runtime indices and alpha-equal retained
function types can satisfy reflexivity. This does not decompose a retained
function or assign a type meta-variable. Family axioms reduce at nominal and
representational roles. Existing phantom normalization leaves family heads
unreduced; ordinary forall binder kinds normalize at nominal role.

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
