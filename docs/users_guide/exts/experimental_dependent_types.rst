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
retained binders with scoped local Id names. GHCi can load these modules as
interpreted source and keep local declarations between commands.

Natural numerals in vector indices
----------------------------------

A library can give its index type an interpretation for nonnegative type
numerals. Import ``FromNatural`` from ``GHC.TypeNats.Experimental`` in the
package ``ghc-experimental``. Define an instance for the index kind. For
example, these declarations register a Peano kind::

    import GHC.TypeNats.Experimental (FromNatural)
    import qualified GHC.TypeNats as TN

    data Nat = Z | S Nat

    type family ToPeano (n :: TN.Nat) :: Nat where
      ToPeano 0 = 'Z
      ToPeano n = 'S (ToPeano (n TN.- 1))

    type instance FromNatural Nat n = ToPeano n

This definition of ``ToPeano`` requires :extension:`UndecidableInstances`.
In a module with :extension:`ExperimentalDependentTypes`, a numeral in
``Vec a 3`` now has the expected kind ``Nat``. The checked instance
determines its value. When GHC infers the kind of a numeral, or expects
``GHC.TypeNats.Nat``, the ordinary built-in literal rules apply.
Family instance patterns keep the ordinary rules for literals.
When a proof needs the full Peano representation of a numeral, each
successor takes a reduction step in this example. Large proofs can
reach GHC's family reduction depth limit.

The packed-vector example defines ``type n + m = plus n m`` and shares
the ordinary definition of ``plus`` with its ``Num Nat`` instance. It can
therefore write signatures such as these::

    three     :: Vec Bool 3
    safeHead  :: Vec a (1 + n) -> a
    appendOne :: Vec a n -> a -> Vec a (n + 1)

The first argument of ``plus`` controls reduction. In ``safeHead``,
``1 + n`` reduces to a successor without inspecting ``n``. Each caller
still uses the ordinary term-level ``fromInteger`` for its runtime literals.
An arbitrary ``Num`` instance cannot establish a runtime index equality.

The library also provides ``replicateAt 3 x :: Vec a 3``, using
:extension:`RequiredTypeArguments`. The first argument specifies the
length in the type. For a length ``n`` read at runtime, its retained
counterpart is ``replicateNative n x :: Vec a n``.

A retained argument can also be a sum of two lengths read from IO::

    import PackedVecLibrary (Nat, Vec, type (+), replicateNative)

    fillSum :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Bool (n + m)
    fillSum n m = replicateNative (n + m) True

The signature uses the library's type operator. The body uses the
standard overloaded ``Prelude.(+)``. The library defines ``(+) = plus``
in its ``Num Nat`` instance. Its interface records both that alias and
the checked equations for ``plus``. GHC checks that this call uses the
same dictionary before relying on the sum in the result type. The
runtime sum stays lazy.

A recursive equation can also call another checked function. For example,
``times (S n) m = plus m (times n m)`` uses the checked equations for
``plus``. The recursive call decreases the first argument of ``times``.
The vector library defines both the type operator ``(*)`` and the
ordinary ``Num Nat`` multiplication method using ``times``.
Enable ``NoStarIsType`` to write the type operator::

    flattenRows :: Vec (Vec a cols) rows -> Vec a (rows * cols)
    flattenRows VNil = VNil
    flattenRows (VCons row rest) = appendVec row (flattenRows rest)

The vector library's ``Num Nat`` instance also supplies checked equations
for ``fromInteger``. A caller can use those equations at a retained
argument, including inside an addition::

    three :: Vec Bool 3
    three = replicateNative 3 True

    oneMore :: foreach (n :: Nat) -> Vec Bool (n + 1)
    oneMore n = replicateNative (n + 1) True

The checker recognises a ground instance whose ``fromInteger n``
first guards against ``n < 0``, returns the zero constructor for
``n == 0``, and otherwise applies the successor constructor to
``fromInteger (n - 1)``. Both guards use the standard ``Integer``
operations. The instance module publishes the constructors in its
interface. Use retained numerals in a client of that module. Each
successor counts against ``-freduction-depth``.
The ordinary solver must select the checked dictionary at the literal.
A result signature still uses ``FromNatural``. GHC checks
that the two interpretations agree when it checks the result type.

A consumer can infer the tail of a vector produced by another call::

    firstCopy :: foreach (n :: Nat) -> a -> a
    firstCopy n x = safeHead (replicateNative (1 + n) x)

The addition equations reduce the formal length ``1 + k`` of ``safeHead``
to ``S k`` and the argument length to ``S n``. GHC can then choose ``k``
to be the caller's runtime ``n``. The first element is available without
forcing that tail length. With an unknown left addend, ``n + 1`` needs a
proof of nonemptiness or a pattern match on the vector.

A constructor argument can determine the erased type of a retained parameter.
For example, the fields of a list cell determine its element type::

    firstCell :: a -> [a] -> a
    firstCell x xs = safeHead (sourceVector (x : xs))

The equation for ``sourceLength (x : xs)`` gives a successor for any tail
``xs``. The caller can therefore take the head without inspecting the tail.

A call can also use the source function directly, as in
``replicateNative (plus n m) True``. The function must be monomorphic
and fully applied to named values or supported constructor trees. A user-defined
operator can supply these equations in the same way as a named function.

For a class method, the current implementation accepts a concrete
instance of a one-parameter class. The method must return its class
parameter and name a checked function as its complete definition.
The producer enables :extension:`ExperimentalDependentTypes` to
publish this information. A client reads it from the completed
interface. Within the instance module, use ``plus n m`` directly.
An abstract ``Num Nat`` dictionary passed to the client
cannot establish the addition equations.

Retained arguments
------------------

The syntax retains a value argument whose identity can occur in the
result type. For example::

    {-# LANGUAGE ExperimentalDependentTypes #-}

    keep :: foreach (b :: Bool) -> Select b -> Select b
    keep _ x = x

Here ``Select`` denotes a type family indexed by ``Bool``. The argument ``b`` is
an ordinary runtime Boolean whose identity can occur in the result type.
The wildcard leaves this argument unnamed in the equation. The payload
and result still have the same index. Under Haskell's usual lazy pattern
semantics, calling ``keep`` does not force the Boolean. In a module with
:extension:`Strict`, the wildcard has the same demand as a named argument.

Applications retain that value argument. They accept supported constructors,
named local values and checked source calls as described above. The
compiler checks their types without evaluating the runtime arguments.

In GHCi, a retained definition can be used in later commands. For example,
with the vector library loaded, enter the definition and use it at the
next prompt::

    ghci> let fill :: foreach (n :: Nat) -> Vec Bool n; fill n = replicateNative n True
    ghci> toList (fill 2)
    [True,True]

A multiline declaration group between ``:{`` and ``:}`` works as well.
Use ``:seti -XExperimentalDependentTypes`` to enable the syntax at the
prompt. A length obtained from IO can be bound in the same expression
that applies the retained function, as in
``do { n <- readIORef ref; print (toList (fill n)) }``.

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
Constructor fields and later arguments accept variable and wildcard patterns.
Their multiplicities must be unrestricted. The result must have lifted kind, and the outer
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
and accepts variables or wildcards for its fields. The result must have lifted kind. Parentheses around
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

Use ``-O0 -fomit-interface-pragmas -fno-write-if-simplified-core`` for
object code and bytecode. The restricted module pipeline forces Core Lint
and STG Lint. GHCi can interpret a module in memory. A no-link build can
also write bytecode for a later GHCi session. Profiling, module plugins,
JavaScript code generation, and optimized module options remain unsupported.
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
