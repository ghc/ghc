# R22. First-class existential quantification

Filtering a vector illustrates why a caller might want to hide an
index. Only the filter knows the result length. A caller can still use
the vector even if it never asks for its length. Proposal #473 gives
this signature for the implicit form:

```haskell
filter :: (a -> Bool) -> Vec n a -> exists m. Vec m a
```

The witness `m` erases. The result has the runtime representation of
its payload. R04's `PackedVec` serves a different caller, who can also
read the stored length at runtime. Agda makes the erasure distinction
explicit with `@0` on an argument. Its vector example shows a cons
constructor that carries only the element and tail at runtime.
[Serokell R22](https://ghc.serokell.io/dh#existential-quantification),
[proposal #473](https://github.com/ghc-proposals/ghc-proposals/pull/473)
and [Agda erasure](https://agda.readthedocs.io/en/v2.8.0/language/runtime-irrelevance.html).

## What to check first

- [x] Form a lifted erased existential in private Core. The formation
  test checks scope, substitution, alpha-equivalence and Core Lint. It
  writes a real interface signature, reads it back from disk, and
  typechecks it. It also reads the HIE type. The reader rejects a binder or
  payload of kind `Constraint` and a reference outside its scope.
- [x] Construct an explicit package in private Core. Core Lint checks
  that the payload has the type specified by the witness. It rejects
  a `Maybe Bool` payload when the chosen witness requires `Maybe Int`.
- [x] Open an arbitrary package parameter in private Core. A fresh
  compiler reads the function body from an interface. Core Lint
  rejects a witness that escapes through the result type.
- [x] Run the imported function on a defined payload and an exceptional
  payload in the bytecode interpreter. The exception occurs when the
  body demands the payload. The STG checks show that a named payload
  goes directly to its consumer and a named package retains its Id.
- [ ] Compile a producer and an interface-only client. Check both
  `Nothing` and `Just`. Applying `seq` to a package with an exceptional
  payload must observe the exception. Opening a package and ignoring its
  payload should preserve laziness. Inspect the generated code for an
  extra constructor or witness argument.
- [x] Infer packing and opening in the vector filter. Test the first
  element of a vector whose tail raises an exception. If the first element
  passes the predicate, the first result should be available immediately.

The explicit slice follows the draft's alternative Core rules for
`Pack` and lexical `Unpack`. Eliminating an immediately constructed
package checks substitution for both binders. A package received from
another module also needs a representation, scope and erasure rule.
The paper gives the inference algorithm for the implicit form.
[Eisenberg, Duboc, Weirich and Lee, *An Existential Crisis Resolved*](https://richarde.dev/papers/2021/exists/exists.pdf).

The private formation checkpoint is `a3bac26492` on the local recovery
ref `refs/archive/dependent-haskell/r22-formation`. Its GHC API test
is `NativeExistentialCore`. The expression checkpoint is `949e49d863`
on the local ref `refs/archive/dependent-haskell/r22-expressions`.
Its `NativeExistentialExpr` test writes and reads the interface in
separate GHC sessions.

## Explicit source packages

With `ExperimentalDependentTypes` and `RequiredTypeArguments`, a
module can import `GHC.Exists` and export an explicit package.

```haskell
package :: exists (a :: Type) | Maybe a
package = Pack (type Bool) (Just True)

present :: Bool
present = case package of
  Pack (type a) payload ->
    case payload of
      Nothing -> False
      Just _ -> True
```

The expected type supplies the package schema. Write `Pack @Schema`
when the application must synthesize its type. A sole Pack alternative
binds a fresh type variable over the payload pattern and right-hand side.
An ordinary variable payload pattern can ignore bottom. A constructor
payload pattern demands the payload.

The frozen source compiler at `750604f34c` compiled the provider and
linked an ordinary consumer after removing the provider source. It
checked Core, STG and HIE, exact constructor identity, both payload
demand paths, witness escape and Template Haskell. The build used a
temporary target package database with the base marker compiled in the
configured base unit. The source test `NativeExistentialSource` runs
those controls against the `GHC.Exists` provided by the built base.

This slice supports one erased witness with a checked kind, saturated
applications and a sole prefix Pack case alternative. Compile modules
that use erased existential types or expressions at `-O0`. The Showcase
module exports its observation as an ordinary tuple for the optimized
Showcase entry point.

The self-hosted compiler at `3aab77ca78` writes
`-erased-existentials-v1` when an interface contains an erased
existential type, package, elimination or congruence rule. Ordinary
interfaces retain the base version. The bootstrap-built and
self-hosted compilers each link a source-free client using a package
interface written by the other. The earlier Type-only trial uses its
own experimental suffix.

### A lazy filter with an erased length

The [example](../examples/erased-existentials/ErasedPackage.hs) gives the
filter an explicit result schema.

```haskell
type Filtered a = exists (m :: Nat) | Vec a m
filterVec :: (a -> Bool) -> Vec a n -> Filtered a
```

On a passing element, the filter opens the recursive result and packs
the new vector with witness `'S m`. The length is erased. Reading the first
cell should demand the predicate and its result while leaving the tail
unevaluated. The source-free client also checks a rejected prefix, an empty
result, and an empty input.

At `7a2fe4027b` the client observes `([7],[7,9],[],[])` when the element
type is fixed to `Int` and the length constructors have kind `Type`.
Using a promoted `Nat` instead reaches GHC-97230 at the existential binder.
Keeping the element polymorphic reaches a missing `liftCoSubst` case.

At `28ee2947a7`, the same ordinary client calls the polymorphic,
`Nat`-indexed filter using only provider interfaces and objects. The
client infers `Int` from the input. It prints `([7],[7,9],[],[])` with
Core and STG lint enabled. A checked head compiles. An unchecked
`safeHead xs` reports GHC-25897 at `xs`, with expected length `S n0`
and actual length `n`. Explicitly repacking a nonempty vector at its
hidden length succeeds. Returning an arbitrary hidden-length package
as a nonempty package fails.

The solver compares payloads under a common fresh length and checks the
resulting nominal or representational coercion against the original
packages. The GHC API tests restore both forms in a fresh compiler
session. The R22 Showcase matches all 268 output lines. Its existential
provider is compiled at `-O0`.

An erased package can preserve a payload whose index comes from a retained
pair. In `forget (Pair _ flag) = Pack _ flag`, the declaration of `flag`
identifies the witness. The checker matches the existential body to that
declaration after family reduction and checks the payload at the selected
index. If the payload also requires the unknown index to equal `True`,
the checker rejects it. A caller can inspect a defined payload without
forcing a bottom-valued witness in the original pair.
[Conversion example](../examples/erased-existentials/ForgetWitness.hs).

For the implicit form the crucial source branch is
`VCons x (filterVec p xs)`. Opening the recursive result introduces a
fresh length `m`. The result witness `'S m` must be packed inside that
scope, as in the explicit branch above. The outer application returns
only the package. An ordinary client should read the same first cell and
should still have to match the vector before calling a nonempty head
function. At `7a2fe4027b`, `exists (m :: Nat). Vec a m` fails at the dot
with GHC-58481. This gives separate checks for parsing, inferred binder
scope, and demand. [Proposal #473](https://github.com/ghc-proposals/ghc-proposals/pull/473)

### An implicit package in an application

The `NativeImplicitExistential` test uses
`type Hidden a = exists (m :: Nat). Vec m a`. Its passing branch is
`Cons x (filterVec p xs)`. The recursive call opens under a fresh
length `m`. The expected result type packs `Cons` at witness
`'S m` inside that scope. Core preserves both operations. Final STG
passes the recursive call to the constructor as a lazy thunk.

The ordinary client imports only interfaces and links against the
provider objects. It reads the first passing cell with a bottom tail.
A function with independent length parameters accepts two packages.
Requiring the same length, assuming a successor, or returning a
caller-chosen length produces GHC-25897. A polymorphic `id` forwards an
existing package. A payload whose type refers to a retained field can
also supply the witness for an implicit package in that field's scope.
The same application rule checks a linear operand once. Using that
operand in two linear arguments is a multiplicity error.

The separator survives in checked types and interfaces. Core uses the
same Pack and Unpack operations for each mode. This is a GHC adaptation
of the draft. It preserves bar-only interface bytes and gives dot
types and coercions their own interface version.

The [combined example](../examples/erased-existentials/ImplicitVector.hs)
also accepts a retained pair directly in the signature of `forgetLength`.
The vector fixes its erased result length. An ordinary interface-only
client prints `([7],[],[9])` at both optimization levels. It can read
the first passing element with an exceptional input tail.

An interface that uses both a scoped sum and a dot existential records
both requirements. A compulsory retained default adds a third requirement.
At all three compression levels, the two historical readers reject these
mixed interfaces at the header. Each reader can still open an interface
that contains only the feature it supports. Reversing the declarations
keeps the same header. Reserializing the cached payload preserves its bytes.

Opening is limited to an application with a known operand head and an
ordinary payload domain. Each opening introduces a fresh scoped
witness. The draft's general opaque witness projections, scope
repacking across lambda, let or case, and `@Pack` remain open.
[Proposal #473](https://github.com/ghc-proposals/ghc-proposals/pull/473)
and [the inference rules](https://richarde.dev/papers/2021/exists/exists.pdf).

At `7a2fe4027b`, the filter signature fails at the dot with GHC-58481.
At `e04f9add03` it fails with GHC-68947. The ordinary GADT control uses
`Some :: Vec n a -> Some a`.
Its successful branch matches `Some tail` from the recursive call before
returning `Some (Cons x tail)`. Reading the first result in this version
raises the tail exception.
