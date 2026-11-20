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

The Showcase can write `case builder n of Callback f call -> call f`.
GHC keeps the hidden witness in scope while it checks the application
and the alternatives. The result `(Int, Bool)` can leave that scope.
Returning the payload at a caller-chosen type still fails at the return
expression. At the 301-line checkpoint the direct case failed with
GHC-46956 and needed a consumer with signature `Callback t -> (Int, Bool)`.
[Builder and direct case](../examples/erased-existentials/RankedPackage.hs).

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
- [x] `NativeExistentialSource` hides the explicit `Maybe` producer's
  source before compiling its clients. `inspect` sees `Nothing` and
  `Just`. The ordinary client uses `` lazyPack `seq` () `` to observe the
  original payload exception. `ignoreBottom` opens the same package
  without evaluating the payload. A strict payload pattern raises the
  same exception. The opener matches `Maybe` directly in final STG.
  At `-O0` and `-O2`, both compiler stages emit the ordinary `Nothing`
  and `Just` closure layouts and a unary opener. This check covers
  lifted payloads in the explicit prefix `Pack` form.
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

The first source compiler supported one erased witness with a checked kind,
saturated applications and a sole prefix Pack case alternative. It compiled
the package provider at `-O0`. The example exported its observations as
an ordinary tuple.

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
Different packages cannot supply a shared length. Assuming a successor
or returning a caller-chosen length produces GHC-25897. A polymorphic `id` forwards an
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

The same example passes `singletonBuilder` to `runBuilder`. The producer
checks a lambda against `exists (n :: Nat). a -> Vec a n`. The consumer
opens that function and binds the filtered result in a local `let`.
Both the producer and the ordinary caller can compile at `-O1`. A client can
also construct a package at the call site. For example,
`runBuilder (\x -> VCons x VNil) odd (7 :: Int)` returns `[7]`.
[Optimizer scope](R05-dependent-core.md#optimizing-erased-packages).

An application with an ordinary payload domain can open an operand whose
type exposes a dot package. An opening introduces a fresh scoped witness.
The recursive filter result can also be named with
`let rest = filterVec p xs in Cons x rest`. The nonrecursive local
binding retains the exact dot package type, including its enclosing
element type. Opening the binding stays inside the application.

An ordinary client can also write `foldr step z (fst (partitionVec p xs))`.
The result of `partitionVec` exposes the dot package in a rigid pair type.
Matching that pair against the domain of `fst` identifies its result
parameter before the fold checks its operands. The compiler then checks the
original operand once at its declared multiplicity. A complete type
annotation can also identify the dot package. The resulting witness remains
local to the fold, and the fold can return its first result with an
exceptional input tail.

The draft's general opaque witness projections, scope
repacking across lambda, let or case, and `@Pack` remain open.
[Proposal #473](https://github.com/ghc-proposals/ghc-proposals/pull/473)
and [the inference rules](https://richarde.dev/papers/2021/exists/exists.pdf).

At `7a2fe4027b`, the filter signature fails at the dot with GHC-58481.
At `e04f9add03` it fails with GHC-68947. The ordinary GADT control uses
`Some :: Vec n a -> Some a`.
Its successful branch matches `Some tail` from the recursive call before
returning `Some (Cons x tail)`. Reading the first result in this version
raises the tail exception.

### A function payload

`exists (n :: Nat). Bool -> Vec n Bool` can check a lambda such as
`\b -> Cons b Nil`. The expected arrow determines the lambda's shape.
The match checker checks the lambda at a fresh instance of that arrow.
Core packs the checked lambda with witness `'S 'Z`. The function body
still waits for application. An ordinary higher-order client opens the
package through the application rule.

The witness is chosen outside the lambda's patterns. A GADT pattern's
local type variable cannot determine this witness. Type binders of an
enclosing forall retain their scope. A binder declared by the dot type
does not enter source scope.

This checks a lambda when its expected dot schema has a visible
ordinary arrow payload. Inference for a lambda without that expected
shape remains open. [Proposal #473](https://github.com/ghc-proposals/ghc-proposals/pull/473)
and [the Elab-Gen rule](https://richarde.dev/papers/2021/exists/exists.pdf).

### A package at the head of a call

A dot package with a visible ordinary arrow payload can head a value
application. An ordinary invisible prefix is instantiated before
the opening. When the result expects a dot package, the witness
stays in scope while the result is packed. When a named call is an
argument to an ordinary observer, that observer can run in the same
opening. Thus `list (f x)` consumes the payload without an intermediate
package.

A case on the call can use the same opening. In
`case builder b of Callback value use -> use value`, the pattern and
its branch share the witness of the call result. The complete case
must return a type independent of that witness. The constructor
pattern still forces the result constructor and leaves unused fields lazy.
This extends the application opening of
[Elab-IExist](https://richarde.dev/papers/2021/exists/exists.pdf#page=18)
to a GHC case scrutinee by checking the alternatives in the same scope.

A leading visible type argument can also select which package a name
produces. For a declared `builder :: forall a. C a => Builder a`,
`case builder @Bool flag of …` checks the type argument and instantiates
the dictionary parameter outside the opening. It applies the opened
payload to `flag`.
An ordinary observer can consume `builder @Bool flag` in the same scope.
[Proposal #378 on application chains](https://ghc-proposals.readthedocs.io/en/latest/proposals/0378-dependent-type-design.html#dependent-application-and-the-static-subset).

Each call opens a fresh witness. The source test rejects a call that
returns its witness at a caller-chosen length. It also rejects an
observer that equates two independent calls. A direct or nested call
can consume a linear package once.

An unsigned local binding can keep the directly declared dot result of
an observer. For example, `let selected = filterVec p (builder x)`
retains the filter's package type. Its result metadata is established
outside the opening. The observer's argument-only length can still
match the witness inside. The checker rejects an observer whose
result would keep that opened length as a free index.

### Repeated uses of a local package

`firstPair p p` can pass the same local package to two arguments that
require a shared length. The typechecker uses one witness and one lazy
payload binding in the application. This covers a monomorphic
let-bound package or a function parameter. The ordinary client reads
a pair of heads while the tail is bottom. A function that ignores both
arguments can also ignore a bottom package.

The binding must resolve to the same checked local variable with the
same schema. Distinct bindings and repeated computed calls have
independent witnesses. A polymorphic binding can choose a different
witness at each use. Witness sharing does not allow the witness to
escape its opening or permit duplicate linear use. This is the
lexical-variable fragment of the paper's IExistCore projection rule.
[An Existential Crisis Resolved, section 6.1](https://richarde.dev/papers/2021/exists/exists.pdf).

An ordinary client can also write `snd p (fst p)` for
`p :: exists t. (t, t -> Int)`. The nested unary call reuses the
unrestricted opening of the same local package in the enclosing
application. Both calls keep the ordinary demand of their selectors.
The source-free test agrees with `case p of (x, k) -> k x` when it
demands the result or ignores the whole application. Returning
`fst p` at a caller-chosen type, combining independent packages, and
duplicating a linear package still fail. The paper also accepts
alpha-equivalent computed package expressions.
[An Existential Crisis Resolved, Aside 2](https://richarde.dev/papers/2021/exists/exists.pdf#page=21).

The same client can write `snd (supply x) (fst (supply x))` when
`supply` is a top-level producer with a known monomorphic unrestricted
arrow to a dot package and both calls use the same local input. The checked producer and input
must resolve to the same Ids. The opening shares the pure producer
call as a local `let` would. Ignoring both projections leaves the
producer unevaluated. Independent calls at different inputs retain
separate witnesses. The rule covers this fragment of computed
expressions. [An Existential Crisis Resolved, sections 7.4 and 9.2.2](https://richarde.dev/papers/2021/exists/exists.pdf).


### Constructor cases on the payload

An ordinary client can match a dot package with a constructor of its
declared data payload. For example, match the result of `filterVec`
with `Nil` and `Cons x _`. The case checker opens the scrutinee once
and checks all alternatives under one fresh witness. A GADT
constructor can refine that witness in its alternative. The result
type stays in the enclosing scope.

The ordinary match determines demand. A constructor match evaluates
a bottom package, and matching its first cell can leave a bottom
tail unevaluated. A variable, wildcard or package-typed pattern
synonym alone continues to match the package. A case that consumes
a linear package still checks the usage of its fields. Direct
constructor matching applies to the dot mode. The bar mode keeps
its explicit Pack alternative.


A function equation can also use a constructor of the payload. In
`firstPassing (Rows (VCons x _) _) = Just x`, the match opens the
package for the source patterns. A preceding equation can match an
empty vector. A wildcard in an earlier argument still leaves a later
package lazy. Each argument has its own witness scope.
[Partition equations](../examples/erased-existentials/ImplicitVector.hs).

A pair of partitions stores each vector with its own erased length.
The equations for `firstKept (VCons x _, _)` and `firstKept (VNil, _)`
open the first package. They cover both vector constructors. The second
package can be bottom. GHC records the opening on the matched field so
that later alternatives can use the same witness. A different field
retains its own witness. The regression checks a false guard, a guard
that forces a second package, and an inaccessible GADT alternative.
[Vector partition](../examples/erased-existentials/ImplicitVector.hs)
and [nested match regression](../../../testsuite/tests/dependent/should_run/NativeNestedPackagePat.sh).

### A package with nested dot binders

A partition can return one package that hides both output lengths.

```haskell
type PackedPair a = exists (n :: Nat). exists (m :: Nat). (Vec n a, Vec m a)
```

The first binder is in scope in the second binder's kind and in
the payload. Checking the payload chooses witnesses in that order.
Core wraps the checked payload with an inner Pack and an outer
Pack. An application that requests the payload opens consecutive
dot binders with separate skolems. The result type stays outside both scopes. An inner bar package requires explicit elimination.

A local binding can store the whole nested package. For example,
`let rows = partitionPair p xs in roundTrip rows` keeps both lengths
hidden. Check free variables on the whole type. The inner package alone
can refer to the length bound by the outer package.

The partition uses a lazy pair pattern in each recursive branch.
The source-free client reads each output's first cell with a bottom
tail. A consumer can also ignore a bottom package. Inspecting the
payload pair forces it. Both length witnesses stay independent,
including after interface readback. The dependent-kind control
checks `exists (k :: Type). exists (v :: k). Proxy v`.
[Elab-Gen and Elab-IExist in An Existential Crisis Resolved](https://richarde.dev/papers/2021/exists/exists.pdf).


### Forwarding at a known package type

An ordinary polymorphic producer can use the expected package as its
result type argument. For example, `choose flag p q` can return an
existing `PackedPair a`. The same rule covers a user-defined
exception producer or `error`. A choice can include consecutive
binders and a kind that depends on an earlier binder. The chosen
type argument preserves the bar and dot modes.

The checker matches the kind and complete package type. It can pass a
package containing an opaque type family or a rank-n function. The
caller needs no equation for the family. Any other ordinary parameters
still use the application checker. Returning a package through this route
preserves the producer's demand. An ordinary client can ignore a
bottom package and observes the producer's original exception when
it demands the payload.

With `ImpredicativeTypes`, an ordinary higher-order caller can infer a
complete erased package from an argument. For example,
`apply (\h -> inspectRanked h) (id ranked)` infers the package type at
`id` and at `apply`. Quick Look checks the binder kinds, payload and
coercions before using the package. A nested runtime index or retained
function still reports GHC-83605 on this path.
[Ordinary client](../../../testsuite/tests/dependent/should_run/implicit-existential-source/QuickLookForward.hs).

### Inferring a higher-rank witness

A package can store `Callback t` with a hidden `t`. If the supplied
value already has type `Callback (forall a. a -> a)`,
`ImpredicativeTypes` lets the producer infer that entire witness when it
constructs the package. Consecutive witnesses are chosen in scope.
Checking the final payload can still reject an incompatible family result.
[Rank-n callback](../examples/erased-existentials/RankedPackage.hs)
and [GenImpredicative in Figure 10](https://richarde.dev/papers/2021/exists/exists.pdf#page=22).

An expected dot package can also hide the callback type in a lambda.
For an expected `exists t. Bool -> Callback t`, the lambda
`\b -> prepare b` takes its witness from a declared result of
`prepare :: Bool -> Callback PolyId`. GHC matches the invariant result
types before it checks the lambda patterns. The usual lambda check then
checks the operands and their usage. A pattern-local type cannot supply
the outer witness. A guarded or multi-equation lambda uses the first
matching result declaration in source order. Every guard and result
checks against the same witness. For example, a declared consumer that
takes a polymorphic callback and returns `Payload PolyId` can fix the
witness for a different branch that constructs `Payload id atTwoTypes`.
A branch with an incompatible result still reports a type error when
its guard is unreachable.
Leading visible type applications can choose input types when the
declared result type is independent of the supplied type arguments.
The ordinary checker checks those applications at their source spans.
An explicitly marked required argument can also select the witness.
For example `\b -> (withWitness @Int (type PolyId) Proxy) id (use b)`
reads the closed `PolyId` synonym from the producer interface.
An ordinary rank-n consumer uses the selected callback at two types.
A type introduced by a lambda pattern cannot supply the outer witness.
Consecutive dot binders can take their witnesses from separate
equations. The interface can declare one branch with
`TwoCallbacks PolyId r` and another with `TwoCallbacks l PolyMaybe`.
The corresponding direct fields fix both witnesses before the client
checks the lambda. Each field needs an agreed closed polytype, and
the ordinary checker still checks every equation.
The witness erases before code generation. Agda's `@0` record fields
use the same runtime distinction between a payload and erased evidence.
[Source-free client](../../../testsuite/tests/dependent/should_run/implicit-existential-source/WitnessClient.hs)
and [Agda erasure](https://agda.readthedocs.io/en/v2.8.0/language/runtime-irrelevance.html).

The expected package type can also connect two fields of a tuple.
For `exists t. (t, t -> (Int, Bool))`, a callback with argument type
`forall a. a -> a` fixes the type of a bare `id` in the first field.
A consumer can use the ordinary equation `observePair (f, k) = k f`.
A witness that escapes the match produces a type error at its use.
The diagnostic identifies the source location where GHC opened the package.
[Tuple example](../examples/erased-existentials/RankedPackage.hs).
