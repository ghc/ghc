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

An expected dot package can supply an independent erased argument of a
retained application. With `exists shape. Tile a shape`, the application
selects `a` from the expected payload. The actual runtime dimensions
determine the hidden `shape`. The ordinary result check still checks the
complete payload. See Note [Erased arguments in a dependent result] in
GHC.Tc.Gen.App and the [shape-indexed tile](R04-dependent-sums.md#sums-in-constructor-fields).

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

`RankedPackageClient.swapRowsLet` checks an unsigned local binding at the
known output package type when the body returns that binding directly.
`swapRowsCase` uses the same type flow through one variable alternative.
The package introduction stays inside the inner alternative, where both
input lengths are in scope. Swapping the vector fields also swaps the
chosen witnesses. Reading the first cell leaves the other field and
the first cell's tail unevaluated. The ordinary binding and match retain
their own demand and multiplicity checks.

The draft's general opaque witness projections, repacking when the result
package must be inferred through a scope, and `@Pack` remain open.
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

An ordinary client can also infer this result from a declared rank-2
callback. With `consumeVector :: (forall j. Vec j a -> r) -> Vec k a -> r`,
the call `consumeVector (filterVec (> 3)) (filterVec odd xs)` opens the
filtered input. The callback's declaration supplies the result package
before the input is opened, so the result hides its own length and can
be kept in an unsigned local binding. The callback must return a package
whose type is independent of its universally bound input length.
Reading one kept element leaves the remaining vector lazy. Quick Look
skips a callback whose expected type starts with `forall`. The package
check uses its declared input type and leaves its ordinary checking in
that scope. Agda likewise permits erased vector indices in compile-time
types.
[Ordinary refilter client](../examples/erased-existentials/RankedPackageClient.hs),
[Source-hidden refilter](../../../testsuite/tests/dependent/should_run/implicit-existential-source/QuickLookForward.hs),
[Quick Look, Figure 5](https://www.microsoft.com/en-us/research/wp-content/uploads/2020/01/quick-look-icfp20.pdf),
and [Agda's erased vector indices](https://agda.readthedocs.io/en/stable/language/runtime-irrelevance.html).

With `ImpredicativeTypes`, the callback can also consume two independently
filtered vectors and return one package with two hidden lengths. In the
source-hidden test,
`withBoth (producer id) (filterVec odd xs) (filterVec even ys)` supplies the
result of an unsigned local binding. The local producer takes a rank-2
transformer and returns a declared `PackedPair a`. The checker matches both
callback inputs with one substitution and checks the complete package before
it opens the inputs. The caller's element type stays in scope. A callback
that requires the lengths to coincide, or leaves either input length free
in its result, cannot supply this schema. A consumer that demands one
common input length fails at the second input. A reader that demands one
common output length fails at the inferred result binding's use.
Reading the first cell of either result leaves its tail and the other
input lazy.

An Agda record can bind two erased length fields and give each vector
field its own length index. Matching the record brings each field into
scope. In GHC the declared dot package supplies that relationship before
ordinary application checking. The callback can quantify its lengths before and after a supplied
value argument. A length that remains free in the proposed result
prevents this inference. A consumer with a fixed package result works
without `ImpredicativeTypes`. The Showcase uses this inference on an
ordinary client that keeps `[7]` on the left and `[2,4]` on the right.
[Showcase client](../examples/erased-existentials/RankedPackageClient.hs).
[Source-hidden local producer](../../../testsuite/tests/dependent/should_run/implicit-existential-source/QuickLookForward.hs),
[Agda record telescopes](https://agda.readthedocs.io/en/v2.8.0/language/record-types.html),
and [Agda erased record fields](https://agda.readthedocs.io/en/v2.8.0/language/runtime-irrelevance.html#syntax).

### Infer a package from a concrete callback input

The ordinary Showcase client zips two written lists of length two. It filters
the sums, then passes the hidden vector to a rank-2 consumer. Its declared
callback takes `Vec Int n`, increments the cells and filters again using a
runtime cutoff. The callback fixes the consumer's element type to `Int`.
The unsigned result binding keeps the callback's declared `Filtered Int`
type. Its length belongs to the new package.

Equal input lengths allow the zip. The element values still determine
which results pass. Reading the first kept cell leaves later fields lazy.
Agda's `zipWith` preserves the length and its filter provides an upper bound.
The Haskell example keeps the exact resulting length erased.
[Showcase client](../examples/erased-existentials/RankedPackageClient.hs),
[Agda zip](https://agda.github.io/agda-stdlib/v2.3/Data.Vec.Base.html#zipWith),
and [Agda filter](https://agda.github.io/agda-stdlib/master/Data.Vec.html).

### A nonrecursive local package pattern

An ordinary client can write `let (value, reader) = p in reader value`
when `p :: exists a. (a, a -> Int)`. The typechecker opens the package
once and keeps its witness in scope for the original binding and the
body. The result type stays outside that scope. Returning `value` at
a caller-chosen type fails at the returned field. Fields from separate
openings keep separate witnesses.

The ordinary local binding checker handles pattern typing, usage,
and generalization. A local identity function in the body still
works at two types. The binding shares its RHS across both fields.
The Showcase client reads an
`Int` and a `Char` with their respective callbacks on the two runtime
branches.

Without `Strict`, a plain tuple binding or `~(value, reader)` can
ignore a bottom package. With `Strict`, the outer tilde still keeps
the binding lazy. An explicit `!(value, reader)` or the `Strict` default
forces the package to a tuple before evaluating the body. This force
leaves the fields lazy. Demand leaves the witness scope unchanged.
A caller can return the fields in another erased package with a closed
result type.

Strict tuple bindings can consume an erased package at `%1`. This follows
the same usage rules as the ordinary tuple control:
```haskell
type LinearPayload = exists a. (a, a %1 -> Int)

readPlain :: (Bool, Bool %1 -> Int) %1 -> Int
readPlain pair = let !(value, reader) = pair in reader value

readLinear :: LinearPayload %1 -> Int
readLinear package = let !(value, reader) = package in reader value
```
Both use `LinearTypes`, `MonoLocalBinds`, and `BangPatterns`. The original
opening fixed the payload Id at `Many`. That accepted `readPlain` and
rejected `readLinear` with GHC-18872. The opening now gives its payload Id
the tuple pattern's inferred multiplicity. Core Lint tracks usage through
the alias, and the ordinary case generated for the pattern forces the tuple.
The source-hidden regression also reads a package at `%1` with `Strict`,
and checks GHC-18872 for a lazy linear binding and a duplicated use.
An exported lexical rule can contain the linear opening. Its interface
advertises `linear-unpack` so earlier readers reject it at the header.
An opening confined to an object file leaves the header's requirements
unchanged.

An ordinary client can write `forwardLinear @p package` as the RHS of
that strict binding under `forall (p :: Multiplicity)`. The wrapper has
type `forall (p :: Multiplicity) a. a %p -> a`. One forwarding argument
determines the erased package type for checking the full call.
The caller can also omit `@p` when normal usage determines it. A
written `@Many` cannot consume an input at a rigid `p`. The forwarding
probe covers leading specified multiplicity arguments.

The same rule accepts `OpenPayload a = exists b. (b, b %1 -> a)` through
an ordinary wrapper that calls a higher-rank identity function. The free
`a` must be rigid and free in the selected operand's full declared type.
That includes a caller's scoped `a` and a witness introduced by an
enclosing package pattern. A producer's quantified `a` still needs an
instantiation. A different result variable or an opened witness cannot escape
the let. An explicit tilde can leave the forwarded package unevaluated.
An explicit bang forces it before the body. This lexical rule follows
one forwarding operand.

An ordinary client can also pass the opened result to a retained reader.
`DeepSubsumption` adapts the reader's dictionary below its retained arrow.
The source-hidden test reads both `Bool` and `Char` results and can ignore
a bottom retained argument or a lazy package.
[Showcase formatter](../examples/erased-existentials/RetainedPackageFormatClient.hs),
[Retained reader client](../../../testsuite/tests/dependent/should_run/implicit-existential-source/LexicalRetainedClient.hs).

This path accepts one unsigned nonrecursive boxed tuple binding with
variable or wildcard fields and at most one outer bang or tilde. The
unguarded RHS must be a named value or ordinary application. The
declaration or one forwarded argument must expose the dot package.
The usual binding rules still apply to
recursive groups, signed bindings, and patterns that bring their own
evidence.
[Ordinary client](../examples/erased-existentials/RankedPackageClient.hs),
[source-hidden regression](../../../testsuite/tests/dependent/should_run/NativeImplicitExistential.sh),
[GHC's existential constructor restrictions](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/existential_quantification.html#restrictions),
and [GHC's strict binding rules](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/strict.html).

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
dot binders with separate skolems. The result type stays outside both scopes.

One outer dot binder can hide the index of an existing bar package. For
`type Inner a = exists b | (a, b)` and `type Mixed = exists a. Inner a`,
a value of type `Inner Bool` fixes the dot witness to `Bool`. An ordinary
client can use this introduction through an interface. A written
`Pack @(Inner Bool) (type Int) (True, 7 :: Int)` also gives the inner
schema. In `case p of Pack (type b) payload -> ...`, the case opens the
dot package before checking the explicit pattern. The pattern binds the
bar witness. The ordinary payload pattern determines demand.

A separately compiled ordinary module can call
`inspectInner p` with `inspectInner :: forall a. Inner a -> Int` and
`p :: Mixed`. The type of the opened bar package fixes `a` before
checking the call. The parameter must occur injectively in the bar
binder kind or body. The result type stays outside the dot witness's
scope. The consumer can open the inner package with its explicit Pack
pattern, or ignore its payload and leave the argument lazy.

The caller can also write `inspectInner (make flag)` when `make`
has a declared mixed result. The producer can have polymorphic
parameters and constraints. The checker checks the call once and gives
its result to a lazy unpack. The run test uses a separate client and
compares the call with a named package. A consumer that ignores the
package also leaves a bottom producer argument unevaluated.

A unary reader can infer several erased parameters from the same opened
bar package. The Showcase's ordinary client calls
`readLabelledNumber (id (hide n))`, where `hide n` has type
`exists a. LabelledNumber a String`. The opened package fixes `a` to
the hidden witness and `label` to `String`. The caller gets back
the label and number. The source-hidden regression also checks that a
bar binder's kind can fix an earlier parameter in the telescope.
The complete bar schema must match, including repeated occurrences.
An occurrence only under an opaque type family gives no witness.
The reader's type arguments cannot capture the bar binder. The opened
dot witness cannot escape in the result.

One call can also open two mixed packages with independent witnesses.
For `chooseInners :: Inner a -> Bool -> Inner b -> Int`, the two
opened bar types determine `a` and `b` together. The ordinary argument
between them still checks against `Bool`. The reader can select either
package without forcing the other one. Two independent openings of the
same dot schema supply distinct outer types. The result cannot expose
either witness. The source-hidden run checks both sides and the selected
producer's original exception at `-O0` and `-O2`, including a call beneath
an ordinary `id` with Quick Look enabled.
[Ordinary client](../examples/erased-existentials/RankedPackageClient.hs),
[source-hidden regression](../../../testsuite/tests/dependent/should_run/NativeImplicitExistential.sh),
and [Proposal #473, implicit existential elimination](https://github.com/ghc-proposals/ghc-proposals/pull/473).

The caller can also write `inspectInner (id (make flag))`. A
polymorphic producer can forward the type of one written ordinary
argument directly to its result. For example, a function of type
`Show a => a -> b -> b` forwards the second argument type. The
compiler opens the checked expression, including its type and evidence
applications. The same call can pass an independent higher-rank callback.
For `relay :: (forall x. x -> x) -> a -> a`, the package argument fixes
`a`. The caller can supply the callback under a rank-n lambda. The
ordinary check validates the complete producer call before opening the
package. It checks the callback type, the producer's constraints, and
argument use. The result still cannot expose the hidden witness.

A selector such as `Bool -> a -> a -> a` can forward either operand
when both declare the same closed erased package schema after expanding
type aliases and comparing binders up to renaming. GHC checks the complete
selector at the common schema, then opens the result. The alternatives
can hide different witnesses. The Showcase uses `bool right left flag`
with a flag read from an `IORef`. It reads either labelled number and
leaves the other package unevaluated. An incompatible branch fails at
that argument. The probe needs a closed schema for every forwarding
operand. Extending the match to types with free variables requires
evidence that those variables are rigid in the caller's scope. A fresh
variable from an omitted producer argument cannot supply that evidence.
[Ordinary selector client](../examples/erased-existentials/RankedPackageClient.hs)
and [implicit opening in Elab-IExist](https://richarde.dev/papers/2021/exists/exists.pdf#page=18).

A result parameter with no corresponding argument type can remain
ambiguous. An opaque family result stays opaque.
An ordinary consumer can open the result of a conditional directly.
For `left, right :: Hidden a`, the source-hidden test calls
`take 1 (listCells (if flag then id left else right))`. Both branches
identify the same package type, including the caller's rigid `a`.
GHC checks both branches at that type and brings the selected length
into scope for the call to `listCells`. Either package can hide a
different length. The unused branch and the selected vector's tail
stay lazy.

The same call accepts `if flag then fromList left else fromList right`,
where `fromList :: [a] -> Hidden a`. Both declarations identify a dot
result. Ordinary inference checks the complete conditional once and
learns its element type from the input lists. GHC then opens that result
at the call. A formal domain that can reduce to a package, such as a type
family application, keeps ordinary forwarding. A custom `ifThenElse`
introduced by `RebindableSyntax` keeps its ordinary application rules.
The Showcase calls `filterVec` on the selected input vector. It reads
the first retained element and leaves both the other input and the
selected tail lazy.

The same check handles a case whose alternatives call `fromList`.
The declaration probe reads every result branch. When they all
identify a dot result, GHC checks the branching expression once in
inference mode. The package retains the original branch demand.
If any branch has no available dot result declaration, GHC checks
the operand against the formal domain. This keeps the expected
higher-rank field type for a case that builds a callback. A branch
local head or a case alternative with local declarations can still
need an annotation. Quick Look can record a named application's
result. Its current judgment does not infer case results.

With `ImpredicativeTypes`, `listCells (id package)` can open a dot
package found by Quick Look. The checker resumes the cached producer
once at its inferred package type. The reader's type applications and
constraints use the fresh witness inside the opening. Its result type
stays outside that witness's scope. This path handles a single ordinary
value argument and a generative reader parameter. It requires a package
type with no unresolved Quick Look variables. With ordinary inference,
when there is no cache for `id`, name the package or annotate the operand
at its package type before the call.
[Source-hidden consumer](../../../testsuite/tests/dependent/should_run/implicit-existential-source/QuickLookForward.hs).

An ignored conditional can need its expected type. For example,
`ignoreInner (if flag then id (error "left") else error "right")`
returns `True` for either flag. The consumer supplies the explicit
package type when GHC checks the branches. If a `case` first binds the
conditional to a variable and passes that variable to `ignoreInner`,
GHC reports GHC-91028. The result variable cannot take the package
polytype at that later point. Two unsigned `Pack` branches also need
their expected schema. Inference on the conditional alone gives
GHC-27432. A general rule for opening computed operands must preserve
these calls that use an expected type.
[Implicit elimination in #473](https://github.com/ghc-proposals/ghc-proposals/pull/473)
and [Quick Look review](https://github.com/ghc-proposals/ghc-proposals/pull/274#issuecomment-529816378).

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

### Ordinary instances for a stream with hidden state

An ordinary module can declare and use separate class instances for
`Stream Char` and `Stream Bool`. The state is hidden in both streams.
The different element types make the closed package types apart.
For an open element type or a stuck type family, GHC keeps the possible
overlap. An equality between packages does not determine a free element type.

An instance `Prefix (Stream a) a` can use the element type requested by
the caller. GHC checks the whole package after ordinary arguments have
fixed `a`. The argument order does not affect the match. A class with
only a `Stream a` argument still cannot infer `a` from the hidden body.

GHC also checks the Paterson conditions on an instance with a package
in its head. A repeated free variable gives GHC-22979. Two instances
that differ only in the name of the hidden witness give GHC-59692.
[Ordinary instances](../examples/erased-existentials/StreamPreview.hs)
and [source-hidden regression](../../../testsuite/tests/dependent/should_run/NativeExistentialInstances.sh).

### Inferring a higher-rank witness

A package can store `Callback t` with a hidden `t`. If the supplied
value already has type `Callback (forall a. a -> a)`,
`ImpredicativeTypes` lets the producer infer that entire witness when it
constructs the package. Consecutive witnesses are chosen in scope.
Checking the final payload can still reject an incompatible family result.
[Rank-n callback](../examples/erased-existentials/RankedPackage.hs)
and [GenImpredicative in Figure 10](https://richarde.dev/papers/2021/exists/exists.pdf#page=22).

With `ImpredicativeTypes` and shallow subsumption, a declared function
can fix a witness under a callback's `forall`. A value of type
`InputAt PolyId -> Int` can supply `exists t. InputAt t -> Int`, where
`InputAt t = forall r. (t -> r) -> t -> Maybe r` and
`PolyId = forall a. a -> a`. With the callback enabled, the result is
`7`. Otherwise, the receiver returns `Nothing` and the producer
supplies `43`.
[Showcase client](../examples/erased-existentials/RankedPackageClient.hs).

GHC checks the full payload type after it substitutes the witness.
A witness such as `forall a. a -> r` would capture the callback's local
result type, so GHC rejects it. Quick Look describes the same restriction
on substitutions beneath a `forall`. The fork applies it here to an
erased existential witness.
[Quick Look, §5.2](https://simon.peytonjones.org/assets/pdfs/impredicativity.pdf#page=12)
and [interface-only regression](../../../testsuite/tests/dependent/should_run/implicit-existential-source/Opening.hs).

An ordinary module can also write `\receive -> produce receive` at this
package type. The closed declared arrow of `produce` fixes the witness.
The checker matches the complete arrow before it checks the lambda.
A local binding of `produce` can supply the same declaration. The
inference rule covers one unguarded forwarding parameter.

An ordinary client with `ImpredicativeTypes` can name a witness in each
branch. In an expected `exists t. Callback t`, `Callback @PolyId` and
`Callback @PolyMaybe` fix the types of their callback arguments.
Each branch introduces its own package. The example observes both
results and leaves an exceptional input unused.
[Ordinary client](../examples/erased-existentials/RankedPackageClient.hs).

A constructor with two parameters can fix both dot witnesses. For
`exists l. exists r. Callbacks l r`, define
`data Callbacks l r = Callbacks (Callback l) (Callback r)`.
Then `Callbacks @PolyId @PolyMaybe left right` checks each field at
its selected witness.

An expected dot package can also hide the callback type in a lambda.
For an expected `exists t. Bool -> Callback t`, the lambda
`\b -> prepare b` takes its witness from a declared result of
`prepare :: Bool -> Callback PolyId`. GHC matches the invariant result
types before it checks the lambda patterns. The usual lambda check then
checks the operands and their usage. A pattern-local type cannot supply
the outer witness. A guarded or multi-equation lambda uses the first
matching result declaration in source order. An ordinary `if` in a lambda result can
also supply a declared witness from either arm. The caller can supply
the expected package type. Every guard and result checks against the
same witness. That check includes unreachable arms. Runtime evaluation
still uses only the selected arm. For example, a declared consumer that
takes a polymorphic callback and returns `Payload PolyId` can fix the
witness for a different branch that constructs `Payload id atTwoTypes`.
A branch with an incompatible result still reports a type error when
its guard is unreachable.
The ordinary Showcase client supplies its conditional producer directly
to the caller that expects the package. The first route leaves its
second input unevaluated.
[Conditional producer](../examples/erased-existentials/RankedPackageClient.hs).
A `case` result can read the witness from a declaration in the outer
scope. The probe skips alternatives with local bindings. Case patterns
can refine how an arm checks. The witness itself is chosen outside
those patterns, so a pattern-bound type cannot become the outer
package's witness. A multi-way `if` can also read a declaration from a
result arm. A pattern guard can refine its own arm after witness selection.
Leading visible type applications can choose input types when the
declared result type is independent of the supplied type arguments.
The ordinary checker checks those applications at their source spans.
For `\b -> id (prepare b)`, Quick Look finds the result
`Callback PolyId` from the argument of `id`. The enclosing package
uses `PolyId` as its witness. The normal lambda check then checks
the application and its operands at their original locations.
[Argument-driven witness](../examples/erased-existentials/RankedPackageClient.hs).
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

The expected package can also tell a boxed tuple to open a field.
For `exists n. (Vec n a, Vec n a -> r)`, an ordinary client writes
`(filterVec predicate xs, firstCell)`. The tuple stores the filtered
vector and checks its reader at that vector's hidden length.
The expected type supplies the tuple shape and the reader can ignore
the vector. Two separate filters cannot supply two fields that require
the same length. Each filter has its own rigid witness.
[Ordinary package and reader](../examples/erased-existentials/RankedPackageClient.hs)
and [source-hidden tuple client](../../../testsuite/tests/dependent/should_run/implicit-existential-source/CaseMain.hs).

The result type can also supply the fields of a plain user product.
For `Product a b` with fields `a` and `b`, both its constructor and the
ordinary function `makeProduct :: a -> b -> Product a b` use the
expected field types in a fully applied call. A fully applied section
can expose the same declaration. The checker still opens each filter
separately. A tuple used inside a wrapper with a plain pair field can
get its own package expectation from a written expression signature.
[Source-hidden product client](../../../testsuite/tests/dependent/should_run/implicit-existential-source/CaseMain.hs).
