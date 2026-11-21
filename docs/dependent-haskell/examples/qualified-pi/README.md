# Qualified retained functions

This example transports a retained function through an ordinary nominal
equality. Its result has the runtime length supplied by the caller:

```haskell
type QPi = foreach (n :: Nat) -> Eq Int => Vec Int n

exposeQ :: forall value. value ~ QPi => value -> QPi
exposeQ f = f
```

The producer enables `ImpredicativeTypes` to admit the equality operand.
The main consumer uses `NoImpredicativeTypes`. Its complete expected arrow
specializes the helper without a written type argument:

```haskell
specialize :: QPi -> QPi
specialize = exposeQ
```

`Eq Int` uses an ordinary GHC dictionary. The retained `n` is a runtime
argument. The compiler keeps their visibility, scope and evidence separate.
No singleton conversion is required.

`ParameterizedIndexLibrary` also provides safe indexing for a retained Boolean
list. Its ordinary `Fin` GADT records a position below a promoted Nat bound:

```haskell
indexClosed :: foreach (xs :: [Bool]) -> Fin (Count xs) -> Bool
mkFinClosed :: foreach (xs :: [Bool]) -> Int -> Maybe (Fin (Count xs))
lookupRuntime :: [Bool] -> Int -> Maybe Bool
```

`lookupRuntime [True, False] 1` returns `Just False`. Negative and out-of-range
requests return `Nothing`. The negative branch does not inspect the list.
Construction and selection inspect only the required prefix. Finite access to
a cyclic list can therefore terminate. `Count` remains a saturated type family.
A finite-position witness proves a bound, not list identity. Equal-count evidence
permits reuse. Static checking verifies construction and use of the witness;
the signed runtime input is checked when `mkFinClosed` runs.

The maintained [Showcase](../../Showcase.hs) includes five labeled observations.
Both stages also pass a separate nineteen-observation client and five rejection
controls through copied interfaces. Generic `[a]`, ordinary `length` promotion,
totality and bounds-check elimination remain outside this example.

Run from the repository root with this fork's built compiler:

```sh
bash docs/dependent-haskell/examples/qualified-pi/run-example.sh "$PWD"
```

Set `QUALIFIED_PI_GHC` to choose the compiler. Set `QUALIFIED_PI_WORK` to
choose a separate output directory. The script compiles the shared
`PackedVecLibrary`, then the value provider, producer, ground application
client, known-argument client, identity client, isolated `IndexBetaClient` and
executable.
It copies interfaces between compilation directories and enables Core and
STG Lint. It runs at `-O0`.

The executable checks 106 observations. Finite lengths come through
independent `readIORef` calls. Other checks cover ignored and demanded bottom,
cyclic indices, partial application, payload sharing and function
`StableName` comparisons. Six dictionary checks use the single-method
`Demand` class with `GHC.Exts.withDict`. They distinguish vector shape from
method demand. They do not inject a bottom global `Eq Int` dictionary.

Both compiler stages pass all 106 observations. The identity results apply to
these programs and build settings.
The full optimizer and GHC test suite remain outside this validation.

The complete expected arrow now admits a separate `Refl` caller:

```haskell
viaWitness :: QPi -> QPi
viaWitness = withQ Refl
```

The proof-demand checks distinguish partial application from the returned
function's WHNF. An ignored bottom proof stays lazy. `withQ` demands its proof
before a bottom function or index. Finite runtime indices also exercise the
original tuple-under-Pi result. Tuple and newtype transport preserve the tested
function identity. Named and lambda patterns distinguish lazy newtypes from
strict data constructors. A data pattern can demand its constructor without
demanding the function field. A leading `Refl` pattern also supplies nominal
evidence for transport beneath Pi. Its checks cover vector contents, function
identity, lazy partial application and proof demand at the returned function.

The value provider uses `NoImpredicativeTypes`. The producer imports its
complete `fixedQ` type for the pattern checks. This avoids an applied native
Quick Look inference path. No case or eta expansion replaces the newtype
patterns.

Full applications now select the erased parameter from the known argument:

```haskell
viaFull :: QPi -> QPi
viaFull f = withQ Refl f
```

The result alone does not determine `value`. The compiler infers `f` once,
selects `value := QPi`, then checks the proof. The added runtime checks call
`withQ Refl original n` and `exposeQ original n` directly at four IORef-fed
lengths. Ignoring the result leaves both a bottom proof and function unforced.
Demanding the result checks the proof before the function. The ignored-proof
worker also preserves the tested function identity.

This argument route uses `NoQL`. Qualified Pi type arguments need
`ImpredicativeTypes`. The separate `GroundApplicationClient` enables it and
checks direct applications whose erased choices are fixed:

```haskell
qRefl :: QPi :~: QPi
qRefl = Refl

groundDirect :: foreach (n :: Nat) -> QPi -> Vec Int n
groundDirect n f = withQ @QPi Refl f n
```

The main consumer reads `n` from an `IORef` and calls this worker. The added
19 observations check named and direct witnesses, an intervening `Eq`
dictionary, inferred kind arguments, phantom native type arguments, proof
demand and two more function identity comparisons. The first 57 observations
are unchanged.

The compiler checks the leading type arguments once. It uses ordinary
checking when no erased choice or free Quick Look variable remains. A
complete expected proof type can also supply immutable choices for `Refl` or
`error`. Final argument checking and nominal entailment still run. General
native Quick Look and mutable native-meta assignment remain separate work.

`KnownArgumentClient` also enables `ImpredicativeTypes`. It omits the explicit
type choice when a declared argument supplies it:

```haskell
knownDirect :: foreach (n :: Nat) -> QPi -> Vec Int n
knownDirect n f = withQ Refl f n
```

The compiler reads the declared monomorphic type of `f` without recording an
occurrence use. It selects `value := QPi` before checking `Refl`. Every argument
then goes through ordinary checking, with the original context/evidence order.
The same route handles named witnesses, generic `const`, and leading visible
type arguments with remaining choices. It does not infer arbitrary applied or
signed expressions during this selection.

The 18 new observations preserve the previous 76-line prefix. They check
IORef-fed lengths, an ignored full worker expression, proof and tag demand,
two more function identity comparisons, partial indices, and a bounded prefix
from a cyclic index. Both stages pass these checks at `-O0` with Core/STG Lint.

`IndexBetaClient` adds 12 observations after the unchanged 94-line prefix.
It passes immediate source lambda applications as retained arguments:

```haskell
betaIdentity :: foreach (n :: Nat) -> Int -> Vec Int n
betaIdentity n x = replicateNative @Int ((\k -> k) n) x

betaSuccessor :: foreach (n :: Nat) -> Int -> Vec Int (S n)
betaSuccessor n x = replicateNative @Int ((\k -> S k) n) x

betaConstant :: Nat -> Int -> Vec Int Z
betaConstant n x = replicateNative @Int ((\k -> Z) n) x
```

The compiler checks the original argument once. The lambda must have one bare
variable pattern, one unguarded body and no local bindings. Its argument must
normalize to an eligible local variable or nullary constructor. Nested
applications and shadowing preserve binder identity. `Strict` rejects this
form. Expression signatures, opaque producers, non-atomic arguments and general
evaluation remain gated. An unsigned captured local alias retains its existing flexible-
multiplicity restriction; a complete local `Nat` signature supplies `Many`.

Independent IORef inputs 0, 2 and 3 check identity, successor, constant zero,
nested beta and shadowing. Constant zero ignores a bottom index and payload.
Identity demands the index when vector construction needs it. Successor leaves
its predecessor lazy until the tail is selected. A cyclic index supplies a
bounded prefix. Evaluating the partial worker to WHNF raises its original
bottom-function marker. Separate checks demand a payload and leave unused
package fields lazy. Both stages pass all 106 observations.
The separate metadata probe also checks six actual lambda Ids in each stage
before desugaring. Each has `Nat` domain, `Many` multiplicity and its renamed
source Name/Unique.
These checks add no singleton argument, Core form or equality rule.

See the [typing rules](../../CORE.md#qualified-retained-function-types),
[source provenance](../../REFERENCES.md#qualified-retained-function-types)
and [paper reading ledger](../../papers/README.md).


## Function-valued retained package fields

[FunctionPackageLibrary.hs](FunctionPackageLibrary.hs) stores `f :: Nat -> Nat`
and `n :: Nat` as lazy fields beside `Vec a (f n)`. The erased parameter `a`
remains ordinary. The client checks24 finite observations through separately
compiled producer/consumer objects in both compiler stages. It checks generic
Char payloads, exact call scope, repacking, ignored bottom fields and distinct
demanded errors.

The maintained [Showcase.hs](../../Showcase.hs) imports this library and shows
three function-field observations in its `functionFields` group. Use the
[showcase recipe](../../README.md#run-the-showcase) to build the combined
program. M52 adds complete ordinary exported function-valued Pi signatures as
described below. Native coercion encoding remains gated. See the
[contract](../../CORE.md#function-valued-retained-package-fields).


## Exported retained function signatures

[FunctionSignatureLibrary.hs](FunctionSignatureLibrary.hs) exports:

```haskell
idCall :: forall a. foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec a (f n) -> Vec a (f n)
idCall f n xs = xs
```

M62 also exports `selectCall`, `forceExplicitCall` and `forceNestedCall`.
They use complete retained-function types as explicit arguments to `id`,
`seq` and a lifted consumer of `Maybe`. Complete types can contain supported
function binders. The lazy container keeps its worker field unevaluated.
The maintained Showcase adds four observations for these operations.

The separate [FunctionSignatureClient.hs](FunctionSignatureClient.hs) consumes
the actual function package and imported worker type:

```haskell
payloadThrough :: forall a. FunctionPack a -> [a]
payloadThrough (FunctionPack f n xs) = toList (idCall @a f n xs)
```

The call keeps the actual stored `f`, `n` and `xs`. The callee is exported;
its retained function operand remains an eligible local variable of the exact
closed Many domain. The compiler does not infer input equality from equal
function results.

Both compiler stages pass19 observations through separately compiled objects.
Independent runtime inputs0/2/3 test identity, successor and constant heads.
Generic Char payloads and an evaluated vector constructor's `StableName` check
contents and sharing. Partial applications ignore a bottom function and input.
An ignored result leaves a bottom payload lazy; observing it raises the
original payload error. A constant head ignores bottom and cyclic inputs.
A successor exposes the head before demanding a partial predecessor.

The maintained showcase imports the library and adds two labeled observations
in `functionFields`. It passes53 lines from thirteen actual source modules in
both stages. Its old51 observations remain in order. The separate qualified
program and its runner retain106 observations; this addition does not change
that runner's output.

Complete ordinary named signatures supply the new source permission. Class,
instance, foreign, expression, partial and hs-boot signatures and general type
arguments retain their restrictions. No singleton, new interface tag or wire
version is added. Native coercion encoding, unfoldings and general optimization
remain gated. See the
[contract](../../CORE.md#exported-function-valued-retained-value-signatures) and
[checkpoint](../../CHECKPOINTS.md#m52-exported-function-valued-retained-value-signatures).

## Closed parameterized runtime indices

[ParameterizedIndexLibrary.hs](ParameterizedIndexLibrary.hs) uses ordinary
`[Bool]`, `Maybe Nat` and `Tree Bool` domains in retained binders. Its explicit
`Count`, `OptionalPayload` and `TreePayload` families compute result types from
promoted constructors. For example:

```haskell
listVector :: foreach (bs :: [Bool]) -> Vec Int (Count bs)
treePayload :: foreach (tree :: Tree Bool) -> TreePayload tree
```

[ParameterizedIndexClient.hs](ParameterizedIndexClient.hs) reads runtime values
from `IORef`s. It checks vector construction, packaging, optional payloads and a
nested tree payload with both `Int` and `Bool` leaves. It also checks ignored
bottoms, a lazy list head, a lazy list tail and a lazy tree subtree.

Both stages pass 14 authored observations. The producer and client compile in
separate one-shot steps with Core Lint and STG Lint. The client has only copied
interfaces and object files for the producers. Producer source is hidden during
that step. Three separate invalid-domain controls reject growing recursion,
GADT fields and actual function fields.

The maintained [Showcase](../../Showcase.hs) adds six labeled observations from
this library. It now emits 59 lines from 14 source modules in both stages. The
previous 53 observations stay in order. Earlier counts above describe their
original checkpoints.

This example requires the fork's rebuilt compiler and matching libraries. From
the repository root, run:

```sh
mkdir -p _build/parameterized-example
_build/stage1/bin/ghc --make -O0 -fomit-interface-pragmas \
  -fno-write-if-simplified-core -dcore-lint -dstg-lint \
  -idocs/dependent-haskell/examples/qualified-pi \
  -idocs/dependent-haskell/examples/packed-vector \
  -outputdir _build/parameterized-example \
  docs/dependent-haskell/examples/qualified-pi/ParameterizedIndexClient.hs \
  -o _build/parameterized-example/parameterized
_build/parameterized-example/parameterized
```

Only closed monotype parameters of kind `Type` qualify. Constructor parameters
are erased. Stored fields retain their ordinary runtime representation and lazy
demand. The family definitions remain explicit. General term evaluation in
types and full dependent-Haskell optimization remain open.


## Nominal source domain conversion

`ParameterizedIndexLibrary` now defines a phantom `DomainToken` parameter and
nominally equal `SourceToken` and `TargetToken` aliases. One alias contains
`NormalizeTokenTag Bool`. The other contains `Bool`.
`adaptTokenVector` converts the retained function domain. Its definition returns
the original function value with a cast. `adaptTokenVectorBack` converts in the
opposite direction. `applyTokenVector` checks an actual converted argument.
`specializedTokenVector` infers its ordinary numeric payload inside body comparison.

The maintained `Showcase.hs` imports these definitions through the library's
interface. Its nine new observations check runtime vectors, ordinary inference,
ignored and demanded indices, function WHNF and function-head demand order.
The complete Showcase has 68 observations and the same 14 source modules.
Run it with the matching compiler, `-O0`, `-fomit-interface-pragmas`, Core Lint and
STG Lint as shown in the project README. Native coercion unfolding serialization
and general optimization remain gated.


## Generic safe list positions

The three workers in `ParameterizedIndexLibrary` reuse the existing `Fin`
datatype. Their bound comes from the ordinary function `sourceLength` in
`PackedVecLibrary`.

```haskell
indexGeneric :: forall (a :: Type). foreach (xs :: [a]) -> Fin (sourceLength xs) -> a
mkFinGeneric :: forall (a :: Type). foreach (xs :: [a]) -> Int -> Maybe (Fin (sourceLength xs))
lookupGeneric :: forall (a :: Type). [a] -> Int -> Maybe a
```

mkFinGeneric checks signed runtime positions. It rejects a negative position
before inspecting the list. lookupGeneric constructs and consumes the witness
with the same actual list. The workers inspect only the required list prefix.
They preserve lazy unselected heads and tails and permit finite access to a
cyclic list. A selected head is demanded only when the caller demands it.
EmptyCase still forces an impossible witness. Fin records a bound, not list
identity. Equal-bound reuse does not establish equality of list values.
GenericCount now forwards to sourceLength. Existing clients can still apply
the family at its original arity. These functions add no totality or bounds-check-elimination result.

Elements can be ordinary values, functions, IO actions or Vec Int n at an earlier
actual runtime n. Returning a function does not grant native call permission or
function equality. The original PackedVec callback passes its literal [vector]
directly to indexGeneric. Declaration advice preserves that actual element type.
Both stages check it through producer interfaces.

Ordinary clients can use the known retained-list contracts without enabling
dependent source syntax. For a vector already bound at its actual runtime n,
the maintained source checks:

```haskell
let selectedVector = case mkFinGeneric [vector] 0 of
        Nothing -> Nothing
        Just checked -> Just (toList (indexGeneric [vector] checked))
```

The Showcase also calls lookupGeneric through fmap. With M70,
`fmap toList (lookupGeneric [vector] 0)` uses the declared vector type to
infer the lookup result before specializing toList. Both source modes pass.
They also pass with an unused bottom element before a parenthesized vector
field. The standalone native let-list inference control in NoEDT still
rejects. The source checks also reject incompatible element types, mismatched
runtime indices and a linear field used in an unrestricted list.

The maintained Showcase appends six observations, reaching 124 output lines
through seventeen source modules in both stages. The API fixture's stdout stays
1/37. See [M69's validation record](../../PLAN.md#m69-generic-safe-list-indexing)
for the separate source, runtime and diagnostic evidence. R03, R05 and R23 stay
open. The final exact-HEAD main rebuild and publication are separate gates.
