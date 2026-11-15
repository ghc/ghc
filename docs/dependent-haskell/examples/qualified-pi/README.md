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

Run from the repository root with this fork's built compiler:

```sh
bash docs/dependent-haskell/examples/qualified-pi/run-example.sh "$PWD"
```

Set `QUALIFIED_PI_GHC` to choose the compiler. Set `QUALIFIED_PI_WORK` to
choose a separate output directory. The script compiles the shared
`PackedVecLibrary`, then the value provider, producer, ground application
client, known-argument client, identity client and
executable.
It copies interfaces between compilation directories and enables Core and
STG Lint. It runs at `-O0`.

The executable checks 94 observations. Finite lengths come through
independent `readIORef` calls. Other checks cover ignored and demanded bottom,
cyclic indices, partial application, payload sharing and function
`StableName` comparisons. Six dictionary checks use the single-method
`Demand` class with `GHC.Exts.withDict`. They distinguish vector shape from
method demand. They do not inject a bottom global `Eq Int` dictionary.

Both compiler stages pass all 94 observations. The identity results apply to
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

See the [typing rules](../../CORE.md#qualified-retained-function-types),
[source provenance](../../REFERENCES.md#qualified-retained-function-types)
and [paper reading ledger](../../papers/README.md).
