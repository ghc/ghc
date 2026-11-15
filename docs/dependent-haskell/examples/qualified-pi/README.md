# Qualified retained functions

This example transports a retained function through an ordinary nominal
equality. Its result has the runtime length supplied by the caller:

```haskell
type QPi = foreach (n :: Nat) -> Eq Int => Vec Int n

exposeQ :: forall value. value ~ QPi => value -> QPi
exposeQ f = f
```

The producer enables `ImpredicativeTypes` to admit the equality operand.
The consumer uses `NoImpredicativeTypes`. Its complete expected arrow
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
`PackedVecLibrary`, then the value provider, producer, identity client and
executable.
It copies interfaces between compilation directories and enables Core and
STG Lint. It runs at `-O0`.

The executable checks 57 observations. Finite lengths come through
independent `readIORef` calls. Other checks cover ignored and demanded bottom,
cyclic indices, partial application, payload sharing and six function
`StableName` comparisons. Six dictionary checks use the single-method
`Demand` class with `GHC.Exts.withDict`. They distinguish vector shape from
method demand. They do not inject a bottom global `Eq Int` dictionary.

Both compiler stages pass all 57 observations. The identity results apply to
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

This argument route uses `NoQL`. Qualified Pi type arguments still need
`ImpredicativeTypes`, whose applied native Quick Look path reports GHC-83605.
General native Quick Look and mutable native-meta assignment remain separate
work.

See the [typing rules](../../CORE.md#qualified-retained-function-types),
[source provenance](../../REFERENCES.md#qualified-retained-function-types)
and [paper reading ledger](../../papers/README.md).
