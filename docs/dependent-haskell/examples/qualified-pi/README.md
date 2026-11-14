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
`PackedVecLibrary`, then the producer, identity client and executable.
It copies interfaces between compilation directories and enables Core and
STG Lint. It runs at `-O0`.

The executable checks 22 observations. Four finite lengths come through
independent `readIORef` calls. Other checks cover ignored and demanded bottom,
cyclic indices, partial application, payload sharing and two function
`StableName` comparisons. Six dictionary checks use the single-method
`Demand` class with `GHC.Exts.withDict`. They distinguish vector shape from
method demand. They do not inject a bottom global `Eq Int` dictionary.

Both compiler stages pass these observations. The identity results apply to
these programs and build settings. The full optimizer and GHC test suite
remain outside this validation.

`withQ` compiles and checks an incoming `:~:` witness. Separate callers that
construct `Refl` or pass a bottom witness at `QPi :~: QPi` still reject with
GHC-91028. Proof demand is therefore unverified for that caller path. The
[next plan](../../PLAN.md#m35-exact-matching-through-algebraic-containers)
tracks the required extension. A source tuple beneath a retained binder
compiles, but this executable uses an ordinary vector argument at that
position. It does not establish the tuple runtime case.

See the [typing rules](../../CORE.md#qualified-retained-function-types),
[source provenance](../../REFERENCES.md#qualified-retained-function-types)
and [paper reading ledger](../../papers/README.md).
