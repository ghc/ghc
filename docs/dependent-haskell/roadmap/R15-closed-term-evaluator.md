# R15. Closed term evaluator

A closed term contains no free variables. Serokell's
example uses the value of `length [1,2,3]` to check a three-element vector.
The card proposes executing the function as bytecode or native code during
compilation. It is a subtask of R13, function promotion. It has no outgoing
dependency on the upstream graph. [Serokell R15](https://ghc.serokell.io/dh#closed-term-evaluator)

## First source example

```haskell
v :: Vec (Length.length '[1, 2, 3]) Bool
v = True :. False :. True :. VNil
```

The producer defines ordinary `length :: [Natural] -> Natural` using
`foldr`. The consumer uses a qualified name, a promoted list and GHC's
built-in `Nat`. This fixes both the input representation and the result
kind. Prelude's `length` returns `Int`. The dependent extension is off in
the producer. The consumer has access to its interface and object file.
[Producer](../examples/closed-evaluation/ClosedLengthProducer.hs)
and [consumer](../examples/closed-evaluation/Client.hs).

| Check | Observed on the fork |
| --- | --- |
| Execute the typed call | The GHC API returns `3` with each interpreter (internal and external) and each producer format (bytecode and object). The probe checks the actual interpreter and the producer dependency reported by the compiler. |
| Observe a change invisible to the producer ABI | Changing the fold seed from zero to one changes the object bytes and preserves the ABI hash at `-O0`. The same probe executable returns `4` with both interpreters in fresh sessions. The probe supplies producer source for target discovery. |
| Typecheck the known result | With `Vec 3 Bool`, the client builds, prints `[True,False,True]` and rejects a two-cell body. |
| Typecheck the computed result | The source declaration builds and prints `[True,False,True]`. GHC rejects a two-cell body at `v`. The producer is imported only by the type signature. |
| Reuse or recheck the client | With an unchanged producer, GHC skips the client. Its interface records the producer object fingerprint. Changing only the fold seed preserves the producer ABI and makes the unchanged client fail with expected `Vec 4 Bool` and actual `Vec 3 Bool`. |

Run the two scripts with absolute paths to a built compiler and its
matching library directory. The interpreter probe requires the matching
`ghc` package and both interpreters. A compiler built without an internal
interpreter can run the source check with `-fexternal-interpreter` as a
third argument.

```sh
dir=docs/dependent-haskell/examples/closed-evaluation
"$dir/check-interpreter.sh" "$PWD/_build/stage1/bin/ghc" "$PWD/_build/stage1/lib"
"$dir/check-source.sh" "$PWD/_build/stage1/bin/ghc" "$PWD/_build/stage1/lib"
```

## Compiler work

- [x] Preserve the qualified imported term at a type-application head.
      Preserve ordinary type-name precedence, import usage, and the
      existing path for checked source equations. Pass the resolved
      name and source span from `lookupTypeAppHeadRn` to `tc_app_ty`.
- [x] Check the actual imported function type and its closed literal
      argument before execution. Construct the typed application from the
      resolved identifier. Execute it once for that elaboration, decode
      the `Natural` result and emit an ordinary `mkNumLitTy` before
      producing Core. A printed value supplies no coercion between
      existing Core types. [Renamer](../../../compiler/GHC/Rename/Env.hs)
      and [type elaboration](../../../compiler/GHC/Tc/Gen/HsType.hs).
- [x] Check the execution boundary. Safe Haskell rejects the call.
      Ordinary inferred modules that execute it lose safe-inferred status.
      Open lists, SOURCE-only imports and explicit-level imports are rejected.
      A missing object and a synchronous exception report GHC-70565 at
      the application. The configured interpreter must support the compiled
      object. Both native interpreter modes respond to a process-group
      interrupt during evaluation of a diverging producer.
- [x] Record the actual linked dependencies in the consumer interface.
      Compile an unchanged client again after rebuilding only the producer.
      Inspect the recorded object fingerprint and first verify that an
      unchanged build skips the client. Its old three-cell definition
      must then fail at the binding. An ABI check alone would miss this
      change. [Splice dependency recording](../../../compiler/GHC/Tc/Gen/Splice.hs)
      and [interface usage calculation](../../../compiler/GHC/Iface/Make.hs).

The compiler checks the imported function and the literal list before
execution. It can still reject the surrounding type or declaration after
the call has run.

The separate-client check passed with the bootstrap-built compiler using
external iserv, and with the self-hosted compiler using each interpreter.
The latter loads a dynamic object in its internal interpreter on this
Linux build. The check replaces only the object named in the client's
interface, preserving the original interfaces and other object variant.
Further controls used an empty list and a literal larger than a machine
word. The producer returned `70` and `18446744073709551617` respectively.
The [Showcase](../Showcase.hs) reflects a computed index with `natVal`.

## Closed character calls

Proposal #509 uses ordinary functions from an installed library in proofs.
The type checker can run `Data.Char.toUpper 'x'` to obtain `'X'` and
`Data.Char.isDigit '7'` to obtain `'True`. A signature that claims
`isDigit '7' :~: 'False` fails at `Refl`. The client imports `Data.Char`
only for its types. [Proposal #509](https://github.com/ghc-proposals/ghc-proposals/pull/509)
and [Showcase](../Showcase.hs).

M101 selects the input and result representations independently. It checks
the resolved function's ordinary unary arrow and the input literal before
execution. A `Char` or list of `Natural` values can produce a `Natural`, a
`Char`, or a `Bool`. A user-defined `Char -> Natural` function can therefore
compute a numeric index. GHC reuses its character literals and DataKinds'
promoted Boolean constructors for the result. A call with an unsupported
result such as `Data.Char.ord :: Char -> Int` reports the actual return
type at the function name.

The client interface lists the direct `base` dependency on this Linux
build. It fingerprints the `ghc-internal` Haskell archive with the
external interpreter and the shared library with the internal interpreter.
For a home module, changing just the recorded `.o` or `.dyn_o` invalidates
the client while the installed interfaces stay byte-identical.
[Character witnesses](../../../testsuite/tests/dependent/should_run/NativeClosedChar.hs)
and [link dependency recording](../../../compiler/GHC/HsToCore/Usage.hs).

## Overloaded library functions

```haskell
total :: Prelude.sum '[2, 3, 5] :~: 10
total = Refl
```

The ordinary type of `sum` has domain `t a`. Matching it against
`[Natural]` determines both type arguments. The type checker then checks
the imported occurrence against `[Natural] -> Natural` as a term. It
selects the `Foldable []` and `Num Natural` instances. The same source
test calls a producer that doubles the total, a `Foldable` function that
returns `Bool`, and a polymorphic identity on a character. A missing or
ambiguous dictionary prevents execution even when the producer would
ignore it. An instance from the module under compilation fails the
ordinary stage check. [Source witnesses](../../../testsuite/tests/dependent/should_run/NativeClosedSum.hs).

The specialization records the imported names that supply its evidence.
This matters when the simplifier replaces a class method call with its
implementation. The instance object can disappear from the evaluated
code. The unchanged client must still recheck its proof when the instance
selects another method implementation. If the bytecode retains a
dictionary reference, the client also records the object that the
interpreter loaded.
[Instance recompilation test](../../../testsuite/tests/dependent/should_run/NativeClosedSumDependencies.sh).

## List results

```haskell
reversed :: Prelude.reverse '[2, 3, 5] :~: '[5, 3, 2]
reversed = Refl
```

The imported definition determines the order. The function's checked result
type supplies the element kind, including for an empty list. R15 can return
a list of `Natural`, `Char` or `Bool`. A function that throws an exception
in a later element fails at the source call. The interpreter must finish
reading the result before GHC accepts the proof.

For a list, the interpreter uses the built-in element dictionary's
`showList` method. A module may define its own `Show [Natural]` instance
that prints every list as `[2,3,5]`. That instance still works for
ordinary printing. The compiler reads the actual reversed order.
[Source checks](../../../testsuite/tests/dependent/should_run/NativeClosedLists.hs)
and [representation references](../REFERENCES.md#reifying-closed-list-results).

## Nested closed calls

```haskell
total :: Prelude.sum (Prelude.reverse '[2, 3, 5]) :~: 10
total = Refl
```

The inner result supplies the input kind for `sum`. In another check, the
outer function reads the first element and its result must be `5`.
This distinguishes a genuinely reversed argument from the original list.
The compiler checks staging before entering a nested argument. A failure
while forcing an inner result reports the inner function's source span.
Draft proposal #509 specifies this argument-first order for closed calls.
[Proposal #509](https://github.com/ghc-proposals/ghc-proposals/pull/509)
and [source tests](../../../testsuite/tests/dependent/should_run/NativeClosedLists.hs).

The client records the objects for both functions. With their interfaces
held fixed, changing either implementation can invalidate the client's
type. [Recompilation test](../../../testsuite/tests/dependent/should_run/NativeClosedNestedDependencies.sh).

Same-module definitions require further work on R21. A nested list result, a `Maybe` result and a list of a newtype over
`Natural` are outside the supported result shapes. General closed expressions
and user-defined representations remain open. Inputs with unknowns belong to [R16](R16-open-term-evaluator.md).
The [implementation references](../REFERENCES.md#executing-closed-calls-and-reducing-open-calls)
give the earlier prototype and the current GHC entry points.
