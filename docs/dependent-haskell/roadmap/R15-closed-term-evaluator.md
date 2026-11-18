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
| Typecheck the computed result | Open. The renamer reports GHC-68947 on `Length.length`. No evaluation occurs. The short-vector and incremental-client checks for the computed call can run after this passes. |

Run the two scripts with absolute paths to a built compiler and its
matching library directory. The interpreter probe requires the matching
`ghc` package and both interpreters. The source check exits at the first
unmet requirement. It can serve as the acceptance test as implementation
continues.

```sh
dir=docs/dependent-haskell/examples/closed-evaluation
"$dir/check-interpreter.sh" "$PWD/_build/stage1/bin/ghc" "$PWD/_build/stage1/lib"
"$dir/check-source.sh" "$PWD/_build/stage1/bin/ghc" "$PWD/_build/stage1/lib"
```

## Compiler work

- [ ] Preserve the qualified imported term at a type-application head.
      Preserve ordinary type-name precedence, import usage, and the
      existing path for checked source equations. Pass the resolved
      name and source span from `lookupTypeAppHeadRn` to `tc_app_ty`.
- [ ] Check the actual imported function type and its closed literal
      argument before execution. Construct the typed application from the
      resolved identifier. Execute it once for that elaboration, decode
      the `Natural` result and emit an ordinary `mkNumLitTy` before
      producing Core. A printed value supplies no coercion between
      existing Core types. [Renamer](../../../compiler/GHC/Rename/Env.hs)
      and [type elaboration](../../../compiler/GHC/Tc/Gen/HsType.hs).
- [ ] Specify which compilations can run imported code, including Safe
      Haskell, interpreter availability and host/target compatibility.
      Check open inputs before executing anything. Preserve a located
      error for a missing object or an exception, and exercise cancellation
      of a diverging computation. A Haskell function type alone gives
      no totality or purity guarantee for an arbitrary producer.
- [ ] Record the actual linked dependencies in the consumer interface.
      Compile an unchanged client again after rebuilding only the producer.
      Inspect the recorded object fingerprint and first verify that an
      unchanged build skips the client. Its old three-cell definition must then fail at the binding. An ABI
      check alone would miss this change. [Splice dependency recording](../../../compiler/GHC/Tc/Gen/Splice.hs)
      and [interface usage calculation](../../../compiler/GHC/Iface/Make.hs).

Same-module definitions require further work on R21. More argument and
result domains, user-defined representations and general closed expressions
remain open. Inputs with unknowns belong to [R16](R16-open-term-evaluator.md).
The [implementation references](../REFERENCES.md#executing-closed-calls-and-reducing-open-calls)
give the earlier prototype and the current GHC entry points.
