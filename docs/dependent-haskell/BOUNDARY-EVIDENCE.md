# Evidence at the boundary with ordinary Haskell

An ordinary module can keep its existing types. A dependent client can use
a more precise fact about one of its results. There are two ways to obtain
that fact in this fork.

1. **Read an equation from an interface.** GHC checks the ordinary
   definition of `fst` once. An exporter can then mention `fst p ~ x` for
   an unknown pair. A client that supplies `'(Int, Bool)` can establish
   that the first field is `Int`. It needs the interfaces during type
   checking. [Installed projection example](examples/open-evaluation/PreludePairExport.hs).
2. **Run a computation for this obligation.** The compiler can run an
   ordinary function at a closed input. A checker can inspect the result
   and construct evidence with a type that states exactly what it found.
   The executable producer is a build dependency. The same module can
   continue to export an ordinary API to other clients.
   [Closed evaluator](roadmap/R15-closed-term-evaluator.md).

GHC already runs Haskell during compilation. A Template Haskell splice
can run a search and emit a witness expression. GHC then typechecks that
expression at the splice site. A typechecker plugin can also participate
when GHC encounters an unsolved constraint during inference. The plugin
below uses that hook so the caller can request a witness with an ordinary
class method. Either route can use a partial search procedure. The type
of the returned witness determines which fact the caller can use.
[Template Haskell splices](../users_guide/exts/template_haskell.rst)
and [typechecker plugins](../users_guide/extending_ghc.rst#typechecker-plugins).

## A first plugin experiment

The ordinary example module exports `offered :: Natural -> [Natural]`.
For input `7`, it returns `[7,8]`. A client wants to select the field
labelled `8`. Its obligation is membership of `8` in this particular
list. The producer can use ordinary Haskell to compute the list. It
needs no dependent annotation or termination proof.

The example plugin uses GHC's existing `tcPluginSolve` hook. The fork's
closed evaluator supplies the result of the ordinary call. The plugin
searches for the requested label. A match at the head gives the
constructor `Here`. A match one position later gives `There Here`.
Those constructors have indexed types. The checked module requests the
path by calling the class method.

```haskell
labelEight :: Member 8 (OrdinaryOffer.offered 7)
labelEight = location
```

A second module uses
`fieldAt labelEight` to select the second field. It imports the witness
and the data declarations with the extension off and the plugin disabled.
An absent label leaves the constraint unsolved. A completely unknown
list does too. A known matching head gives `Here` even when the tail is
unknown. R15 forces the finite list it returns from a closed computation.
The prefix case instead starts with a type that already exposes a head.
[Ordinary producer](examples/boundary-evidence/OrdinaryOffer.hs),
[solver](examples/boundary-evidence/LocatePlugin.hs),
[checked export](examples/boundary-evidence/CheckedOffer.hs)
and [ordinary consumer](examples/boundary-evidence/Main.hs).

The evidence is a Haskell value with ordinary constructors. A compiler
plugin is trusted code and can return invalid Core. This experiment limits
its evidence builder to the membership constructors and the actual class
dictionary. Core Lint checks the resulting Core. A user-supplied Boolean
or an SMT solver's exit code cannot by itself construct a membership path.
[GHC's plugin evidence interface](../users_guide/extending_ghc.rst#typechecker-plugins)
and [Agda's decidable propositions and witnesses](https://agda.github.io/agda-stdlib/v2.3/Relation.Nullary.Decidable.Core.html).

## Running a search in a splice

The same obligation has a plugin-free spelling. The ordinary
`emitMembership` function searches the supplied list and generates an
expression built from `Here` and `There`.

```haskell
labelEight :: Member 8 (OrdinaryOffer.offered 7)
labelEight = $(emitMembership 8 (OrdinaryOffer.offered 7))
```

The two appearances of `offered 7` have different jobs. In the splice,
the producer supplies the list to search. In the signature, the closed
evaluator makes the actual list indices available to GHC. For `[7,8]`,
the splice emits `There Here`. GHC checks its type at the binding. A
generator that accidentally searches `offered 8` returns `Here`, which
puts the label in the wrong field for the declared input. GHC can detect
that mistake even though the search itself uses ordinary Haskell.

The emitted expression uses only `Here` and `There`. This restriction
matters in Haskell. A splice could emit `undefined :: Member 9 '[7,8]`.
That expression typechecks, but a caller that inspects the supposed path
raises an exception. A general-purpose checker can return an ordinary
description of a candidate path. A small evidence builder can admit only
the finite sequence of constructors, as this example does.

The example uses a pure producer. A search that reads an external file
observes that file at build time. Reuse across builds requires a declared
dependency on the file. Runtime IO can produce a different value.

The search stops at its first match. The signature currently requires
R15 to read back the whole finite list. A successful search alone gives
no termination guarantee for that second step.
[Compile-time search](examples/boundary-evidence/EmitMembership.hs)
and [dependent client](examples/boundary-evidence/SplicedOffer.hs).

## What does an observation establish?

Record the input, the observation, and the claim before choosing a checker.
For example, a program can ask whether a parser returns `Right`, what
integer it contains, or whether its result is in normal form. These
observations can demand different amounts of a lazy result.

| Check | What the client can use |
| --- | --- |
| Run an ordinary producer at a closed input and reconstruct a witness. | The proposition in the type of that witness, at that input. |
| Enumerate a finite domain and construct evidence for every case. | A claim for that domain, provided the evidence covers every inhabitant specified by the claim. |
| Run QuickCheck with selected seeds and generators. | A counterexample or a report of the cases that passed. A report can guide proof search. A universal claim needs separate evidence. |
| Reflect a definition into a logic and discharge a verification condition. | The theorem justified by that logic, its translation, and its assumptions about the Haskell program. |
| Check input obtained later from IO. | A runtime branch can carry a witness for the value it actually checked. |

Consider two functions with the same ordinary type.

```haskell
lazyTrue, strictTrue :: Bool -> Bool
lazyTrue _   = True
strictTrue x = x `seq` True
```

They agree when the observation asks for their result on `False` and
`True`. A context that asks for the result on `undefined` distinguishes
them. Calling this *agreement on the two defined Booleans* tells a
client exactly when it can use the result. Full contextual equivalence
would have to account for every observation available in the language.
Observational type theory concerns the design of equality and transport
in a type theory. The boundary experiment uses Core's existing equality
rules. The runnable example asks GHC for two separate `Refl` witnesses.
It then demands the results on an exceptional input at runtime.
[Ordinary functions](examples/boundary-evidence/BoolObservation.hs)
and [the two-input observation](examples/boundary-evidence/ObserveBoth.hs). [Observational equality and partial Haskell](REFERENCES.md#observational-equality-and-partial-haskell).

## Where totality matters

A successful closed check finishes and produces its particular evidence.
This permits useful gradual adoption before a general totality checker
exists. If the producer throws, GHC reports a failed evaluation. If it
diverges, type checking can diverge too. A future time limit could report
that the check yielded no evidence. It could not decide that the
proposition was false. An abstract proof still needs assumptions about
every input it quantifies over. [Refinement types for lazy Haskell](https://simon.peytonjones.org/assets/pdfs/refinement-types-for-haskell.pdf).

A value obtained from `readIORef` during program execution becomes
available at runtime. The existing `fromListExact n xs` adapter inspects the actual
list spine. Only its successful branch supplies a `Vec a n`. If neither
an interface equation nor a closed computation establishes a requested
fact, this explicit adapter is available at the call site.
Dagand, Tabareau and Tanter formalize this direction of conversion for
ordinary and dependent values in Coq. Their account makes the possible
failure of a conversion part of its contract. [Runtime adapter](examples/packed-vector/PackedVecLibrary.hs)
and [Foundations of dependent interoperability](https://doi.org/10.1017/S0956796818000011).

## Run the example

Run this command from the checkout root on the Linux dynamic build.

```sh
bash docs/dependent-haskell/examples/boundary-evidence/check.sh \
  "$PWD/_build/stage1/bin/ghc" "$PWD/_build/stage1/lib" -dynamic
```

The script compiles the plugin and checked module. It copies only the
witness library and checked export interfaces to a fresh directory and
checks the ordinary consumer there.
The self-hosted compiler verified these results with Core and STG lint.

| Change or input | Observed result |
| --- | --- |
| Ordinary producer returns `[7,8]`. | The plugin-free consumer selects `"eight"` with the computed path and `"seven"` with the known-head path. |
| Request `9`, request `8` in a completely unknown list, or disable the plugin at the original request. | GHC reports an unsolved `Locate` constraint at the binding. |
| Change the producer body to return `[7,9]` with the same signature. | GHC rechecks the unchanged binding for label `8` and rejects it. |
| Search `offered 7` with a splice for label `8`. | The generated path selects the second field. |
| Change only the search input to `8`. | GHC rejects the generated `Here` at the binding for input `7`. |
| Change the producer to return `[8,7]` at input `7`. | The unchanged splice recompiles and selects the first field. |
| Compare `lazyTrue` and `strictTrue`. | Both closed proofs check. At an exceptional input, the first returns `True` and the second raises. |
| Change `strictTrue x` to return `x`. | GHC rejects the unchanged proof at `False`. |

[Verification script](examples/boundary-evidence/check.sh).

## Further work

- Use the same separation for property testing. Keep the seed, generated
  inputs, observer, and counterexample in the report. Feed candidate
  witnesses back to the checker when a property has a witness type.
- For an external solver, specify the fragment and the certificate the
  checker can validate. If a translation or answer must be trusted,
  record that assumption. Liquid Haskell's refinement reflection and
  Thoralf's translation of type constraints provide two precedents.
- On reuse, identify the producer, implementation dependencies, checker,
  and exact inputs. R15 already tracks the executable dependencies of
  a closed call. A checker that reads other files or changes its rules
  needs its own recompilation dependencies before an exported fact can
  be reused reliably.

[Liquid Haskell refinement reflection](https://arxiv.org/abs/1711.03842),
[the Thoralf plugin](https://richarde.dev/papers/2018/thoralf/thoralf.pdf),
[QuickCheck](https://hackage-content.haskell.org/package/QuickCheck)
and [QuickSpec's candidate equations](https://research.chalmers.se/en/publication/525705).
