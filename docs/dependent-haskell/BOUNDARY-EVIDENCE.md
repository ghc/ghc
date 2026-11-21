# Evidence at the boundary with ordinary Haskell

An ordinary module can keep its existing types. A dependent client can use
a more precise fact about one of its results. There are two ways to obtain
that fact in this fork.

1. **Read an equation from an interface.** GHC checks the ordinary
   definition of `fst` once. An exporter can then mention `fst p ~ x` for
   an unknown pair. A client that supplies `'(Int, Bool)` can establish
   that the first field is `Int`. It needs the interfaces during type
   checking. An ordinary module can now also export a checked equation
   for `swapPair (x,y) = (y,x)`. A dependent module publishes `Swap p`
   for an unknown pair, and a later ordinary client supplies the
   constructor to reduce it. [Installed projection example](examples/open-evaluation/PreludePairExport.hs)
   and [ordinary producer and separate client](roadmap/R16-open-term-evaluator.md#m142-exporting-a-swap-from-an-ordinary-module).
2. **Run a computation for this obligation.** The compiler can run an
   ordinary function at a closed input. A checker can inspect the result
   and construct evidence with a type that states exactly what it found.
   The executable producer is a build dependency. The same module can
   continue to export an ordinary API to other clients.
   [Closed evaluator](roadmap/R15-closed-term-evaluator.md).

An ordinary producer can request checked equations with
`{-# OPTIONS_GHC -fwrite-source-equations #-}`. For example, the
`size` module exports its recursion on the list spine. Both the
map-length proof and the filter-length bound use these equations
from a stripped interface. The producer can compile with `-fno-code`.
GHC checks the source body before it exports an equation. Making
`size` strict in each element or recurring on the original list
prevents it from exporting this equation.
If the library changes the step to count each element twice, GHC
rechecks an unchanged proof that a singleton has size one. The error
points to `Refl` in the client. If the library withdraws the equation,
GHC reports the use of `size` in the intervening type alias.
[Ordinary size producer](examples/open-evaluation/PreludeMapLengthSize.hs)
and [proof and compiler checks](roadmap/R16-open-term-evaluator.md#prove-that-map-preserves-length).

GHC already runs Haskell during compilation. A Template Haskell splice
can run a search and emit a witness expression. GHC then typechecks that
expression at the splice site. A typechecker plugin can also participate
when GHC encounters an unsolved constraint during inference. The plugin
below uses that hook so the caller can request a witness with an ordinary
class method. Either route can use a partial search procedure. The type
of the returned witness determines which fact the caller can use.
For a question about program equivalence, specify the observations that
the witness covers. The Boolean example checks agreement on two inputs.
One more Haskell context distinguishes the same functions.
[Boolean observation](#what-does-an-observation-establish),
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

The file-backed example below uses a seed to generate field positions.
GHC checks a proposed position by typechecking the corresponding
membership path. To establish absence in a finite list, a checker must
account for every position.

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

The search stops at its first match. The signature currently requires
R15 to read back the whole finite list. A successful search alone gives
no termination guarantee for that second step.
[Compile-time search](examples/boundary-evidence/EmitMembership.hs)
and [dependent client](examples/boundary-evidence/SplicedOffer.hs).

## Checking a position from an external program

An external search can write one zero-based field position to
`offer.position` beside the checked module. The offset `1` selects
the second field. The file format is a decimal integer
without a sign or leading zeroes, with an optional final line feed.

```haskell
labelEight :: Member 8 (OrdinaryOffer.offered 7)
labelEight = $(emitFilePosition "offer.position" (OrdinaryOffer.offered 7))
```

The splice obtains the length of this finite offer. It reads at most
the decimal width of that length plus two bytes, enough to recognize a
line feed and detect trailing data. It checks the range before emitting
constructors. With `[7,8]`, offset `1` produces `There Here`.
Offset `0` produces `Here`, which GHC rejects at the binding for label
eight. A malformed or out-of-range position fails in the splice. The
producer and the external search can still be partial.

The splice registers the absolute file path with `addDependentFile`
before it reads the candidate. GHC tracks the file's contents in the
interface. The example harness checks reuse, changes only the position
file, and checks that GHC rejects the unchanged binding. The checked
fact refers to the producer's result at build time.
[Checked module](examples/boundary-evidence/FileOffer.hs),
[evidence builder](examples/boundary-evidence/EmitMembership.hs)
and [TH file dependencies](../../libraries/ghc-internal/src/GHC/Internal/TH/Monad.hs).

### Replay a sampled membership search

`SearchOffer` runs the ordinary producer at the supplied input. It samples
up to two positions with replacement and compares each label it reads
with the requested label. The harness saves the first matching position
in the candidate file. The search reports the seed, input, requested label,
offer, and observed position-label pairs. A miss reports the same
information and emits no position.

| Seed and inputs | What happened |
| --- | --- |
| Seed `4`, input `7`, label `8` | The search observes `(0,7)` then `(1,8)`. A replay gives the same report and candidate. GHC checks `There Here` at `Member 8 (OrdinaryOffer.offered 7)`, and the program selects the second field. |
| Seed `0`, input `8`, label `8` | The search observes `(0,8)` and proposes position `0`. GHC rejects `Here` for the unchanged obligation at input `7`. |
| Seed `0`, input `7`, label `8` | The search observes `(0,7)` twice and reports that it found no candidate. Position `1` still has label `8`. |

Sampling tests the reported observations. The checked witness establishes
membership for the specified producer input. A universal claim or a
claim of absence would require additional evidence.
QuickCheck uses generated inputs to test Haskell properties. Agda's
`Any` uses `here` and `there` to carry evidence for an element of a list.
[Search source](examples/boundary-evidence/SearchOffer.hs),
[QuickCheck paper](https://www.cis.upenn.edu/~bcpierce/courses/552-2008/resources/icfp-quickcheck.pdf)
and [Agda standard library, Any](https://agda.github.io/agda-stdlib/v2.3/Data.List.Relation.Unary.Any.html).

### Replay a candidate from an installed plugin

An installed typechecker plugin can search for an ordinary `Locate`
constraint in a separate frontend invocation. For
`Locate 8 (OrdinaryOffer.offered 7)`, the plugin writes position `1`.
The frontend uses `-fno-code` and emits neither a module interface nor
an object. It uses the dictionary returned by the solver to finish
typechecking the request. Only the position file passes to the native
build.

The native invocation uses the global package database and the
ordinary producer source. It uses `emitFilePosition` to build
`There Here` at the source type `Member 8 (OrdinaryOffer.offered 7)`.
This proof selects the second field in the `True` branch of the native
function `choose :: foreach (b :: Bool) -> Result b`. A separately
compiled ordinary caller reads the stripped interface and prints
`("eight",True)`. The harness checks both `-O0` and `-O2` with Core
and STG Lint. The [Showcase](Showcase.hs) replays the saved position
in the example directory. The command below runs the search as well.
[Native provider](examples/boundary-evidence/NativeOffer.hs)
and [ordinary caller](examples/boundary-evidence/UseNativeOffer.hs).

With producer input `8`, the plugin finds position `0`. Replacing only
the position file makes the native invocation recheck the same proof.
It rejects `Here` at the unchanged obligation for input `7`.
The successful proof concerns one finite list at one input.
Its validity comes from the constructors that GHC checks during replay.
It gives no guarantee about the plugin's search or any other input.

Source plugins can transform compiler state. The review of GHC proposal
107 also raised how a typechecker plugin can solve constraints on its
own authority. A GHC `TcPlugin` can supply evidence terms and run
`IO`. Even Core Lint assumes the validity of plugin-supplied coercions.
The native module gate continues to reject plugins in the invocation
that compiles retained Core. The harness checks the rejection for both
the membership solver and an installed Core pass. The example assumes
the source, checker, compiler and build environment are trusted. A
process with access to those files can still modify them.
[Source plugins proposal](https://ghc-proposals.readthedocs.io/en/latest/proposals/0107-source-plugins.html),
[proposal review](https://github.com/ghc-proposals/ghc-proposals/pull/107#issuecomment-361558362),
[typechecker plugins](../users_guide/extending_ghc.rst#typechecker-plugins)
and [Core Lint assumptions](../../compiler/GHC/Core/Lint.hs).

Run the installed package and native replay checks with a compiler and
package tool from the same Linux build.

```sh
bash docs/dependent-haskell/examples/boundary-evidence/check-frontend-candidate.sh \
  "$PWD/_build/stage1/bin/ghc" "$PWD/_build/stage1/lib" \
  "$PWD/_build/stage1/bin/ghc-pkg" /tmp/boundary-candidate
```

Use an output path that does not exist. The script keeps the candidate
files, the stripped interfaces, the native results, and the failure
diagnostics.

## What does an observation establish?

The [pair readback experiment](roadmap/R15-closed-term-evaluator.md#m161-observe-a-field-decision-from-the-loaded-executable)
checks which fields an executable observer reports at input `7`. The
number is `8` in both runs. One implementation throws when asked for
the character. The other returns `'x'`. A `Refl` proof about either
reported result applies to that observation. Establishing that a field
can never return requires evidence about the worker and its dependencies.

Running Haskell at compile time can establish equivalence for a specified
finite set of observations. In the runnable example, the ordinary
function `BoolObservation.sameResult` applies two functions to one input
and demands their Boolean results using `(==)`.

```haskell
lazyTrue _   = True
strictTrue x = x `seq` True
sameResult x = lazyTrue x == strictTrue x
```

The dependent client checks both defined inputs. Each equality below says
that one call to the named observer returned `True`.

```haskell
agreement :: (B.sameResult 'False :~: 'True, B.sameResult 'True :~: 'True)
agreement = (Refl, Refl)
```

GHC checks two actual `Refl` constructors, covering all of `{False, True}`.
A signature alone admits `undefined` even at a false lifted equality
type, and demanding that value raises an exception. The first `Refl`
here permits a client to replace `B.sameResult 'False` with `'True` in an
index.

Now supply `errorWithoutStackTrace "missing Bool"` as the argument and
demand the result. `lazyTrue` returns `True`. Evaluating `strictTrue`
raises the exception. The harness checks both outcomes on that same
input. [Ordinary observer](examples/boundary-evidence/BoolObservation.hs),
[checked equalities and distinguishing context](examples/boundary-evidence/ObserveBoth.hs)
and [Haskell 2010 on strict evaluation, §6.2](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html).

**Contextual equivalence** quantifies over every permitted program context.
For example, in a lazy calculus whose observation is convergence, substituting
either expression into any closing context must give the same answer to
“does evaluation reach a value?” One distinguishing context refutes that
claim. To justify general substitution from a finite comparison, a separate
argument must show that those observations suffice for the language and
property in question.

For a closed computation `e :: ()` in the pure fragment, the expression `seq e True` is equivalent to `True` under
this observation exactly when `e` reaches `()`. A procedure that decided
all such equivalences would decide termination.
[Schmidt-Schauß, Sabel and Machkasova, Definition 2.2](https://lmcs.episciences.org/930/pdf#page=6).

The membership examples make the same distinction. Template Haskell
emits constructor syntax that GHC checks at the splice. The trusted solver
plugin supplies a method dictionary containing a path. Both yield
`Member 8 (OrdinaryOffer.offered 7)`. The exact proposition determines
where a client can use the evidence. This experiment uses GHC’s existing
equality rules. Observational type theory addresses equality and transport
inside a type theory. [Observational type theory and partial Haskell](REFERENCES.md#observational-equality-and-partial-haskell).

Agreement on a parser’s result tag can justify substitution in a type
indexed by that tag. Other callers can also inspect the payload or force
the input.

The compiler environment can change a proposed equation too. With the
usual list fusion rules, the first element of `map not (True : xs)`
is `False`. In the tested optimized client an additional rule on
`build` changes that element to `True`. The rule fires after `map`
has rewritten to its list producer. An exported proof about the head
must therefore account for the rules visible to the client that uses
the equation. HERMIT provides an example of replaying a proof against
the Core in the current compilation. A boundary plugin could supply
such a proof for a particular rewrite.
[Source and rule firings](examples/open-evaluation/check-map-fusion.sh)
and [HERMIT reading notes](papers/README.md#checking-ghc-rewrite-equations-with-hermit-haskell-2015).

| Check | What the client can use |
| --- | --- |
| Run an ordinary producer at a closed input and reconstruct a witness. | The proposition in the type of that witness, at that input. |
| Enumerate a finite domain and construct evidence for every case. | A claim for that domain, provided the evidence covers every inhabitant specified by the claim. |
| Run QuickCheck with selected seeds and generators. | A counterexample or a report of the cases that passed. A report can guide proof search. A universal claim needs separate evidence. |
| Reflect a definition into a logic and discharge a verification condition. | The theorem justified by that logic, its translation, and its assumptions about the Haskell program. |
| Check input obtained later from IO. | A runtime branch can carry a witness for the value it actually checked. |

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
| Keep `offer.position` at `1`, then change only that file to `0`. | GHC reuses the first build. The edit makes it recheck the unchanged binding and reject `Here`. |
| Supply a negative, malformed or out-of-range position, then restore `1`. | The splice rejects each invalid file. The restored program selects the second field. |
| Compare `lazyTrue` and `strictTrue`. | Both closed proofs check. At an exceptional input, the first returns `True` and the second raises. |
| Change `strictTrue x` to return `x`. | GHC rejects the unchanged proof at `False`. |

[Verification script](examples/boundary-evidence/check.sh).

## Further work

- Extend the [seeded membership example](#replay-a-sampled-membership-search)
  to generators with larger domains. Record the seed, generated inputs,
  observer, and counterexample. Check generated paths at their witness types.
- For an external solver, specify the fragment and the certificate the
  checker can validate. If a translation or answer must be trusted,
  record that assumption. Liquid Haskell's refinement reflection and
  Thoralf's translation of type constraints provide two precedents.
- On reuse, identify the producer, implementation dependencies, checker,
  and exact inputs. R15 already tracks the executable dependencies of
  a closed call. A checker that reads other files or changes its rules
  needs its own recompilation dependencies before an exported fact can
  be reused reliably. The file-backed example tracks one candidate file
  with `addDependentFile`.

[Liquid Haskell refinement reflection](https://arxiv.org/abs/1711.03842),
[the Thoralf plugin](https://richarde.dev/papers/2018/thoralf/thoralf.pdf),
[QuickCheck](https://hackage-content.haskell.org/package/QuickCheck)
and [the QuickSpec paper and its choice of observations](papers/README.md#quickspec-tap-2010).

## An imported equality and caller rewrite rules

A module compiled with rewrite rules disabled can prove
`NegateAll ('True ': xs) :~: ('False ': NegateAll xs)`, where
`NegateAll xs` is the checked application of `Prelude.map Prelude.not`.
The provider exports a `Checked (NegateAll '[ 'True])`. It contains a
head indexed by a hidden list and an equality with the public list.
The caller matches the imported `Refl` to return the head at the public
index. Discarding the equality gives a type error at the returned head.
The caller infers the public index from the stripped interface. It can
return the head at that index or use the witness inside a `Maybe` or
list. Inference keeps the existing family application as the index.
A fresh `Refl` at the concrete equation gives GHC-83605 with rules enabled.

The caller uses a rule that replaces the builder introduced by the
ordinary runtime `map` call. The executable prints
`(False,Just False,Just False,True)`. The first three results read the
head carried by the imported witness. The final result comes from the
caller's rewritten runtime call.
[Run the example](examples/boundary-evidence/check-map-witness.sh).

An ordinary mapper with a rule that exposes its checked cons equation
can also supply a fresh equality with rules enabled. Its rule keeps the
same callback application and recursive call under a lazy cons. Rules
that force the head or drop the tail block the equality.
[Checked cons rule](roadmap/R16-open-term-evaluator.md#using-the-mappers-cons-equation-as-a-rewrite-rule).
