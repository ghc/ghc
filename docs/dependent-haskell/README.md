# Dependent Haskell project

The long-term goal is dependent types in Haskell. The first experiment is a
retained Boolean argument whose value determines the result type. Extend Core
from Eisenberg's historical `nokinds-pi` work, ported and completed on current
GHC. Keep the validated `singletons` encoding as a comparison implementation.
The current source subset includes runtime Peano naturals and length-indexed vectors.

Last updated: 2026-11-14.

## Project documents

- [Implementation plan](PLAN.md): Core extension, port tasks, and acceptance checks.
- [References](REFERENCES.md): annotated sources, reading order, and status dates.
- [Packed vector source example](examples/packed-vector/README.md): producer, consumer, runtime observations, and a separate-module runner.
- [Historical port](PORT.md): pinned commits, module mapping, and missing rules.
- [Restricted Core](CORE.md): representation, typing, substitution, and gates.
- This file: current status, validation results, and instructions to resume work.

## Current status

| Item | State |
| --- | --- |
| GHC source baseline | `19ff36f2aede0ca2e554016ff22c71e73aa9803b`, dated 2026-11-09 |
| Project branch | `dependent-haskell` |
| Baseline build | Passed on 2026-11-11 |
| Singleton reference translation | Validated and retained in `reference/` on 2026-11-11 |
| Native Core | Closed algebraic domains, field-scoped Nat proofs, and fixed-domain `DepFunCo` pass the bootstrap API regression |
| Execution | Nat/Vec and Boolean source programs pass checked bytecode lowering and separate-module machine-code execution |
| Program entry | Ordinary single-module executable passes the bootstrap API, self-hosted CLI, and registered regression |
| Native interfaces | Nat/Vec and Boolean signatures pass disk imports and cross-module execution; CLI reads check both format-rejection directions |
| Configured fork version | `10.1.20261113.2`, with interface header `1001202611132`; matching compiler and libraries rebuilt |
| Source syntax | Algebraic domains, outer erased `forall`, retained telescopes, checked source `case`, nested ordinary fields, and one constructor layer on retained package fields |
| Historical starting point | `nokinds-pi` at `b64ef80f8002727422c05092f429c38d0527ed0b`, dated 2014-08-19 |
| Arithmetic proofs | Runtime Nat induction returns ordinary `:~:` evidence and transports vectors; ignored proofs and partial transport stay lazy |
| Generic consumers | Ordinary `force`, `id`, `Just`, and `const` accept named retained values; linear proof use and retained-function WHNF pass |
| Class consumers | Proof/vector `Show`, constrained native results, scoped dictionaries, and equality-Given transport pass |
| Source contexts | Class and equality contexts after retained binders pass variable, constructor, and explicit case checking |
| Case evidence | Incoming dictionaries and primitive equality evidence pass through case captures; arrow evidence scope and partial-case strictness pass |
| Checked lambdas | Expected retained telescopes check anonymous and nested lambdas, constructor lambda forms, and empty-domain elimination |
| Higher-order closures | Complete signatures accept retained function arguments and results; named and checked closures pass the bootstrap runtime |
| Last verified checkpoint | Both compiler stages and all five registered regressions pass; the source example has 417 runtime observations and the runtime fixtures check 122 package/bound rejections |
| Worker instantiation | Complete expected types infer erased worker parameters; scoped matching, contexts, nested aliases, and bottom-function behavior pass API/runtime and separate CLI checks |
| Stored dependent pairs | Both compiler stages build and run the two-module packed-vector example from source; all 15 runtime observations pass with Core and STG Lint |
| Source filter | Both compiler stages pass 36 package, filter and section observations through a separate interface; incorrect result lengths and linear or rigid package patterns reject with located diagnostics |
| Bounded source filter | Both compiler stages run the three-field source package; the filter constructs a checked LE proof while unused, partial and cyclic proof fields retain their tested laziness |
| Source package patterns | Nested vector/proof patterns, wildcard proof fields and guarded clauses pass in both stages; separate cases preserve each row's runtime index Ids |
| Source vector tails | Source uncons and repeated draining use the actual runtime predecessor; partial fields stay lazy and the tested cyclic input produces a bounded prefix |
| Source equality and zip | Both compiler stages pass 136 observations; ordinary `Refl` aligns independently computed lengths; four equality-scope errors have pinned codes and source locations |
| Source append | Both stages pass 180 observations; a runtime sum and ordinary equality witness determine the result length; declaration-only imports, partial VTA and unused retained fields pass |
| Retained field patterns | Both stages pass 209 source observations, including constructor refinement through later fields, proof demand, views, guards and row order |
| Source reverse | Both compiler stages preserve the original runtime length through source reverse, with checked arithmetic proofs and 40 new demand/order observations |
| Later retained equations | Both compiler stages pass 269 observations, including mixed argument/evidence order, local captures and checked arithmetic constructor equations |
| Two-index equations | Both compiler stages pass four-clause sameNat and payload transport through two constructor proofs |
| Terminal variable equations | Both compiler stages pass three-clause sameNat, original-index repackaging and non-strict fallback of an unmatched second column |
| Local payload signatures | Complete local signatures capture eligible already-bound runtime indices; both stages pass sharing, scope, demand and ordinary-Haskell controls |
| Native IO binds | Saved and inline actions preserve native inputs through do and explicit bind; ordinary local computation results infer, with effects, demand and sharing controls |
| Scoped source inference | Local monomorphic vectors/actions and native callback results infer at existing outer runtime indices; deep metadata scope and callback escape checks apply |
| Native boxed tuples | Complete boxed tuples infer exact native field types through ordinary constructor applications; runtime syntax, constructor and helper comparisons pass |
| Native tuple sections | Inferred and expected boxed sections preserve exact native fields, hole multiplicities, field laziness and captured vector sharing |
| Next task | M28: infer native generic results before consuming extra arguments; direct `fst pair 42` rejects while a named `fst pair` result works |

The current direction is native dependent Core. The historical branch is
unfinished. It supplies binder and application design work, but dependent type
computation and Boolean elimination need completion. The native target must accept
ordinary runtime Boolean bindings directly and preserve laziness. Singleton
reification remains specific to the comparison implementation.

## Native Core implementation

`TermVarTy` refers to the index of an existing runtime identifier in a supported
closed algebraic domain, including Bool and user-defined Nat.
`DepFunTy` binds that identifier in a dependent result type. Type recovery and
Core Lint agree on retained application. The implementation includes scope,
capture avoidance, supported index substitution, alpha equality, and diagnostic
printing. It does not add a singleton argument or evaluate index variables.

Boolean alternatives can bind checked nominal equality evidence. The strict case
justifies each proof. Proof-bearing cases must cover both Boolean values, and
each proof stays inside its branch. The existing result annotation scope remains.

The `NativeDependentCore` GHC API regression checks constructor and shared-variable
applications, substitution composition, binder cloning, invalid indices, scope
errors, and boundary gates. It also checks `Select True = Int`,
`Select False = Bool`, shared-index composition, branch casts, and forged proofs.

The restricted frontend produces native Core for retained functions. The
ordinary optimizer and native unfolding serialization remain gated. Program
compilation has an explicit checked entry for native signatures and ordinary
exported types. Signature validation is recorded below.
There is no performance measurement for the changed type traversals.

## Native execution

`hscCompileCoreExpr` now has a restricted native path. It skips the ordinary
simplifier. It runs `simpleOptNativeExpr` between mandatory Core Lint checks,
then checks Core again after CorePrep. CorePrep preserves native
types, clones proof types with their Boolean indices, and supports retained eta
binders. CoreToStg removes virtual Boolean proofs and casts. STG Lint checks the
runtime argument representations before bytecode generation. The normal pipeline
keeps its rejection gates. JavaScript execution remains unsupported.

`NativeDependentCoreRun` compiles each `choose` and shared `keep`/`choose` worker
once. An ordinary helper supplies both Boolean inputs through an IORef. The
results are `42` for `True` and `False` for `False`. The shared path uses an
explicit retained eta expansion. It does not add a singleton argument.

The laziness observations have separate types:

- `keepSelect bottom` reaches function WHNF. This test does not construct a
  non-bottom value of type `Select bottom`.
- A fully applied indexed `keep` returns `42` with a bottom Boolean. Its value has
  type `(b ~ b) => Int`. Ordinary reflexivity supplies the dictionary without
  evaluating `b`.
- A lazy local index can refer to a global Boolean thunk. CorePrep must preserve
  the local binding when its RHS cannot replace an index.
- `choose bottom` raises the named Boolean-bottom exception.

Prepared Core and STG inspection confirmed one Boolean argument for `choose` and
Boolean/value arguments for `keep`. The `keep` closure returns the value without
scrutinizing the Boolean. Index-only dependencies do not add runtime captures.
The branch proofs disappear from STG, and the strict Boolean cases remain.

Compiler source commit `5a636d9274` passed the final default Hadrian rebuild on
2026-11-12. The command used `-j6 --flavour=default --docs=none`. Hadrian reported
7m31s. The memory scope recorded a peak of 4,658 MiB under a 16 GiB cap with no
swap. This was an incremental rebuild.

The registered test run selected `NativeDependentCore NativeDependentCoreRun`
with `--test-way=normal`. Both tests passed. Twenty other ways were skipped, and
there were no unexpected failures or framework warnings. The test scope peaked
at 2,476 MiB under an 8 GiB cap. Both tests also passed against the bootstrap API
library; runtime execution used the matching target libraries and an external
interpreter.

Ordinary smoke checks passed package registration, `-O2 -dcore-lint`, Template
Haskell, threaded execution, GHCi, and Haddock startup. The standalone singleton
comparison and exact pinned `Data.Singletons` source passed at `-O0` and `-O2`
with Core Lint. The expected mismatch was rejected.

Hadrian reported a nonfatal configuration comparison between `lib` and `lib/.`.
It continued with its in-tree arguments. Package registration reported missing
documentation because documentation generation was disabled.

The full testsuite and documentation build have not run. This work establishes
bytecode execution for the restricted Core examples. It does not establish
ordinary optimizer preservation or the later source implementation. That
increment kept machine-code compilation behind the program gate. All commits
are local.

## Restricted simplification

The native SimpleOpt entry substitutes supported Boolean beta arguments in
terms and types together. Exact Boolean cases replace their virtual proof with
checked nominal reflexivity. Variable cases remain strict. Unsupported Boolean
producers keep their application redex. General inlining and occurrence analysis
remain disabled. The mode creates no rules, Core unfoldings, or join points.

The Core API checks now cover constructor and variable beta reduction, capture
avoidance, literal proof elimination, opaque indices with constructor unfoldings,
and retained binding metadata. Both bootstrap API and runtime checks passed.
The runtime checks include a local bottom index used only in erased annotations
and an unsupported producer application. Both reach function WHNF. Prepared
Core and STG retain the necessary scope without a runtime use of that index in
the function body. The dump run matched all 13 expected output lines.

Compiler source commit `0bbc32d396` passed the default Hadrian rebuild with
`-j6 --flavour=default --docs=none`. Hadrian reported 21m51s. The memory scope
recorded a peak of 7,726 MiB under a 16 GiB cap with no swap. This was an
incremental rebuild of both compiler stages and their dependencies.

The registered normal-way run passed both tests. Twenty other ways were skipped.
There were no unexpected failures or framework warnings. Its peak was 2,241 MiB
under an 8 GiB cap. The bootstrap API and runtime checks peaked at 907 MiB and
918 MiB. Ordinary `-O2 -dcore-lint`, Template Haskell, threaded execution, GHCi,
package registration, and Haddock startup passed. Both singleton comparisons
passed at `-O0` and `-O2` with Core Lint, including the expected rejection.

Unsupported compiler operations now report their phase with `Sorry`. The native
JavaScript entry uses this diagnostic instead of a compiler panic. The API checks
verify the exception class and phase. No JavaScript execution was attempted.

This increment does not enable the ordinary optimizer or establish the full M6
acceptance check. The full testsuite, documentation build, and performance checks
have not run.

The first source step uses one explicit retained variable pattern. Existing
pattern checking and desugaring preserve its `Unique`; `MatchGroupTc` needs no
new field for that subset. See [the frontend plan](CORE.md#frontend-identity-plan)
and [Agda provenance](REFERENCES.md#agda-provenance).

## Experimental source syntax

The local `ExperimentalDependentTypes` flag enables `foreach` in types. The AST
uses `HsForEachTy`, separate from erased `HsForAllTy`. The flag is disabled by
default and implies no other extensions. With the flag disabled, `foreach`
remains an ordinary identifier. The initial syntax increment rejected all retained
signatures. The checking increment below accepts a restricted subset.

The printers, exact printer, signature traversals, HIE traversal, and Haddock
visitors handle the new syntax. Haddock counts retained binders as runtime
arguments. The source regressions check the diagnostic and flag-disabled
compatibility. The existing API regression also checks flag parsing, the
distinct AST node, a printing round trip, brace rejection, and visible `forall`.
The printer regression checks ASCII pretty/exact round trips and Unicode exact
printing. The suite already records a Unicode pretty-printing limitation.

Compiler source commit `03c4343351` passed the final default Hadrian rebuild
with `-j6 --flavour=default --docs=none`. Hadrian reported 8m07s. The memory scope
recorded a peak of 6,659 MiB under a 16 GiB cap with no swap. Both compiler stages
and their dependencies built. The preceding complete rebuild took 26m56s and
exposed missing AST cases. The final rebuild contains those fixes and has no
new exhaustiveness warnings.

All five registered normal-way regressions passed: `NativeDependentCore`,
`NativeDependentCoreRun`, `RetainedKeywordOff`, `RetainedQuantification`, and
`RetainedPrint`. Four passed in the first run. The diagnostic passed after its
golden file matched the harness's no-caret format. The first run skipped 34
other ways. The scopes peaked at 2,320 MiB and 856 MiB under 8 GiB caps. Neither
run had framework failures or warnings. The runtime regression retained its 13
expected output lines, including the non-strict `keep` cases.

The bootstrap API check passed and peaked at 963 MiB under a 4 GiB cap. Separate
printer checks passed. Haddock generated HTML and source links for
`RetainedKeywordOff`; each `foreach` token kept identifier styling. That check
peaked at 181 MiB under a 4 GiB cap. Fourmolu parsing and Cabal-gild checks passed.
The compiler and Haddock files retain their existing formatting.

## Restricted source checking

The verified Boolean checkpoint accepts one explicit `foreach (b :: Bool)` binder at the start of a
complete monomorphic value signature. A dependent function has one equation with
a bare retained variable pattern, or one unguarded equation for each Boolean
constructor. Its result has lifted kind, and its outer match wrapper
must be identity. Calls accept Boolean constructors and named local Boolean Ids.
The first dependent result example is `Select b -> Select b`.

The renamer allocates one value `Name` and adds a scoped type-lookup alias. The
checker registers its fixed Bool/Many Id. For a variable equation, pattern
checking reuses the existing pattern Name. The runtime lambda and its result
type have the same Unique.
Zonking preserves the fixed index prototype. Looking up its runtime metadata
would force GHC's recursively tied Id environment. Runtime occurrences still
use ordinary zonking.

Validity preserves index scope through nested constraints and applies ordinary
rank rules. The first subset permits only reflexive Boolean equalities that
mention the index. Constraint instantiation creates their checked boxed
reflexivity evidence at the call site. This keeps the evidence inside the
runtime lambda. Other index-dependent predicates need a solver evidence scope.
Unsigned inferred aliases, Quick Look, index-bearing metavariable assignments,
and optimisation pragmas remain restricted. Dedicated rejection checks use
located GHC-83605 diagnostics. Ordinary payload mismatches use GHC-83865.

The existing runtime regression now parses, renames, checks, and desugars real
source. It retains module evidence bindings around an ordinary `entry :: IO String`
and runs that closed expression through native lowering. One source `keep`
receives both runtime Boolean values through an IORef and returns `42` for each.
The fully applied indexed call with a bottom Boolean also returns `42`.
The source family example returns `42` for `Select True` and `False` for
`Select False`. A partial `keep bottom` reaches function WHNF.

Initial source `keep` validation at `0781706b2e` passed six source observations
and twelve Core observations against the bootstrap API and rebuilt compiler.
All 27 source rejection cases checked the
diagnostic code, message, and source span. The opaque-index matching checks also
pass. The bootstrap API and runtime checks each used a 4 GiB cap and peaked at
913 MiB and 918 MiB. Core Lint and STG Lint remain mandatory in native lowering.

That default both-stage rebuild passed in 10 minutes 30 seconds with
`-j6 --flavour=default --docs=none`. It peaked at 6,055 MiB under a verified
16 GiB cap with swap disabled. All five registered normal-way regressions passed:
`NativeDependentCore`, `NativeDependentCoreRun`, `RetainedKeywordOff`,
`RetainedQuantification`, and `RetainedPrint`. The run skipped 34 other ways and
had no unexpected failures or framework warnings. It peaked at 2,403 MiB under
an 8 GiB cap. Fourmolu and Cabal-gild checks passed. Compiler files retain their
existing formatting.

Local commits separate opaque matching (`723a588ed5`), native checking support
(`eb29ee0a5d`), and source checking with regressions (`0781706b2e`). Reproduction
scripts, logs, and memory records are in the ignored `_build/verification/`
directory. The final runs used `build.sh` and `native-source-syntax-tests.sh`.

The constructor increment adds source `choose` and a constructor-based observer.
Each equation checks later argument patterns and its RHS against types with the
constructor substituted for the retained index. Explicit argument casts and
inverse result casts preserve the outer types. Generated typed `AsPat` nodes
retain one hidden runtime Id across both equations. The existing constructor
dictionary field binds each nominal proof inside its Core alternative.

Keep coercion patterns even for reflexive casts. The `Ignore b = Int` regression
checks that desugaring preserves the original indexed argument annotation.
Family normalisation keeps a runtime index opaque. The solver keeps its unknown
constructor equality potentially soluble, so PMC does not discard a live branch.
These rules add no singleton, equality axiom, or general runtime-index rewriting.

Validation at `dfe9637939` passes twelve source and twelve Core observations,
plus 35 located rejection cases. One source worker defines
`again b = keep b (choose b)` under the same unknown Boolean. Constructor
consumers return `42` and `False` for the
two inputs. A bang on the selected payload exposes `choose bottom`'s named
exception. A separate constrained-payload WHNF control leaves its Boolean
unforced. The bootstrap API and runtime checks peaked at 920 MiB and 915 MiB
under 4 GiB caps. The final registered run also verifies the named `again` worker.

The constructor increment's default both-stage rebuild passed in 14m36s with
`-j6 --flavour=default --docs=none`. Its peak was 7,007 MiB under a verified
16 GiB cap with swap disabled. All five registered normal-way regressions passed;
34 other ways were skipped. There were no unexpected failures or framework
warnings. The test scope peaked at 2,490 MiB under an 8 GiB cap with swap disabled.
Fourmolu parsing, fixture formatting, Cabal-gild, and local documentation link
checks passed. Compiler files retain their existing formatting.

Reproduction records use `native-source-choose-complete-build` and
`native-source-choose-registered` in `_build/verification/`. The registered run
uses `native-source-syntax-tests.sh` with the self-hosted stage1 compiler.

Dependent source `case` expressions remain unfinished. General runtime-index
inference, the ordinary dependent optimizer, and interfaces remain open. The full
testsuite, documentation build, and performance
checks have not run. See the
[frontend plan](CORE.md#frontend-identity-plan) and
[Agda provenance](REFERENCES.md#agda-provenance).

## Ordinary executable integration

The restricted module driver now accepts ordinary top-level types with local
retained workers. Use `-O0 -fomit-interface-pragmas -fno-write-if-simplified-core`.
It applies native SimpleOpt to each RHS and keeps top-level binding identities.
Core Lint runs around simplification, tidying, and program CorePrep. CorePrep
disables speculation and optional arity analysis, then skips its final ordinary
occurrence-analysis cleanup. CoreToStg erases the checked virtual proofs and
casts. STG Lint is mandatory before the Cmm backend.

`NativeDependentCoreRun` creates a fresh compiler context and a self-contained
`Main (main)` source file. It invokes normal module compilation and linking,
checks that an object file exists, and runs the resulting binary. Its six
observations cover both shared `choose`/`keep` results, partial and fully applied
non-strict `keep bottom`, the constrained-payload WHNF control, and strict
`choose bottom`. Eight negative cases require a phase-specific `Sorry`; a panic
or a generic failed load does not pass. These cover optimized options, interface
unfoldings, serialized Core, late cost centres, `INLINE`, `INLINABLE`, native
top-level signatures, and native-containing class-default methods. The bootstrap
API runtime scope passed and peaked at 921 MiB under a 4 GiB cap.

Compiler source commit `300eefffd1` passed the default both-stage Hadrian build
on 2026-11-12 with six workers and `--docs=none`. This incremental build took
9m41s and peaked at 5,330 MiB under a 16 GiB cap. The self-hosted stage1 CLI
then compiled, linked, and ran the same source. Its six observations passed;
the scope peaked at 115 MiB under 4 GiB. Final STG retains the Boolean argument
to `choose`, removes the virtual branch proofs and casts, and represents `keep`
as two arguments followed by the unchanged payload, without a case on the index.

All five registered normal-way tests passed: `NativeDependentCore`,
`NativeDependentCoreRun`, `RetainedKeywordOff`, `RetainedQuantification`, and
`RetainedPrint`. The run had 39 cases, with 34 skipped, no unexpected results,
and no framework warnings. It peaked at 2,517 MiB under an 8 GiB cap. Swap was
disabled for each capped run. The runtime fixture now covers 30 observations,
35 located source diagnostics, and the eight program rejection cases.

The ignored verification scripts are `build.sh`, `native-program-runtime.sh`,
`native-program-cli.sh`, and `native-source-syntax-tests.sh` under
`_build/verification/`. The executable checks use the native code generator.
The full testsuite, documentation build, and performance checks have not run.

That increment kept native top-level signatures gated. The signature increment
below removes that gate; native unfoldings remain unsupported.
Profiling, plugins, non-object module backends, and optimized module options
remain unsupported. Native-containing `INLINE`, `INLINABLE`, and class-default
unfoldings reject before desugaring can invoke ordinary optimization. Omitting
interface pragmas alone does not prevent those calls.

## Native interface signatures

The signature codec uses local interface Id binders and names, then reconstructs
fresh runtime identities on import. It has binary round-trip checks at all three
compression levels. A separate executable fixture imports a producer `.hi` in a
fresh one-shot compiler context and links its `.o`. Its consumer composes imported
`choose` and `keep` with one runtime Boolean and repeats the six program
observations above. It also checks concrete True/False applications, which need
the imported family equations. Native expression unfoldings remain gated.

The Boolean signature checkpoint pinned the fork to `10.1.20261112.1`. Its header version was
`1001202611121`, distinct from the previous `100120261109`. Matching libraries
must be rebuilt. The preserved old reader accepts its old-format control file
and rejects the new native interface with both `CheckHiWay` and `IgnoreHiWay`.
The new reader also rejects the old file. These are actual header reads,
including a native producer interface written by the rebuilt CLI.

Compiler sources at `b3b0a835c8` passed the default both-stage version rebuild
on 2026-11-12 in 29m56s, with a peak of 9,625 MiB under 16 GiB. A final
incremental pass included the late diagnostic detail and Note edits in both
stages; it passed in 7m54s and peaked at 4,700 MiB. Both runs used six workers,
`--docs=none`, and disabled swap.

The bootstrap API checks passed at all three compression levels, including
fresh binder identity, repeated references, tidied nested names, constructor and
caller substitution, malformed scopes and annotations, diagnostic-only forms,
and unknown tags. This scope peaked at 972 MiB under 4 GiB. The runtime scope
peaked at 913 MiB under 4 GiB. It covers 36 value/laziness observations plus two
concrete cross-module applications, 35 located source diagnostics, and eight
phase-specific program rejections.

Separate CLI processes compiled the producer and consumer, linked their objects,
and passed the same cross-module observations. `--show-iface` printed retained
signatures without unfoldings. This scope, including the version checks, peaked
at 93 MiB under 4 GiB. The ignored scripts are `native-iface-api.sh`,
`native-program-runtime.sh`, and `native-iface-cli.sh` in `_build/verification/`.

All five registered normal-way tests passed after the fixture-only correction
at `18fafdda4f`: 39 cases, 34 skipped, no unexpected results, and no framework
warnings. The run peaked at 2,277 MiB under 8 GiB. The initial run exposed an
inconsistent bytecode/executable-link request in the negative fixture. The fix
uses `NoLink` and retains the expected non-object phase rejection.

The ordinary compatibility smoke passed package checks, an optimized threaded
Template Haskell program using containers/text/bytestring, GHCi evaluation, and
Haddock startup. Its peak was 147 MiB under 4 GiB. Missing-documentation package
warnings are expected with `--docs=none`. Swap was disabled in each capped run.

The full testsuite, documentation build, performance checks, and native unfoldings
remain open. Signature fingerprint checks are recorded below. See the scoped
representation in [Core](CORE.md#native-interface-signatures) and the
[Agda provenance](REFERENCES.md#agda-provenance).

## Signature fingerprints and recompilation

The existing `NativeDependentCoreRun` regression now checks incremental consumer
compilation. It reads actual declaration and module ABI hashes from producer
interfaces. Rebuilding the same producer with fresh runtime Ids preserves these
hashes. Changing only `choose True` from `42` to `43` also preserves them when
unfoldings are omitted. In both cases an unchanged consumer skips code generation;
fixed earlier object and interface timestamps make that check independent of
filesystem timestamp resolution.

The producer then changes `Select True` from `Int` to `Bool` and updates its
payloads. The `choose` and `keep` signature text stays unchanged. Their declaration
hashes and the module ABI change. The `keepIndependent` hash stays unchanged.
The consumer imports only `choose` and `keep`, with no explicit `Select` use.
Recompiling without `-fforce-recomp` rejects its stale `Int` use with located
`GHC-83865`, including the `Int`, `Bool`, and `choose` context. Fresh one-shot
contexts read only producer interfaces; producer sources stay outside the import
path. These checks pass through the bootstrap API, at 917 MiB under 4 GiB with
swap disabled.

All five registered normal-way tests also pass: 39 cases, 34 skipped, no
unexpected results, and no framework warnings. Hadrian reported 1m11s; the
scope peaked at 2,558 MiB under 8 GiB with swap disabled. The fixture remains
inside the existing regression; the compiler sources did not change. The
ignored scripts are `native-recompilation-runtime.sh` and
`native-source-choose-registered.py` in `_build/verification/`. Their logs and
peak records use the `native-recompilation` prefix. Fourmolu, Cabal-gild parsing,
whitespace checks, and local documentation link checks pass.

The test Note records these controls and links the existing GHC fingerprinting
Note. The [Agda import-validation comparison](REFERENCES.md#agda-provenance)
records the shared dependency check and GHC's distinct declaration ABI policy.
Normal module unfolding generation remains gated. The expression-table and
declaration-reconstruction increment below supplies the checked template path.
See [the remaining M8 work](PLAN.md#m8-support-interfaces-and-validate-the-compiler-change).

## Native expression templates

At `bb998056d5`, expression tables reconstruct fresh Boolean branch proofs from
the scrutinee and constructor. Imported native templates run mandatory Core Lint
and match their declared types inside deferred RHS reconstruction. The unfolding
builder preserves runtime index bindings, native proofs, and native stable
metadata. It leaves unused unstable templates deferred and preserves the wire
cache. Native rule/dictionary payloads reject before ordinary occurrence analysis.

The regression covers all three compression levels, variable and literal proof
cases, ordinary Boolean cases, DEFAULT coverage, index-only lets, fresh proof
identity, scope, malformed annotations, both signature-mismatch directions,
named DEFAULT/literal binders, native rule/dictionary gates, and lazy metadata.
It imports actual `IfaceId` declarations and forces their templates with optional
Core Lint disabled. These are binary-table and declaration checks; they do not
establish real native unfolding files or cross-module inlining.

Both-stage default builds pass with six workers, `--docs=none`, and swap disabled.
The first pass took 9m44s and peaked at 5,417 MiB under 16 GiB. The final alignment
at `bb998056d5` took 8m28s and peaked at 4,946 MiB. The target API run peaked at
832 MiB under 4 GiB. The aligned bootstrap API run peaked at 987 MiB under 4 GiB.

All five registered normal-way tests pass: 39 cases, 34 skipped, no unexpected
results, and no framework warnings. Hadrian reported 1m09s; the scope peaked at
2,493 MiB under 8 GiB. The ordinary optimized/threaded TH, package, GHCi, and
Haddock smoke also passes, at 145 MiB under 4 GiB. Swap was disabled in each scope.
The full testsuite, documentation build, and performance measurements have not run.

Ignored logs and peak records use the `native-expr` prefix under
`_build/verification/`. Scripts include `build.sh`, `native-expr-self-api.sh`,
`native-iface-api.sh`, `native-source-choose-registered.py`, and
`native-iface-smoke.sh`. Compiler formatting checks are nonmutating parses;
the fixture passes canonical Fourmolu. Cabal-gild, diff, and local link checks pass.
See [the representation and checks](CORE.md#native-expression-templates).

A statically linked header-only reader and native Boolean control interface are
preserved in `_build/verification/interface-bool-v1/`. The reader accepts the
`.1` control with both `CheckHiWay` and `IgnoreHiWay`. Its preservation scope
peaked at 848 MiB under 4 GiB. `SHA256SUMS` records both artifacts. Keep this
binary for the next format's rejection check; do not rebuild it after the bump.
Rejection of the future format has not been tested yet.

## Core naturals and vectors

Closed first-order algebraic domains now work as native Core indices. The domain
check follows actual TyCon and DataCon structure. It accepts recursive and
mutually recursive domains. It rejects parameters, existentials, constraints,
newtypes, unsupported fields, and constructor wrappers.
The user-defined `Nat` has no wired compiler identity. Constructor indices use
the ordinary promoted constructors from DataKinds.

The existing API fixture checks Nat kinding, saturated constructor indices,
substitution, capture avoidance, closed `Add` reduction, and field-scoped branch
proofs. A native `S` alternative binds its runtime predecessor before its virtual
proof. Core Lint checks both. Backend lowering keeps the predecessor and removes
only the proof. Opaque indices remain opaque even when their unfoldings contain
known constructors. Apartness does not assume that every Nat is finite.

The runtime fixture loads ordinary source definitions of `Nat`, `Vec`, `Add`,
and a GADT append helper. It constructs checked native Core for recursive
`replicate`, `head`, and a retained append wrapper. One worker reads unknown
lengths from IORefs and uses the same runtime Ids across producers and consumers.
Lengths two and three produce `[42,42,7,7,7]` with observed length five.

The same fixture checks empty inputs and unused bottom elements. A replication
at `S bottomNat` produces a first element without forcing its predecessor.
Forcing the remaining spine raises the named Nat exception. A cyclic Nat permits
a finite vector prefix. `head bottomNat (VCons 42 bottomVec)` returns `42`.
Fully applying append to bottom indices and vectors raises the vector exception,
which checks that append does not force its retained lengths first.

These Core programs pass through the checked bytecode entry. The source fixture
below separately validates source elaboration, machine code, and Nat interface
imports through the bootstrap API, registered self-hosted tests, and separate CLI
processes. The checker and signature reconstruction use the structural domain rule.
At this Core checkpoint, CorePrep rejected retained constructor-tree arguments
with fields. Naming `S k` as an opaque local Id needs checked index transport.
The current implementation is described under constructor-argument transport.

The implementation checkpoint is `df623104fd`. Both-stage default builds pass
with six workers and `--docs=none`. The final incremental alignment took
16m07s and peaked at 7,344 MiB under 16 GiB. Bootstrap
execution passed at 966 MiB under 4 GiB. Swap was disabled in both scopes.

All five registered normal-way tests pass: 39 cases, 34 skipped, no unexpected
results, and no framework warnings. Hadrian reported 1m35s. The scope peaked at
2,557 MiB under 8 GiB, with swap disabled. This includes the final Nat substitution
and field-capture checks and the full runtime fixture.

The ordinary optimized/threaded TH, package, GHCi, and Haddock smoke also passes
at 145 MiB under 4 GiB, with swap disabled. Compiler sources pass nonmutating
Fourmolu parses. Both fixtures pass canonical Fourmolu. Cabal-gild, diff, and
local document link checks pass. The repository merge-request template requires
the focused regressions. The full testsuite, documentation build, and performance
measurements have not run.

Logs and peak records use the `native-nat` prefix in `_build/verification/`.

## Source naturals and vectors

The [M9 target](PLAN.md#m9-runtime-natural-numbers-and-indexed-vectors) is a
user-defined Peano Nat with `replicate`, `append`, and `head` for indexed vectors.
Runtime lengths become native indices directly. `replicate` inspects a length;
`append` can use an ordinary GADT helper without forcing its retained lengths.
The [pinned Agda definitions](REFERENCES.md#agda-provenance) supply the typing
and recursion comparison. Generic algebraic Core support is implemented.
Source checkpoint `714689a2fa` checks and executes these definitions:

```haskell
replicateNative :: forall a. foreach (n :: Nat) -> a -> Vec a n
replicateNative Z x = VNil
replicateNative (S k) x = VCons x (replicateNative k x)

headNative :: forall a. foreach (n :: Nat) -> Vec a (S n) -> a
headNative n (VCons x _) = x

appendNative :: forall a. foreach (n :: Nat) (m :: Nat)
             -> Vec a n -> Vec a m -> Vec a (Add n m)
appendNative n m xs ys = appendVec xs ys
```

`appendVec` is the ordinary polymorphic GADT helper in the M9 target.
An IORef supplies runtime lengths 0, 2, and 3. The source regression checks
replication, four append pairs, and head at both `Int` and `Bool`. It also checks
partial and cyclic Nat values, bottom tails, and unused bottom indices.
Wrong lengths, unsupported field patterns, and an ambiguous erased payload
produce located diagnostics. No singleton conversion or wired Nat is used.

A producer owns the actual Nat/Vec declarations and exports the retained
functions. A fresh one-shot consumer reads only its interface and object.
The linked executable repeats the runtime length and laziness observations.
The bootstrap API, registered self-hosted fixture, and separate CLI processes
pass these checks. The CLI writes the producer interface, imports it, and links
the object in separate processes. A preserved Boolean-only reader accepts its
old control and rejects the new interface with both way settings. The new reader
rejects the old format before type decoding. Its saved binary and control file
retain their original hashes.

The audit identifies four preservation requirements: field binders before proofs
that mention them, scoped GADT index instantiation, retained telescopes, and
CorePrep transport when naming constructor arguments. M9 records source-level
and cross-module acceptance checks, including partial and cyclic Nat values.
Direct constructor-tree arguments now use checked CorePrep transport.
Constructor equations currently
split one leading retained index. Variable equations can bind several retained
indices. Source `case` refinement and broader inference remain separate work.
Producers and consumers must use the same named index. Its defining expression
does not establish an index equality.

On 2026-11-13, the full both-stage build passed in 15m26s, peaking at 6,616 MiB
under 16 GiB. The final diagnostic synchronization passed in 8m32s at 5,244 MiB.
Both used six workers, `--docs=none`, and no swap. The five registered normal-way
tests passed across the initial run and focused rerun. The latter corrected the
domain-error span and the expected rigid-variable diagnostic, without compiler
changes. The test scopes peaked at 2,546 MiB and 1,704 MiB under 8 GiB.
Separate CLI execution checks ten observations and both version-mismatch
directions. Ordinary optimized code, Template Haskell, GHCi, package-database,
and Haddock startup smoke checks also pass. Repository contribution guidance
requires regressions; this change extends the existing API fixtures and golden.
The guide's exact Nat/Vec example also compiles with Core and STG Lint.

## Constructor-argument transport

The implementation checkpoint is `a2f6553bdf`.

CorePrep accepts direct `S n` and nested `S (S n)` retained arguments. It names
constructor fields recursively and composes checked nominal equalities. It
transports later indexed payloads and the result across the supplied telescope.
The proof cases preserve lazy fields. CoreToStg erases the virtual evidence.
No singleton witness, unfolding equality, or universal coercion supplies it.
See [CORE.md](CORE.md) and [the provenance](REFERENCES.md).

The source checks cover `replicateNative (S n) x`,
`headNative n (replicateNative (S n) x)`, and two-index append with constructed
arguments. Partial and cyclic fields preserve finite prefixes. A head consumer
does not force the unused predecessor. Forcing a bottom function still raises.

On 2026-11-13, both compiler stages built in 8m58s, peaking at 5,600 MiB under
16 GiB. The five registered normal-way tests passed in 1m10s at 2,439 MiB under
8 GiB: 39 cases, 34 skipped, no unexpected results or framework warnings.
Bootstrap Core/API and runtime checks passed. A separate CLI producer and
interface-only consumer passed 19 observations. Both interface-format rejection
directions passed. The exact guide example and ordinary optimized TH, GHCi,
package, and Haddock smoke passed. All scopes disabled swap. The CLI, guide,
and ordinary smoke scope peaked at 162 MiB under 4 GiB.

At this checkpoint, dependent-function congruence and dependent erased binder
kinds were gated. The `lazy`/`noinline`/`nospec` stripping path can bypass preprocessing
and still reject a constructed retained argument. Known partial-function heads
now pass PMC as described below. Source `case` refinement and native unfolding
files remain open.
These checks do not establish a general preservation theorem. The full
testsuite, documentation build, and performance measurements have not run.

Logs use the `native-alias` prefix in `_build/verification/`. Compiler sources
pass nonmutating Fourmolu parses. Both existing fixtures pass canonical
Fourmolu. Cabal-gild, diff, and local document link checks pass. The repository
merge-request template requires the focused regressions.

## PMC and retained function heads

The implementation checkpoint is `0335c399f3`.

Strict wildcard matching on a partial retained function now passes the source
pipeline. With a local runtime index `n`, `case appendNative @Int n of !_ -> ...`
does not force the unused index or the function body. The tests cover IORef
inputs zero, two, and three, bottom and cyclic indices, and a canonical mixed
ordinary/retained telescope. An empty match on a function type reports located
GHC-62161 under `-Werror=incomplete-patterns`.

PMC preserves a known function head without rewriting its domain and codomain.
The immutable matcher can bind a whole retained type to a template variable
under its usual scope and occurs checks. Unsupported comparisons produce
`MaybeApart MARRetainedFunction`. Alpha-equivalent retained types without free
TyCoVars can compare reflexively. At this checkpoint, Pi congruence and general
matching under a retained binder were open. See [the Agda WHNF provenance](REFERENCES.md) and Notes
[Coverage checking retained functions] and [Matching retained function types].

The new source checks preserve possible function inhabitation. They do not
assume a function expression is evaluated. At this checkpoint, constructed partial
applications such as `appendNative (S n)` reported the CorePrep result-congruence restriction.
The former PMC and immutable-matcher panics no longer obscure that boundary.

The both-stage build passed in 9m21s, peaking at 6,804 MiB under 16 GiB. All five
registered normal-way tests passed in 1m31s at 2,593 MiB under 8 GiB, with no
unexpected results or framework warnings. Bootstrap matching and source runtime
checks passed. The finalized runtime scope peaked at 972 MiB under 4 GiB.
A separate CLI producer and interface-only consumer passed 21 observations.
CLI checks also confirmed the located empty-case diagnostic and the named
constructed partial-result restriction, with neither reported as a panic.
Both format-rejection directions, the guide example, and ordinary compiler
smoke passed. The final CLI and smoke scope peaked at 165 MiB under 4 GiB.
All scopes disabled swap. Compiler sources passed nonmutating Fourmolu parses,
both existing fixtures passed canonical Fourmolu, and cabal-gild passed.
Logs use the `native-pmc` prefix in `_build/verification/`.

## Source case refinement

At `3621781dc4`, expected-type-directed source `case` expressions reuse the native
constructor proofs. For example:

```haskell
replicateCaseNative :: forall a. foreach (n :: Nat) -> a -> Vec a n
replicateCaseNative n x = case n of
  Z -> VNil
  S k -> VCons x (replicateCaseNative k x)
```

The expected result supplies the motive. Referenced locals such as
`xs :: Vec a n` become fresh arguments of the generated retained pattern
function. This refines their types together with the result. Constructor
substitution also refines direct uses of the original index. Resolved Names
preserve field shadowing. The elaboration adds no Core form, singleton argument,
interface codec, or unfolding equality. See Note [Source retained case refinement]
and [the Agda provenance](REFERENCES.md).

The rule requires a named local scrutinee, complete unguarded constructor
branches, bare fields, unrestricted captures, and a lifted checked result.
Parentheses are accepted.
Unsupported clause shapes and quotations report located GHC-83605. Incorrect
vector lengths use the existing type mismatch. Ordinary cases keep their
existing checker. General inference, guards, defaults, and nested patterns remain
open. This checkpoint preceded fixed-domain retained congruence.

The bootstrap Core and runtime checks pass. The new source matrix checks
recursive vectors, captured Boolean payloads, direct index substitution, field
shadowing, partial and cyclic inputs, and function-result strictness. It checks
six located rejection cases. The standalone executable has nine observations;
the separate producer and interface-only consumer have 24. The successful
runtime scope peaked at 973 MiB under 4 GiB, with swap disabled.

The both-stage build passed in 12m56s at 6,934 MiB under 16 GiB. All five
registered normal-way regressions passed: 39 cases, 34 skipped, no unexpected
results or framework warnings. The test command took 8m13s, including a fresh
bootstrap prerequisite rebuild, and peaked at 5,731 MiB under 8 GiB. The test
driver itself took eight seconds. Both scopes disabled swap.

Separate CLI processes passed the 24-observation interface-only consumer.
Four additional machine-code observations check function-result strictness and
payload laziness. An unsupported constructor field reports GHC-83605 at its
source location. At this checkpoint, the previous empty-case diagnostic,
constructed partial-result restriction, and both interface format-rejection
directions passed.
The updated guide example compiles with Core and STG Lint. Ordinary optimized
Template Haskell, package, GHCi, and Haddock smoke checks pass. The guide and
ordinary smoke scope peaked at 149 MiB under 4 GiB, with swap disabled.

When checking exceptions from `IO String`, force the result inside `try`.
`observeInt x = pure (show x)` can return a lazy string before it forces `x`.
The CLI strictness harness now checks that evaluation inside the protected
action. Core and final STG dumps also preserve payload inspection inside the
returned lambda. Compiler sources passed nonmutating Fourmolu parses, both
existing fixtures passed canonical Fourmolu, and cabal-gild passed.
Logs use `native-case` in `_build/verification/`. Generated bootstrap artifacts
are retained with hashes in `native-case-bootstrap-work/` under that directory.
The signature format is unchanged.

## Fixed-domain retained congruence

On 2026-11-13, `DepFunCo` adds checked coercion transport under an unchanged
closed algebraic domain and `Many` runtime binder. The binder scopes over the
whole proof, including intermediate types absent from the final endpoints.
Each endpoint independently canonicalizes a vacuous retained binder to an
ordinary arrow. See [the Core rule](CORE.md#fixed-domain-retained-congruence)
and [the Agda and D/DC provenance](REFERENCES.md#agda-provenance).

A complete signature can now check a definition with one source pattern and
a remaining retained function result:

```haskell
appendAfterSuccessor :: foreach (n :: Nat) (m :: Nat)
                     -> Vec Int (S n) -> Vec Int m -> Vec Int (Add (S n) m)
appendAfterSuccessor n = appendNative @Int (S n)
```

Later applications preserve the actual runtime indices and indexed payloads.
Unsigned let aliases still report located GHC-83605 and require a complete
signature. This increment does not enable retained inference.

Bootstrap kernel and runtime checks pass. The kernel covers roles, endpoints,
scope, capture avoidance, one-sided endpoint dependence, proof-only dependence,
and malformed binders or result kinds. Runtime checks apply saved Core partial
functions to IORef inputs zero, two, and three. They check function WHNF,
bottom and cyclic indices, and delayed payload evaluation. The source definition
above and its later applications pass. The bootstrap interface-only consumer
now checks 26 observations. The logs are `native-pico-api-v2.log` and
`native-pico-runtime-v2.log` in `_build/verification/`.

The both-stage build passed in 17m14s at 7,442 MiB under 16 GiB. All five
registered normal-way regressions passed: 39 cases, 34 skipped, no unexpected
results or framework warnings. The test command took 1m36s, including helper
builds, at 2,531 MiB under 8 GiB. The test driver took eight seconds.

Separate CLI processes passed the 26-observation interface-only consumer,
eight constructed partial/application observations, and nine standalone
observations. Four additional observations check function-result strictness and
payload laziness. Located GHC-83605, the empty-case diagnostic, both interface
format-rejection directions, the exact guide example, and ordinary optimized
Template Haskell, package, GHCi, and Haddock smoke checks passed. The serial
CLI, guide, and smoke scopes peaked at 146 MiB under 4 GiB. All scopes disabled
swap. Compiler sources passed nonmutating Fourmolu parses. Both fixtures passed
canonical Fourmolu, and cabal-gild passed.

Logs use `native-pico` in `_build/verification/`. Bootstrap-generated files and
their hashes are retained in `native-pico-bootstrap-work/` there.
The signature format is unchanged. Native coercion
serialization in unfolding bodies, structural Pi matching and ambient-evidence normalization,
heterogeneous domains, changed multiplicities, and the ordinary optimizer remain
open. Non-reflexive eta-cast handling also remains gated. Value casts preserve
Haskell forcing behavior; no function eta law is assumed.

## Family-axiom normalization

The next increment reduces family axioms under a retained binder with a fixed
closed algebraic domain and `Many` multiplicity. `mkDepFunRedn` preserves scoped
coercion evidence and separately canonicalizes each endpoint. The Core lifting
context permits shared valid same-domain index alpha-renaming. The solver keeps
global family reduction, ordinary meta-variable reading, roles, depth limits,
and locations. Its binder-local mode suppresses plugins, inert evidence,
family-cache reads and writes, and coercion zapping. Ordinary erased-forall
binder-kind handling is unchanged. Family axioms reduce at nominal and
representational roles; phantom family heads remain unreduced. Ordinary forall
binder kinds normalize at nominal role. See [the contract](CORE.md#family-axiom-normalization-under-retained-binders)
and [the Agda sources](REFERENCES.md#agda-provenance).

New source checks target complete signatures with no source argument patterns:

```haskell
leftUnit :: foreach (n :: Nat) -> Vec Int (Add Z n) -> Vec Int n
leftUnit = nativeId

leftUnitAppend :: foreach (n :: Nat) (m :: Nat)
               -> Vec Int (Add Z n) -> Vec Int m -> Vec Int (Add n m)
leftUnitAppend = appendNative @Int
```

Here `nativeId n xs = xs` has an explicit retained identity signature. A further
case reduces `ConstantNat n = Int`, so `constantAlias = ordinaryNat` checks
against a signature that becomes an ordinary `Nat -> Int` arrow. These checks
do not enable unsigned retained aliases or general structural unification.
Runtime indices stay opaque, including bottom and cyclic values.

The equality canonicalizer rechecks reflexivity after both endpoints have been
rewritten. Rewriting returns to `can_eq_nc` without repeating `zonkEqTypes`.
Same-Id runtime indices and `tcEqType`-equal retained functions use the existing
reflexivity rule. The rewriting evidence already relates the original and
normalized endpoints. This step adds no structural Pi decomposition or
meta-variable assignment. See Note [Reflexivity after retained normalization].

The initial bootstrap compiler build took 2m10s. Bootstrap API v3 and runtime v3 pass.
The runtime matrix checks 17 dedicated source observations and 38 imported
observations: the previous 26 plus 12 normalization observations. It also checks
the opaque right-unit mismatch at GHC-83865 and reduction-depth error GHC-40404.
The kernel checks roles and binder freshening. The successful logs are
`native-pinorm-api-v3.log` and `native-pinorm-runtime-v3.log` in
`_build/verification/`.

The both-stage build passed in 11m11s at 6,188 MiB under 16 GiB. All five
registered normal-way regressions passed: 39 cases, 34 skipped, no unexpected
results or framework warnings. The test command took 1m33s, including helper
builds, at 2,390 MiB under 8 GiB. The test driver took eight seconds.

Separate CLI processes passed the 38-observation interface-only consumer,
eight constructed partial/application observations, and nine standalone
observations. Four additional observations check function-result strictness
and payload laziness. The right-unit GHC-83865 and depth-limit GHC-40404 point
to their actual RHS lines. The ordinary type-error control, empty-case error,
located GHC-83605, and both interface format-rejection directions passed.
All three Nat/Vec guide blocks compiled with Core and STG Lint. Ordinary
optimized Template Haskell, package, GHCi, and Haddock smoke checks passed.
The serial CLI, guide, and smoke scopes peaked at 147 MiB under 4 GiB.
All scopes disabled swap. Compiler sources passed nonmutating Fourmolu parses.
Both existing fixtures passed canonical Fourmolu, and cabal-gild passed.

Logs use `native-pinorm` in `_build/verification/`. Bootstrap-generated files
and their hashes are retained in `native-pinorm-bootstrap-work/` there.
Full normalization with ambient Given or plugin evidence remains future work.
The signature format is unchanged.

## Arithmetic equality proofs

At `89c5ce92bf`, the source subset proves an arithmetic property of an unknown runtime
Nat and uses it to transport a vector. Enable `TypeOperators` and import
`Data.Type.Equality ((:~:)(Refl))`. Use the existing `Nat`, `Vec`, `Add`, and
`appendNative` definitions:

```haskell
rightZero :: foreach (n :: Nat) -> Add n Z :~: n
rightZero Z = Refl
rightZero (S k) = case rightZero k of Refl -> Refl

rightUnit :: foreach (n :: Nat) -> Vec Int (Add n Z) -> Vec Int n
rightUnit n xs = case rightZero n of Refl -> xs

appendRightUnit :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
appendRightUnit n xs = rightUnit n (appendNative @Int n Z xs VNil)
```

`Add Z n` reduces directly. `Add n Z` at an unknown `n` needs the induction
proof. The successor branch uses the recursive equality under `Refl`; the
checker combines it with successor congruence and the Add equation. An ordinary
`congruentSuccessor Refl = Refl` helper can express the same step. This follows
[Agda's right-identity proof and transport](REFERENCES.md#arithmetic-equality-proofs)
and the existing Haskell equality datatype. The native function receives the
actual Nat, with no singleton wrapper.

The constructor checker now selects immutable type arguments from the expected
result before ordinary subsumption proves the full equality. This permits the
direct recursive `Refl` spelling. The fallback applies to supported data
constructor heads. It does not assign retained syntax to a meta-variable.
See [the checking and evaluation contract](CORE.md#arithmetic-equality-proofs).

Matching `Refl` forces the proof. The recursive proof can force the complete Nat
spine. An ignored proof remains lazy; a bottom proof establishes no equality.
The transport therefore has runtime proof work. GHC does not check termination.
The fixture checks finite unknown lengths 0, 2, and 3, ignored bottom and cyclic
proofs, lazy partial applications, proof-before-payload failure, and rejection
when a supplied proof is not matched or its equality leaves its branch.

The focused bootstrap compiler check passes direct and congruence proofs,
vector transport, and a user-defined equality GADT with Core and STG Lint.
Unproved successor `Refl` and `n :~: S n` fail with GHC-83865 at the actual RHS.
The both-stage build passed in 7m58s at 5,191 MiB under 16 GiB. All five
registered normal-way regressions passed: 39 cases, 34 skipped, no unexpected
results or framework warnings. The command took 1m11s at 2,419 MiB under 8 GiB.
The test driver took 8.5 seconds. The bootstrap API and expanded runtime fixture
pass together at 980 MiB under 4 GiB.

The runtime fixture checks 17 dedicated proof observations, five exception-order
cases, and four GHC-83865 rejection controls. The interface-only consumer now
has 55 observations: the previous 38 plus the same 17 proof observations.
Separate CLI processes pass all 55, five proof exception-order observations,
the user-defined equality GADT, and four proof negatives at their actual RHS
lines. The earlier eight partial/application observations, nine standalone
observations, four source-case strictness observations, ordinary diagnostics,
and both interface format-rejection directions still pass.

All five literal Nat/Vec guide blocks compile together with Core and STG Lint.
Ordinary optimized Template Haskell, package, GHCi, and Haddock smoke checks pass.
The final serial CLI, guide, and smoke scope peaked at 157 MiB under 4 GiB.
All build and verification scopes disabled swap. Both existing fixtures pass
canonical Fourmolu. The compiler source passes a nonmutating Fourmolu parse,
and cabal-gild passes.

At this checkpoint, generic `forall a. a -> ...` argument inference rejected a
whole type that contained a retained index. The next increment below accepts
named argument spines. General retained inference and ambient-evidence
normalization under Pi remain open.

Successful logs use `native-proof-api`, `native-proof-runtime`,
`native-proof-registered`, `native-proof-cli`, `native-proof-extra-cli`,
`native-proof-runtime-cli`, `native-proof-guide`, and `native-proof-smoke` in
`_build/verification/`. The final CLI scope log is
`native-proof-cli-after-import-fix.log`. The first wrapper stopped at an import
collision in its standalone control; the corrected control hides the producer's
proof names. Bootstrap-generated files and their hashes are retained in
`native-proof-bootstrap-work/`. The signature format is unchanged.

## Generic retained consumers

At `c90959323a`, ordinary polymorphic functions can consume named values whose types contain
runtime indices. The inferred erased type argument can be a whole proof type,
a vector type, or a retained dependent function type. With the preceding Nat
and proof definitions, these examples use ordinary signatures:

```haskell
force :: a -> IO String
force x = x `seq` pure "forced"

ignoreAny :: a -> Int
ignoreAny _ = 42

forceProof :: Nat -> IO String
forceProof n = force (id (rightZero n))

forceBoxedProof :: Nat -> IO String
forceBoxedProof n = force (Just (rightZero n))

ignoreProofValue :: Nat -> Int
ignoreProofValue n = ignoreAny (rightZero n)

forceRetainedFunction :: Nat -> IO String
forceRetainedFunction n = force (id (appendNative @Int n))
```

`forceProof` forces the recursive proof and can force the complete Nat spine.
`forceBoxedProof` forces only `Just`, so its proof stays lazy at bottom and
cyclic indices. `ignoreProofValue` returns 42 without forcing its proof.
`forceRetainedFunction` observes the partial function's closure and keeps its
index lazy. Inference selects type arguments; it does not evaluate these values.

The checker extends the existing typed-argument cache. A known native Id in a
named argument spine enables immutable inference for the call. Other named
arguments can determine remaining binders, including `const proof Nothing`.
The expected result can determine binders first. Lambdas, literals, and other
checking-dependent forms keep their expected-type checks. Complete candidates,
kind and runtime-index scope checks, exact cached-type agreement, and ordinary
subsumption remain required. Source classification does not expand splices.
See [the contract](CORE.md#generic-retained-argument-inference) and
[the pinned Agda application and meta-context sources](REFERENCES.md#generic-retained-argument-inference).

The bootstrap fixture adds 34 generic observations after the previous 55,
for 89 imported observations. These cover ordinary consumers, linear proof
consumption, and retained-function values. Generic proof forcing also has
bottom and partial-index exception controls. Reusing a linear proof is rejected
with GHC-18872. The local linear helper has an explicit unrestricted let binding;
the same annotation is needed by the ordinary local-capture control.

The both-stage build passed in 7m30s at 4,573 MiB under 16 GiB. All five
registered normal-way regressions passed: 39 cases, 34 skipped, no unexpected
results or framework warnings. The command took 1m11s at 2,565 MiB under 8 GiB.
The test driver took 8.4 seconds. Bootstrap API and runtime v3 pass at 987 MiB
under 4 GiB.

Separate CLI processes pass all 89 imported observations, seven proof failure
observations, the generic equality GADT, four located proof rejections, and the
located GHC-18872 linear duplication rejection. Earlier partial/application,
standalone, source-case, and interface-version checks still pass. Eight
additional invalid applications produce located GHC-83865, GHC-91028, or
GHC-83605 without a panic. This covers the tested forms; native type-map support
remains future work.

Ordinary GADT/lambda, rank-n, and literal controls pass with the experimental
extension enabled. A higher-kinded consumer accepts a wrapped native proof.
A Template Haskell splice in a native vector argument expands exactly once.
All six literal Nat/Vec and generic-consumer guide blocks compile together with
Core and STG Lint. Optimized Template Haskell, package, GHCi, and Haddock smoke
checks pass. The final CLI/control/smoke scope peaked at 166 MiB under 4 GiB.
Build and verification scopes disabled swap. Compiler Fourmolu parsing, both
existing fixtures' canonical Fourmolu checks, and cabal-gild pass.

Successful logs use `native-generic` in `_build/verification/`. In particular,
`native-generic-cli.log` records 89 imported observations and
`native-generic-cli-after-path-fix.log` records the final CLI/control/smoke run.
Its first wrapper stopped at a missing copied standalone-control source; no
compiler change was needed. `native-generic-guide-final.log` checks the six
current literal blocks. Bootstrap artifacts and SHA256SUMS are retained in
`native-generic-bootstrap-work/`. The ordinary linear comparison and its
explicit unrestricted-binding controls are retained in `native-generic-linear/`.

Native meta-variable assignment, Quick Look, and general
checking-dependent argument inference remain open. The signature format is unchanged.

## Ordinary class consumers

Native map checkpoint: `1b37399cd5`. Class consumer checkpoint: `17740f7135`.

Ordinary class dictionaries can refer to types that contain a runtime index.
The existing `Show (a :~: b)` instance can render `rightZero n`. A normal vector
instance can render a retained vector:

```haskell
instance Show a => Show (Vec a n) where
  showsPrec _ VNil = showString "VNil"
  showsPrec d (VCons x xs) = showParen (d > 10)
    (showString "VCons " . showsPrec 11 x . showChar ' ' . showsPrec 11 xs)

showProof :: foreach (n :: Nat) -> String
showProof n = show (rightZero n)

showVector :: foreach (n :: Nat) -> Vec Int n -> String
showVector n xs = show xs

ignoreShow :: Show a => a -> Int
ignoreShow _ = 42

ignoreProof :: foreach (n :: Nat) -> Int
ignoreProof n = ignoreShow (rightZero n)
```

Rendering the proof forces it. Ignoring the proof remains lazy at bottom and
cyclic indices. Dictionary selection inspects types. A `Show` dictionary does
not supply equality evidence for vector transport. An equality-constrained
consumer can transport a vector after an explicit `Refl` case. It fails without
that evidence.

Immutable application matching now separates the leading constraints from the
ordinary body. It retains the substituted constraints for GHC's normal evidence
generation. An application-local implication binds native dictionary evidence
below the runtime index lambdas. It preserves enclosing Givens and the current
type-checking level. Native residual constraints remain in scope for solving or
a located error. The type maps now support free runtime indices and retained
binders with scoped alpha-equivalence. Ordinary family and optimizer gates remain.

The implementation passes the source/runtime matrix and the Core API checks. The
class block adds 42 observations after the previous 89. It checks repeated,
distinct, and mixed dictionary keys, inferred native results, visible type
applications, later quantifiers, local lets, and equality-Given transport.
Missing instances produce located GHC-39999. Equality constraints without proof
produce GHC-18872; direct failed transport produces GHC-83865.

The full build passes both compiler stages and matching libraries. Hadrian
reported 13 minutes 14 seconds. Peak memory was 6,953 MiB under a 16 GiB cap.
The five registered normal-way regressions pass: 39 cases, 34 skipped, five
expected passes, and no unexpected results or framework warnings. The driver
took 9.0 seconds; the command also rebuilt test tools and took 1 minute 38 seconds.
Its peak was 2,556 MiB under an 8 GiB cap.

Separate CLI processes reproduce all 131 imported observations. They also pass
the earlier proof, partial-application, standalone, and source-case controls;
three new proof exceptions; four located class/equality rejections; both interface
version-rejection directions; and ordinary bidirectional, rank-n, higher-kinded,
and one-expansion Template Haskell controls. Fourteen further observations use
Nat-indexed class parameters and retained function types with distinct free
indices, including ignored bottom and cyclic closures. All seven literal native
guide blocks pass Core and STG Lint. Optimized package/TH, GHCi, and Haddock smoke
checks pass. The final CLI scope peaked at 223 MiB under a 4 GiB cap. Swap was
disabled in all heavy scopes. The full GHC testsuite was not run.

Logs and scripts use the prefix `_build/verification/native-class-`. The final
wrapper is `native-class-final-checks.sh`. The bootstrap runtime and API logs are
`native-class-runtime-v5.log` and `native-class-api-v2.log`. The archived bootstrap
outputs and 27 file checksums, including CLI-generated STG dumps, are in
`native-class-bootstrap-work/`. The registered run's 24 files are saved separately
in `native-class-registered-work/`. The API
fixture localizes embedded module binders through ordinary Core substitution;
Core Lint remains enabled. Earlier prototype logs retain the reproduced map
panic, fixture export-flag failure, and corrected diagnostic expectation.

See [the contract](CORE.md#retained-class-consumers),
[instance-context provenance](REFERENCES.md#retained-class-consumers), and
[native type-map provenance](REFERENCES.md#native-type-map-equality).
Source-written class contexts with runtime binders, general inference, and
Quick Look remain open. This increment adds no interface format or equality axiom.

## Source retained constraints

A signature can put an ordinary class or equality context after its runtime
binder. The function receives its dictionary after that index. The body can use
the incoming evidence through normal GHC checking:

```haskell
renderWith :: foreach (n :: Nat) -> Show (Vec Int n) => Vec Int n -> String
renderWith n xs = show xs

transportGiven :: foreach (n :: Nat) -> (Add n Z ~ n)
               => Vec Int (Add n Z) -> Vec Int n
transportGiven n xs = xs

rightUnitViaContext :: foreach (n :: Nat) -> Vec Int (Add n Z) -> Vec Int n
rightUnitViaContext n xs = case rightZero n of
  Refl -> transportGiven n xs
```

These definitions reuse the Nat, Vec, Add, and proof definitions above. Enable
`FlexibleContexts` for the class signature. An equality context supplies evidence;
it does not cause GHC to evaluate `n` or prove the arithmetic identity. The caller
above supplies that evidence by checking `rightZero n` against `Refl`.

The equation checker keeps suffix patterns, guards, and local bindings under the
incoming dictionaries. Constructor equations keep their full argument lambdas
before the constructor case. Each branch transports its dictionaries with the
existing checked constructor proof. With lazy patterns, an ignored index stays
lazy. Inspecting the index or rendering a bottom vector can still force it.

Native quantified and irreducible predicates remain gated. A split with remaining
value patterns requires a nonforcing prefix and excludes `Strict`. A trailing
context preserves the earlier prefix pattern shapes. Inner retained lambdas,
general native inference, and ordinary optimization remain separate work.
The next checkpoint adds
[incoming evidence to explicit native cases](#source-case-evidence).

The implementation checkpoint is `670bcef166`. The shared fixture adds 46
observations after the previous 131. All 177 pass through interface-only module
consumers. The new 46 also pass source execution. The fixture checks no-instance
Givens, tuple-derived evidence, guards, views, local bindings, trailing contexts, and two retained
indices. Bottom and cyclic indices remain lazy in the tested partial applications.
Missing instances, missing equality proofs, unsupported predicate families,
quantified predicates, and strict prefixes produce located errors.

Both compiler stages and matching libraries build. The final build took 8 minutes
47 seconds and peaked at 4,956 MiB under a 16 GiB cap. All five registered normal-way
regressions pass: 39 cases, 34 skipped, and five expected passes. The driver reports
no unexpected results, framework failures, or framework warnings. It took 9.4
seconds; the command rebuilt test tools and took 1 minute 39 seconds. Its peak
was 2,430 MiB under an 8 GiB cap. Hadrian's setup
comparison warns about `lib` versus `lib/.`; both resolve to the same directory.

Separate CLI checks reproduce the 177 rows, earlier proof and linear controls,
both interface-version rejection directions, and the ordinary bidirectional and
single-expansion TH controls. The scoped CLI checks add two exception-order
observations and seven located rejection controls. All eight literal native guide
blocks and the optimized package/TH, GHCi, and Haddock startup smoke checks pass.
Missing-documentation warnings are expected with `--docs=none`. The full GHC
testsuite was not run.

Artifacts use `_build/verification/native-context-`. The final build log is
`native-context-full-build-v2.log`. Runtime and registered logs are
`native-context-runtime-v8.log` and `native-context-registered.log`. CLI results
are in `native-context-final-checks.log` and `native-context-scoped-cli-v2.log`.
The bootstrap archive `native-context-bootstrap-v8-work/` has 27 file checksums.
A final comment-only cleanup removes a local Haddock warning. Its targeted
`-haddock -fno-code` check passes without changing compiler artifacts.

See [the contract](CORE.md#source-retained-constraints) and the
[pinned Agda telescope sources](REFERENCES.md#source-retained-constraints).

## Source case evidence

An explicit dependent case can use the incoming class and equality evidence:

```haskell
renderCaseWith :: foreach (n :: Nat) -> Show (Vec Int n) => Vec Int n -> String
renderCaseWith n xs = case n of
  Z -> show xs
  S k -> show xs

transportCaseGiven :: foreach (n :: Nat) -> (Add n Z ~ n)
                   => Vec Int (Add n Z) -> Vec Int n
transportCaseGiven n xs = case n of
  Z -> xs
  S k -> xs
```

These definitions reuse the earlier Nat, Vec, Add, and Show definitions. The case
helper receives the original index, dependent evidence, and captured values.
Each branch transports the evidence with its checked constructor proof. The
ordinary solver can then derive superclass and tuple evidence. A global instance
is not needed when the caller supplies a Given. Equality transport still needs
an equality proof.

The checker records actual lexical evidence parameters, including primitive
equality CoVars. Static forms exclude enclosing evidence. Arrow heads restore
their enclosing scope. Nested typed splices restore the bracket-entry scope with
the outer constraint accumulator. This adds no native quotation support.

An explicit case must inspect its index before it can return a branch function.
It need not force an unused constructor field or captured vector. The shared
fixture appends 19 runtime observations to the previous 177. It also checks
no-instance declarations, tuple evidence, local bindings, nested shadowing,
boxed equality, primitive GADT equality, and arrow evidence scope. An incoming
`OnlyGiven (S k)` does not supply `OnlyGiven k`.

The constructor pattern limits remain unchanged: use complete, unguarded
alternatives with bare field variables. General motive inference, inner retained
lambdas, native coercion unfolding serialization, and ordinary optimization remain
separate work. The Core forms, dictionary ABI, and interface format stay unchanged.

The implementation checkpoint is `fe7329d1b6`. Both compiler stages and matching
libraries build. The full build took 11 minutes 45 seconds and peaked at 6,550 MiB
under a 16 GiB cap. All five registered normal-way tests pass: 39 cases, 34 skips,
five expected passes, and no unexpected results or framework failures or warnings.
The driver took 9.5 seconds. The command took 1 minute 37 seconds and peaked at
2,616 MiB under an 8 GiB cap. The bootstrap runtime and API kernel also pass.

Separate CLI compilation reproduces all 196 imported observations with Core and
STG Lint. The no-instance case and outer-Given arrow compile. Eight context
rejections have the expected codes and source locations. Six strictness controls,
both interface-version rejection directions, earlier proof and linear controls,
ordinary bidirectional typing, and single-expansion TH checks pass. All nine
literal native guide blocks compile. Ordinary nested typed splices, a static
pointer, optimized package/TH execution, GHCi, and Haddock startup also pass.
The CLI scope peaked at 269 MiB under a 4 GiB cap. All scopes used no swap.
The full GHC testsuite was not run.

Artifacts use `_build/verification/native-case-evidence-`. The build log is
`native-case-evidence-full-build.log`. The runtime log is
`native-case-evidence-runtime-v5.log`; earlier failures were in new arrow-control
fixtures and are retained in `native-case-runtime-v2.log` through `v4.log`.
The registered and final CLI logs end in `registered.log` and `final-cli.log`.
The bootstrap archive is `native-case-evidence-bootstrap-work/`.
Compatibility scripts use `native-case-compat-` and separate artifact copies.
Fixtures pass Fourmolu. Compiler files parse without rewriting; `Splice.hs`
requires CPP preprocessing in both interpreter branches. Its unchanged source
reproduces the raw Fourmolu parser failure. The Cabal template passes cabal-gild.
Missing-documentation warnings are expected with `--docs=none`.

See [the contract](CORE.md#explicit-case-evidence),
[the task list](PLAN.md#source-case-evidence-follow-up), and
[the pinned Agda implementation](REFERENCES.md#source-case-evidence).

## Checked retained lambdas

A complete retained signature now checks an anonymous lambda:

```haskell
lambdaIdentity :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a n
lambdaIdentity = \n xs -> xs
```

This uses the earlier Nat and Vec declarations. The runtime Id for `n` also
instantiates the later argument and result types. Named equations and lambdas
share the retained match checker. The rule preserves outer erased type patterns,
nested lambdas, later retained indices, and class and equality contexts.

Preserve the source value arity. A one-pattern `\case` can inspect the index
before its branch returns a function. A two-pattern `\cases` waits for both
arguments before matching. Unused indices, fields, and payloads stay lazy.
Lambdas retain their ambient usage. A local retained lambda can capture a linear
value and consume it once. Duplicated linear proof use remains an error.

An empty-domain lambda-case binds its index explicitly before an empty case
inspects it. The result type refers to the same Id. This fixes an unbound-index
Core Lint error that the empty match group exposed. Empty source cases need no
branch refinement and stay in their original context. Demanding either form at
a bottom index still raises that bottom exception.

The shared fixture appends 30 observations to the previous 196. It also checks
four located lambda rejections and two empty-domain bottom exceptions. The
bootstrap runtime and API kernel pass. Separate probes check explicit outer type
patterns, linear captures, and the existing inference and type-position limits.

The implementation checkpoint is `a0c493d5f9`. Both compiler stages and matching
libraries build in 8 minutes 26 seconds. The scope peaks at 4,833 MiB under a
16 GiB cap. All five registered normal-way tests pass: 39 cases, 34 skips,
five expected passes, and no unexpected results or framework failures or warnings.
The driver takes 9.6 seconds. The command takes 1 minute 13 seconds and peaks at
2,492 MiB under an 8 GiB cap. The final bootstrap scope peaks at 1,000 MiB
under a 4 GiB cap.

Separate CLI processes reproduce all 226 imported observations with Core and
STG Lint. Six new lambda rejections have the expected codes and source locations.
Empty lambdas with trailing class and equality contexts compile. Four strictness
controls include ordinary empty-case behavior. STG keeps arity one for `\case`
and arity two for `\cases`. The ignored-index worker has no case.

Earlier proof, linear, source-case, interface-version, and ordinary typing checks
pass. All ten literal native guide blocks compile. Single-expansion TH, nested
typed splices, static pointers, optimized package execution, GHCi, and Haddock
startup pass. The initial CLI scope peaks at 210 MiB under a 4 GiB cap.
A copied TH fixture used its old counter path. The corrected remaining checks
pass in a separate scope with a 167 MiB peak. All scopes use no swap.
The full GHC testsuite was not run.

Artifacts use `_build/verification/native-checked-lambda-`. The final bootstrap
logs are `runtime-v4.log`, `api.log`, and `bootstrap-v3.log`. Build and registered
logs end in `full-build.log` and `registered.log`. CLI evidence spans
`final-cli.log` and `final-remainder.log`; the latter records the corrected TH
fixture and remaining checks. The bootstrap archive has 27 checksummed files,
including three STG dumps. The source manifest covers 11 compiler files and both
API fixtures. Fourmolu, compiler-file parsing, cabal-gild, and diff checks pass.

This checks an expected native type. It adds no new Core form, singleton ABI, or
interface format. Retained syntax in nested type positions, general native
inference, structural Pi unification, and ordinary optimization remain open.
See [the contract](CORE.md#checked-source-lambdas),
[the tasks](PLAN.md#checked-retained-lambdas), and
[Agda's lambda and absurd-lambda rules](REFERENCES.md#checked-retained-lambdas).

## Higher-order retained closures

A retained function can be an ordinary closure argument in a complete signature:

```haskell
applyNative :: forall a.
               (foreach (m :: Nat) -> Vec a m -> Vec a m)
            -> foreach (n :: Nat) -> Vec a n -> Vec a n
applyNative f n xs = f n xs
```

This uses the earlier Nat and Vec declarations. The caller can pass an explicitly
specialized named worker or a lambda checked against the supplied retained type.
The argument `f` is a function value. It is not a runtime index. An inner
function type can also capture an enclosing runtime index. Each nested binder
scopes only its own codomain. Both ordinary-leading and retained-leading
signatures follow the same arrow traversal.

The ambiguity check opens retained binders in argument and result positions in
one fresh runtime context. The actual signature stays retained. Ordinary helpers
can also receive a known retained function type through immutable instantiation.
This checks `id` around a lambda and a genuine polymorphic bottom without native
meta assignment or eta expansion. Erased inner foralls and structural Pi
unification remain gated. Native templates can still require explicit type
applications. Retained syntax under type constructors, kinds, and other source
positions remains separate work.

The fixture appends 29 observations to the previous 226. These cover polymorphic
calls, ordinary-leading functions, checked lambdas, value-changing vector
closures, captured indices, and erased/runtime shadowing. Unused bottom functions
and bottom vectors stay lazy. Demanding the signed bottom function raises its
named exception. Wrong lengths, escaping indices, unsupported domains, unsigned
aliases, and partial signatures have located diagnostics. Constructor clauses
and explicit case transport a captured closure through its fixed-domain Pi.
The transport uses the checked branch proof and existing `DepFunCo` lifting.
It preserves the function value with a cast. Changed domains and dependent
erased foralls remain gated.

The implementation is in `64044341e8` and `94d7d8f2f3`. The final both-stage
build passed in 7m35s at 4,651 MiB under 16 GiB. All five registered normal-way
regressions passed: 39 cases, 34 skipped, no unexpected results or framework
warnings. The command took 1m11s at 2,560 MiB under 8 GiB. The driver took
9.725 seconds. The repository requires regression coverage for compiler changes;
this change extends the existing runtime fixture.

The bootstrap runtime passes 255 imported rows and 124 source-context rows.
The API kernel and standalone known-result controls pass. Separate self-hosted
CLI processes reproduce the 255 rows and check 13 further closure observations.
These include ignored and returned cast bottom functions. Six new located
rejections, previous lambda/context/proof/linear controls, 177 earlier
compatibility rows, and both interface-version rejection directions pass.

All 11 literal guide blocks pass Core and STG Lint. Ordinary typing,
single-expansion Template Haskell, 14 native function/class observations,
optimized package/GHCi/Haddock smoke, nested typed splices, and static pointers
pass. The final CLI scope peaked at 291 MiB under 4 GiB. All heavy jobs were
serial and disabled swap. The full GHC testsuite was not run.

STG shows ordinary three-argument closure passing in `applyAt` and `applyFirst`.
`ignoreClosure` has no case. `bottomClosure` remains a bottom thunk.
`returnCapturedCase` has arity two and returns the original closure after its
index case. The Core forms, dictionary ABI, and interface version are unchanged.
General native inference, Quick Look, structural Pi unification, and ordinary
optimization remain gated.

Artifacts use `_build/verification/native-higher-order-`. The final logs are
`full-build-v2.log`, `registered-v2.log`, and `final-cli-v2.log`. The
`validation.txt` record includes the earlier prototype failure, source hashes,
artifact hashes, and the older compatibility snapshot's stale STG hashes.

See [the contract](CORE.md#explicit-higher-order-closure-arguments),
[the tasks](PLAN.md#explicit-higher-order-arguments-follow-up), and
[Agda provenance](REFERENCES.md#explicit-higher-order-arguments).

## Expected-type instantiation of retained workers

A complete expected retained function type can supply a known worker's erased
type arguments. This uses the Nat and Vec types and `applyNative` from above:

```haskell
polyIdentity :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a n
polyIdentity n xs = xs

identityInt :: foreach (k :: Nat) -> Vec Int k -> Vec Int k
identityInt = polyIdentity

applyIdentityInt :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
applyIdentityInt n xs = applyNative @Int polyIdentity n xs
```

The expected type determines `a = Int`. The worker receives an ordinary type
application. Its retained runtime binder stays intact. Nested telescopes,
ordinary-leading aliases, and contexts follow the same checking rule. Explicit
type applications remain available.

The new immutable matcher is opt-in. It pairs equal closed-domain `Many`
binders, seeds their scope with input free variables, and rejects solutions
that capture an inner index. Default matching and general unification retain
their existing native gates. Require a known head without concreteness
requirements, NoQL, all erased arguments, kind/scope agreement, exact result
equality, and ordinary final subsumption. The compiler does not guess a runtime
index or eta-expand the worker.

The bootstrap API and runtime pass. The runtime fixture appends 22 rows to the
previous 255. These cover implicit worker arguments, polymorphic and monomorphic
aliases, ordinary-leading and applied aliases, nested telescopes, and laziness.
The source checks reject inconsistent payloads, inner-index capture, domain
mismatch, and an unsigned alias. A signed bottom worker remains bottom when
demanded. The API regression also checks a shared-pointer binder/free-index
case that failed before the matching scope included both input types.

Implementation `dae037074c` passes the full both-stage build in 9m22s at
5,872 MiB under 16 GiB. All five registered normal-way regressions pass:
39 cases, 34 skipped, no unexpected results or framework warnings. The command
took 1m36s at 2,613 MiB under 8 GiB. The driver took 10.529 seconds. The
repository requires regression coverage; this change extends both existing
native Core API fixtures.

The bootstrap API and runtime pass together at 1,098 MiB under 4 GiB. The
runtime checks 277 imported rows and 146 source-context rows. Separate
self-hosted processes reproduce the 277 rows. Seven further worker observations
check bottom-function behavior and runtime class-context dispatch. Four new
located rejection controls, the previous 13 closure controls, earlier
lambda/context/proof/linear checks, 177 compatibility rows, and both interface
rejection directions pass.

All 12 literal guide blocks pass Core and STG Lint. Ordinary typing,
single-expansion Template Haskell, 14 native function/class observations,
optimized package/GHCi/Haddock smoke, nested typed splices, and static pointers
pass. The CLI scope peaked at 302 MiB under 4 GiB. All heavy jobs were serial
and disabled swap. The full GHC testsuite was not run.

CorePrep shows actual `@Int` applications for captured, constrained, and
ordinary-leading aliases. STG preserves direct worker aliases. The generic
bottom worker and its specialization remain thunks. Ordinary partial
applications can retain CorePrep's existing arity wrappers. The Core forms,
dictionary ABI, and interface version are unchanged. General native meta
assignment, unification, and ordinary optimization remain gated.

Artifacts use `_build/verification/native-worker-instantiation-`. Final logs
are `full-build.log`, `registered.log`, `final-cli.log`, `api-v3.log`, and
`runtime-v2.log`. The source manifest covers 18 compiler files and two fixtures.
The bootstrap and isolated compatibility archives contain 27 files each.
`validation.txt` records the failing freshness regression and its verified fix.
See
[the contract](CORE.md#explicit-higher-order-closure-arguments),
[the tasks](PLAN.md#expected-type-instantiation-of-retained-workers), and
[Agda provenance](REFERENCES.md#expected-type-instantiation-of-retained-workers).

## Verified baseline

The default Hadrian build passed with six workers and `--docs=none`. The build
produced `_build/stage1/bin/ghc`, version `10.1.20261109`. Hadrian reported
30 minutes 48 seconds. The memory scope recorded a peak of 10,577 MiB under a
16 GiB cap with swap disabled.

The boot compiler was GHC 9.14.1. The Cabal version was 3.18.1.0. Nix supplied
Autoconf 2.72, Automake 1.18.1, Happy 2.1.7, and Alex 3.5.4.0.

Focused smoke checks passed. They covered package registration, optimized
compilation with Core Lint, Template Haskell, threaded execution, GHCi, and
Haddock startup. Documentation generation and the full testsuite were not run.
Package registration emitted missing-documentation warnings because documentation
was disabled.

The local build commands used these steps:

```sh
export CC=/usr/bin/gcc CXX=/usr/bin/g++ LD=/usr/bin/ld
./boot
./configure GHC=/home/tritlo/.ghcup/bin/ghc-9.14.1
cabal --project-file="$PWD/hadrian/cabal.project" build \
  --with-compiler=/home/tritlo/.ghcup/bin/ghc-9.14.1 \
  --disable-documentation --disable-profiling --disable-library-profiling \
  -j6 exe:hadrian
cabal --project-file="$PWD/hadrian/cabal.project" exec \
  --with-compiler=/home/tritlo/.ghcup/bin/ghc-9.14.1 \
  --disable-documentation --disable-profiling --disable-library-profiling \
  hadrian -- --directory "$PWD" -j6 --flavour=default --docs=none
```

Use an absolute path for `--project-file`. A relative path resolved the package
locations incorrectly. Supply the required tools on `PATH`. On this WSL2 host,
run builds and checks through `/home/tritlo/Code/heavy-management`. Read its
current scripts before reuse. Preserve the queue's concurrency settings.

Local build scripts, logs, and memory records are in `_build/verification/`.
They are ignored build artifacts. The results above describe the pinned baseline;
they do not describe later source revisions.

## Verified singleton translation

The unmodified upstream `Data.Singletons` module compiled against the baseline
with a small `SBool` and `SingKind Bool` adapter. Its source blob was
`c6bf5970d85d28ed6edb4d887f14cfa127a01be2`.

The reference program passed at `-O0` and `-O2`, both with `-dcore-lint`. It checked:

- `Select True = Int` and `Select False = Bool`.
- Branch refinement through `STrue` and `SFalse`.
- Shared index use in `keepS sb (chooseS sb)`.
- Runtime input through `withSomeSing @Bool`.
- Non-strict behavior when `keepS` ignores its witness.
- Rejection of `keepS STrue False`.

The reification check confirmed this distinction:

```haskell
keepS @True undefined `seq` ()                 -- Terminates.
withSomeSing @Bool undefined (\_ -> ())        -- Diverges.
```

These results establish the reference encoding. They do not establish a frontend
implementation or full-package compatibility. HEAD has `base-4.23.0.0`. The
inspected `singletons` package excludes that version in its bounds.
`singletons-base` also targets the GHC 9.14 Template Haskell API.

The temporary evidence is at `/tmp/ghc-retained-bool-plan.DJqnmf/`:
`Oracle.hs`, `Reject.hs`, `LibraryOracle.hs`, `check.log`, and `library-check.log`.
The temporary directory can expire. The maintained comparison sources and
reproduction recipe are now in [reference/](reference/README.md).

## How to resume

1. Read this file and the current priority in [the plan](PLAN.md). The Nat/Vec
   source target passes. Continue the open M6 and M8 work in bounded steps.
2. Check `git status`, the current branch, and the source revision. Compare them
   with the baseline above before reusing build results.
3. Read the relevant primary sources in [the references](REFERENCES.md).
   Recheck proposal and package status when the work depends on it.
4. Complete the next bounded step in the plan. Keep changes small and reviewable.
   Run its acceptance checks. Keep the broader milestone open until its gate passes.
5. Update this status and the plan after a material result or scope change.
   Record the source revision, command, outcome, and remaining boundary.

Keep new research in `REFERENCES.md`. Keep implementation tasks in `PLAN.md`.
Do not turn the project index into an implementation log. Significant changes
to the language scope or Core strategy require a new design discussion.

## Retained constructor field checkpoint

Specification: `cae8e34694`. Core implementation: `151bd9d6ad`.
This checkpoint adds constructor field binders and sequential Core pattern
instantiation. The new API checks construction, retained consumption,
repackaging, fresh identities, nested Pi capture avoidance, payload identity and
result nonescape. It rejects flat instantiated-field APIs and interface
declarations that would discard the binders. It excludes even vacuous retained
constructors from native index domains and rejects unsupported promotion.

The API fixture uses an indexed equality-dictionary function as its payload.
It does not execute a stored Vec package. Source constructor signatures remain
gated by located GHC-43582. The next steps are source declaration/pattern
checking, coherent field identities during desugaring, and interface field-binder
encoding, followed by real Nat/Vec execution
and field-laziness controls. See [M10](PLAN.md#m10-retained-constructor-field-telescopes),
[the contract](CORE.md#retained-constructor-fields), and
[the pinned provenance](REFERENCES.md#retained-constructor-field-telescopes).

Validation passed on 2026-11-13:

- Both compiler stages and libraries: 17m17s, 6,800 MiB peak under 16 GiB.
- Final bootstrap Core API: passed, 984 MiB under 4 GiB. The initial API run
  passed before the extra guards. The new flat-API regression then failed
  against that library and passed after the guards were rebuilt.
- Bootstrap runtime: passed the unchanged stdout golden, including the new
  located source rejection, module execution and fingerprint/recompilation
  checks. Peak: 992 MiB under 4 GiB. Existing observations remain 277 imported
  and 146 source-context rows; these are compatibility checks for this change.
- Five registered normal-way tests: 39 cases, 34 skips, five expected passes,
  and no unexpected results or framework failures/warnings. Driver time:
  11.018s; command time: 1m43s. Peak: 2,562 MiB under 8 GiB.
- Separate CLI compatibility controls: 277 main imported observations, 177
  older compatibility observations, 13 closure controls, seven worker controls,
  existing lambda/context/proof/linear checks, both interface-version rejection
  directions, 12 literal guides, ordinary typing and single-expansion TH,
  14 native function/class observations, package/GHCi/Haddock smoke checks,
  nested typed splices and static pointers all passed. Peak: 306 MiB under 4 GiB.
- The final constructor gate initially failed because its imported interface
  was outside the one-shot compilation directory. The combined CLI wrapper
  exited there after its earlier controls passed. Copying the interface into
  that directory fixed the harness. Rerunning only the gate passed with
  GHC-43582 at `NativePackedVecGate.hs:5:14`. No compiler fix was needed.
- Fourmolu formatting of both fixtures, nonmutating parsing of the five changed
  compiler modules, cabal-gild and `git diff --check` passed. The full GHC
  testsuite was not run. Existing Cabal/Haddock warnings and Hadrian's libdir
  normalization assertion remain outside this change.

Artifacts use `_build/verification/native-constructor-fields-`. Main logs are
`full-build.log`, `api-v2.log`, `runtime.log`, `registered.log`,
`final-cli.log` and `source-gate-v2.log`. Source hashes cover 24 compiler files
and two fixtures. Harness hashes cover 32 files. Current archive manifests cover
27 bootstrap, 27 compatibility, 30 worker, 34 closure, 32 lambda and four source
gate files. Current hashes, the immutable interface reader, and both previous
worker-instantiation archives verify. No serialized declaration format changed.
Source, interfaces and runtime package execution remain pending in M10.

## Retained constructor interface checkpoint

Implementation: `6a0bfbf0fd`. Schema and pinned Agda provenance:
`65611cfdb8`. Constructor declarations now serialize aligned optional field
names. The reader allocates fresh runtime Ids in order. It keeps field scope
out of sibling constructors and rejects unsupported promotion. The field types
remain the only encoding of domains and multiplicities.

Recursive imports expose metadata before field-domain checks. Validate Core
constructor telescopes on worker/user type demand. Allocate named Id skeletons
with literal Many; Id construction forces multiplicity. Inspect metadata before
a domain constructor's full signature. This avoids reentering a validation thunk.
See the [interface provenance](REFERENCES.md#constructor-field-interface-provenance).

The fork is `10.1.20261113.2`, with interface header `1001202611132`.
The preserved version-1 reader still accepts its original interface. Both
readers reject the other version with CheckHiWay and IgnoreHiWay.

Validation uses `_build/verification/native-constructor-iface-`:

- Both compiler stages and libraries passed. The full rebuild took 27m38s and
  peaked at 9,600 MiB under 16 GiB. A later Note correction triggered another
  dependency check. Its first scope reached the actual 10m limit, exit 124;
  continuation passed in 3m44s, with 3,995 MiB peak. No compiler fix followed.
- The final bootstrap API and runtime fixtures passed under 4 GiB, with a
  combined peak of 1,154 MiB. The API checks Normal/MaximumCompression declaration
  round trips, repeated imports, vacuous binders, parameters, name tidying,
  shared payload Ids, construction/elimination/repackaging Lint, malformed scope,
  promotion and bounded self/mutual imported-domain rejection. Runtime stdout
  remains unchanged. These package payloads are equality-dictionary functions.
- All five selected registered tests passed across the initial run and a focused
  rerun. The first run had 39 cases, 34 skips and four passes. The API fixture
  missed an import after warning cleanup; adding it made the one-test rerun pass.
  Neither run had framework failures or warnings. Peaks were 1,922/1,776 MiB
  under 8 GiB. An earlier API setup fix initializes its package environment.
- Separate CLI controls passed: 277 current and 177 older imported observations,
  closure/worker/proof/class/lambda controls, 12 literal guides, ordinary typing
  and TH, package/GHCi/Haddock smoke checks, typed splices and static pointers.
  Version assertions needed the new header number; the copied guide harness
  also needed its 12 input files. The remainder alone was rerun and passed.
  Maximum peak across these scopes was 1,057 MiB under 4 GiB.
- Source constructor syntax still rejects with located GHC-43582. The full GHC
  suite was not run. Fourmolu fixture formatting, nonmutating compiler parsing,
  cabal-gild and whitespace checks passed. Existing Cabal/Haddock notices and
  Hadrian's libdir-normalization assertion remain outside this change.

The successful logs are `build-v4.log`, `build-v6.log`, `bootstrap-final.log`,
`registered-recheck.log` and `cli-remainder.log`; earlier CLI results are in
`cli-final-v2.log`. Source hashes cover 28 compiler files, two fixtures and
VERSION_DATE. Harness hashes cover 42 files. Archive manifests cover 27 bootstrap,
27 older compatibility, 30 worker, 34 closure, 32 lambda and four source-gate files.
The previous interface reader and earlier archives remain immutable.

This checkpoint tests binary declaration codecs through the API. It does not
execute a stored Vec package or establish full on-disk package compilation.
The next gate is the [standalone source example](examples/packed-vector/README.md).
Constructor/pattern checking, roles, PMC, desugaring and runtime argument counting
remain open in M10. Native unfoldings, optimization and profiling remain gated.

## Retained constructor source checkpoint

Implementation: `8e90e0386b`. The [packed-vector example](examples/packed-vector/README.md)
now compiles from Haskell source through both compiler stages. It declares its
own Nat, Vec and PackedVec. A separate consumer imports the producer's interface
and links its object. The producer source is absent from the consumer import path.
The program constructs, opens and repackages stored vectors without singleton
reification or a GHC API producer.

Constructor checking preserves each runtime binder through renaming, kind
checking, generalization and zonking. Field substitution remains lazy until the
parent datatype knot is complete. Role checking, source patterns, PMC and
desugaring preserve ordered field scope. Runtime argument counters handle retained
Pi binders. Automatic Typeable generation skips the unsupported promoted
constructor and retains the ordinary parent. The existing immutable application
matcher reads resolved expected-type metas before matching polymorphic payloads.
See the [Core contract](CORE.md#source-constructor-checking).

Validation on 2026-11-13:

- Both compiler stages and matching libraries build in 26m35s. Peak memory was
  6,458 MiB under the 16 GiB limit, with swap disabled.
- Five focused registered regressions pass. The run has 39 cases, 34 skips,
  no unexpected results, and no framework failures or warnings. Its scope peaked
  at 2,715 MiB under 8 GiB. The repository MR template requires regression coverage.
  The existing runtime fixture now reads the two example files directly.
- All 15 source observations pass in the standalone bootstrap and self-hosted
  runs and in the runtime fixtures. The Core API and bootstrap runtime regression
  pass. The final source/runtime scope peaked at 1,058 MiB under 4 GiB.
- Twelve located negative cases pass: StrictData, type data, existentials,
  contexts, refined universals, promotion, constructor field patterns, lazy
  patterns, let patterns, index mismatch, index escape and shared equations.
  They require GHC-83605, GHC-83865 or GHC-94726 as applicable. Panics and timeouts
  are failures. The standalone negative scope peaked at 37 MiB under 4 GiB.
- CorePrep and STG show a constructor with two runtime arguments and two lazy
  fields. Repacking passes both original fields. `ignorePacked` inspects only
  the package constructor. The consumer retains its IORef read.
- Fourmolu parses all 19 changed compiler files and checks the runtime fixture.
  Its upstream parser needs the fork-only extension pragma removed to parse the
  example files. The fork compiles the unchanged examples. Cabal-gild ran on an
  ignored copy of the generated Cabal file. Whitespace checks pass.

Artifacts use `_build/verification/native-source-package-`. The main commands
are `build.sh`, `native-source-package-registered.sh`,
`native-source-package-selfhost-run.sh`, `native-source-package-api.sh` and
`native-source-package-runtime.sh`, with the resource limits above. Successful
logs are `build-v1.log`, `registered-v1.log`, `selfhost-run.log`, `api-v1.log`,
`bootstrap-runtime-v2.log` and `negatives-v1.log`. Source, documentation, harness
and executable-artifact manifests accompany them. The old interface readers and
archives remain unchanged. This increment does not change the interface schema.

The initial source package has one constructor, bare variable fields and one
equation per eliminator. Complete signatures and the prototype's O0/interface
flags remain required. Native unfolding optimization, profiling, records and
general projections remain separate work. The full GHC suite was not run.
The following checkpoint implements the filter with a length computed at runtime.

## Source vector filter checkpoint

Implementation: `c9aa3545c0`. The [source example](examples/packed-vector/README.md#filter-with-a-computed-result-length)
now filters `[1,2,3]` with cutoffs read through an IORef. It returns lengths 3,
2 and 0, with the corresponding vectors. Each retained element constructs
`PackVec (S n) (VCons x ys)`. Storing `n` instead rejects with GHC-05617 at the
constructor application. The compiler checks agreement between the output
fields. Element selection and order have separate runtime checks.

Direct package `case` patterns use GHC's existing multiplicity constraints.
Their fresh multiplicity variable can resolve to `Many`; linear and rigid
multiplicities retain ordinary diagnostics. Native simplification also expands
the two wired-in section helpers through their existing compulsory templates.
This prevents the representation-polymorphic temporary for `(> cutoff)` from
reaching code generation. Partial sections keep their lambda and remain lazy.
Both changes have GHC Notes. The [references](REFERENCES.md#source-vector-filter)
record the Agda filter structure and the GHC-specific lowering rules.

Validation on 2026-11-14:

- Both stages compile and run the separate-module source example with Core and
  STG Lint. The consumer imports only the producer interface and links its object.
  All 36 observations pass: 15 package, 13 filter and 8 section checks.
- The resumed self-hosted compiler target and its dependencies build in 4m12s.
  This is an incremental build after the requested pause, not a clean build.
  Peak memory was 4,718 MiB under 16 GiB, with swap disabled.
- All five focused registered tests pass: 39 cases, 34 skips, no unexpected
  results, and no framework failures or warnings. The driver took 20.624s; the
  complete job took 3m24s and peaked at 3,904 MiB under 8 GiB.
- The bootstrap Core API and runtime fixture pass. The initial combined job
  passed the source executable and API, then stopped on a diagnostic-text
  expectation: API printing retains the tick in `'S`, while CLI printing omits
  it. The fixture now checks the constructor expression, code and location.
  The corrected runtime run peaked at 1,045 MiB under 4 GiB.
- Sixteen source rejections pass. The four additions cover linear clause and
  case patterns (GHC-18872), rigid multiplicity (GHC-25897), and wrong filter
  length (GHC-05617). The standalone negative run peaked at 38 MiB under 4 GiB.
- CorePrep/STG retain the two runtime package fields and the filter recursion.
  The temporary section helpers are absent. The immutable previous-format
  reader's three hashes still match. No interface schema change was needed.
- Fourmolu parses the two compiler files, checks the fixture, and checks the
  examples with their existing leading-comma style. Its upstream parser needs
  only the fork extension pragma omitted; the fork compiles the actual sources.
  Cabal-gild ran on an ignored copy of the generated Cabal file. Whitespace
  checks pass. The repository MR template requires regression coverage.

Artifacts use `_build/verification/native-filter-source-`. Successful logs are
`selfhost-build-v2.log`, `selfhost-run-v1.log`, `api-v1.log`,
`bootstrap-runtime-v2.log`, `registered-v1.log` and `negatives-v2.log`.
`build-input.SHA256SUMS`, `source.SHA256SUMS`, `docs.SHA256SUMS` and
`harness.SHA256SUMS` identify the inputs. The source work directories contain
their own output manifests. The first failed expectation remains in
`bootstrap-runtime-v1.log`.

This filter scans a finite input spine before it returns a package. It leaves
retained elements lazy when the predicate permits it. It does not prove the
output-length bound or predicate correctness. The following M12 checkpoint adds
a source `LE` proof for the bound. Broader package
patterns, general projections, native optimization and proof erasure remain
separate work. The full GHC suite was not run.

## Source filter bound proof checkpoint

Implementation: `7eae40be91`; prerequisite frontend fix: `4ad47fdeca`.
The [source example](examples/packed-vector/README.md#filter-with-a-checked-length-bound)
now returns a runtime length, a vector at that length and an `LE` proof that
the output length is at most the input bound. It uses ordinary GADTs and the
existing retained constructor fields. No new Core form or interface format is
required. The retained branch constructs `S len`, `VCons x ys` and `LES proof`.
The rejected branch weakens only the proof's upper bound. The constructors and
weakening function follow the [pinned Agda definitions](REFERENCES.md#source-filter-bound-proof).

The source observer exposed a frontend gap in `evaluate proof >> pure ()`.
Dictionary selectors put their method foralls after the class constraint.
Native instantiation now matches across that invisible prefix and retains an
immutable plan in binder order. Existing IALL and `instCall` emit each group's
type and evidence applications in the original order. The usage cache, runtime
scope checks and final subsumption checks remain in place. Native types do not
enter mutable type metas. See
Note [Immutable instantiation of nested contexts] in `GHC.Tc.Gen.App`.

Validation on 2026-11-14:

- Both compiler stages compile and run the producer and interface-only consumer
  with Core and STG Lint. All 58 observations pass: the previous 36 plus 22 bound
  and proof-demand checks. Cutoffs 0, 1 and 3 produce vector lengths and proof
  depths 3, 2 and 0. No GHC API producer adds these source declarations.
- Nineteen package/bound rejection checks pass in the runtime fixtures. The
  three new source mutations separately break the vector length, proof lower
  index and proof upper index. They reject with located GHC-05617, GHC-05617
  and GHC-25897 diagnostics. The standalone harness pins the same results.
- The bootstrap compiler build took 51.86s and peaked at 2,040 MiB. The
  self-hosted compiler target and its dependencies built in 13m33s and peaked
  at 6,276 MiB. Both jobs used 16 GiB caps with swap disabled.
- The bootstrap runtime passes at 1,053 MiB under 4 GiB. The self-hosted source,
  Core API and pinned negative checks pass together at 1,118 MiB under 4 GiB.
- All five focused registered tests pass: 39 cases, 34 skips, no unexpected
  results, and no framework failures or warnings. The driver took 11.864s;
  the complete job took 1m56s and peaked at 2,923 MiB under 8 GiB.
- CorePrep/STG show three runtime constructor fields. Proof observations remain
  separate from package and vector demand. The immutable previous-format reader
  remains unchanged. Fourmolu parsing/checks, an ignored Cabal-gild copy and
  whitespace checks pass. The repository MR template requires regression coverage.

Artifacts use `_build/verification/native-filter-bound-`. Successful logs are
`bootstrap-build-v1.log`, `bootstrap-run-v1.log`, `bootstrap-runtime-v1.log`,
`selfhost-build-v1.log`, `selfhost-run-v2.log`, `api-v1.log`, `negatives-v2.log`
and `registered-v1.log`. The initial `selfhost-run-v1.log` records the scoped
IO error before the frontend fix. Input, source, documentation, harness and
output manifests identify this checkpoint. The Agda snapshots are in
`prior-art/` under the same prefix.

The proof is an ordinary lazy runtime field. Package WHNF and vector observation
can ignore an undefined proof. A defined proof constructor can contain an
undefined recursive proof. A bound interpretation requires terminating indices
and a terminating proof. This does not establish totality, predicate selection
correctness or general proof erasure. Native optimization, broader package
patterns and general projections remain separate work. The full GHC suite was
not run.

## Nested source package pattern checkpoint

Implementation: `7a98362197`; source target and provenance: `0b87e9fe16`.
The [source library](examples/packed-vector/README.md#nested-package-patterns)
now checks complete consumers with nested vector and proof patterns:

```haskell
headPacked :: PackedVec a -> Maybe a
headPacked (PackVec n VNil) = Nothing
headPacked (PackVec m (VCons x xs)) = Just x
```

Retained fields remain bare source variables. Ordinary fields use `tcConArg`,
so nested GADT evidence scopes over later fields and the RHS. Each package
equation compiles to its own case on the shared scrutinee. Existing
`MatchResult` failure continuations preserve source clause order. The compiler
does not merge different index Ids through let aliases. PMC uses each checked
outer pattern type and retains sequential speculative field instantiation.
Ordinary constructor-family compilation keeps its existing path. Core and
interface formats are unchanged. See Note [Retained package equation scope]
in `GHC.HsToCore.Match.Constructor` and the
[pinned Agda sources](REFERENCES.md#source-package-patterns).

Validation on 2026-11-14:

- Both compiler stages compile and run separate producer and interface-only
  consumer modules with Core and STG Lint. All 81 observations pass: the
  previous 58 plus 23 nested-pattern, wildcard and clause-order observations.
  Runtime cutoffs produce heads `Just 1`, `Just 2`, `Nothing` and proof depths
  3, 2, 0. The source snapshots match the committed examples exactly.
- The runtime fixtures check 20 package/bound rejections. The previous shared
  equation restriction is replaced by positive guarded clauses. New checks
  reject a nested index escape with located GHC-05617 and a retained-field
  wildcard with GHC-83605. A standalone harness pins those results and the
  retained-field constructor rejection. No GHC API producer adds the examples.
- The successful bootstrap compiler build took 22.14s and peaked at 1,876 MiB.
  The self-hosted compiler target and dependencies built in 17m26s and peaked
  at 4,844 MiB. Both jobs used serial 16 GiB caps with swap disabled.
- The bootstrap source run peaked at 96 MiB under 4 GiB. The self-hosted
  source, Core API, bootstrap runtime and pinned negative checks pass together
  at 1,203 MiB under 4 GiB.
- All five focused registered tests pass: 39 cases, 34 skips, no unexpected
  results, and no framework failures or warnings. The driver took 25.560s.
  The full job took 3m41s and peaked at 3,029 MiB under 8 GiB.
- CorePrep/STG preserve distinct `n` and `m` fields in separate cases on the
  shared scrutinee. Proof/wildcard and catchall observations preserve the
  checked field demand. The previous-format reader's three hashes are
  unchanged. Fourmolu checks, compiler parsing, an ignored Cabal-gild copy and
  whitespace checks pass. The Default MR template requires regression coverage.

Artifacts use `_build/verification/native-package-pattern-`. Successful logs
are `bootstrap-build-v2.log`, `bootstrap-run-v1.log`, `selfhost-build-v1.log`,
`selfhost-run-v1.log`, `api-v1.log`, `bootstrap-runtime-v1.log`,
`negatives-v2.log` and `registered-v1.log`. Input, source, documentation,
harness and output manifests identify this checkpoint. The saved Agda
`Compile.hs` has Git blob `962a80bbaee38601c392a2f9bfd2af8ea0f972d1`.
The initial build log records a missing `hsPatType` import, fixed before v2.
The first probe needed its imported interface copied to the one-shot hidir;
the corrected baseline probes are in `probes-v2/`. These are separate from
the passing source execution and final negative checks.

Retained index constructor patterns, lazy package patterns, pattern bindings,
linear package elimination and general projections remain gated. Native
optimization, proof erasure and totality remain separate work. The full GHC
suite was not run. The source uncons target is verified below.

## Source vector uncons checkpoint

Implementation: `ed5b0f56e4`; source target and provenance: `5a297d202e`.
The Core Tidy adaptation was specified at `296eb26a01` after the full source
observer exposed the failure. The
[source library](examples/packed-vector/README.md#uncons-with-a-runtime-predecessor)
now defines `unconsPacked` and `toListPacked`. In the successor branch,
`PackVec k rest` stores the actual predecessor obtained from `S k`.
The checked case rule transports the captured vector. Ordinary GADT evidence
relates its tail to `k`. No singleton or inferred runtime predecessor is added.

The full observer's `S bottomLength` argument passed typechecking but failed
Core Lint after Tidy globalised `bottomLength`. Before tidying a native RHS,
the compiler now creates lazy local aliases for its free top-level indices.
Existing Core substitution replaces term, type and coercion occurrences
together. The alias RHSs retain the original top-level references. CorePrep's
existing native alias guard preserves the local index. Ordinary RHSs, the
index grammar and interface format keep their existing paths. See
Note [Local indices across Core Tidy] in `GHC.Iface.Tidy` and the
[pinned Agda sources](REFERENCES.md#source-vector-uncons).

Validation on 2026-11-14:

- Both compiler stages compile separate producer and interface-only consumer
  modules with Core and STG Lint. All 102 observations pass: the previous 81
  plus 21 uncons and draining observations. IORef lengths 0, 2 and 3 give
  `Nothing`, a tail of length 1, and a tail of length 2. Filtered inputs drain
  in their original element order. Result, pair, tail package, predecessor,
  head and payload demand are observed separately. A bounded cyclic prefix
  produces `[42,42,42]`.
- The Core API and bootstrap runtime controls pass. The runtime fixture
  checks 22 package/bound rejections. A separate harness pins three located
  GHC-25897 errors for the original length, successor length and an independent
  length used to package the tail. Some relevant-binding details still show
  generated capture names; the primary errors identify source locations.
- The bootstrap compiler target built in 28.72s and peaked at 2,027 MiB.
  The self-hosted target and dependencies built in 16m39s and peaked at
  4,698 MiB. Both jobs used serial 16 GiB caps with swap disabled.
- The bootstrap source run peaked at 99 MiB under 4 GiB. Core API and runtime
  checks peaked at 1,129 MiB under 4 GiB. The final self-hosted source and
  pinned rejection checks peaked at 108 MiB under 4 GiB.
- All five focused registered tests pass: 39 cases, 34 skips, no unexpected
  results, and no framework failures or warnings. The driver took 26.071s
  and the job peaked at 1,735 MiB under 8 GiB. The initial direct invocation
  omitted Hadrian's Make/tool environment. Its printer failure is preserved;
  v2 supplies that environment and runs the same five tests.
- The 22 compiler input hashes and the previous-format reader's three hashes
  remain unchanged. Both source snapshots match the example files. Fourmolu
  checks, compiler parsing, an ignored Cabal-gild copy and whitespace checks
  pass. The Default MR template requires regression coverage.

Artifacts use `_build/verification/native-uncons-source-`. Successful logs
are `bootstrap-build-v1.log`, `bootstrap-run-v2.log`, `selfhost-build-v1.log`,
`selfhost-run-v1.log`, `api-v1.log`, `bootstrap-runtime-v1.log`,
`negatives-v2.log` and `registered-v2.log`. `bootstrap-run-v1.log` preserves
the original Tidy Core Lint failure. Input, source, documentation, harness
and output manifests identify this checkpoint. The saved Agda vector source
has Git blob `8c03fb8a83e6cec1fa7b8e1f64bb7e606c531cb8`.

The source programs run at `-O0` with the existing native interface gates.
Native optimization, general projections, retained index patterns, proof
erasure and totality remain separate work. The full GHC suite was not run.
Next, probe runtime equality evidence and use it to zip two source packages.

## Source runtime equality and aligned zip checkpoint

Source and fixture commit `f3350988c3`, with frontend commit `78460ce4f4`,
passes all 136 source observations in both compiler stages. The consumer
imports only the producer interface, then links its object file. Core and
STG Lint run in both stages. The bootstrap runtime fixture and registered
runtime test pass all 27 package/bound rejection checks. Four standalone
equality-scope errors have pinned GHC-83865 codes and primary source tokens
in both stages. All five focused registered regressions pass: 39 cases,
34 skipped, five expected passes, no framework warnings or failures.

`sameNat` compares two independently supplied runtime lengths. Its successful
result contains ordinary `:~:` evidence. `zipPacked` matches `Refl` before
zipping their vectors. The source observations cover independent IORef
inputs, runtime-filtered vectors, mismatch laziness, package order, partial
lengths, vector tags, pair components and tails. Equal-length success
compares both finite Nat spines. `Nothing` carries no negative proof.
See the [source guide](examples/packed-vector/README.md#runtime-equality-and-aligned-zip).

The consumer required three bounded frontend changes. Index resolution
normalizes assigned Id metadata and validates current bindings. Result
matching can select a complete index while an ordinary payload type remains
unresolved. Fully applied wired-in `($)` can use a complete expected or
independently declared result and one cached right operand. It preserves the
original operator, concrete representation checks and final subsumption.
Higher-rank callbacks keep their expected-checking path. These changes add
no Core rule or interface format. The fork version remains
`10.1.20261113.2`.

The final bootstrap compiler build took 46.36 seconds and peaked at
1,852 MiB under 16 GiB. The self-hosted compiler target and dependencies
took 9m08s and peaked at 5,274 MiB under 16 GiB. Bootstrap source execution
peaked at 106 MiB under 4 GiB. Bootstrap runtime and standalone rejection
checks peaked at 999 MiB under 4 GiB. Self-hosted source and standalone
rejection checks peaked at 117 MiB under 4 GiB. The registered normal-way
run took 12.620 seconds and peaked at 1,647 MiB under 8 GiB. All final
commands passed with zero recorded swap.

Artifacts use `_build/verification/native-equality-zip-`. Final compiler
inputs are in `build-input-v5.SHA256SUMS`. Successful source logs are
`bootstrap-run-v5.log` and `selfhost-run-v2.log`. The updated runtime log is
`bootstrap-runtime-v2.log`; standalone diagnostics are `negatives-v3/` and
`negatives-v4/`. The registered command and environment are in
`registered-argv.json` and `registered-env-v1.json`. Source, documentation,
harness and output manifests identify the checkpoint. Earlier compiler,
source and harness failures remain saved. The previous-format reader,
executable and interface fixture retain their original three hashes.

An exploratory imported-global probe reached an unlocated late-pipeline
gate. It was excluded from the located rejection counts. M16 resolves the
declaration classification and checks the actual index restriction below. Native optimization,
general index expressions, totality and proof erasure remain separate work.
The full GHC suite was not run. Next, probe source append with a computed
runtime length and ordinary equality evidence. The
[M16 checkpoint](#source-append-with-a-runtime-sum-checkpoint) records that extension.

## Source append with a runtime sum checkpoint

Source and fixture `fba00f8f0c`, with compiler changes `491c5fb0c9`, pass all
180 source observations in both compiler stages. `sumNat` returns a runtime
length `p` and ordinary evidence `p :~: Add n m`. `appendPacked` matches
`Refl` before repackaging the appended vector at that length. Independent
IORef inputs and runtime filters preserve the left elements before the right
elements. The [source guide](examples/packed-vector/README.md#append-with-a-computed-result-length)
shows the implementation and demand observations.

The runtime fixture checks 34 package/bound rejections in bootstrap and
registered runs. Seven standalone sum/append errors pin GHC-05617 or GHC-83865
and the primary source token in both stages. A separate declaration-only
producer and interface-only consumer pass three observations: inferred
constructor parameters, partial VTA, and an unused retained field. The
self-hosted declaration checks also preserve the optimization, unfolding,
Core-serialization and profiling gates. Those option failures are separate
from the located source-error counts.

Constructor result matching now follows validated field metadata through a
fully applied real constructor's current signature. It chooses only outer
erased parameters. It keeps the actual instantiated telescope and normal
proof checking. An unused retained binder can have a canonical ordinary Many
arrow. Matching checks that arrow's closed domain. Module classification
also includes local retained worker types before implicit bindings are
generated. This gives declaration-only modules the existing checked native
pipeline. These changes add no Core rule or interface schema.

The final incremental bootstrap compiler build took 22.96 seconds and peaked
at 1,627 MiB under 16 GiB. The self-hosted compiler target and dependencies
took 9m10s and peaked at 4,510 MiB under 16 GiB. Final bootstrap source,
runtime and standalone diagnostic checks peaked at 1,022 MiB under 4 GiB.
Self-hosted source, diagnostics and capability checks peaked at 120 MiB
under 4 GiB. All five registered tests passed: 39 cases, 34 skipped, no
framework warnings or failures. The driver took 12.064 seconds and peaked
at 1,659 MiB under 8 GiB. Final commands recorded zero swap.

The earlier imported-global probe has a vacuous Boolean binder. GHC
canonicalizes that binder to an ordinary arrow, so its imported `otherwise`
argument is valid. It now compiles. A separate genuinely dependent stored
Boolean still rejects `otherwise` with GHC-83605 at that token. These two
scope controls passed at 49 MiB under 4 GiB.

Artifacts use `_build/verification/native-append-sum-`. Final compiler inputs
are in `build-input-v3.SHA256SUMS`. Passing source logs are
`bootstrap-run-v2.log` and `selfhost-run-v1.log`; the runtime log is
`bootstrap-runtime-v1.log`. Diagnostic directories `negatives-v2/` and
`negatives-v3/` retain sources, commands, messages and primary tokens.
`gates-v1/` records declaration capability checks. Registered command and
environment files are retained. Source, documentation, harness and output
manifests identify the checkpoint. The original declaration/application
failures and a failed bootstrap build remain saved. M15's 31 harness and
337 output hashes still match. The protected previous-format reader retains
its original three hashes.

The sum witness scans the entire first length before it returns a constructor.
A finite first length can leave the right length and vector payloads lazy.
The proof is an ordinary lifted field. Totality, proof erasure, native
optimization and general index expressions remain separate work. The full
GHC suite was not run. Next, probe constructor patterns on retained package
fields in [M17](PLAN.md#m17-constructor-patterns-on-retained-package-fields).


## Retained field constructor pattern checkpoint

Compiler implementation: `ab985a150e`; import cleanup: `f423a0ce97`.
Source and runtime fixture: `e54802bcfd`. Both compiler stages pass 209 source
observations with Core Lint and STG Lint. The separate consumer uses only the
producer interface and object. The bootstrap and registered runtime fixtures
pass 40 package/bound rejections. Seven new standalone pattern errors and the
seven existing sum/append errors have pinned codes and primary source tokens
in both stages. All five focused registered regressions pass.

The public source worker now reads:

```haskell
unconsPacked :: PackedVec a -> Maybe (a, PackedVec a)
unconsPacked (PackVec Z _) = Nothing
unconsPacked (PackVec (S k) (VCons x rest)) = Just (x, PackVec k rest)
```

The second row refines the vector using the actual stored length and packages
its tail with the actual predecessor `k`. The zero row leaves the vector lazy.
A separate `PackVec Z VNil` check confirms that an explicit vector pattern
forces that vector. The 29 new observations also cover two retained fields,
ordinary equality proof demand, partial predecessors, other arguments, views,
guards and fallback rows. A failed pattern can try another row. A demanded
bottom cannot.

`tcRetainedCon` is shared by retained function, case and package pattern
checking. A fresh whole-field Id sits in an outer generated as-pattern.
The inner constructor alternative binds the existing nominal index equality
using the actual source subfield Ids. Later field types receive that transport
in telescope order. Checked coercion patterns retain their original outer
types. Ordinary pattern checking keeps GADT evidence in scope. Existing
per-equation match compilation preserves source row order. No Core rule,
interface schema or backend lowering rule changes in this increment.
See [the pinned Agda LHS and RHS sources](REFERENCES.md#retained-constructor-fields-in-source-patterns).

Retained fields currently admit bare variables or one constructor layer with
bare variable subfields. Nested retained constructors, wildcards and source
as-patterns remain gated. This restriction does not apply to ordinary vector
or proof fields. The proof fields remain ordinary lazy Haskell fields. These
runtime observations do not prove totality, preservation or general proof
erasure. The full GHC suite was not run.

Artifacts use `_build/verification/native-index-pattern-`. The final compiler
input manifest is `build-input-v6.SHA256SUMS` with 26 paths. Exact Agda source
snapshots are in `prior-art/`. The initial rejection and three failed
bootstrap builds remain saved. `bootstrap-run-v4.log` and
`selfhost-run-v1.log` record the final source output. The final runtime log is
`bootstrap-runtime-v2.log`. Pattern diagnostics use `negatives-v4/` and
`negatives-v5/`; sum diagnostics use `legacy-negatives-v3/` and
`legacy-negatives-v4/`. Source, documentation, harness and output manifests
identify the checkpoint. M16's 26 harness and 326 output hashes still match.
The protected previous-format reader retains its original three hashes.

The self-hosted compiler target and dependencies took 10m40s and peaked at
5,640 MiB under 16 GiB. That build reported the redundant imports. The final
cleanup build rebuilt both stages in 1m54s and peaked at 3,883 MiB under
16 GiB, with no source warnings. Final source, bootstrap runtime and both
stages' diagnostic checks peaked at 1,030 MiB under 4 GiB. All five registered
tests passed: 39 cases, 34 skipped, no framework warnings or failures. The
driver took 12.844 seconds and peaked at 1,622 MiB under 8 GiB. The resource
wrapper disables swap. [M18 source reverse](PLAN.md#m18-source-reverse-with-the-original-stored-length)
is verified below.


## Source reverse with a preserved length checkpoint

Compiler implementation: `12bfbef0b3`. Source and runtime fixture:
`9fefa1a496`. Both compiler stages pass 249 source observations with Core Lint
and STG Lint. The bootstrap and registered runtime fixtures pass 47
package/bound rejections. Seven standalone reverse errors have pinned codes
and primary source tokens in both stages. The source consumer uses the
producer's interface and object with the producer source absent from its
import path. All five registered regressions pass: 39 cases, 34 normal-way
skips and five expected passes, with no framework warnings or failures.

`reverseAppendNative` reverses one vector onto an accumulator and returns
`Vec a (Add n m)`. The ordinary equality proofs `addSucc` and `rightZero`
follow Agda's addition lemmas. Matching `Refl` permits the accumulator step
and the final result at `n`. `reversePacked` stores the original runtime `n`
with the delayed vector. The native worker's signature relates input and
output lengths. It does not specify reversal order. Separate finite observations
check distinct elements selected by runtime predicates and an independently
sized accumulator.

The wrapper can return package WHNF without evaluating either field. Reading
the stored length does not demand the reversed payload. Demanding that payload
first completes the right-zero proof over the Nat spine, then scans the vector
spine. Elements remain lazy. The reverse-append worker has its own demand order:
it inspects the current vector constructor before its recursive successor proof.
The observations distinguish those demands and partial applications. Repeated
proof induction can traverse the remaining Nat spine at each step. This is not
a linear-time or totality claim. These proofs remain ordinary lifted values.

The consumer exposed `const (42 :: Int) (rightZero n)` under nested inference.
The signed literal was not eligible for immutable argument inference. Skipping
it left the first parameter unknown; ordinary fallback could not instantiate
the native proof type. Complete signed expressions now enter the existing
infer-once cache. A read-only syntax guard excludes wildcards, outer quantified
variables, written inner quantifiers, retained binders, contexts and type
splices. Normal signature checking and all final argument/result checks remain.
A signature supplies a checking type, not an equality proof. Both signed
argument positions and a signed function pass the controls in both compiler
stages. A wrong annotation still produces a located GHC-83865 error. See
[the exact source precedent](REFERENCES.md#signed-expression-arguments-in-native-inference).

The first proof used constructor equations with two retained arguments.
At M18, that form rejected with the misleading result-wrapper diagnostic.
The M18 source used the explicit-case form, which binds both runtime
indices before discrimination. M19 below now supports those equations. [M19](PLAN.md#m19-constructor-equations-with-later-retained-arguments)
records the required shared-identity extension and its source-error obligations.
No Core rule or interface schema changed in M18. Optimization, arbitrary index
expressions, general inference, totality and proof erasure remain separate work.
The full GHC suite was not run.

Artifacts use `_build/verification/native-reverse-source-`. Final compiler
inputs are in `build-input-v1.SHA256SUMS`, with 26 paths. Initial clause and
consumer failures remain saved. Five exact prior-art snapshots are in
`prior-art/`. The source, documentation, harness, outputs and per-directory
files have separate SHA-256 manifests. M17's 42 harness hashes, 499 output
hashes and the original three reader hashes remain unchanged.

The bootstrap compiler build took 25.73 s and peaked at 1898 MiB under 16 GiB.
The self-host build took 9m30s and peaked at 5172 MiB under 16 GiB. The final
bootstrap checks peaked at 1054 MiB under 4 GiB; the self-host source, reverse
errors and annotation controls peaked at 145 MiB under 4 GiB. The registered
run took 12.255137 s and peaked at 1718 MiB under 8 GiB. The resource wrapper
sets `MemorySwapMax=0`. Initial failures and final logs remain saved.


## Constructor equations with later retained indices checkpoint

Compiler: `fdf23acaea`, with local-comment cleanup at `a367bd0a7a`.
Source and runtime fixture: `dc44d9d25c`, with the missing-proof rejection at
`6f7d5f7e5f`. Both compiler stages pass 269 source observations. The bootstrap
and registered runtime fixtures pass 55 package/bound rejections. Eight new
errors have pinned GHC-83605/GHC-83865 codes and primary source tokens in both
stages; the previous seven reverse errors also pass. All five registered
regressions pass: 39 cases, 34 skips and five expected passes, with no framework
warnings or failures.

The existing `addSucc` and `reverseAppendNative` now use constructor equations.
Their later argument has different source names in each row. The checker
renames those resolved Names to one shared runtime identity per position,
including local captures and stored free-name sets. It preserves source spelling,
spans and original equations for diagnostic context. Shared indices remain
outermost lambda variables. Ordinary payloads still receive the existing
constructor transport, and evidence has separate specialised Givens.

Twenty new source observations cover multiple later indices, ordinary values
between retained positions, local captures, shadowing, partial applications
and demand order. A primitive equality context between retained binders checks
that the branch can use its transported evidence. A leading `Show` context
checks ordinary class dictionaries before the retained telescope. This required
the signature selector, renamer and kind checker to continue through qualified
bodies with their existing constraint routines. [References](REFERENCES.md#later-retained-arguments-in-constructor-equations)
pin the Agda telescope rules and ordinary GHC context routines.

Later retained patterns still require bare variables. Both quotation forms
reject at the quotation expression before resolved Names can change. Wrong
result indices still reject, and an error in the second row prints that row's
original variable names. Unsupported ordinary patterns point at that pattern.
No Core rule or interface schema changed. The full GHC suite was not run.

Artifacts use `_build/verification/native-later-index-`. Final selected compiler
inputs are in `build-input-v5.SHA256SUMS`, with 27 paths. All five historical
input versions have exact archived bytes and verified hashes. The initial failed
build and the qualified-body source failure remain saved. Six exact prior-art
snapshots cover Agda telescopes, clause compilation, the next equality example,
and GHC's ordinary qualified-signature routines.

The initial self-host build took 12m08s and peaked at 5002 MiB under 16 GiB.
It reported the new local Haddock warning and pre-existing warnings in App,
Match/Constructor and Main/Passes. The final cleanup rebuilt both compiler
stages in 1m33s, at 2653 MiB under 16 GiB, without source warnings. Final
bootstrap source/API/runtime checks peaked at 1048 MiB under 4 GiB. Self-hosted
source and both diagnostic groups peaked at 147 MiB under 4 GiB. The registered
run took 13.905708 s and peaked at 1726 MiB under 8 GiB. The wrapper sets
`MemorySwapMax=0`.

M18's 31 harness and 372 output hashes, M17's 42 harness and 499 output hashes,
and the original reader's three hashes remain unchanged. The final source,
docs, harness, output and directory manifests are frozen separately.
[M20](PLAN.md#m20-constructor-equations-over-two-runtime-indices) below continues
with a complete two-index constructor matrix for `sameNat`.


## M20: source equations over two runtime indices

Both compiler stages and all five registered regressions pass.
`PackedVecLibrary.sameNat` now has four source equations. Each row refines
both runtime indices. The `S/S` row calls the same worker on its actual
predecessors. It must match the recursive `Refl` before returning `Just Refl`.
The source result stays `Maybe (n :~: m)`. `zipPacked` consumes that evidence
before calling the equal-length vector zipper.

The checker keeps shared whole-argument Ids and each row's actual field Ids.
It lifts the first constructor proof through a type. It lifts the second
proof through that result, then composes the coercions. Each proof sits on
its own inner `ConPat`, beneath the shared whole-argument `AsPat`. Payloads
keep their original outer types. Each RHS casts back beneath both proofs.
The existing one-column path remains. Desugaring compiles proof-bearing rows
separately. All source argument lambdas precede discrimination. This change adds no Core or
interface forms or equality axioms.

Both compiler stages pass 301 observations through separate producer
and importer compilations with Core and STG lint. The previous 269 lines stay
in order. Sixteen IORef comparisons check lengths 0 through 3 against a private
nested-case worker and an independent expected table. Ten checks compare
partial WHNF, bottom order, lazy mismatches and bounded cyclic mismatch.
Four payload matrix cases place an ordinary vector argument before the second
retained column. Two more checks cover partial WHNF and index-before-payload
demand. The API/runtime fixture passes 63 package/bound rejections.

The initial matrix requires two constructor columns, complete unique pairs,
one unguarded RHS per row and bare constructor subfields. Other retained
positions keep M19's bare-variable rule. Contexts within this matrix telescope,
mixed constructor/variable rows and deeper constructor fields reject. This
restriction applies to dependent checking. `mkDepFunTy` turns an unused retained
binder into an ordinary unrestricted arrow and keeps the runtime argument.
The coverage probes use `Maybe (n :~: m)` even when every RHS is `Nothing`.

The eight new standalone probes check incomplete and duplicate pairs, a wrong
domain, a nested field, both false zero/successor proofs, a missing recursive
proof and an interior context. The existing eight later-index and seven
reverse probes also pass in both stages. The old mixed `LaterConstructor`
probe now reports the matrix pattern error at its later variable.

The pinned [Agda equality example and split-tree compiler](REFERENCES.md#two-index-constructor-equations)
supply the program and compilation precedents. `Nothing` carries no refutation.
Equal cyclic inputs have no termination claim. The next source probe is an
ordered trailing variable fallback. It must preserve proof scope and the
same demand order before this pattern restriction can be relaxed.


Validation uses `_build/verification/native-index-matrix-*`. Final compiler
bytes are in `build-input-v3.SHA256SUMS` with 27 selected inputs. Three input
archives preserve both failed compiler attempts and the final implementation.
The initial compiler probes found a pretty-printing `(<>)` collision and a
list/`NonEmpty` mismatch. Both are fixed. Direct `evaluate` and `seq` reference
probes reported GHC-91028. The final reference checks use a generic WHNF helper
with fixed `IO ()` result. A global `ImpredicativeTypes` probe hit the existing
index-expression gate. It is not part of the accepted source flags.
The first coverage probe returned `Int` and used ordinary checking. The
corrected dependent result rejects at the first equation's group location.
All failed probes remain in the evidence. The full GHC suite was not run.

The final bootstrap compiler rebuild took 1m27s and peaked at 1823 MiB under
16 GiB. The self-hosted build took 12m49s and peaked at 5182 MiB under 16 GiB.
It reports pre-existing warnings in `App.hs`, `Constructor.hs` and `Passes.hs`.
The final bootstrap source run peaked at 146 MiB under 4 GiB. Bootstrap
matrix/API/runtime checks peaked at 993 MiB under 4 GiB. Self-hosted source
and all located errors peaked at 144 MiB under 4 GiB. The registered run took
12.425778 s and peaked at 1723 MiB under 8 GiB. The wrapper disables swap.

M19's 40 harness and 596 output hashes, M18's 31 harness and 372 output hashes,
M17's 42 harness and 499 output hashes, and the original reader's three hashes
remain unchanged. M19's 34 source and five document hashes also match commit
`7e2d2a6224`. Final M20 source, docs, harness, output and directory manifests
are frozen separately.


## M21: an ordered variable fallback from source

Both compiler stages and all five registered regressions pass.
The library now uses the ordinary ordered fallback form:

```haskell
sameNat :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)
sameNat Z Z = Just Refl
sameNat (S left) (S right) = case sameNat left right of
    Nothing -> Nothing
    Just Refl -> Just Refl
sameNat left right = Nothing
```

The final equation has no constructor proof. The checker renames both source
variable Names to the shared whole-index Names before TC. It checks the RHS
against the original result type and keeps its index VarPats unwrapped.
It uses reflexive transports, without calling the constructor-only lifting
helper. Constructor rows keep their actual fields and M20's two proofs.
GHC's existing ordered `PgCon` and `PgAny` blocks put fallback execution
outside failed row proofs. The backend and Core/interface forms stay the same.
Note `[Retained variable fallback]` records these invariants. The
[Agda catchall and GHC failure-continuation sources](REFERENCES.md#ordered-variable-fallback-equations)
record their exact blobs and ranges.

Both stages pass 316 observations through separate producer/importer
compilations with Core and STG lint. The first 301 lines remain unchanged.
The existing 16 finite equality pairs and ten demand checks now also compare
with a private four-clause worker. The nested-case reference remains.
Four new runtime package pairs repackage vectors under the actual fallback
indices. Four `zeroOnly` pairs check a worker with only `Z/Z` before fallback.
Seven new demand checks cover an untouched second index when the first
constructor is absent, the opposite demand, first-before-second errors,
partial function WHNF and lazy packages at mismatched partial indices.

The API/runtime fixture passes 70 package/bound rejections. Seven new
standalone errors reject an earlier fallback, a repeated fallback, wildcard
patterns, a forged `Refl`, a wrong-index payload and both quotation forms.
Their exact source positions are pinned. The previous 23 located errors
also pass in both stages. The earlier/repeated fallback error points to the
first misplaced variable pattern. A fallback has no equality between its
original indices. It cannot inherit evidence from an earlier failed row.

The initial extension accepts only one final equation with bare variables in
both selected index columns. Constructor pairs before it must remain unique.
Without it, constructor-pair coverage must remain complete. A row with one
constructor and one variable, an anonymous wildcard, guards and interior
contexts remain unsupported. Other retained columns retain their existing
bare-variable rule. Quotations reject before source Name renaming.
The next source task is an anonymous final fallback such as `sameNat _ _`.


M21 evidence uses `_build/verification/native-index-fallback-*`. The final
`build-input-v2.SHA256SUMS` has 28 selected inputs. It includes the original
`compiler/ghc.cabal.in` template. Compiler changes are `c3a068a7fe`; source and
runtime-fixture changes are `571ae3d690`. The implementation plan and pinned
Agda/GHC provenance were committed first in `80ba85869f`.

Compiler and source checks passed on the first attempt. A formatter check
used cabal-gild's positional in-place mode and changed the Cabal template's
formatting. The original bytes were restored from HEAD. The diff and formatted
bytes remain as evidence. The corrected command writes a separate output.
Both compiler targets then passed a verification build in 54.83 s, with no
compiler modules recompiled. Fresh producer/importer source executions in both
stages and the fresh API fixture pass against the restored template. The final
source and Cabal input bytes are pinned separately from the initial 27 inputs.

The bootstrap compiler build took 3m06s and peaked at 2710 MiB under 16 GiB.
The self-hosted build took 12m15s and peaked at 5009 MiB under 16 GiB. Existing
warnings remain in `App.hs`, `Constructor.hs` and `Passes.hs`. The verification
build peaked at 742 MiB under 16 GiB. Initial bootstrap source execution peaked
at 144 MiB under 4 GiB; source rejections and API/runtime checks peaked at
995 MiB. Self-hosted source and located errors peaked at 145 MiB under 4 GiB.
The final restored-state source/API checks peaked at 1111 MiB under 4 GiB.
The wrapper disables swap. The full GHC suite was not run.

M20's 42 harness and 514 output hashes, M19's 40 harness and 596 output hashes,
M18's 31 harness and 372 output hashes, M17's 42 harness and 499 output hashes,
and the original reader's three hashes remain unchanged. M20 and M19 source
and document hashes match commits `74cf40fd57` and `7e2d2a6224`. Final M21
source, docs, harness, output and directory manifests are frozen separately.


The final registered run took 13.666040 s and peaked at 1667 MiB under 8 GiB:
five tests, 39 cases, 34 skips and five passes. The earlier run also passed
in 13.129000 s at 1708 MiB. The complete compiler, source and registered checks
pass with the restored Cabal template. No Cabal file change remains.


## M22: anonymous terminal fallback from source

Both compiler stages and all five registered regressions pass. The library
now ends `sameNat` with `sameNat _ _ = Nothing`. Each selected fallback column
accepts a bare variable or wildcard. The checker replaces a selected wildcard
with a VarPat carrying its existing shared whole-index Name before TC. This
binds the actual index Id for later payload and result types. It introduces no
source-visible name, fresh index Id or equality proof. The fallback retains
reflexive transports and the original result type. Constructor rows keep their
actual fields and proofs. Core, lowering and interface forms stay the same.
Note `[Retained variable fallback]` records these rules. The
[Agda anonymous-context and GHC wildcard sources](REFERENCES.md#anonymous-terminal-fallback-equations)
are pinned by blob hash and exact source ranges.

Both stages pass 331 observations through separate producer/importer source
compilations with Core and STG lint. The original 316 outputs remain unchanged.
The existing 16 finite equality pairs and ten demand checks also compare with
a named fallback worker. Four new runtime pairs check `first _` and `_ second`
package workers against independent stored-length/content expectations.
A `_ _` zero-only worker also compares with its named reference. Eleven new
demand observations check a missing first constructor, a cyclic first index,
first-before-second failure, partial function WHNF and lazy package fields.
Forcing the selected payload still raises its original error.

The API/runtime fixture passes 76 package/bound rejections. Each compiler stage
also passes 42 standalone located errors: the previous 23 later-index, reverse
and matrix cases, plus 19 fallback cases. New controls reject forged Refl and
wrong-index payloads for all three wildcard combinations. They check original
source equation text, early or repeated fallbacks, both anonymous quotation
forms, an unsupported third-index wildcard and an unbound source name.
A wildcard supplies no user-visible binding. Mixed constructor/wildcard rows,
guards, contexts and other pattern extensions remain outside this probe.

The first source run used unsigned local vector bindings. The existing inferred
retained-type gate rejected both at their source Names. The final probe uses
inline payload expressions. Complete ambient-index local signatures were also
unsupported at M22. M23 below implements that source-scope rule. The initial
failure remains in the artifacts.

Evidence uses `_build/verification/native-index-wildcard-*`. The 28 selected
compiler inputs are archived in `inputs-v1` with `build-input-v1.SHA256SUMS`.
The plan/provenance commit is `df9600f757`; frontend `7b7586bfe0`; source and
existing runtime fixture `2435b1a7df`. The bootstrap build took 40.90 s and
peaked at 1836 MiB under 16 GiB. The self-hosted build took 7m59s and peaked
at 4333 MiB under 16 GiB. No GHC source warnings were emitted; Cabal's existing `${pkgroot}` prefix
warning remains. Bootstrap source
execution peaked at 151 MiB under 4 GiB; located errors and API/runtime checks
peaked at 996 MiB. Self-hosted source and located errors peaked at 147 MiB
under 4 GiB. The registered run took 12.427260 s and peaked at 1722 MiB under
8 GiB: five tests, 39 cases, 34 skips and five passes. The wrapper disables
swap. The full GHC suite was not run.

M21's 26 manifest bytes, 42 harness hashes and 600 output hashes remain
unchanged. Its 35 source hashes and five document hashes match `cdd782d28e`.
M20, M19, M18 and M17 harness/output evidence and the original reader's three
hashes remain intact. M22 source, docs, harness and outputs are frozen
separately.

## M23: local signatures at runtime indices

Both compiler stages and all five registered regressions pass. A complete
local value signature can refer to an eligible
already-bound runtime index under `ExperimentalDependentTypes`:

```haskell
n <- readIORef cell
let xs :: Vec Int n
    xs = replicateNative @Int n 7
```

The signature uses the actual runtime `n`. The renamer adds a signature-local
lookup alias before implicit forall collection. The checker validates the
existing unrestricted local Id and its closed index domain. It reads assigned
metadata without assigning a native metavariable, inventing equality or
evaluating `n`. Validity applies the same metadata normalization to the lexical
binding. Notes `[Local retained signature aliases]` and
`[Local runtime indices in signatures]` state these rules.

An erased type variable or explicit `forall` binder takes precedence. Under
the experimental flag, a free `m` in `depth :: LE n m -> Int` captures a local
runtime `m`. Use `forall n m.` to preserve erased polymorphic intent. The
example's proof observer now states that intent. Named wildcards keep their
ordinary meaning; a runtime `_n` requires `NoNamedWildCards`. This extension
does not infer unsigned local native bindings or capture globals and members
of the same recursive group.

Both source clients pass 343 observations, preserving the previous 331.
Twelve additions check finite IORef indices, repeated payload sharing, runtime
shadowing, erased-variable precedence, `_n`, partial and cyclic indices,
payload demand and deferred IO effects. The API/runtime fixture passes 87
package/bound rejections. Standalone probes pass 53 located errors, including
eleven new signature cases. Four ordinary-Haskell controls pass in both stages,
including RequiredTypeArguments capture of an erased variable. Source
compilation uses separate producer and importer modules with Core and STG Lint.

Evidence uses `_build/verification/native-local-signature-*`. Compiler inputs
are archived in `inputs-v1` and `inputs-v2`; the latter records the lexical
validity fix. The specification is `b84462b0bc`, frontend `d45eac2a85`, validity
fix `da16ec16e6`, source/API fixture `58fe0a1e10` and source formatting
`e4766d99ac`. Initial source failures remain recorded. No Core, backend or
interface extension was needed.

The final bootstrap rebuild took 21.90s and peaked at 1774 MiB. The self-hosted
build took 12m53s and peaked at 5200 MiB. Both used 16 GiB caps. Final bootstrap
source execution peaked at 149 MiB under 4 GiB. Its API/control run peaked at
1018 MiB. Self-hosted source, errors and both-stage ordinary controls peaked
at 231 MiB under 4 GiB. The registered run took 12.63s and peaked at 1725 MiB
under 8 GiB: five passes, 34 skipped ways and no unexpected results. Swap was
disabled for all jobs. Existing GHC and Cabal warnings remain. The full GHC
suite was not run. Source, docs, harness and outputs are frozen separately;
the M22 evidence and earlier runtime/interface records remain unchanged.

At M23, the saved direct `xs <- action` client failed when
`action :: IO (Vec Int n)`. M24 below implements that source path.

## M24: ordinary IO binds with native payloads

Both compiler stages and all five registered regressions pass. A saved action
preserves its native payload type through ordinary do:

```haskell
n <- readIORef cell
let action :: IO (Vec Int n)
    action = pure (replicateNative @Int n 7)
xs <- action
pure (toList xs)
```

The checker fixes bind's input type as `Vec Int n` before it checks the
generated continuation. It exposes existing expected-type assignments and
keeps native choices in an immutable substitution. Missing ordinary parameters
use GHC's standard fresh TauTvs. Inline `pure` can keep its ordinary head meta
until final result subsumption resolves it to IO. A separate joint match must
still satisfy every cached argument. Known native result choices also anchor
checking. Kind-first matching determines representation parameters for error
without a native meta assignment or special error rewrite.

Notes `[Available expected types in native applications]`,
`[Ordinary parameters in native applications]` and
`[Ordinary heads in native argument matching]` record these decisions.
[Pinned Agda lambda, implicit-argument and application sources](REFERENCES.md#direct-do-bind-of-a-saved-native-action)
supply the checking and scope precedents. GHC retains its own meta, kind,
scope, concreteness, dictionary-order and usage safeguards. The do translation,
Core forms, lowering and interface version remain unchanged.

Both source clients pass 360 observations, preserving the previous
343. Four runtime IORef lengths compare saved and inline do/bind forms and
two inferred local computations. Per-call counters check effects. Thirteen
further observations check deferred effects, ignored bottom fields, original
errors, partial and cyclic indices and shared payloads. The API fixture passes
94 package/bound rejections. Standalone probes pass 60 located errors and four
ordinary-Haskell controls. New errors cover independent indices, both wrong
monads, conflicting cached inputs, native-result inference, unsigned native
bindings and linear capture.

Evidence uses `_build/verification/native-action-bind-*`. Four compiler input
archives retain each implementation increment. The first self-hosted build
was interrupted by a VM restart. Its log is retained. The resumed build passed;
the final build includes the later expected-result fixes. The final bootstrap
build took 20.17s and peaked at 2049 MiB under 16 GiB. The final self-hosted
build took 6m22s and peaked at 5060 MiB under 16 GiB. Bootstrap source and
bottom probes peaked at 189 MiB under 4 GiB. Its rejection/API run peaked at
1027 MiB. Self-hosted source and controls peaked at 195 MiB under 4 GiB. The
registered run took 12.57s and peaked at 1844 MiB under 8 GiB: five passes and
34 skipped ways. No unexpected results occurred. Swap was disabled for all
jobs. Existing GHC/Cabal warnings remain; the full GHC suite was not run.
Source, docs, harness and outputs are frozen separately. M23 and earlier
evidence remain unchanged. At M24, native-result inference and unsigned native
bindings remained gated. [M25](#m25-scoped-native-source-inference) admits the
scoped source forms below.

## M25: scoped native source inference

Both compiler stages and all five registered regressions pass. Local values
and callback results can infer native types at an existing runtime index:

```haskell
n <- readIORef cell
let vector = replicateNative @Int n 7
    action = modifyIORef' effects (+ 1) >> pure vector
    compute = do
      xs <- action
      pure xs
xs <- compute
pure (PackVec n xs)
```

The existing single nonrecursive FunBind path infers its RHS before allocating
the Id. M25 admits the resulting monotype for a local NoGen value with one
zero-argument equation. The validity checker checks free runtime indices
against their actual lexical bindings. It also checks its existing deep
free-variable list, since the structural walk skips variable kinds and
coercions. Bound indices and erased CoVars are excluded from that list's
runtime checks. No native type is assigned to a meta-variable.

After an action fixes a callback input, the existing match checker infers the
body through an InferResult. Its result is checked after the callback context
closes. A callback-local or inner local runtime Id cannot escape in an
ordinary arrow result. Explicit retained arrows and packages keep their own
checking paths. Generic argument matching can unify ordinary head metas for
`effect >> pure vector`, then repeats exact whole-argument matching. Both heads
must contain no native syntax or outer templates. Final subsumption remains.

Notes `[Scoped inferred native values]`, `[Scoped native callback results]`,
`[Ordinary action heads in native inference]` and
`[Validity of retained binders]` record the rules.
[Pinned Agda let, lambda and contextual-meta sources](REFERENCES.md#scoped-native-source-inference)
supply scope and checking precedents. GHC retains its own laziness, usage,
FRR, promotion, generalization, kind, concreteness and evidence checks.
The callback admission starts with one ordinary Many argument, a variable
pattern and one unguarded equation. Recursive groups, pattern bindings,
partial signatures and inferred retained function telescopes remain gated.

Both source clients pass 377 observations, keeping the previous 360 unchanged.
The 17 additions cover four IORef inputs, repeated action effects, bottom
indices and payloads, original errors, partial/cyclic prefixes, shared vectors
and the original shared IORef payload. Both stages pass 69 located rejection
cases and four ordinary controls. Two M24 rejection forms are now positive;
the other 58 remain. Eleven new cases cover scope, independent indices,
generalization, GADT escape and linear capture. The API/runtime fixture passes
103 package/bound rejection cases. The five registered tests pass with 34
skipped ways, no unexpected results and no framework warnings.

Evidence uses `_build/verification/native-source-inference-*`. Three compiler
input archives each contain 31 selected paths. The final self-hosted build
took 11m28s and peaked at 5761 MiB under 16 GiB. Final both-stage source/API
checks peaked at 1099 MiB under 4 GiB. The registered run took 14.07s and
peaked at 1734 MiB under 8 GiB. Swap was disabled. Full GHC tests were not run.
Earlier evidence and the original reader remain preserved. Initial source
failures, the rejected Hadrian target and a corrected probe-generator error
are retained. The deep metadata check is justified by the structural audit;
no source-level exploit is claimed. Core and the interface version are unchanged.

The next failure is `pure ((42 :: Int), xs)` inside the inferred callback.
Equivalent `(,)` and ordinary polymorphic helper applications compile.
[M26](PLAN.md#m26-boxed-tuple-source-inference) completes tuple syntax inference.

## M26: inferred boxed tuples

Both compiler stages accept native fields in complete boxed tuple syntax:

```haskell
n <- readIORef cell
let action = pure (replicateNative @Int n 7)
    compute = action >>= \xs -> pure ((42 :: Int), xs)
pair <- compute
pure (PackVec n (snd pair))
```

For an `Infer` result under `ExperimentalDependentTypes`, a read-only native
anchor permits expansion to the ordinary tuple data constructor. The existing
application checker caches fields and chooses native types immutably. Ordinary
fields can use fresh ordinary parameters. The tuple-size check remains.
The expansion retains the written tuple context and field locations.
Known expected types retain component checking. Tuple sections and unboxed
tuples retain their existing paths. Core and the interface version are unchanged.

Note `[Inference of native tuple fields]` in `GHC.Tc.Gen.Expr` owns the rule.
The argument classifier refers to that Note.
[Agda product, Sigma and constructor references](REFERENCES.md#boxed-tuple-inference-prior-art)
record the precedent. An ordinary tuple does not bind a new runtime index.
Returning an inner index with its vector still requires an explicit package.

Both source clients pass 395 observations. The previous 377 remain unchanged.
The 18 additions compare syntax, constructor and helper forms at IORef inputs
0 through 3. They check effects, lazy tuple fields, original errors, partial
and cyclic prefixes, and sharing of the vector and original IORef payload.
Both stages pass 79 located rejection cases and four ordinary controls.
Ten new rejections cover index escape, independent indices, linear usage,
generalization, field mismatch, GADT escape and the unsupported native section.
Ten tuple compile controls cover nested tuples, unsigned literals, linear
lets, known expected types, ordinary higher-rank fields, sections and unboxed
tuples. The API/runtime fixture passes 113 package/bound rejections.
All five registered regressions pass, with 34 skipped ways and no framework
warnings or unexpected results. Full GHC tests were not run.

Evidence uses `_build/verification/native-boxed-tuple-*`. Three archives contain
32 selected compiler inputs each. The self-hosted build took 6m16s and peaked
at 7210 MiB under 16 GiB. Final source/API checks peaked at 1047 MiB under
4 GiB. Registered tests took 13.10s and peaked at 1907 MiB under 8 GiB.
Swap was disabled. Failed setup runs and the original M25 evidence remain.

The next baseline compiles an ordinary unit case returning the tuple.
It also compiles `((,) xs) (42 :: Int)`. Applying `(xs,)` still reports
GHC-83865. These are compile/Lint controls, not runtime validation.
[M27](PLAN.md#m27-native-tuple-sections) targets that separate section path.

## M27: native tuple sections

Both compiler stages accept inferred and expected boxed tuple sections:

```haskell
n <- readIORef cell
let action = pure (replicateNative @Int n 7)
    compute = action >>= \xs ->
        let section = (xs,)
         in pure (section (42 :: Int))
pair <- compute
pure (PackVec n (fst pair))
```

The checker retains `ExplicitTuple` and its existing desugaring. It infers an
anchored native present field once with its exact type and checks its lifted
kind. Inference keeps ordinary missing-field metas. A known native function
type supplies the tuple components and hole multiplicities. Whole-function
subsumption checks the reconstructed section against that expected type, so
input and result casts stay connected. Projection crosses only ordinary
visible arrows. Other expected shapes retain their existing path.
The owning `[Inference of native tuple fields]` Note records these rules.
Core and the interface version are unchanged.

Both source clients pass 417 observations, preserving the previous 395 outputs.
The 22 additions compare sections and constructor applications at IORef lengths
0 through 3. They check effects, repeated applications, hole order, ignored and
demanded bottom fields, partial/cyclic prefixes, and sharing of a named captured
vector and its original IORef payload. Existing desugaring places present
expressions inside the missing-field lambdas. It adds no sharing let; repeated
applications need not share a newly evaluated present expression.

Both stages pass 88 located rejections and four ordinary runtime controls.
The old section rejection is now positive; the other 78 M26 cases remain.
Ten new cases check scope, independent indices, mismatched holes/fields,
linear usage, generalization, lifted kinds and the existing native forall gate.
Ten section compile controls cover inferred and expected sections, multiple
holes, nested storage, linear captures, all ordinary hole multiplicities and
ordinary higher-rank checking. The API/runtime fixture passes 122 package/bound
rejections. All five registered tests pass, with 34 skipped ways and no
unexpected results or framework warnings. The full GHC suite was not run.

Evidence uses `_build/verification/native-tuple-section-*`. Two compiler input
archives contain 32 selected paths each. The self-hosted build took 7m00s and
peaked at 4674 MiB under 16 GiB. Bootstrap source/API checks peaked at 1063 MiB
under 4 GiB; the remaining both-stage checks peaked at 242 MiB under 4 GiB.
Registered tests took 13.68s and peaked at 1917 MiB under 8 GiB. Swap was
disabled. Failed discovery probes and all earlier frozen evidence remain.
The [pinned prior art](REFERENCES.md#native-tuple-sections-and-application-spines)
distinguishes Agda's typing precedent from GHC's demand and sharing behavior.

The next compile/Lint baseline stores a section in a pair. Naming `fst pair`
before applying it compiles; direct `(fst pair) (42 :: Int)` reports GHC-83865
in both stages. This is the separate generic overapplication path in
[M28](PLAN.md#m28-native-generic-result-overapplication), not section creation.
