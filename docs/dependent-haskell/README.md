# Dependent Haskell project

The long-term goal is dependent types in Haskell. The first experiment is a
retained Boolean argument whose value determines the result type. Extend Core
from Eisenberg's historical `nokinds-pi` work, ported and completed on current
GHC. Keep the validated `singletons` encoding as a comparison implementation.
The current source subset includes runtime Peano naturals and length-indexed vectors.

Last updated: 2026-11-13.

## Project documents

- [Implementation plan](PLAN.md): Core extension, port tasks, and acceptance checks.
- [References](REFERENCES.md): annotated sources, reading order, and status dates.
- [Historical port](PORT.md): pinned commits, module mapping, and missing rules.
- [Restricted Core](CORE.md): representation, typing, substitution, and gates.
- This file: current status, validation results, and instructions to resume work.

## Current status

| Item | State |
| --- | --- |
| GHC source baseline | `19ff36f2aede0ca2e554016ff22c71e73aa9803b`, dated 2026-11-09 |
| Project branch | `codex/dependent-haskell` |
| Baseline build | Passed on 2026-11-11 |
| Singleton reference translation | Validated and retained in `reference/` on 2026-11-11 |
| Native Core | Closed algebraic domains, field-scoped Nat proofs, and fixed-domain `DepFunCo` pass the bootstrap API regression |
| Execution | Nat/Vec and Boolean source programs pass checked bytecode lowering and separate-module machine-code execution |
| Program entry | Ordinary single-module executable passes the bootstrap API, self-hosted CLI, and registered regression |
| Native interfaces | Nat/Vec and Boolean signatures pass disk imports and cross-module execution; CLI reads check both format-rejection directions |
| Configured fork version | `10.1.20261113.1`, with interface header `1001202611131`; matching libraries rebuilt |
| Source syntax | Algebraic domains, outer erased `forall`, retained telescopes, bare fields, and checked source `case` pass the runtime fixtures |
| Historical starting point | `nokinds-pi` at `b64ef80f8002727422c05092f429c38d0527ed0b`, dated 2014-08-19 |
| Arithmetic proofs | Runtime Nat induction returns ordinary `:~:` evidence and transports vectors; ignored proofs and partial transport stay lazy |
| Current increment validation | Arithmetic proofs pass both-stage build, five registered regressions, bootstrap API/runtime, separate CLI, guide, and ordinary smoke checks |
| Next task | Ambient-evidence rewriting, structural Pi unification, retained inference, broader cases, native coercion unfolding codecs, and conservative lowering gaps remain open |

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

Generic `forall a. a -> ...` argument inference still rejects a whole type that
contains a retained index. The CLI records this GHC-83865 at the actual RHS.
The proof evaluation checks use explicit `Refl` cases. General retained
inference and ambient-evidence normalization under Pi remain open.

Successful logs use `native-proof-api`, `native-proof-runtime`,
`native-proof-registered`, `native-proof-cli`, `native-proof-extra-cli`,
`native-proof-runtime-cli`, `native-proof-guide`, and `native-proof-smoke` in
`_build/verification/`. The final CLI scope log is
`native-proof-cli-after-import-fix.log`. The first wrapper stopped at an import
collision in its standalone control; the corrected control hides the producer's
proof names. Bootstrap-generated files and their hashes are retained in
`native-proof-bootstrap-work/`. The signature format is unchanged.

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
