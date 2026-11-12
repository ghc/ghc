# Dependent Haskell project

The long-term goal is dependent types in Haskell. The first experiment is a
retained Boolean argument whose value determines the result type. Extend Core
from Eisenberg's historical `nokinds-pi` work, ported and completed on current
GHC. Keep the validated `singletons` encoding as a comparison implementation.

Last updated: 2026-11-12.

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
| Native Core | Retained Boolean lambda, application, and checked cases pass the API regression |
| Execution | Native Core and source `keep` run through the checked bytecode entry |
| Source syntax | `ExperimentalDependentTypes` checks one explicit retained Boolean binder; unsupported source positions report GHC-43582 |
| Historical starting point | `nokinds-pi` at `b64ef80f8002727422c05092f429c38d0527ed0b`, dated 2014-08-19 |
| Next task | Source Boolean refinement for `choose` in M7; broader M6 optimizer work remains open |

The current direction is native dependent Core. The historical branch is
unfinished. It supplies binder and application design work, but dependent type
computation and Boolean elimination need completion. The native target must accept
ordinary runtime Boolean bindings directly and preserve laziness. Singleton
reification remains specific to the comparison implementation.

## Native Core implementation

`TermVarTy` refers to the index of an existing runtime Boolean identifier.
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

The restricted frontend produces native Core for local retained functions. The ordinary optimizer, default
CorePrep entry, program backend entry, and interface serialization reject them.
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
ordinary optimizer preservation or the later source implementation. Native machine-code
compilation remains behind the program gate. All commits are local.

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

The frontend accepts one explicit `foreach (b :: Bool)` binder at the start of a
complete monomorphic value signature. A dependent function has one equation with a bare
retained variable pattern. Its result has lifted kind, and its match wrapper
must be identity. Calls accept Boolean constructors and named local Boolean Ids.
The first dependent result example is `Select b -> Select b`.

The renamer allocates one value `Name` and adds a scoped type-lookup alias. The
checker registers its fixed Bool/Many Id. Pattern checking reuses the existing
pattern Name, so the runtime lambda and its result type have the same Unique.
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

The six source observations and twelve Core observations pass against the
bootstrap API and rebuilt compiler. All 27 source rejection cases check the
diagnostic code, message, and source span. The opaque-index matching checks also
pass. The bootstrap API and runtime checks each used a 4 GiB cap and peaked at
913 MiB and 918 MiB. Core Lint and STG Lint remain mandatory in native lowering.

The final default both-stage rebuild passed in 10 minutes 30 seconds with
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

Source constructor patterns, dependent Boolean cases, and source `choose` remain
unfinished. General runtime-index inference, the ordinary dependent optimizer,
interfaces, and machine-code integration remain open. The full testsuite,
documentation build, and performance checks have not run. See the
[frontend plan](CORE.md#frontend-identity-plan) and
[Agda provenance](REFERENCES.md#agda-provenance).

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

1. Read this file and the next unchecked milestone in [the plan](PLAN.md).
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
