# Dependent Haskell project

The long-term goal is dependent types in Haskell. The first experiment is a
retained Boolean argument whose value determines the result type. Extend Core
from Eisenberg's historical `nokinds-pi` work, ported and completed on current
GHC. Keep the validated `singletons` encoding as a comparison implementation.

Last updated: 2026-11-11.

## Project documents

- [Implementation plan](PLAN.md): Core extension, port tasks, and acceptance checks.
- [References](REFERENCES.md): annotated sources, reading order, and status dates.
- This file: current status, validation results, and instructions to resume work.

## Current status

| Item | State |
| --- | --- |
| GHC source baseline | `19ff36f2aede0ca2e554016ff22c71e73aa9803b`, dated 2026-11-09 |
| Project branch | `codex/dependent-haskell` |
| Baseline build | Passed on 2026-11-11 |
| Singleton reference translation | Checked in temporary files on 2026-11-11 |
| Native Core and frontend | Planned; no compiler implementation changes yet |
| Historical starting point | `nokinds-pi` at `b64ef80f8002727422c05092f429c38d0527ed0b`, dated 2014-08-19 |
| First task | [M1: audit and pin the historical port](PLAN.md#m1-audit-and-pin-the-historical-port) |

The current direction is native dependent Core. The historical branch is
unfinished. It supplies binder and application design work, but dependent type
computation and Boolean elimination need completion. The native target accepts
ordinary runtime Boolean bindings directly and preserves laziness. Singleton
reification remains specific to the comparison implementation.

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
The temporary directory can expire. M1 must retain a reproducible reference
before implementation depends on these files.

## How to resume

1. Read this file and the next unchecked milestone in [the plan](PLAN.md).
2. Check `git status`, the current branch, and the source revision. Compare them
   with the baseline above before reusing build results.
3. Read the relevant primary sources in [the references](REFERENCES.md).
   Recheck proposal and package status when the work depends on it.
4. Complete one milestone at a time. Keep changes small and reviewable. Run the
   acceptance checks that apply to the milestone.
5. Update this status and the plan after a material result or scope change.
   Record the source revision, command, outcome, and remaining boundary.

Keep new research in `REFERENCES.md`. Keep implementation tasks in `PLAN.md`.
Do not turn the project index into an implementation log. Significant changes
to the language scope or Core strategy require a new design discussion.
