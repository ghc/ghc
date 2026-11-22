# Dependent Haskell handoff

Start from the `dependent-haskell` branch of [Tritlo/ghc](https://github.com/Tritlo/ghc/tree/dependent-haskell).
Use this file to resume an interrupted build or review. Update the revision,
test results and next action whenever a checkpoint lands or a build stops.
The [roadmap](ROADMAP.md) tracks feature scope. The [Showcase](Showcase.hs)
and its [expected output](Showcase.stdout) are the executable examples.

## Current checkpoint

Verified 22 November 2026, 14:24 CET. The development branch is
`dependent-haskell`. To find its latest commit, use the fork command below.

| Revision | Result |
| --- | --- |
| `3a4e5c237a` | Both normal compiler stages built. Both report this compiler revision. An isolated Core API test reproduced the callback cast failure on `70ded7e706` and passed with the changed optimizer. |
| `54d1d5c54a` | Both stages matched the 542-line Showcase at `-O0` and `-O2`. Stage 2 GHCi matched it too. The native runs checked HIE files and clients that could read only provider interfaces. |
| `89d450a893` | All 29 selected normal-way registered tests passed across two runs. Job `397` passed 28. After three diagnostic assertions were corrected in the remaining test, job `409` passed it on the same compiler. |

An ordinary client proves that `Prelude.zip` and `Prelude.zipWith` return
the shorter input length. The proof module checks the equations with
rewrite rules disabled. The optimized Showcase imports concrete witnesses.
The Core optimizer retains the callback and its nominal evidence when it
substitutes a cast argument into a dependent result proof.
[Checkpoint and test selection](CHECKPOINTS.md#ordered-zippers-and-nominal-callback-casts).

## Work in progress

- **R05.** The local candidate at `56dd0a2099` lets a rewrite rule fire when it
  matches a nominally cast callback. Its isolated compiler passed the
  rule tests and the 543-line Showcase. The normal two-stage build uses
  the integrated candidate at `5fe06625c3`.
  A separate rule-free program on the 542-line compiler exposes a Core Lint
  failure at `-O1` and `-O2`. Full laziness floats a binding for an unused
  polymorphic callback that is still needed in a type. The program passes
  at `-O0` and with the provider's optimization pragmas omitted.
- **R19.** Two independent signatures can share a named case result at an
  unknown index. Two separately written anonymous cases can still disagree.
  The next experiment checks whether exhaustive cases for a nullary
  enumeration can share one ordinary closed type family across interfaces.
  [Evidence and alternatives](roadmap/R19-type-level-cases.md#separate-providers-at-an-unknown-index).

The normal build from job `383` remains a live input to both experiments.
Its two compiler hashes are in `final-3a4e5c237a/compilers.sha256` under
the build area below. The `fixture-54d1d5c54a` and `fixture-89d450a893`
directories hold the test reports. [Git recovery](BRANCHES.md#22-november-restart).

## Resume after a restart

1. Check `git status -sb` and `git ls-remote fork refs/heads/dependent-haskell`.
   Compare both revisions with the checkpoint table. Fetch the fork branch
   if this checkout predates the release.
2. Check `env -u XDG_RUNTIME_DIR pueue status` and inspect the named job and
   its checkout. On 22 November, Pueue recorded signal 15 (exit 143) for
   job `382`. Its shell wrote a zero exit status during shutdown. The
   missing `build.pass` and the unfinished Hadrian log confirmed the
   interruption. Check Pueue, both compiler revisions and the success
   record before reusing a result.
   The earlier WSL restart erased `/tmp` while Git and Pueue retained
   records for its old paths. Keep active builds under `_build/on-demand/`.
3. Restore the [shared WSL build queue](CHECKPOINTS.md#restore-the-shared-wsl-build-queue)
   before submitting work. The group has three slots. All GHC jobs share
   16 logical CPUs, 64,000,000,000 bytes of memory and zero swap.
   Higher-priority jobs pause the queue and freeze the GHC slice.

## Build and verify

The current normal checkout is
`_build/on-demand/live-recovery/normal-70ded7e706/source`. Its sibling
`build.sh` records the commands and writes logs in the parent directory.
On this host the bootstrap tools are GHC 9.14.1, Cabal 3.18.1.0, Happy 2.1.7
and Alex 3.5.4.0. A fresh checkout needs the Git submodules at their recorded
revisions. This build used four workers. The shared queue now allows five per job.
The normal build runs `./boot`, `./configure`, builds Hadrian
from `hadrian/cabal.project`, and runs Hadrian with
`-j4 --flavour=quick --docs=none`. Reuse the configured checkout and Hadrian
binary if a build stops partway. Keep each compiler with its own libraries.

Hadrian puts the stage 1 compiler in `_build/stage0/bin/ghc` and the
self-hosted stage 2 compiler in `_build/stage1/bin/ghc`. Both use the
target libraries in `_build/stage1/lib`. For example, from the main checkout:

```sh
area="$PWD/_build/on-demand/live-recovery/normal-70ded7e706"
queue="$PWD/docs/dependent-haskell/tools/ghc-build-job"
check="$PWD/docs/dependent-haskell/check-showcase.sh"
lib="$area/source/_build/stage1/lib"
"$queue" showcase-stage1 bash "$check" "$area/source/_build/stage0/bin/ghc" "$lib"
"$queue" showcase-stage1-o2 bash "$check" "$area/source/_build/stage0/bin/ghc" "$lib" --default-o2
"$queue" showcase-stage2-ghci bash "$check" "$area/source/_build/stage1/bin/ghc" "$lib" --ghci
"$queue" showcase-stage2-o2 bash "$check" "$area/source/_build/stage1/bin/ghc" "$lib" --default-o2
```

The runner checks native output, source-hidden clients and Core/STG Lint.
The interactive flag also checks GHCi output. Verify the installed
`GHC.Internal.List` interfaces and the registered tests as separate checks.
Record the number of tests run, failed or skipped. A compiler revision,
a built executable and a passing Showcase answer different questions.

After qualification, update this handoff and [the checkpoint record](CHECKPOINTS.md).
Keep the Git revisions, the commands and any outstanding failure. Release
superseded build products after their final consumer finishes. The main
`_build` cleanup threshold is 200 GB. [Retention rules](CHECKPOINTS.md#clean-up-after-each-experiment).
