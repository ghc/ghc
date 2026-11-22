# Dependent Haskell handoff

Start from the `dependent-haskell` branch of [Tritlo/ghc](https://github.com/Tritlo/ghc/tree/dependent-haskell).
Use this file to resume an interrupted build or review. Update the revision,
test results and next action whenever a checkpoint lands or a build stops.
The [roadmap](ROADMAP.md) tracks feature scope. The [Showcase](Showcase.hs)
and its [expected output](Showcase.stdout) are the executable examples.

## Current checkpoint

Updated 22 November 2026, 13:56 CET.

| Revision | What to recover | Verification |
| --- | --- | --- |
| `97f6622b03` | Last published checkpoint. An ordinary client opens a sample with a hidden calibration status. | Both compiler stages passed the 538-line Showcase at `-O0` and `-O2`. Stage 2 GHCi and 28 selected registered tests passed. [Record](CHECKPOINTS.md#erased-calibration-status). |
| `70ded7e706` | CorePrep preserves the index of a constructed argument in a curried call. The evaluator checks the ordered equations of the installed `Prelude.zip` and `Prelude.zipWith`. | Pueue job `379` completed a clean two-stage build. Job `381` found both equations and the fusion rules in static and dynamic `GHC.Internal.List` interfaces. |
| `3a4e5c237a` | Current compiler and tests. Includes the 542-line Showcase from `e62ddf4e9f` and the optimizer change from `9011965d43`. | Job `380` reproduced the callback cast regression on the baseline. The candidate optimizer passed the same test. Job `383` resumed the combined compiler build after the second restart. |

The Core optimizer keeps the lexical callback and its nominal evidence
when it substitutes a cast argument into a dependent result proof.
An ordinary client can prove that each installed zipper takes the shorter
length. The [checkpoint](CHECKPOINTS.md#ordered-zippers-and-nominal-callback-casts)
gives the source tests and the selected testsuite command.

The final build reuses the configured baseline checkout. Its
`incremental.sh` builds the candidate and checks both compiler revisions.
The normal area's `qualify.sh` records the combined results in
`final-3a4e5c237a/results/qualification.tsv`. The VM is back in service. Push the verified checkpoint.
Keep its compiler
available while the R05 and R19 source tests use it. [Branch recovery ledger](BRANCHES.md#22-november-restart).

R05 is testing a rewrite rule whose later argument mentions a nominally
cast callback. R19 is testing a client that composes two separately
compiled providers with the same type-level case at an unknown `Bool`.
These two investigations use separate checkouts.

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
