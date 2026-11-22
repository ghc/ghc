# Dependent Haskell handoff

Start from the `dependent-haskell` branch of [Tritlo/ghc](https://github.com/Tritlo/ghc/tree/dependent-haskell).
Use this file to resume an interrupted build or review. Update the revision,
test results and next action whenever a checkpoint lands or a build stops.
The [roadmap](ROADMAP.md) tracks feature scope. The [Showcase](Showcase.hs)
and its [expected output](Showcase.stdout) are the executable examples.

## Current checkpoint

Updated 22 November 2026, 13:37 CET.

| Revision | What to recover | Verification |
| --- | --- | --- |
| `97f6622b03` | Last published checkpoint. An ordinary client opens a sample with a hidden calibration status. | Both compiler stages passed the 538-line Showcase at `-O0` and `-O2`. Stage 2 GHCi and 28 selected registered tests passed. [Record](CHECKPOINTS.md#erased-calibration-status). |
| `70ded7e706` | Consolidated compiler. A retained callback accepts nominally equal argument and result types. CorePrep preserves the index when it names a constructor in a curried call. The evaluator checks the ordered equations of the installed `Prelude.zip` and `Prelude.zipWith`. | Focused tests passed before the restart. The earlier 540-output two-stage run was interrupted. Rebuild this compiler for the combined checks. |
| `e62ddf4e9f` | Same compiler and libraries, with the merged 542-line Showcase and zipper length proofs. The ordinary client proves that each zipper takes the shorter length. | Use these sources and expected output with the rebuilt compiler. Full combined verification is pending. |

Pueue job `379` is building `70ded7e706` in the normal checkout below.
Verify that its label is `normal-70ded7e706` when inspecting the queue.
The baseline started with four workers. After WSL exposed 24 CPUs, the
active build gained affinity to CPUs 0–15. The shared slice reported
`cpu.max = 1600000 100000`, a 64 GB memory limit and zero swap. The priority probe froze the slice while a job in another group
ran, then resumed it. The local branch includes the 542-line fixture.
[Branch recovery ledger](BRANCHES.md#22-november-restart).

The final compiler candidate is `3a4e5c237a`. It includes the Core
optimizer change from `9011965d43` and the 542-line fixture. When a
retained function cast is applied to a nominally cast lexical callback,
the optimizer can substitute that callback into the result proof. It checks
the full binder type and preserves the callback's identity and evidence.
Pueue job `380` ran the same Core API test before and after the optimizer
change. The baseline failed at the forward cast. The candidate object
passed the full fixture, including the reverse cast and the newtype
control. Job `381` found both zipper equations and their fusion rules
in the baseline's static and dynamic `GHC.Internal.List` interfaces.
The combined compiler still needs its two-stage build and tests.

The baseline reader has finished. The clean build owns the normal checkout.
The recovery area's `core/verify.sh` records the focused before/after check.
The normal area's `incremental.sh` rebuilds the same checkout at the final
revision. `qualify.sh` records the final tests in
`final-3a4e5c237a/results/qualification.tsv`. All three commands use the
shared queue. The user requested a pause after the verified branch is pushed.

## Resume after a restart

1. Check `git status -sb` and `git ls-remote fork refs/heads/dependent-haskell`.
   Compare both revisions with the checkpoint table. Fetch the fork branch
   if this checkout predates the release.
2. Check `env -u XDG_RUNTIME_DIR pueue status` and inspect the named job and
   its checkout. The WSL restart erased `/tmp` while Git and Pueue retained
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
