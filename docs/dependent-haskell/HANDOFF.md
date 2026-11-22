# Dependent Haskell handoff

Start from the `dependent-haskell` branch of [Tritlo/ghc](https://github.com/Tritlo/ghc/tree/dependent-haskell).
Use this file to resume an interrupted build or review. Update the revision,
test results and next action whenever a checkpoint lands or a build stops.
The [roadmap](ROADMAP.md) tracks feature scope. The [Showcase](Showcase.hs)
and its [expected output](Showcase.stdout) are the executable examples.

## Current checkpoint

Updated 22 November 2026, 16:07 CET. The development branch is
`dependent-haskell`. Use the fork command below to find its latest commit.

| Revision | Verified result |
| --- | --- |
| `5fe06625c3` | Clean two-stage build. Both stages matched the 543-line Showcase at `-O0` and `-O2`. Stage 2 GHCi and all 29 selected registered tests passed. The 542 and 543 compilers read each other's imported rule interfaces. |
| `c88606fe92` | Same compiler with the proof over the two runtime lists. Both stages passed the updated zipper tests and Showcase. The proof uses `size (zip xs ys) :~: Minimum (size xs) (size ys)`. |
| `6d442d71b8` | Both normal compiler stages matched 543 native outputs at `-O0` and `-O2`. Stage 2 GHCi matched the same output with breakpoints enabled. An ordinary recursive `minSize` works in the same module as the zip proof or in a library. |
| `dcaa2a244a` | All 30 selected normal-way tests passed on that compiler, including the single-file proof, imported proof, rule controls and no-code map regression. Both compiler-upgrade checks passed. |

[Ordinary minimum](CHECKPOINTS.md#an-ordinary-minimum-in-the-zip-length-proof)
and [previous release](CHECKPOINTS.md#direct-zip-proof-and-retained-callback-rules).

## Current R16 result

**R16 open evaluation.** The [single-file example](examples/open-evaluation/ZipMinimum.hs)
defines `size`, `minSize` and the proof together. The result type is
`size (zip xs ys) :~: minSize (size xs) (size ys)`. The
[importing example](examples/open-evaluation/PreludeZipRuntime.hs) reads
the equations from the provider interface. An earlier home provider
rebuilds once. Rebuild an older installed provider to obtain its equations.

Job `499` built both normal compiler stages at `6d442d71b8`. The
registered tests use `dcaa2a244a`, which adds a signature to the
interactive test caller. The compiler sources are identical at both pins.
Job `503` completed qualification. Its results are in
`_build/on-demand/r16-normal-6d442d71b8-20261122/verification`.
The `source` link in that area points to the actual checkout at
`_build/on-demand/r16-normal-20261122/source`. Treat them as one build.
The 543 comparison compiler is at `5fe06625c3`.
R16 remains open for ordinary recursion beyond the checked case shapes.
The zip proof uses the order in which the installed function inspects
its lists and disables rewrite rules in the proof module.
[Open evaluator card](roadmap/R16-open-term-evaluator.md#prove-an-equation-for-preludezipwith).

Other local candidates are recoverable from Git. Each name is relative
to `refs/archive/dependent-haskell/2026-11-22/`.

| Ref | Evidence and remaining work |
| --- | --- |
| `r05-absent-float` (`c5b4c8a5ea`) | Full laziness preserves an absent callback's lexical identity. The private compiler passed the 544-line Showcase, demand controls and the unrelated-binding control. A normal build remains open. |
| `r04-strict-witness` (`e71f7098f1`) | Bang patterns on stored witnesses passed source-hidden tests with Core and STG Lint. A normal build remains open. |
| `r19-enum-experiment` (`edee3f0ed5`) | Cross-module enum cases shared a generic selector, but wide cases exceeded the default reduction budget. The measured owner-ordinal alternative requires owner participation. |

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

The current normal build area is
`_build/on-demand/r16-normal-6d442d71b8-20261122`. The scripts
`build.sh` and `incremental.sh` in `_build/on-demand/r16-normal-20261122`
record the clean and incremental build commands.
On this host the bootstrap tools are GHC 9.14.1, Cabal 3.18.1.0, Happy 2.1.7
and Alex 3.5.4.0. A fresh checkout needs the Git submodules at their recorded
revisions. This build used five workers per job.
The normal build runs `./boot`, `./configure`, builds Hadrian
from `hadrian/cabal.project`, and runs Hadrian with
`-j5 --flavour=quick --docs=none`. Reuse the configured checkout and Hadrian
binary if a build stops partway. Keep each compiler with its own libraries.

Hadrian puts the stage 1 compiler in `_build/stage0/bin/ghc` and the
self-hosted stage 2 compiler in `_build/stage1/bin/ghc`. Both use the
target libraries in `_build/stage1/lib`. For example, from the main checkout:

```sh
area="$PWD/_build/on-demand/r16-normal-6d442d71b8-20261122"
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
