# Dependent Haskell handoff

Start from the `dependent-haskell` branch of [Tritlo/ghc](https://github.com/Tritlo/ghc/tree/dependent-haskell).
Use this file to resume an interrupted build or review. Update the revision,
test results and next action whenever a checkpoint lands or a build stops.
The [roadmap](ROADMAP.md) tracks feature scope. The [Showcase](Showcase.hs)
and its [expected output](Showcase.stdout) are the executable examples.

## Current checkpoint

Updated 22 November 2026. The development branch is
`dependent-haskell`. Use the fork command below to find its latest commit.

| Revision | Verified result |
| --- | --- |
| `5fe06625c3` | Clean two-stage build. Both stages matched the 543-line Showcase at `-O0` and `-O2`. Stage 2 GHCi and all 29 selected registered tests passed. The 542 and 543 compilers read each other's imported rule interfaces. |
| `c88606fe92` | Same compiler with the proof over the two runtime lists. Both stages passed the updated zipper tests and Showcase. The proof uses `size (zip xs ys) :~: Minimum (size xs) (size ys)`. |
| `6d442d71b8` | Both normal compiler stages matched 543 native outputs at `-O0` and `-O2`. Stage 2 GHCi matched the same output with breakpoints enabled. An ordinary recursive `minSize` works in the same module as the zip proof or in a library. |
| `dcaa2a244a` | All 30 selected normal-way tests passed on that compiler, including the single-file proof, imported proof, rule controls and no-code map regression. Both compiler-upgrade checks passed. |
| `c24ffa9ca0` and `f84b71ab53` | Clean two-stage compiler and separate 545-line fixtures. All four native Showcase runs, stage 2 GHCi, 30 registered normal tests and the upgrade checks passed. The compiler preserves demand on strict stored witnesses and tracks absent callbacks during full laziness. |
| `a7f42b75db` | HaskLib packages four runtime-list laws. Cabal built and installed its source distribution outside the checkout. Its package test and a separate client passed. |
| `5292956a82` | HaskLib also exports right-zero and right-successor lemmas for addition. A downstream module combines them with `lengthAppend` to prove the length of a list with one element appended. |
| `8e57520a14` | HaskLib's `lengthReverse` proves the length law for a runtime list. Its package test and separate clients passed. The public theorem mentions the installed Prelude function. Only HaskLib imports its accumulator worker. |
| `8d756d44da` and `193e3f35d0` | Both normal compiler stages matched 546 native outputs at `-O0` and `-O2`. Stage 2 GHCi, all 31 selected normal tests, the package consumers and the selective upgrade checks passed. The public source at `999f0ed489` has the same Git tree as the tested fixture. |
| `2d6368dd57` and `3ecc8d6fc9` | Both stages matched 546 native outputs at `-O0` and `-O2`. Stage 2 GHCi and all 31 selected normal tests passed. Installed clients used `zip` and `zipWith` proofs at ordinary `-O2`. A scoped equality with a runtime callback result rewrote the argument of `minSize`. |
| `6765da7957` | HaskLib exports `trans`, `congSucc` and `congFirst`. A single-import client uses them to prove the append-singleton and zip-after-map laws. Job `692` passed the final package and demand checks with the normal `2d6368dd57` compiler at both optimization levels. |
| `1011998c82` | The Showcase calls the direct and composed proofs on the same lists from IO. It prints `([False],Refl,Refl)`. Job `704` matched all 546 lines at native `-O0`, native `-O2` with stripped and ordinary interfaces, and GHCi. |

[HaskLib pipeline and parametricity](CHECKPOINTS.md#compose-installed-list-proofs), [Ordinary minimum](CHECKPOINTS.md#an-ordinary-minimum-in-the-zip-length-proof)
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
The proofs for Prelude's `zip` and `zipWith` now compile at standard
`-O2`. The compiler verifies the installed fusion rules and the order
in which both functions inspect their lists.
[Open evaluator card](roadmap/R16-open-term-evaluator.md#prove-an-equation-for-preludezipwith).

The 546-line Showcase uses HaskLib's direct proof of the length of
`zipWith combine (map pre xs) ys`. The proof involving `map` still
disables rewrite rules in its own module. A downstream client derives
the same equation from `lengthZipWith` and `lengthMap`. Only that
derivation requires the whole left spine.
[Library guide](README.md#reuse-a-proof-with-hasklib) and
[equality controls](roadmap/R11-homogeneous-equality.md#rewrite-a-size-in-a-downstream-proof).

The standalone [filter example](examples/open-evaluation/ParametricFilter.hs)
states a free theorem at two element types. For Prelude's filter, the
client can import a proof by induction. For an arbitrary erased
polymorphic function, a bare `Refl` fails at the proof expression.
[Hypotheses and controls](REFERENCES.md#filter-naturality-in-the-presence-of-seq).

Private candidates are recoverable under
`refs/archive/dependent-haskell/2026-11-22/`.

| Ref | Current evidence |
| --- | --- |
| `r23-structural-kind-inference` (`e5f37b3fce`) | An unannotated injective kind interpreter accepts the original `T13822`. A family that distinguishes its results by arrow matchability still passes. An older home provider needs a selective rebuild before a new client can rely on the improved kind. |
| `r19-eight-constructor-selector` (`568979c39e`) | Separate providers agree on a case over an unknown enum with up to eight nullary constructors. The wider recursive alternative in `r19-selector-depth-comparison` (`732d9c5bb8`) exceeds the default reduction depth for the last of 1,610 constructors. |

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

The latest 546-line qualification area is
`_build/on-demand/r16-zip-family-next-qualification-20261122`. Its
`source` link points to the configured compiler checkout. The normal
incremental build is recorded in
`_build/on-demand/r16-zip-fusion-inverse-20261122/next-normal-intake`.
Check the driver's `--info` revision against `2d6368dd57` before replay.
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
stable="$PWD/_build/on-demand/r16-zip-family-next-qualification-20261122/source/_build/stage1"
queue="$PWD/docs/dependent-haskell/tools/ghc-build-job"
check="$PWD/docs/dependent-haskell/check-showcase.sh"
"$queue" showcase-stage2-ghci bash "$check" "$stable/bin/ghc" "$stable/lib" --ghci
"$queue" showcase-stage2-o2 bash "$check" "$stable/bin/ghc" "$stable/lib" --default-o2
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
