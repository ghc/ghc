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
| `a395eea233` and `f73e659381` | Both stages built and matched 546 native Showcase lines at `-O0` and `-O2`. Stage 2 GHCi, installed HaskLib and the selective-upgrade checks passed. The official normal-way selection passed 39 of 40 tests. Unchanged `T15874` failed with the same GHC-45219 as on the preceding released compiler. |
| `d41294266d` and `a7088dd69f` | R19 composes type-level cases across two modules at a runtime enum index. The compiler sources match the normal two-stage build at `4e0c9b393a`. Job `828` passed `NativeDependentCoreRun` and the 547-line Showcase at stage 2 in native `-O0`, native `-O2` and GHCi. |
| `4f2fc1146f` and `229cecd250` | A client passes `HaskLib.size` as a retained callback. A scoped equality lets `safeHead` infer the tail index of a vector of length one. The installed-package clients and 548-line Showcase passed. Of 41 registered regressions, only the known `T15874` failed. |

[Equality continuations and kind inference](CHECKPOINTS.md#equality-continuations-and-structural-kind-inference), [HaskLib pipeline and parametricity](CHECKPOINTS.md#compose-installed-list-proofs), [Ordinary minimum](CHECKPOINTS.md#an-ordinary-minimum-in-the-zip-length-proof)
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
`_build/on-demand/r16-normal-20261122/source`. Rebuild this compiler
from the checkout to repeat job `503`. The 543 and 544 comparison
compilers at `5fe06625c3` and `c24ffa9ca0` still run. After removal
of their intermediate build products, job `785` verified both compiler
hash manifests, compiled and ran native programs at `-O0` and `-O2`,
and passed the 543-to-current minimum-equation upgrade and warm rebuild.
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

The inductive steps of `lengthMap`, `lengthAppend`, `plusZero` and
`plusSucc` now use `congSucc`. Job `792` ran the package client and
compared nine demand observations before and after this change at
`-O0` and `-O2`. Job `794` installed the source archive offline and
repeated all nine checks from a client with only the installed package.

Job `760` checks `oneCellHead` with the same 546-line output at native
`-O0`, native `-O2` and in GHCi. It uses `gcastWith` to return a
`Maybe Char` under the equality supplied by the runtime length test.
HaskLib now exports that operation as `withEquality` at `3f22060189`.
The installed clients in job `801` used it at kind `Type` and at kind
`Size`, forced the witness and rejected an unrelated equality.
Job `804` checked the new spelling in all 546 Showcase outputs at both
native optimization levels and in GHCi.

The standalone [filter example](examples/open-evaluation/ParametricFilter.hs)
states a free theorem at two element types. For Prelude's filter, the
client can import a proof by induction. For an arbitrary erased
polymorphic function, a bare `Refl` fails at the proof expression.
[Hypotheses and controls](REFERENCES.md#filter-naturality-in-the-presence-of-seq).

The earlier R19 selector designs are recoverable under
`refs/archive/dependent-haskell/2026-11-22/`. The published R19
implementation compares the generated case equations directly. Its private
compiler passed both known and unknown scrutinees at 1,610 constructors.
Jobs 802 and 815 built and tested that implementation in a normal compiler.

| Ref | Current evidence |
| --- | --- |
| `r19-eight-constructor-selector` (`568979c39e`) | Separate providers agree on a case over an unknown enum with up to eight nullary constructors. The wider recursive alternative in `r19-selector-depth-comparison` (`732d9c5bb8`) exceeds the default reduction depth for the last of 1,610 constructors. |

## Compiler builds submitted on 22 November

Check Pueue for each job before resuming it. The experiment directories
below are under `_build/on-demand/`.

| Job | Directory | What to verify |
| --- | --- | --- |
| 783, 806 | `r03-named-measured-zip-normal-20261122` | Both stages built at `0eb0718f35`. The dedicated named-callback, lexical-callback and rewrite-rule tests passed, as did the source-hidden HaskLib clients. |
| 798, 809, 816, 817 | `r03-named-safehead-normal-20261122` | Both stages built at `a43a310c08` and matched all 547 lines of fixture `cde69193d8` at `-O0` and `-O2`. Stage 2 GHCi and the installed HaskLib clients passed. The registered normal suite passed 39 of 41. The Core Lint repair in `4f2fc1146f` restores the stored-function test. |
| 802, 815 | `r19-closure-normal-20261122` | Both stages built at `4e0c9b393a`. Four registered tests passed. A stripped-interface client used the two case results at an unknown index. Changing a row rebuilt the consumer and rejected its old proof. |

The generated compiler trees for `0eb0718f35` and `a43a310c08`
were removed after qualification of `4f2fc1146f`. Each experiment
directory retains its Git source bundle, recipe and test results.
Rebuild the old compiler only if an experiment needs to run it.

For the current compiler, use `r03-r19-r36-normal-4f2fc1146f-20261122`.
Job 855 rebuilt both stages there. Jobs 864, 865, 868 and 871 checked
the Showcase, selected tests, installed HaskLib and upstream compatibility.
The build reuses the physical checkout under
`r03-r19-r36-normal-91fd7365bf-20261122/source`. Verify its actual
revision against the `compiler.source` file in the newer directory.
[Results and test counts](CHECKPOINTS.md#a-named-measurement-and-an-inferred-vector-length).

**R36 pun warnings.** Job 881 qualified the independent normal
compiler at `a20088cba1`. All 15 warning controls passed. The rename
suite recorded 479 expected passes, three expected failures and the
same 16 unexpected failures as the preceding compiler. Job 887 builds
the cumulative source `575e35e0d6` on the released callback compiler.
The splice-scope check in job 889 then exposed a missing binding warning across declaration
groups. The repaired cumulative source is `0a19ed1190` in
`r36-combined-top-groups-20261122/source`. Check the treatment of
implicit pattern binders against the accepted warning rule before
closing the card.

**R13 ordinary typechecking.** With only `RequiredTypeArguments`
enabled, a name in a type can resolve to a term. The released compiler
reports missing source equations in several of these cases. The repair
`faad9b1e9a` sends the captured name through the ordinary checker
when dependent types are off. Job 899 passed all ten original
`T23740a`–`T23740j` tests. Compiled clients used an imported proof
and HaskLib at both optimization levels. Four direct function calls in
types still checked with the extension enabled. This repair shares the
R19 two-stage build below.

**R19 cases on constructors with fields.** At `96efc76a77`, a
source-hidden client composes two cases on `Maybe Bool` at an unknown
index. Job 893 checked the unused `Just` field, a changed row, a
different index, old readers and reuse after rebuilding. The 549-line
Showcase also passed in the private compiler in job 902. Job 904
builds the combined R19 and R13 source `4e169241d2` in
`r19-ignored-fields-next-20261122/normal`. After its focused test
run frees Hadrian, run `r13-r19-normal-compatibility-20261122/check.sh`
with the shared build queue. This checks the ten unchanged diagnostic
tests and the 158 upstream dependent tests.

**R41 nested GADT quantifiers.** Private compiler `7df3cd2a79`
reads fieldless ordered constructors and rejects unsupported interfaces
before running an external splice. It retains the 24 one-field orders.
Jobs 890 and 901 checked the interface cases and twelve registered
tests. Job 903 builds the combined source `9884c35ff0` in
`r41-r36-normal-20261122`. Check nonuniform result indices,
unboxed fields and the upstream record diagnostic before closing R41.
[R41 ordering tests](roadmap/R41-nested-quantification-in-gadts.md#which-prefix-orders-work)
and [R19 scope](roadmap/R19-type-level-cases.md).

**R27 CUSK migration study.** The comparison source `aea0428256` in
`r27-cusk-impact-20261122/source` applies the three compiler lines
from [GHC MR !1602](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/1602).
Job 905 compares legacy and standalone signatures on stock and fork
GHC. Job 907 relinks the proposed change. Job 911 depends on both
and compares the upstream polykinds and standalone-kind-signature
tests on the two compilers. Record provider kinds and client results
before interpreting these runs as migration evidence.
[R27 scope](roadmap/R27-deprecate-cusks.md).

The [release-readiness assessment](ROADMAP.md#release-readiness) identifies
R36 as the next full-card target.

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

The current 547-line Showcase uses the normal two-stage compiler in
`_build/on-demand/r19-closure-normal-20261122`. Its driver's
`--info` revision is `4e0c9b393a`. Job 828 used the published
HaskLib and the fixture at `a7088dd69f`. The preceding 546-line
compiler at `a395eea233` is in `_build/on-demand/r23-r03-normal-20261122`.
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
stable="$PWD/_build/on-demand/r19-closure-normal-20261122/source/_build/stage1"
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
