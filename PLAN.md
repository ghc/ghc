# Plan: cgwork over all compiler perf tests → aggregate + analyse

## Context

A single `cgwork` profile of `T17904` surfaced three hot functions
([[sometyperep-ord-compare-iface-userdata-lookup]],
[[genunit-eq-hot-inline-reduce-work]], [[readp-alternative-startup-config-read]]),
but T17904 is a *trivial* module, so its Ir counts are dominated by fixed
startup/iface-loading cost and one profile is a biased sample. We want a
**representative, suite-wide** picture of where compiler instructions actually go,
and a way to separate fixed startup overhead from cost that scales with real work.

Plan: run `cgwork` (cachegrind, Ir-only) across the perf/compiler tests using a
**fixed already-built compiler** (`~/ghc/head0/_build/stage1/bin/ghc`), keep one
raw cachegrind file per test, then aggregate per-function Ir across the suite and
analyse.

Decisions taken with the user:
- **Self-contained batch script** (no cachegrind-under-the-testsuite wrapper).
- **Everything tractable** in one batch (all `compile` + `multimod_compile` tests).
- **Each test's own flags** (representative weighting), not a standardised `-O`.
- **Fresh worktree as workspace only**: it hosts the scripts + results under git;
  we still profile the prebuilt `~/ghc/head0/_build/stage1/bin/ghc` and copy test
  sources/generators from head0's testsuite. No compiler build happens in the
  worktree.

## Setup: fresh worktree (no build)

Since nothing is compiled here, skip the heavyweight `~/ghc/wt` (boot/configure/
hadrian warmup) and create a plain worktree:

```
git worktree add ~/ghc/work-trees/cgwork-perf-aggregate \
  -b wip/sjakobi/cgwork-perf-aggregate master
```

All deliverables and outputs live under that worktree
(`~/ghc/work-trees/cgwork-perf-aggregate/`), referred to below as `$WT`.

## Background facts (verified)

- `~/ghc/cgwork` (`/home/simon/prog/ghc-personal/cgwork.py`) already does the hard
  parts and is **importable as a module**: `parse(path)` → `(defaultdict fn→Ir, summary)`,
  `demangle(s)`, `is_comp(k)` (excludes GC/RTS buckets), `buckets(fns)`.
- `cgwork run OUT -- CMD...` runs CMD under `valgrind --tool=cachegrind
  --cache-sim=no --branch-sim=no`, follows the wrapper exec into the real ghc,
  picks the largest ghc process, and **moves its raw cachegrind file to `OUT`**
  (full per-function data — exactly what aggregation needs). It deletes the other
  child processes' files: a one-shot/recomp/TH/ghci test that forks several ghc
  invocations keeps only its single largest invocation (documented limitation).
- The compiler binary to profile is `~/ghc/head0/_build/stage1/bin/ghc`
  (head0 @ `3b2a9409ba`). Use head0's **own** testsuite tree
  (`~/ghc/head0/testsuite/tests/perf/compiler/`) so test sources/generators match
  the binary.
- Test command shape comes from `simple_build` in
  `~/ghc/head0/testsuite/driver/testlib.py:2270`:
  `{compiler} {to_do} {srcname} {flags} {extra_hc_opts}` where
  - `compile`/`compile_fail`: `to_do = -c`, `srcname = <Name>.hs`, `extra_hc_opts = args[0]`.
  - `multimod_compile`: `to_do = --make`, `srcname = args[0]` (root module),
    `extra_hc_opts = args[1]`; needs `extra_files(...)` present and any
    `pre_cmd(...)` generator run first, in a working dir.
- Generators live in `tests/perf/compiler/gen*` (e.g. `genMultiLayerModules`,
  `genManyConstructors`, `genManyAlternatives`, `genMultiComp.py`) and are run by
  `pre_cmd` inside the test dir.
- Suite size: 65 `compile`/`compile_fail` (single module), 24 `multimod_compile`,
  plus 6 `ghci_script` / 2 `makefile_test` / TH+recomp variants (out of scope for
  batch 1; see "Out of scope").
- Append `+RTS -V0 -I0 -RTS` to every command (matches the existing T17904 run:
  `-V0` zeroes the timer, `-I0` disables idle GC, for determinism). Do **not** add
  the stats `-t`/`--machine-readable` RTS opts — cgwork measures Ir, not allocs.
  Add `-fforce-recomp` so a warm `.o`/`.hi` can't short-circuit the compile.

## Deliverables

Two scripts plus an output report, all under the fresh worktree `$WT`
(`~/ghc/work-trees/cgwork-perf-aggregate/`). The profiled binary and test sources
come from head0, not from `$WT`.

1. `$WT/cg_batch.py` — drive cgwork per test.
2. `$WT/cg_aggregate.py` — parse all `.cg`, rank, split startup vs scaling.
3. `$WT/cg-out/` — raw `.cg` files, `manifest.json`, `aggregate.md`.

### 1. Batch driver — `cg_batch.py`

- Holds an explicit **per-test config table** (the self-contained part): a list of
  entries `{name, kind, srcname, flags, extra_files=[], pre_cmd=None}`. Authoring
  aid (one-time, not part of the recurring pipeline): cross-check `srcname`/`flags`
  against `all.T` and, if needed, harvest the exact command the framework builds by
  running the test once natively with the verbose driver (`TT_VERBOSE=1 ~/ghc/tt
  <name>` in head0) and copying the printed `ghc ... -c/--make ...` line. This keeps
  the table accurate without parsing all.T's Python.
- For each entry:
  1. Make a fresh per-test working dir under a batch root
     (`~/ghc/cfg-perf-tests/cg-out/<name>/`); copy in `<srcname>` + `extra_files`
     from `~/ghc/head0/testsuite/tests/perf/compiler/`.
  2. If `pre_cmd` set, run it there (generators emit the module tree).
  3. Invoke:
     `~/ghc/cgwork run $WT/cg-out/<name>.cg -- \
        ~/ghc/head0/_build/stage1/bin/ghc <to_do> <srcname> <flags> \
        -fforce-recomp +RTS -V0 -I0 -RTS`
     (`to_do` = `-c` for compile, `--make` for multimod).
  4. Record exit status + the COMPILER subtotal (reuse `cgwork.buckets`) into a
     manifest (`cg-out/manifest.json`): name, command, total/gc/rts/comp Ir,
     module count if known, ok/failed.
- **Resumable & bounded**: skip tests whose `.cg` already exists; cachegrind is
  ~50x, so run sequentially (or honour the shared build-slot semaphore —
  `~/ghc/buildslot.sh`) and expect an overnight/background batch
  (`run_in_background`). `compile_fail` tests still produce a profile; mark
  expected-failure so a non-zero exit isn't treated as an error.

### 2. Aggregator — `cg_aggregate.py`

- `import sys; sys.path.insert(0, '/home/simon/prog/ghc-personal'); import cgwork`.
- For each `<name>.cg`: `fns,_ = cgwork.parse(p)`; keep only `cgwork.is_comp(k)`;
  map each key through `cgwork.demangle`.
- Aggregate across all tests:
  - **Total Ir per function** (summed), ranked desc — the suite-wide hit list.
  - **Breadth**: number of tests each function appears in, and its
    min/max/mean share — distinguishes "hot everywhere" from "fluke in one".
  - **Per-test top-N** kept alongside, for drill-down.
- **Startup-vs-scaling split**: classify tests by size (e.g. module count or
  total comp Ir) into "trivial" (T17904-class) vs "large" (MultiLayerModules,
  ManyConstructors, ManyAlternatives, MultiComponentModules, large-project).
  Report each function's **absolute Ir in trivial modules** (≈ pay-per-invocation
  startup cost) vs **marginal Ir in large modules** (scales with real work).
  Functions flat across sizes = startup (matters for one-shot `-c`, amortised
  under `--make`); functions that grow = genuine compilation cost.
- Output: a ranked markdown/text table to stdout and a saved
  `cg-out/aggregate.md`. Columns: function, total Ir, % of suite comp Ir,
  #tests, startup-Ir, scaling-flag.

## Out of scope for batch 1 (note in the report)

`ghci_script` (6), `makefile_test` (T4007, T11068), and the TH/recomp/OneShot
multi-invocation variants — they fork several ghc processes and/or need a GHCi
driver, which cgwork's single-process pick doesn't represent cleanly. Revisit
once the core aggregate exists.

## Verification

**This implementation pass stops after step 3.** Build the scripts and run only
the three verification steps below; do **not** launch the full overnight batch.
Hand back for sign-off; step 4 happens in a later pass.

1. **Smoke test (1 test):** run `cg_batch.py` restricted to `T17904` (or
   `CoOpt_Read`); confirm a raw `.cg` lands in `$WT/cg-out/`, that
   `cgwork show $WT/cg-out/T17904.cg` reproduces the known buckets
   (~70.6M COMPILER for T17904), and that `parse` reads it.
2. **Two-test aggregate:** run a trivial (`T9020`) + a large (`ManyConstructors`)
   test, run `cg_aggregate.py`, eyeball that the startup-vs-scaling split puts
   config/iface-load functions in the startup column and Simplify/typecheck
   functions in the scaling column.
3. **Determinism check:** re-run one test; COMPILER subtotal should match to
   ~1e-4 (cgwork's stated precision). **← stop here.**

### Deferred to a later pass (after sign-off)

4. **Full batch:** launch in background, then run the aggregator; sanity-check the
   top of the ranked table against the three known hunches
   ([[sometyperep-ord-compare-iface-userdata-lookup]] etc.) — expect the
   startup-flagged ones (config `read`, iface userdata lookup) to drop in relative
   rank once large modules are weighted in.

## Follow-up

Worth a memory once results exist (ranked suite-wide hit list + which of the three
original hunches survive aggregation). Could extend cgwork itself with an
`aggregate` subcommand if this proves a recurring need.
