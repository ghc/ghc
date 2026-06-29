# Investigating a hot GHC compiler function (perf + profiled_ghc)

A recipe for: *"Investigate function `xyz` in GHC using this guide."* Run everything
from `~/ghc/work-trees/cgwork-perf-aggregate`. Background and worked examples are in
`FINDINGS.md`; this is the operational short form.

## The framing: two axes

Every hot function is attacked on exactly two levers. Decide which one(s) apply
before proposing anything:

- **(A) make the body cheaper** — boxing, a missing fast path, a lazy accumulator,
  redundant re-allocation. Checked by *reading the generated Core*.
- **(B) fix the callers** — the function is already optimal but its callers use it
  wastefully. This is usually the bigger lever, and it has three depths, from
  shallowest to deepest:
  1. *Call it fewer times* — memoise, hoist out of a loop, deduplicate.
  2. *Call it on smaller/cheaper data* — the caller passes more, or bigger, than the
     result actually depends on.
  3. **Question the requirement** — walk the (transitive) hot callers and ask what
     each one *actually needs* from the call. Often the caller over-asks: it forces a
     whole structure when a shallow check suffices, materialises and orders a list it
     only folds over, or compares full types when a cheap predicate would do. If the
     caller's real need can be met another way, the hot function isn't called less —
     it isn't called at all. (This is where the largest wins in this suite live, e.g.
     `RoughMap.lookupRM'` materialising a full element list it only filters.)

A third, cross-cutting axis falls out of the data: **entries vs. allocation are
decoupled**. The highest-*entry* functions (`seqType`, `eq_type_*`, `coVarsOfType`)
allocate **0 B** — pure walks, only (B) helps. The highest-*allocation* functions
are list/map *materialisation* (`Word64Map.elems1` = 11.2 GB), often invisible in a
cachegrind Ir profile because a cons is Ir-cheap. Always look at both numbers.

## Build & data sources (all at commit `3b2a9409ba`)

| What | Where |
|---|---|
| Profiled (late-CCS) GHC | `~/ghc/prof-late/_build/stage1/bin/ghc` (`perf+profiled_ghc`, `-fprof-late`) |
| Ticky GHC | `~/ghc/ticky/_build/stage1/bin/ghc` (`perf+ticky_ghc+no_profiled_libs`) |
| Plain perf GHC (Ir + Core) | `~/ghc/head0/_build/stage1/bin/ghc` |
| **Generated Core** of the compiler | `~/ghc/head0/_build/stage1/compiler/build/compiler/<Module/Path>.dump-simpl` |
| Suite-wide cachegrind Ir ranking | `cg-out/aggregate.md` (which functions are hot suite-wide, SCALES vs startup) |
| Late-CCS caller profiles (all 82 tests) | `prof-out/<TEST>.prof` (JSON cost-centre stack tree) |
| Ticky entry+alloc profiles | `ticky-out/<TEST>.ticky` (all 82 tests) |
| Per-test command table | `cg_batch.py` `TESTS` (drives every runner) |

`-fprof-late` inserts cost centres *after* optimisation, so attribution is on the
real optimised program; `+RTS -V0` turns off the time sampler but **entries and
alloc stay exact** and match ticky to the digit.

## Procedure

1. **Locate the function & its driver test.** Find where `xyz` is hottest:
   - `grep` it in `cg-out/aggregate.md` for its suite rank, % of suite Ir, and the
     `SCALES`/`startup` flag (startup = fixed per-invocation cost, usually not worth
     chasing).
   - Pick the test that *drives* it. The driver map at the end of `FINDINGS.md`
     covers the known top functions; otherwise scan ticky/prof across tests:
     `python3 ticky_top.py xyz` (aggregated) or `python3 prof_callers.py xyz --file <T>`
     across a few candidate tests to see which test entry-count is largest.

2. **Get the two definition-site numbers (entries & alloc).** Ticky, keyed to the
   real definition site even through thunks:
   ```
   python3 ticky_top.py xyz --file <TEST>            # entries + Alloc + Alloc'd
   python3 ticky_top.py --alloc --file <TEST>        # what this test allocates most
   ```
   (`ticky-out/` is complete; `python3 ticky_run.py <TEST> --force` to regenerate.)
   High entries + 0 alloc ⇒ pure walk ⇒ (B) only. High alloc ⇒ ask *what structure
   is being built* (list, env, map) and whether it must be.

3. **Read the Core for axis (A).** Open the defining module's `*.dump-simpl` under
   `~/ghc/head0/.../compiler/build/compiler/` and read the worker (`$w…`). Look for:
   boxing of an `Int#`/`Word#`, a `foldr (:) []` that materialises a list, a
   re-`cons`ing `map` (`strictMap`), a missing `reallyUnsafePtrEquality#` short
   circuit, an accumulator that isn't strict. If the body is a tight unboxed
   traversal with a fast path already present, declare (A) done and move to (B).

4. **Caller attribution for axis (B)** — the late-CCS profile, which pierces laziness
   (the *logical* source caller, not the `stg_ap_*` thunk that forced it):
   ```
   python3 prof_callers.py xyz --file <TEST>             # external callers by entries
   python3 prof_callers.py xyz --file <TEST> --by alloc  # ... by inherited alloc
   python3 prof_callers.py --file <TEST> --top 20        # flat top compiler ccs (context)
   ```
   This names *who* drives the entries/alloc and how concentrated it is (one caller
   at 99 % vs. broad fan-out across many). That concentration decides the fix.

   Then **don't stop at the immediate caller — interrogate it.** Climb the hot callers
   (rerun `prof_callers.py` on the dominant caller itself, and read its source) and for
   each ask: *what does this caller actually need from `xyz`?* Does it consume the whole
   result or one field; the ordered list or just a fold/membership test; a deep
   comparison or a shallow one? Where the caller over-asks, the fix lives there, not in
   `xyz` — the deepest (B) win is replacing the call with one that matches the real
   requirement (or removing it).

## What "performance opportunities" to look for

Map the findings onto one of these recurring patterns (each seen in this suite):

- **List/map materialisation** — a deterministic fold that builds and orders an
  element list (`Word64Map.elems1` ← `RoughMap.lookupRM'`, 11.2 GB). Lever: stream /
  fold without materialising, or avoid the ordering.
- **Env building by repeated single insertion** — `Word64Map.$winsert` from
  `extendVarSet`/`extendNameEnv` one element at a time. Lever: bulk/`fromList`
  construction, or a cheaper structure.
- **Pure structural re-walks** — `seqType`, `eq_type_expand_ignore`, `coVarsOfType`:
  0-alloc, body optimal, but entered 10²–10⁸×. Lever: cache/memoise the result, hoist
  the walk out of a loop, or avoid forcing/comparing whole types when a cheaper
  predicate suffices. Watch for **fanout** (T8095: 251k top-level compares → 187M node
  compares) — the win is upstream, in the caller that triggers the fanout.
- **Price of determinism** — `$fFoldableUniqDFM2` is a `sortBy` for deterministic
  UniqDFM folds. Lever: does this fold need to be deterministic/ordered here?
- **Cross-cutting allocators** — `strictMap`, the iface Alex lexer (`$walexGetByte`).
  Broad, no single caller; flag as systemic rather than point-fixable.

Output: state the rank/%, the driver test, the entries & alloc, the (A) verdict from
Core, the dominant caller(s) from the late-CCS profile, and a concrete (A) and/or (B)
lever — with the concentration that justifies it.

## Pitfalls (don't repeat these)

- **Don't trust callgrind inclusive cost** for GHC's recursive workers (it prints
  45,000 %+); and on the shipped `.so`, `addr2line`/nearest-symbol guesses are wrong
  (it named a span `Data.OldList.permutations`, which the compiler never calls). Use
  self-Ir + exact call counts, or prefer ticky/late-CCS, which key to the real site.
- **Co-residence ≠ causation.** Two hot functions appearing together does not mean one
  calls the other (callgrind-era guesses "seqType ← zonk+tidy" and "elems1 ← solver
  folds" were both wrong; the truth was Simplifier and `RoughMap.lookupRM'`). Confirm
  callers with the late-CCS stack, not by what's nearby in the ranking.
- **A trivial single-module profile overweights startup.** Use `cg-out/aggregate.md`'s
  SCALES flag to tell pay-per-invocation startup from work that scales.
