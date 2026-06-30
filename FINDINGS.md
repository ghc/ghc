# Systematic investigation of the suite's hot compiler functions

Goal: for each top hot function from the suite-wide cachegrind aggregate, decide
how to attack it on two axes — **(A) make the function more efficient** and
**(B) call it less** — using appropriate tools, not guesswork.

## Tools & method

| Question | Tool | Reliability here |
|---|---|---|
| Which function is hot? | cachegrind via cgwork (`aggregate.md`) | solid — flat per-symbol Ir |
| (A) Is the *body* inefficient? | generated Core in `~/ghc/head0/.../*.dump-simpl` | solid |
| (B) Who calls it, how often? | **callgrind** (`cg_callgraph.py` + `cg_callers.py`) | call **counts** solid; see limits |

Added on this branch:
- `cg_callgraph.py` — runs the prebuilt `~/ghc/head0` ghc under callgrind
  (`--cache-sim=no --branch-sim=no`, Ir only), reusing `cg_batch`'s test table /
  command builder / workdir prep; keeps the largest ghc child →
  `cg-out/cg/<test>.callgrind`.
- `cg_callers.py` — inverts the call graph: compiler-bucket self Ir + per-edge
  **call counts**, splitting self-recursion from external entry callers.

Profiled tests (callgrind, Ir total): ManyConstructors 17.5B, InstanceMatching1
76.9B, T16577 53.5B, MultiLayerModules 7.1B, T9872b 12.3B. Each test drives a
different part of the suite-wide hit list (driver map at the end).

## Three tool-boundary results that shaped the method (the methodological core)

1. **Inclusive cost is unusable.** GHC's hot workers are deeply *recursive*
   (`Word64Map.$winsert`, `elems1`, `seqType`, `coVarsOfType`, `eq_type_*`).
   callgrind attributes the whole nested subtree to each self-edge, so
   `callgrind_annotate` prints inclusive shares of 45,000 %–900,000 %. → discard
   inclusive; use **self Ir** (exact) and **call counts** (exact).

2. **The shipped `.so` is `.symtab`-stripped.** `nm --defined-only` → 0 symbols.
   So callgrind can only name *exported* functions; the rest show as `0x...`. In
   InstanceMatching1 ~37 % of self Ir sits in such `0x...` functions. `addr2line`
   *appears* to name them via DWARF but it anchors to the nearest DWARF
   subprogram and is **wrong**: it labelled a 1 KB+ span `Data.OldList.permutations`,
   yet the compiler never calls `permutations` (grep: only a code comment).
   Treat every `0x...→name` as a low-confidence guess. **Call counts, however,
   are exact regardless of symbolisation** — and they carry the story (below).

3. **Laziness hides the source caller.** Many hot calls arrive via
   `stg_ap_0_fast` / `stg_ap_pp_fast` / `stg_BLACKHOLE` (generic apply / thunk
   forcing), so the *source* caller is obscured. e.g. the UniqDFM comparator is
   99.5 % "called from `stg_ap_pp_fast`" = passed as a function to a sort.

→ Consequence: callgrind on the prebuilt binary reliably answers "how *often*,
and from which *exported* function." For trustworthy **source-level** "who calls
it" through thunks and stripped code, the appropriate next tool is a
**cost-centre profiled GHC** (`-fprof-auto` + `+RTS -p`): it threads a source
cost-centre stack through thunks and does proper cycle handling. `-ticky` gives
exact entry counts + per-function allocation as a lighter cross-check. Both need
one rebuild — the only heavyweight step, deferred pending sign-off.

## (A) axis — the bodies are already well-compiled

Reading the Core for the top functions: **none has an obvious body inefficiency.**
The cost is volume, not waste.

- `Word64Map.elems1` (`elems = foldr (:) []`): tight accumulating traversal,
  arity 2, strict in the map, plain cons, no boxing. Optimal.
- `TyCo.Compare.eq_type_expand_ignore_x`: **already** opens with
  `reallyUnsafePtrEquality#` (reflexive/shared short-circuit) before structural
  comparison. The cost is genuine compares of distinct-but-equal types.
- `Core.Type.seqType` / `$wseqTypes`: allocation-free structural walk returning
  `(# #)`. Exists only to force a type deeply. Pure (B).
- `Unique.DFM.$fFoldableUniqDFM2`: **not a fold** — it is the insertion-order
  comparator (`TaggedVal → TaggedVal → Bool`, unboxed `Int#`) used to **sort**
  when deterministically folding a `UniqDFM`. Body optimal; its cost is the
  O(n log n) **price of determinism** per fold → (B) lever.

So the leverage is overwhelmingly **(B)**, which is why caller attribution is the
deliverable.

## (B) axis — caller attribution (reliable parts)

### Word64Map / UniqDFM iteration  (elems1 #1, $fFoldableUniqDFM2 #3, $winsert #6)

- **InstanceMatching1 = deterministic UniqDFM/Word64Map folds.** `elems1`
  (25.75 % self, 203.9M calls) and the UniqDFM comparator (11.86 % self, 201.0M
  calls) have **matching call counts** → every deterministic fold both
  materialises the elements (`elems1`) and sorts them by insertion tag. Together
  ~37 % of the test. The reliably-named (small) entry callers reveal the source —
  **solver-state folds**: `Tc.Solver.Types.foldTcAppMap`,
  `Tc.Solver.InertSet.foldTyEqs`, `Unique.DSet.uniqDSetToList`,
  `Tc.Types.Evidence.foldEvBindsMap`. (The 98 %-share `0x39711d0` is the
  inlined/specialised bulk of the same; nearest-DWARF guess: `Word64Map.foldr`.)
  → (B) lever: repeated *ordered* traversal of solver maps.
- **`$winsert` = environment building** (ManyConstructors): 89 % self-recursion;
  external entries `extendVarSet1` 18.7 %, `extendNameEnvList1` 6.4 %,
  `extendNameSetList_go1` 5.0 %, `extendVarEnv1` 5.0 %, `initTidyOccEnv_go1` 3.7 %.
  → cost is building large Var/Name envs by repeated single insertion.

### Type forcing & free-vars  (seqType-family ~10 %, coVarsOfType(s) ~6.5 %)

- **T16577** is the driver: `seqType`+`$wseqTypes`+`seqTypes` ≈ 37 %,
  `coVarsOfTypes1`+`coVarsOfType1` ≈ 17.6 %. Internally these are
  mutual/self recursion (`seqTypes`→`$wseqTypes`, `coVarsOfType1`↔`coVarsOfTypes1`);
  external entries arrive via thunks (`stg_ap_0_fast`), with the co-resident hot
  functions being `tidyType`, `$wzonkTyVarOcc` → forcing/tidying/FV-scanning whole
  types during **zonk + tidy**. Source-level attribution here needs the profiled
  build (laziness).
- **`coVarsOfType(s)` (~6.5 %) is resolved**: the late-CCS profile pins 99 % of
  `coVarsOfType1`'s external entries on `$woccAnal` — i.e. `occAnal (Type ty)`
  (`OccurAnal:2583`) computing free CoVars of *every* `Type` argument for CoVar
  deadness. The walk is 0-alloc and the result is ~always ∅ (T16577's types are
  coercion-free), so it's a pure wasted deep traversal redone each occanal pass.
  Investigation + (B) levers (short-circuit/memoise coercion-free types):
  [[occanal-covarsoftype-coercion-free-walk]] in `~/ghc/todos`.

### Type-keyed maps & equality  (Core.Map.Type, eq_type)

- **T9872b** (type-family solving): `Core.Map.Type.$wgo` (7.65 %) is **79.3 %**
  from `$weqDeBruijnType` + 20.7 % from `splitAppTyNoView_maybe` → the cost is
  **de Bruijn type-key equality in the solver's TrieMaps**. `rewriteType2`
  (16.29 %) entered 10.8 % via `TyCon_con`. Cleanly symbolised.
- **`rewriteType2` = `rewrite_one`** (the central rewriter loop) is itself a
  T9872b driver: 8 top-level `rewrite` calls (← `can_eq_nc`) explode into 1.98 M
  node-rewrites via the fam-app FINISH-1 re-walk. Body already optimal (one-shot
  monad, famapp-cache); levers are a cheap per-tyvar rebuild short-circuit (A) and
  the hard "don't re-walk already-normal sub-structure" (B). Investigation +
  levers: [[rewriteType2-rewrite-one-tyvar-rebuild-and-famapp-rewalk]] in
  `~/ghc/todos`.
- **`ruleMatchTyKiX3` (0.59 %, `SCALES`) is the shared type matcher**, not RULE
  matching — it is `unify_ty`/`tc_unify_tys` specialized into `ruleMatchTyKiX`.
  Driver **T9872b**: 897,764 entries, ~86 MB suite-wide alloc (it is *not* a
  0-alloc walk — it allocates `UM`-monad plumbing). Caller chain
  `← tc_unify_tys ← $wtc_match_tys`/`reduceTyFamApp_maybe ← rewriteType2`, i.e.
  type-family rewriting again. Two (A) levers (the dead `()` in the
  `(UMState,())` result pair; the `CanEqLHS`/`UMEnv` boxing at `uVarOrFam` call
  sites — `uVarOrFam` is the #3 suite allocator at 158 MB) plus a (B) re-match
  hypothesis shared with the rewriter:
  [[unify-matcher-um-monad-pair-and-uvarorfam-boxing]] in `~/ghc/todos`.
- **`eq_type_expand_ignore` (suite #2, 6.81 %) is resolved**: driver **T8095**
  (98.7 % of entries — the deep type-family reduction), a 0-alloc structural walk
  with the `reallyUnsafePtrEquality#` fast path already present (A done). Late-CCS
  pins **100 %** of callers on a single source line: the *last-resort* Identity
  rule of `opt_trans_rule` (`Opt.hs:979`, `RedTypeDirRefl`), which runs a full
  `eqType (coercionLKind co1) (coercionRKind co2)` at every transitivity
  composition. 251 k checks → 187 M node compares (745× fanout, deep common
  prefix), and it returns `False` in ≥96 % of calls (mkReflCo entered only 10 k×),
  so it's ~O(n·depth) wasted re-walking of near-identical endpoint types. (The
  `assertPpr` at `Opt.hs:779` is compiled out in perf builds — not the culprit.)
  Investigation + (B) levers (guard the optional check / share endpoint types /
  reshape chain optimisation): [[opt-trans-rule-identity-eqtype-deep-walk]] in
  `~/ghc/todos`.
- **`castCoercionKind_go` (suite #15, 1.06 %) is the *construction half* of that
  same Identity rule** — not an independent opportunity. The mangled name hides two
  coercion-kind walkers: `castCoercionKind_go` = `coercionLKind`, `castCoercionKind_go1`
  = `coercionRKind` (the two inlined `coercion_lr_kind` copies). They **freshly
  materialise** the `ty1`/`ty2` that the rule's `eqType` then compares, so the
  earlier "no allocation" note undercounts the rule. **Driver is T13386** (95 %
  `$wopt_trans_rule`), and it *inverts* T8095: building `coercionLKind co1` = 27.1 M
  node-ops vs. the `eqType` walk's 1.0 M (shallow reject, reflexive 0.45 % of calls)
  — we build a deep type ~27x larger than the comparison needs, then reject it after
  ~4 nodes. So the rule's true cost = construction (#15) **+** comparison (#2); a
  comparison-only fix misses T13386. Folded into
  [[opt-trans-rule-identity-eqtype-deep-walk]] (construction-half section).

### Zonk type/coercion mapper  (`Tc.Zonk.Type` `mapTyCoX`)

- **`$wgo_tys` (suite #13, 1.14 %) is resolved**: it is the `[Type]` arm of
  `mapTyCoX` (the zonker's map). Driver **T9872b_defer** (67 % of entries), where
  `$wgo_tys` is the **#1 allocator in the whole test**: 22.3M entries / **417.6 MB**.
  The decisive ratio: across ~45M visited type/coercion nodes the mapper does only
  **273** tyvar substitutions and **6** coercion-hole fills — so essentially the
  entire alloc is the zonker **re-allocating `TyConApp` arg-lists/nodes structurally
  identical to their inputs**. Caller chain `go_tys ← go_ty(TyConApp) ← go_co`, i.e.
  driven by zonking the huge coercions in deferred-error evidence. Body already
  optimal (A done); lever is a `reallyUnsafePtrEquality#` identity short-circuit in
  the shared mapper — an **allocation** win (GC/bytes-allocated), Ir-neutral, and it
  also covers `Tc.Zonk.TcType`'s zonker. Investigation + lever:
  [[zonk-maptyco-identity-rebuild-ptr-eq]] in `~/ghc/todos`.

### Interface load/write  (MultiLayerModules)

- Dominated by `utf8CompareByteArray#` (17.6 %), `Lexer.Interface.$walexGetByte`
  (7.8 %), `Binary.$w$sputULEB1` (2.85 %), `Word64Map.insertWithKey`,
  `GenUnit.==` → interface read/write + relinking. These are the
  per-invocation/iface costs that the suite aggregate flagged as ~flat startup.

## Which test drives which suite-top function (driver map)

| suite-top function | best driver among profiled tests |
|---|---|
| `Word64Map.elems1` (#1) | InstanceMatching1 |
| `eq_type_expand_ignore` (#2) | **T8095** (187M entries; see ticky section) |
| `$fFoldableUniqDFM2` (#3) | InstanceMatching1 |
| `seqType` / `$wseqTypes` / `seqTypes` | T16577 |
| `Word64Map.$winsert` (#6) | ManyConstructors |
| `coVarsOfType(s)` | T16577 |
| `Core.Map.Type` / de Bruijn eq | T9872b |
| `Tc.Zonk.Type.$wgo_tys`/`go_ty` (zonk mapper) | **T9872b_defer** (67 % of `go_tys` entries) |
| iface (utf8/alex/ULEB, GenUnit==) | MultiLayerModules |

## Ticky phase — source-level entry counts + an allocation axis

callgrind on the prebuilt head0 binary stalled at two walls (laziness routes calls
through `stg_ap_*`; the `.so` is `.symtab`-stripped, so ~37 % of one test was
`0x...`). The fix is a **ticky-instrumented ghc built at the same commit**
(`3b2a9409ba`): `~/ghc/ticky`, flavour `perf+ticky_ghc+no_profiled_libs`. The ticky
RTS reports, per STG closure, exact **entry counts** and **bytes allocated**, keyed
by the real module-qualified definition site. Tooling: `ticky_run.py` (reuses
`cg_batch`'s table, runs the ticky ghc with `+RTS -r`) → `ticky-out/<test>.ticky`;
`ticky_top.py` (ranks compiler closures by entries or `--alloc`).

### The headline: entries and allocation are *decoupled* (a new axis)

The cachegrind Ir profile conflated "does lots of cheap work" with "allocates a
lot". Ticky separates them, and they point at *different* functions:

- **Highest entry counts = pure structural walks that allocate nothing.**
  `seqType`-family (T16577: `$wseqTypes` 300M, `seqType` 158M, `seqTypes` 143M),
  `eq_type_expand_ignore` (T8095: 187M + its list-companion `gos2` 187M),
  `coVarsOfTypes1` (T16577: 152M) — **0 B allocated** each. Confirms axis (A):
  bodies are optimal; the only lever is (B) call them less.
- **Highest allocation = list/map building, NOT the hot walkers.** Suite-wide
  alloc ranking (7 tests):

  | Alloc (B) | Entries | closure |
  |--:|--:|---|
  | **11.23 G** | 401.7M | `Word64Map.elems1` |
  | 1.55 G | 36.5M | `Word64Map.$winsert` |
  | 0.51 G | 2.9M | `Parser.Lexer.Interface.$walexGetByte` |
  | 0.33 G | 1.3M | `Core.Unify.uVarOrFam` |
  | 0.30 G | 17.4M | `Utils.Misc.strictMap` |

### Per-test source-level attribution (what was hidden before)

- **InstanceMatching1 — `elems1` allocates 11.2 GB** (400.99M entries) against the
  determinism comparator `$fFoldableUniqDFM2` at 200.04M entries / 0 B. Every
  deterministic `UniqDFM` fold materialises its element list (`foldr (:) []`) *and*
  sorts it by insertion tag. This single list-materialisation is the largest cost
  source in the whole investigation — invisible to Ir (cons is Ir-cheap), enormous
  in bytes. Lever (B): avoid materialising/ordering these solver-map folds.
- **T8095 = the `eq_type_expand_ignore` driver** (187.1M entries, **0 B**), entered
  via `eqTypeIgnoringMultiplicity` only 251k times → ~745 recursive node-compares
  per top-level type comparison. Pure volume; body already has the
  `reallyUnsafePtrEquality#` fast path.
- **T16577** confirms the `seqType`/`coVars` walks are 0-alloc volume; the test's
  *allocation* is elsewhere — `$winsert` 536 MB, `strictMap` 189 MB, `tidyType`
  112 MB, `$wdelete` 101 MB (tidy-env building, not the forcing walk).
- **ManyConstructors** — `$winsert` 7.4M entries / **312 MB**: env-building by
  repeated single insertion, now confirmed as the test's dominant allocator.
- **MultiLayerModules** — allocation is the **iface lexer**: `$walexGetByte` 359 MB,
  `alex_scan_tkn` 69 MB, `insertWithKey` 88 MB (callgrind had flagged
  `utf8CompareByteArray#` by Ir; ticky shows the *bytes* are in the Alex scanner).
- **T9872b** — top entry closure is `$wgo1 (Tc.Solver.Rewrite)` (3.7M / 130 MB),
  the rewriter worker callgrind could only see as `0x...`; de Bruijn map cluster
  confirmed and now quantified (`$wxtT` 107 MB, `$fTrieMapBndrMap_$s$sxtG` 76 MB).
- **`strictMap` is ubiquitous** (every test; 300 MB aggregate): `map` that forces
  and re-conses the spine — a broad, cross-cutting allocation source.

### Reproduce (ticky)

```
cd ~/ghc/work-trees/cgwork-perf-aggregate
python3 ticky_run.py <TEST>...                   # -> ticky-out/<TEST>.ticky
python3 ticky_top.py --file <TEST> --top 15      # top compiler closures by entries
python3 ticky_top.py --alloc --top 15            # suite-wide, ranked by allocation
python3 ticky_top.py <FUNC_SUBSTR>... --file <TEST>   # rows matching a name
```

## Profiled (late-CCS) phase — caller attribution, with corrections

callgrind's call graph hit the laziness wall (the *logical* source caller hides
behind `stg_ap_*`/`0x...`); ticky gave no caller context at all. A cost-centre
profile threads the source cost-centre through thunks, so it answers "who calls it"
directly. Build: `~/ghc/prof-late`, flavour **`perf+profiled_ghc`** at the same
commit `3b2a9409ba`. `profiled_ghc = enableLateCCS . enableProfiledLibs`, and
`enableLateCCS` adds **`-fprof-late`** — cost centres inserted *after* optimisation,
so attribution is on the real optimised program (plain `-fprof-auto` would block the
inlining/strictness we found optimal). Tooling: `prof_run.py` (runs the profiled ghc
with `+RTS -V0 -I0 -po<stem> -pj`, JSON profile) and `prof_callers.py` (walks the
cost-centre *stack* tree: per target cc, external callers by entries + inherited
alloc). With `-V0` the time sampler is off (ticks 0) but **entries and alloc are
exact** — and they match ticky's counts to the digit (e.g. T9872b `$wgo1` 3,725,671;
`$winsert` 1,555,168), confirming late-CCS does not distort the program.

### It corrected two callgrind-era inferences

| hot fn (test) | TRUE source caller (late-CCS) | earlier inference |
|---|---|---|
| `elems1` 11.2 GB (InstanceMatching1) | **`RoughMap.lookupRM'` 99.9 %** (399.96M entries / 11.20 GB) — instance-match rough-filter materialising map elements | *wrong*: "deterministic solver-map folds (foldTcAppMap…)" — those are real but tiny (3.8 MB) |
| `seqType`-family 602M (T16577) | **Simplifier**: `simplExprF1-App` 68M, `$wseqIds` 57M, `seqBndr`/`seqExpr` 15M ea. | *wrong*: "zonk + tidy forcing whole types" (was co-residence, not causation) |
| `coVarsOfType` 232M (T16577) | **`$woccAnal` (OccurAnal) 79.5M** + `coVarsOfCo` recursion | *wrong*: "zonk + tidy" |
| `$fFoldableUniqDFM2` 200M (InstanceMatching1) | `actualSort` — a real `sortBy`, a **separate** op from `elems1` | *conflated* with elems1 ("matching call counts → same fold") |

### It confirmed and sharpened the rest

- `eq_type_expand_ignore` 187M (T8095) ← `eqTypeIgnoringMultiplicity` (251k) ←
  **`opt_trans_rule` (Coercion.Opt)**: the coercion optimiser's trans rule does 251k
  type-equality checks that fan out to 187M node-compares (0 alloc).
- de Bruijn key eq 999k (T9872b) ← `lkBndr` lookups (616k) + `$sxtG` inserts (371k)
  in the solver's TrieMaps.
- `$winsert` 316 MB (ManyConstructors): **broad** env-building, no single driver —
  `extendVarSet` 62 MB, `TypeEnv` 28 MB, `NameSet` 22 MB, `StgToCmm.Env.addBindC`
  21 MB, CoreToStg/Prep 17.5 MB ea., then NameEnv/VarEnv/UFM/tidyOccName…
- `alexGetByte` 358 MB (MultiLayerModules) ← 100 % `alex_scan_tkn`: the iface lexer.

**Lesson:** caller attribution by *static co-residence / nearest-symbol guessing*
(what callgrind forced us into) misdirected the two largest (B) levers. The
late-CCS profile is the tool that resolves "who, through laziness", and entry counts
cross-validate exactly against ticky, so the two together (entries+alloc at the
definition site, plus caller stacks) are the trustworthy combination.

### Reproduce (profiled / late-CCS)

```
cd ~/ghc/work-trees/cgwork-perf-aggregate
python3 prof_run.py <TEST>...                       # -> prof-out/<TEST>.prof (JSON)
python3 prof_callers.py --file <TEST> --top 20      # flat compiler ccs by alloc
python3 prof_callers.py <FUNC_SUBSTR>... --file <TEST> [--by alloc]   # caller stacks
```

## Reproduce (callgrind)

```
cd ~/ghc/work-trees/cgwork-perf-aggregate
python3 cg_callgraph.py run <TEST>...            # produce cg-out/cg/<TEST>.callgrind
python3 cg_callers.py --top 12 --file <TEST>     # compiler self-cost ranking
python3 cg_callers.py <FUNC_SUBSTR>... --file <TEST>   # caller call-count breakdown
```
