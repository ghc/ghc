# Slim EPS: overview of the branch

Branch `wip/fendor/pit-copy-small-slices`, on top of `e2dade8a61` ("Lazy copy
of byte strings"). This is a proof of concept. Hard cases are skipped with a
comment and, where behaviour would be wrong, a `panic`.

**Goal.** In `--make` mode, reduce the memory that the External Package State
(EPS) retains after interfaces of external packages have been loaded:

1. Remove the Package Interface Table (`eps_PIT :: ModuleEnv ModIface`) from
   the EPS. Replace it with a slim per-module record.
2. Make sure nothing in the EPS retains the byte buffer of a whole `.hi` file.

## Result

Measured by compiling Cabal-syntax with `-O1 -j1` (`bench/eps-memory`, see
`README.md`). Values are absolute; the percentage is relative to HEAD. "MiB"
means 2^20 bytes; values in MB (10^6, as `eventlog2html` shows them) are in
brackets.

**End of the build** (after `--make` finished and a major GC ran):

| Metric | HEAD | this branch |
|---|---:|---:|
| RTS live bytes | 202.3 MiB [212.1 MB] | 186.9 MiB [196.0 MB] (−7.6%) |
| Reachable from the EPS ¹ | 108.4 MiB [113.6 MB] | 91.6 MiB [96.0 MB] (−15.5%) |
| Byte arrays reachable from the EPS | 37.5 MiB [39.3 MB] | 18.1 MiB [19.0 MB] (−51.8%) |
| Byte arrays ≥ 16 KiB (whole `.hi` buffers) | 30.1 MiB [31.6 MB] (374) | 1.8 MiB [1.9 MB] (58) (−93.9%) |
| `eps_PIT` / `eps_ext_ifaces`, reachable ¹ | 66.6 MiB [69.8 MB] | 33.8 MiB [35.4 MB] (−49.2%) |
| `eps_PTE`, reachable ¹ | 94.8 MiB [99.4 MB] | 77.3 MiB [81.1 MB] (−18.4%) |
| bytes allocated | 80523.2 MiB [84434.7 MB] | 82144.5 MiB [86134.7 MB] (+2.0%) |

¹ The traversal stops at `EPS` and `HscEnv` constructors. Lazy typechecking
thunks retain the session, and through its IORef the whole EPS.

**Peak** (`run-peak.sh`: heap census every 0.25 s, so a major GC every 0.25 s):

| Metric | HEAD | this branch |
|---|---:|---:|
| Max live bytes (`eventlog2html`'s peak) | 303.1 MiB [317.8 MB] | 287.7 MiB [301.7 MB] (−5.1%) |
| Largest `-hT` census total | 230.5 MiB [241.7 MB] | 207.6 MiB [217.6 MB] (−10.0%) |
| `ARR_WORDS` in the census at the peak | 44.3 MiB [46.5 MB] | 18.7 MiB [19.6 MB] (−57.9%) |
| Max heap size | 814.0 MiB [853.5 MB] | 784.0 MiB [822.1 MB] (−3.7%) |
| RTS `max_bytes_used`, plain run with `+RTS -F1.2` | 239.4 MiB [251.0 MB] | 224.7 MiB [235.7 MB] (−6.1%) |
| RTS `max_bytes_used`, plain run (default GC settings) | 215.5 MiB [225.9 MB] | 206.6 MiB [216.6 MB] (−4.1%) |

Observations:

* **The peak goes down by about as much as the end state.** It happens near
  the end of the build, during `CodeGen` of
  `Distribution.PackageDescription.FieldGrammar`. The saving there is
  entirely in byte arrays: the `.hi` buffers.
* **Plain-run `max_bytes_used` is unreliable.** It is the maximum over only
  ~20 major GCs per build, which miss the few seconds of the real peak. That
  is why the earlier single plain runs suggested an unchanged peak. Use
  `run-peak.sh` or `+RTS -F1.2` to measure the peak.
* **Compile time was not measured reliably.** There was a single, contended
  run per variant: wall time −2.5%, which is within noise.
* **What still holds big byte arrays:**
  * The lazily kept `mi_docs` slices, 37 arrays and about 1.2 MiB, retained
    via `ExternalIface.ei_docs`. Keeping the docs was a deliberate choice.
  * About 0.7 MiB via `eps_PTE`, presumably large single `IfaceId` copies
    (big unfoldings). This is not verified.

## The logical steps (commits)

### 1. `ghc-debug: only start the server on request; add a pause-at-end hook` (benchmark support)

`ghc/Main.hs` only.

* The ghc-debug server starts only when `GHC_DEBUG_SOCKET` is set. Before, it
  always printed to stderr, which broke the testsuite, and all ghc processes
  shared one socket.
* With `GHC_DEBUG_PAUSE_AT_END` set, ghc does the following at the end:
  1. saves `[EPS, HscEnv]` as ghc-debug roots;
  2. performs a major GC;
  3. prints `ghc-debug: live_bytes=N`;
  4. pauses forever.

  We cannot use `gcRoots` after a self-pause: the RTS refuses with
  "rts_listThreads: called from a different OS thread than rts_pause".

### 2. `Binary: fix putFullBinData …; factor out copyBinReaderSlice` (preparatory, bug fix of HEAD)

`compiler/GHC/Utils/Binary.hs` only.

* **Bug in the HEAD commit.** After `fbd_off_e` was removed, `putFullBinData`
  wrote `fbd_size` (the whole buffer) bytes starting at `fbd_off_s`. For an
  interface read from disk, `fbd_off_s` is the header length, which is
  greater than 0, so it read past the end of the buffer. It now writes
  `fbd_size - fbd_off_s` bytes.
* `copyBinReaderSlice` copies a region `[start, end)` of a `ReadBinHandle` into
  its own buffer. `lazyGetCopy'` now uses it.
* Strictness changes, so that no thunk retains the old handle and its buffer:
  * `fbd_readerUserData` is strict;
  * `addReaderToUserData` forces its record update.

### 3. `Replace the PIT by slim ExternalIfaces in the EPS` (the main refactoring)

**New module `GHC.Unit.External.Iface`.** See Note [The EPS does not keep
ModIfaces].

* `ExternalIface` is a strict record. It holds:
  * identity: module, `sig_of`, `hsc_src`;
  * exports;
  * fixities, plus an `OccEnv`;
  * warnings, plus strict lookup functions;
  * `IfaceAbiHashes`;
  * a strict decl-hash `OccEnv`, a strict replacement for `mi_hash_fn`, whose
    cache thunk used to retain all of `mi_decls`;
  * dependencies;
  * trust;
  * docs, lazily.
* `mkExternalIface` builds it from the interface returned by the plugins'
  `interfaceLoadAction`.
* `emptyExternalIface` is the negative cache entry.
* `LoadedIface = LoadedHome ModIface | LoadedExternal ExternalIface`, with
  `li_*` accessors.

**EPS.** `eps_PIT` is replaced by `eps_ext_ifaces :: ModuleEnv ExternalIface`.
`PackageIfaceTable` and `emptyPackageIfaceTable` are gone, and
`lookupExternalIface` is new.

**API changes** (types now `LoadedIface`):

* `loadInterface`, `loadSysInterface`, `loadUserInterface`, `loadSrcInterface[_maybe]`,
  `loadInterfaceForName/Module`, `loadModuleInterface`, `loadPluginInterface`,
  `loadInterfaceWithException`;
* `lookupIfaceByModule[Hsc]`, which now takes the EPS rather than the PIT;
* `GHC.Tc.Module.getModuleInterface`, `hscGetModuleInterface`;
* `LoadedPlugin.lpModule`, `ImportLookupBad`, `lookupRdrNameInModuleForPlugins`;
* `GHC.Rename.Names`: `calculateAvails`, `importsFromIface`, `filterImports`
  helpers, `badImportItemErr`.

**Call sites.** These switch from `mi_*` to `li_*`:

* renamer: `Rename.Names`, `Utils`, `Fixity`, `Env`;
* recompilation: `Iface.Recomp` (`checkModUsage`, `mkHashFun`, `getOrphanHashes`);
* `HsToCore.Usage`, `Tc.Instance.Family` (`checkFamInstConsistency`);
* Safe Haskell: `hscCheckSafe'`, duplicated in `Driver/Main/Passes.hs` and
  `Interactive.hs`;
* Backpack: `checkImplements`, `Iface.Rename`;
* `Tc.Deriv.Utils`, `Tc.Errors.Hole`, `Tc.Module` (orphans);
* TH docs; `Runtime.Eval` (`getDocs`); plugin loading; `ghc --abi-hash`;
  `HsToCore` / `Iface.Make` (plugin modules).

**Dropped fields.** These are re-read from disk via the new
`readFullIfaceFromDisk` (`computeInterface` without touching the EPS):

* `mi_top_env`, for TH `reifyModule`;
* the full `ModIface` of package modules for the GHC API (`modInfoIface`, used
  by GHCi `:issafe`).

`mi_usages` and self-recompilation info are dropped outright.

**Behavioural change.** The EPS now remembers what the plugin's
`interfaceLoadAction` returned. Before, only the first caller of
`loadInterface` saw the plugin's version; later lookups got the unmodified PIT
entry. `plugins14` relies on the first-load behaviour.

### 4. `Don't retain .hi buffers from the EPS` (the memory fix)

* **IfaceType table.** `getTables` copies the bytes of the IfaceType table into
  their own buffer before decoding it lazily. This was the main retention
  path: every lazily read slice reached the table through its
  `ReaderUserData`, and each table entry held the whole file buffer. The
  table's extent comes from the layout written by `putAllTables`, from the
  target of the IfaceType pointer to the target of the Name pointer. See
  Note [Copying the IfaceType table].
* **`IfaceId`.** The lazy `(type, details, idinfo)` uses `lazyGetCopy`, so an
  unforced Id in `eps_PTE` keeps only its own bytes.
* **Bytecode action.** `loadIfaceByteCode` / `iface_core_bindings` force
  `mi_module` instead of keeping lazy selector thunks on the whole `ModIface`.

Not needed: copying the `IfacePublic` sub-fields separately. `tcIfaceDecls`
and `mkExternalIface` force all of them, so the copied `public` slice dies.

### 5. Benchmark framework (`bench/eps-memory`)

* `freeze-compiler.sh`, `run-bench.sh`;
* Haskell tools `eps-flags` (hie-bios), `eps-analyse` (ghc-debug) and
  `eps-compare`;
* `README.md`.

Results and frozen compilers are git-ignored.

### 6. Cherry-pick: `One-shot mode: home-unit interfaces go to the HPT, never the EPS` (from `wip/fendor/mhu-linking`, `4c44e66e05`)

* In one-shot mode, demand-loaded home interfaces are typechecked into
  `ModDetails` and added to the HPT.
* Their instances, family instances, rules, annotations, COMPLETE pragmas
  and bytecode go into a per-HPT `HomeLoadedState` (new
  `GHC.Unit.Home.LoadedState`).
* `eps_is_boot` moves into `HomeLoadedState` as `hls_is_boot`.
* Consumers read the EPS pool together with the home pool.

Conflict resolution:

* For home modules, `loadInterface` returns `LoadedHome`. The HPT keeps a
  trimmed `ModIface`, built from the plugins' result.
* An `ExternalIface` is built, and forced, only for external modules.

### 7. `Resolve the "one-shot PIT access" TODOs`

* `lookupIfaceByModule`: the comment no longer has a one-shot case.
* `readFixedModule`: checks the HUG/EPS again before reading the `.hi`,
  using `li_deps`.
* Haddock `loadHiFile`: the instances come from the module's HPT
  `ModDetails`. The declarations still require re-reading the `.hi`. A failed
  re-read now reports the interface error instead of panicking.

### 8. `One-shot: don't count demand-loaded home instances twice in runTcInteractive`

`runTcInteractive` adds the instances of all HPT modules (`hugAllInstances`).
In one-shot mode those are exactly the demand-loaded modules, whose instances
are also in the `HomeLoadedState` pool. As a result, `haddockHtmlTest` and
`haddockLatexTest` showed every instance twice. `runTcInteractive` now skips
`hugAllInstances` in one-shot mode, and the Note documents this.

## Re-evaluation after steps 6-8 (variant `oneshot-hpt`)

| Metric | head | slim-eps | oneshot-hpt |
|---|---:|---:|---:|
| Reachable from the EPS | 108.4 MiB [113.6 MB] | 91.6 MiB [96.0 MB] (−15.5%) | 91.6 MiB [96.0 MB] (−15.5%) |
| Byte arrays ≥ 16 KiB from the EPS | 30.1 MiB [31.6 MB] | 1.8 MiB [1.9 MB] (−93.9%) | 1.8 MiB [1.9 MB] (−93.9%) |
| RTS live bytes at the end | 202.3 MiB [212.1 MB] | 186.9 MiB [195.9 MB] (−7.6%) | 186.9 MiB [196.0 MB] (−7.6%) |
| Max live bytes (`run-peak.sh`) | 303.1 MiB [317.8 MB] | 286.6 MiB [300.5 MB] (−5.4%) | 287.7 MiB [301.7 MB] (−5.1%) |
| bytes allocated (`--make`) | 80523.2 MiB [84434.7 MB] | 80605.4 MiB [84520.9 MB] (+0.1%) | 82144.5 MiB [86134.7 MB] (**+2.0%**) |

* **EPS memory is unchanged by the cherry-pick**, as expected: `--make` mode
  doesn't demand-load home interfaces.
* **The cherry-pick costs about 2% allocation in `--make` mode.** Allocation
  is deterministic, so this is real. Every `tcGetInstEnvs`,
  `tcGetFamInstEnvs` and rule-env query now does the following, even though
  the pools are always empty in `--make`:
  1. folds over the IORefs of all home units' `HomeLoadedState`;
  2. unions the result into the environment.

  Short-circuiting this when not in one-shot mode, or when the pool is empty,
  should recover it. That isn't done yet.

Tests on the final tree, `default+ipe --freeze1`: 0 unexpected failures in
driver (419, including the new `OneShotHomeHpt`), backpack, plugins, haddock,
th, indexed-types, simplCore, typecheck, safeHaskell, rename, ghci and
annotations.

### 9. GHC.Prim, haddock, `ifaceReadHook`

* **`Remove the GHC.Prim special case …`**
  * Why it was special: `GHC.Internal.Prim` has a virtual interface. `readIface`
    replaces its `.hi` with `getGhcPrimIface`.
  * It is a home module only while `ghc-internal` itself is compiled:
    * In `--make` mode, it is an ordinary module of the graph, and the virtual
      interface goes into the HPT.
    * In one-shot mode, the cherry-pick still excluded it from the home path.
  * Now it is demand-loaded into the HPT like any other home module. Its pools
    are empty.
  * Also removed: the out-of-date "unsavoury" filter in `hugSomeThingsBelowUs`.
  * Verified by a full devel2+ipe build, which compiles `ghc-internal` in
    one-shot mode with the new compiler.
* **`haddock: take default-method names from ModDetails …`**
  * Haddock re-read every `.hi` only to get the default-method `Name`s from
    `mi_decls`. It now takes them from `md_types`.
  * `--no-compilation` reads each `.hi` exactly once, checked with
    `-ddump-if-trace`.
* **`Add ifaceReadHook …`**
  * `ifaceReadHook :: Maybe (FilePath -> ModIface -> IO ())` is an observe-only
    hook. `readIface` calls it with the full interface for every interface read
    from disk.
  * Covered by the test `ghc-api/IfaceReadHook`.
  * The changelog entry `changelog.d/slim-eps-api` lists the API changes.

## What is open / hacky

* **No home module is in the EPS any more,** not even `GHC.Prim`.
* **`--make` allocation regression (+2%)** from the `HomeLoadedState`
  lookups. This is out of scope for now.
* **`eps_known_keys`** may still hold `GHC.Essentials` entries of a home unit.
* **`eps_free_holes`** (Backpack) is a separate side cache.
* **Trimmed home `ModIface`.** The HPT entries of demand-loaded home modules
  are `ModIface`s whose `mi_decls`, `mi_insts` and similar fields are panics.
  Nothing reads those fields any more, but the type doesn't express it.
* **Re-reading from disk.** `modInfoIface` of package modules and TH
  `reifyModule` re-read `.hi` files. With `ifaceReadHook`, API users can
  instead capture full interfaces as they are read.
* **Plugin semantics.** The EPS remembers the `interfaceLoadAction` result.
* **Peak memory** goes down by 5.1% (`run-peak.sh`). The plain-run
  `max_bytes_used` misses the peak, see the Result section.
* **Not measured:** pinned fragmentation from the per-`IfaceId` copies, and
  compile time.
