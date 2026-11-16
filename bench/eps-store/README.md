# eps-store: low-memory External Package State stores

GHC's External Package State (EPS) is an abstract, query-only handle (see
Note [Abstract External Package State] in `compiler/GHC/Unit/External/Store.hs`).
This package uses the GHC API to replace GHC's in-memory store with one that
keeps the declarations of loaded interfaces **out of the Haskell heap**, and
rehydrates them on demand.

## Design

`EpsStore.LowMem` delegates everything to GHC's in-memory store except the
declarations:

* On `eps_addModule`, the module's `IfaceDecl`s go to a `DeclStore`. In memory
  we keep only an index (`OccName` → declaration, including implicit binders),
  each declaration's hash (which replaces the `ExternalIface` hash cache, since
  that retains the declarations), and the module's `epm_rehydrate` closure. The
  lazily typechecked `epm_decls` are dropped without being forced.
* On `eps_lookupTyThing`, a miss fetches the declaration from the `DeclStore`
  and typechecks it again with `epm_rehydrate`. The result (the declared thing
  and its implicit things) is cached:
  * first as `Pending` (a strong reference to the still-lazy `TyThing`);
  * once it is forced, as a **weak pointer keyed on the inner object** (the
    `TyCon`, `Id`, `DataCon`, ...), not on the `TyThing` box, which callers
    drop right away.

  So a declaration stays in memory exactly as long as GHC references it.
  Afterwards, a later lookup rehydrates it again. That copy is equal to the
  old one by `Unique`, but not shared with it.
* Instances, family instances, rules, annotations and COMPLETE pragmas stay in
  the in-memory store: they are snapshot queries (pure environments).

Declaration stores (`EpsStore.DeclStore`):

| Store | `ds_put` | `ds_get` |
|---|---|---|
| `EpsStore.Sqlite` | serialises each decl (`putWithUserData`) into a row of a temporary SQLite DB | `SELECT` + `getWithUserData` with the session's `NameCache` |
| `EpsStore.HiFile` | records (module, `.hi` path, `OccName`), nothing else | re-reads the `.hi` file (`readIfaceCopying True`); keeps the decls of the last 16 files |

## Building

It must be built with the compiler of this tree:

```sh
bench/eps-store/cabal.sh build all                         # _build (devel2)
BUILD_ROOT=_default+ipe bench/eps-store/cabal.sh build all # optimised build
```

## Running

```sh
exe=$(cd bench/eps-store && ./cabal.sh list-bin -v0 ghc-eps-store)
$exe -B$(_build/stage1/bin/ghc --print-libdir) --eps-store=sqlite --eps-stats \
     [ghc --make flags, @response-files and targets]
```

`--eps-store=memory|sqlite[:PATH]|hi`. `--eps-stats` prints store
statistics, and the live bytes after a major GC at the end of the build
(the session, and with it the EPS, is still alive then).

## Benchmarking

```sh
bench/eps-store/run-stores.sh LABEL [STORE...]
```

compiles a package (default: Cabal-syntax, flags from
`bench/eps-memory/work/slim-eps/ghc.rsp`) once per store with the
`_default+ipe` build. It compares the outputs (only meaningful with
`GHC_OPTS="-j1 -fobject-determinism"`) and writes
`results/LABEL/summary.md`.

### Results (2026-11-16, Cabal-syntax `-O1 -j1 -fobject-determinism`)

| store | live at end (MiB) | max live (MiB) | allocated (MiB) | rehydrations | wall (s) |
|---|---:|---:|---:|---:|---:|
| memory | 184.6 | 217.1 | 81089.0 |  | 53.0 |
| hi | 166.9 (−9.6%) | 197.3 | 83614.6 (+3.1%) | 11797 | 55.8 |
| sqlite | 163.7 (−11.3%) | 193.8 | 83262.4 (+2.7%) | 13907 | 55.1 |

All `.hi` and `.o` outputs are identical across stores. These are single runs
(wall times are noisy), and max live is the RTS maximum over major GCs only,
which can miss the real peak (see `bench/eps-memory/README.md`).
